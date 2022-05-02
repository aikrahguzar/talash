{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a split searcher and seeker, a simple server-client version of the search in which searcher runs in the background and can communicate
--   with clients using named pipes. The searcher reads input as a UTF8 encoded bytestring from one named pipe and outputs the search results to another named
--   pipe. It quits when it receives an empty bytestring as input. The main function for starting the server is `runSearch` while a simple client is provided by
--   `askSearcher`. One motivation for this is be able to use this library as search backend for some searches in Emacs (though the implementation may have to wait
--  for a massive improvement in my elisp skills).
module Talash.Piped ( -- * Types and Lenses
                      SearchResult (..) , query , allMatches , matches , IOPipes (..) , CaseSensitivity (..)
                    ,  makeMatcher , lister , displayer , searchFunctionOL , searchFunctionFuzzy , searchFunctionFuzzyCustom , searchFunctionOLCustom
                      -- * Searcher
                    , searchLoop , runSearch , runSearchStdIn  ,  runSearchStdInDef , runSearchStdInColor , showMatch , showMatchColor
                      -- * Seeker
                    , askSearcher
                      -- * Default program
                    , run , run'
                      -- * Exposed Internals
                    , response , event , withIOPipes , send , recieve
                    , searchWithMatcher , readVectorStdIn , readVectorHandle , readVectorHandleWith , emptyIndices) where

import qualified Data.ByteString.Char8 as B
import Data.Monoid.Colorful
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Compact
import Intro
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO hiding (print , putStrLn , putStr)
import System.Posix.Files
import System.Posix.Process
import Talash.Core hiding (makeMatcher)
import Talash.Internal

data SearchResult = SearchResult { _query :: Maybe Text -- ^ The query that was searched for.
                                 , _allMatches :: U.Vector Int -- ^ The vector contaning the filtered indices of the candidates using the query.
                                 , _matches :: V.Vector [Text] -- ^ The matches obtained using the query.
                                 } deriving (Show , Eq)
makeLenses ''SearchResult

data IOPipes = IOPipes { input :: Handle -- ^ The handle to the named piped on which the server receives input to search for.
                       , output :: Handle -- ^ The handle to the named piped on which the searcher outputs the search results.
                       }

response :: SearchFunction -- ^ The functions determining how to match.
              -> V.Vector Text -- ^ The vector of candidates.
              -> Text -- ^ The text to match
              -> SearchResult -- ^ The last result result. This is used to determine which candidates to search among.
              -> SearchResult
response f v t s
  | maybe False (`T.isInfixOf` t) (s ^. query)  = go . f v (Just t) $ s ^. allMatches
  | otherwise                                   = go . f v (Just t) $ U.enumFromN 0 (V.length v)
  where
    go (a , (_ , m)) = SearchResult (Just t) a m

-- | One search event consisiting of the searcher reading a bytestring from the input named pipe. If the bytestring is empty the searcher exists. If not it
--   outputs the search results to the output handle and also returns them.
--
--   The first line of the output of results is the query. The second is an decimal integer @n@ which is the number of results to follow. There are @n@ more lines each
--  contaning a result presented according the function supplied.
event :: SearchFunction -> (Handle -> [Text] -> IO ()) -- ^ The functions that determines how a results is presented. Must not introduce newlines.
          -> IOPipes -- ^ The handles to the named pipes
          -> V.Vector Text -- ^ The candidates
          -> SearchResult -- ^ The last search result
          -> IO (Maybe SearchResult) -- ^ The final result. The Nothing is for the case if the input was empty signalling that the searcher should exit.
event f g (IOPipes i o) v s = (\t -> if T.null t then pure Nothing else go t) . T.strip . decodeStringLenient =<< B.hGetLine i
  where
    go t     = (\s' -> pream s' *> V.mapM_ (g o) (s' ^. matches) *> hFlush o $> Just s') . response f v t $ s
    pream s' = B.hPutStrLn o (encodeString . fromMaybe "" $ s' ^. query) *> B.hPutStrLn o (show . V.length $ s' ^. matches)

-- | Starts with the dummy `initialSearchResult` and handles `event` in a loop until the searcher receives an empty input and exits.
searchLoop :: SearchFunction -> (Handle -> [Text] -> IO ())  -> V.Vector Text -> IO ()
searchLoop f g v = withIOPipes (\p -> go p . Just . initialSearchResult $ v)
  where
    go p = maybe (pure ()) (go p <=<  event f g p v)

-- | The dummy `SearchResult` use as the initial value. Contains an empty query, all the indices and no matches.
initialSearchResult :: V.Vector Text -> SearchResult
initialSearchResult v = SearchResult Nothing (U.enumFromN 0 (V.length v)) V.empty

-- | Outputs the parts of a matching candidate to handle as space separated double quoted strings alternating between a match and a gap. The first text is
-- always a gap and can be empty the rest should be non-empty
showMatch :: Handle -> [Text] -> IO ()
showMatch o = B.hPutStrLn o . foldl' (\b n -> b <> " \"" <> encodeString n <> "\" ") ""

-- | Outputs a matching candidate for the terminal with the matches highlighted in blue. Uses the `Colored` `Text` monoid from `colorful-monoids` for coloring.
showMatchColor :: Handle -> [Text] -> IO ()
showMatchColor o t = (hPrintColored (\h -> B.hPutStr h . encodeString) o Term8 . fst . foldl' go (Value "" , False) $ t) *> B.hPutStrLn o ""
  where
    go (c , False) n = (c <> Value n , True)
    go (c , True ) n = (c <> Style Bold (Fg Blue (Value n)) , False)

-- | Run an IO action that needs two handles to named pipes by creating two named pipes, opening the handles to them performing the action
--   and then cleaning up by closing the handles and deleting the named pipes created. The names of the pipes are printed on the stdout and are of the
--   form @\/tmp\/talash-input-pipe@ or @\/tmp\/talash-input-pipe\<n\>@ where n is an integer for the input-pipe and @\/tmp\/talash-output-pipe@ or
--   @\/tmp\/talash-output-pipe\<n\>@ for the output pipe. The integer @n@ will be the same for both.
withIOPipes :: (IOPipes -> IO a) -> IO a
withIOPipes f = doAct =<< openP =<< ifM ((||) <$> fileExist i <*> fileExist o) (go 1) ((,) <$> mkp i <*> mkp o)
  where
    i = "/tmp/talash-input-pipe"
    o = "/tmp/talash-output-pipe"
    doAct (fi , fo , p@(IOPipes ip op)) = finally (f p) (hClose ip *> hClose op *> removeFile fi *> removeFile fo)
    openP (ip , op) = (\h g -> (ip , op , IOPipes h g)) <$> openFile ip ReadWriteMode <*> openFile op ReadWriteMode
    mkp p = createNamedPipe p stdFileMode *> print p  $> p
    go n = ifM ((||) <$> fileExist i' <*> fileExist o') (go $ n + 1) ((,) <$> mkp i' <*> mkp o')
      where
        i' = i <> show n
        o' = o <> show n

-- | Run search create a new session for the searcher to run in, forks a process in which the `searchLoop` is run in the background and exits.
runSearch :: SearchFunction -> (Handle -> [Text] -> IO ()) -> V.Vector Text -> IO ()
runSearch f g v = createSession *> forkProcess (searchLoop f g v) *> exitImmediately ExitSuccess

-- | Version of `runSearch` in which the vector of candidates is built by reading lines from stdin.
runSearchStdIn :: SearchFunction -> (Handle -> [Text] -> IO ()) -> IO ()
runSearchStdIn f g = runSearch f g . getCompact =<< compact . V.force =<< readVectorStdIn

-- | Version of `runSearchStdIn` which uses `showMatch` to put the output on the handle.
runSearchStdInDef :: SearchFunction  -> IO ()
runSearchStdInDef f = runSearchStdIn f showMatch

-- | Version of `runSearchStdIn` for viewing the matches on a terminal which uses `showMatchColor` to put the output on the handle.
runSearchStdInColor :: SearchFunction -> IO ()
runSearchStdInColor f = runSearchStdIn f showMatchColor

-- Send a query to the searcher by writing the text in the second argument to the named-pipe with path given by the first argument.
-- Does not check if the file is a named pipe.
send :: String -> Text -> IO ()
send i q = ifM (fileExist i) (withFile i WriteMode (`B.hPutStrLn` encodeString q)) (putStrLn . convertString $ "the named pipe" <> i <> " does not exist")

-- Read the results from the searcher from the named pipe with the path given as the argument. Does not check if the file exists or is a named pipe.
recieve :: String -> IO ()
recieve o = withFile o ReadMode (\h -> B.hGetLine h *> (go h . readMaybe . B.unpack =<< B.hGetLine h))
  where
    go h = maybe (putStrLn "Couldn't read the number of results.") (\n -> replicateM_ n (B.putStrLn =<< B.hGetLine h))

-- Do one round of sending a qeury to the searcher and receiving the results.
askSearcher :: String -- ^ Path to the input named pipe to which to write the query.
  -> String -- ^ Path to the output named pipe from which to read the results.
  -> Text -- ^ Th qeury itself.
  -> IO ()
askSearcher ip op q = if q == "" then send ip q else send ip q *> recieve op

-- | run' is the backend of `run` which is just `run\' =<< getArgs`
run' :: [String] -> IO ()
run' ["load"]               = runSearchStdInColor (searchFunctionOL IgnoreCase)
run' ["load" , "fuzzy"]     = runSearchStdInColor (searchFunctionFuzzy IgnoreCase)
run' ["load" , "orderless"] = runSearchStdInColor (searchFunctionOL IgnoreCase)
run' ["find" , x]           = askSearcher "/tmp/talash-input-pipe"  "/tmp/talash-output-pipe" . convertString $ x
run' ["find" , n , x]       = askSearcher ("/tmp/talash-input-pipe" <> n)  ("/tmp/talash-output-pipe" <> n) . convertString $ x
run' ["find" , i , o , x]   = askSearcher i o . convertString $ x
run' ["exit"]               = askSearcher "/tmp/talash-input-pipe"  "/tmp/talash-output-pipe" ""
run' ["exit" , n]           = askSearcher ("/tmp/talash-input-pipe" <> n)  ("/tmp/talash-output-pipe" <> n) ""
run' ["exit" , i , o]       = askSearcher i o ""
run' xs                     = (\t -> printColored putStr t usageString) =<< getTerm

usageString :: Colored Text
usageString =   "talash piped is a set of commands for loading data into a talash instance or searching from a running one. \n"
             <> "The input pipe for default instance is at " <> Fg Green " /tmp/talash-input-pipe " <> " and the output pipe is at " <> Fg Green " /tmp/talash-output-pipe \n"
             <> "The input and output pipes for the <n>-th default instances are at " <> Fg Green " /tmp/talash-input-pipe<n> and " <> Fg Green " /tmp/talash-output-pipe<n> \n"
             <> Fg Blue " talash piped load" <> " : loads the candidates from the stdin (uses orderless style) for later searches. \n"
             <> Fg Blue " talash piped load fuzzy" <> " : loads the candidates and uses fuzzy style for search \n"
             <> Fg Blue " talash piped load orderless" <> " : loads the candidates and uses orderless style for search \n"
             <> "All the load command print the input and output pipes for their instances on the stdout."
             <> Fg Blue " talash piped find <x>" <> " : prints the search results for query <x> from the already running default instance \n"
             <> Fg Blue " talash piped find <i> <o> x" <> " : prints the search results for query <x> from the instance with input pipe <i> and output pipe <o>\n"
             <> Fg Blue " talash piped find <n> <x>" <> " : prints the search results for query <x> from the <n>-th default instance.\n"
             <> Fg Blue " talash piped exit" <> " : causes the default instance to exit and deletes its pipes.\n"
             <> Fg Blue " talash piped exit <n>" <> " : causes the <n>-th instance to exit and deletes its pipes.\n"
             <> Fg Blue " talash piped exit <i> <o>" <> " : causes the instance at pipes <i> and <o> to exist and deletes the pipes.\n"
             <> " A running instance also exits on the usage of a find command with empty query. \n"

-- | run is a small demo program for the piped search. Run `talash piped` to see usage information.
run :: IO ()
run = run' =<< getArgs
