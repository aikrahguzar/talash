{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a split searcher and seeker, a simple server-client version of the search in which searcher runs in the background and can communicate
--   with clients using named pipes. The searcher reads input as a UTF8 encoded bytestring from one named pipe and outputs the search results to another named
--   pipe. It quits when it receives an empty bytestring as input. The main function for starting the server is `runSearch` while a simple client is provided by
--   `askSearcher`. One motivation for this is be able to use this library as search backend for some searches in Emacs (though the implementation may have to wait
--  for a massive improvement in my elisp skills).
module Talash.Piped ( -- * Types and Lenses
                      PipedSearcher (..) , query , allMatches , CaseSensitivity (..)
                    --   -- * Searcher
                    , searchLoop , runSearch , runSearchStdIn  ,  runSearchStdInDef , showMatchColor
                    --   -- * Seeker
                    , askSearcher
                    --   -- * Default program
                    , run , run'
                    --   -- * Exposed Internals
                    , withNamedPipes , send , recieve)
where

import qualified Data.ByteString.Char8 as B
import Data.Monoid.Colorful
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import GHC.Compact
import System.Directory
import System.Environment (getArgs)
import System.Exit
import System.IO hiding (print , putStrLn , putStr)
import System.Posix.Files
import System.Posix.Process
import Talash.Chunked hiding (makeMatcher , send)
import Talash.Intro
import Lens.Micro.TH (makeLenses)

data PipedSearcher = PipedSearcher { _inputHandle :: Handle
                                   , _outputHandle :: Handle
                                   , _maximumMatches :: Int
                                   , _printStrategy :: SearchReport -> Bool
                                   , _printer :: Handle -> [Text] -> IO ()}
makeLenses ''PipedSearcher

printMatches :: forall n m a. (KnownNat n , KnownNat m) => SearchFunctions a Text -> PipedSearcher -> Chunks n
                                                                -> SearchReport -> MatcherSized m a -> MatchSetSized m -> IO ()
printMatches funcs searcher store r m s = when ((searcher ^. printStrategy) r) (T.hPutStrLn out (T.pack . show $ r ^. nummatches) *> traverse_ doPrint s *> hFlush out)
  where
    out = searcher ^. outputHandle
    doPrint (ScoredMatchSized _ c v) =  (searcher ^. printer) out . (funcs ^. display) (const id) m (store ! c) $ v

pipedEnv :: KnownNat n => SearchFunctions a Text -> PipedSearcher -> Chunks n -> IO (SearchEnv n a Text)
pipedEnv funcs searcher = searchEnv funcs (searcher ^. maximumMatches) (printMatches funcs searcher)

search :: KnownNat n => IO () -> Handle -> SearchEnv n a b -> IO ()
search fin inp env = finally (startSearcher env *> (loop =<< T.hGetLine inp)) fin
  where
    loop ""    = stopSearcher env
    loop query = sendQuery env query *> (loop =<< T.hGetLine inp)

-- | Outputs a matching candidate for the terminal with the matches highlighted in blue. Uses the `Colored` `Text` monoid from `colorful-monoids` for coloring.
showMatchColor :: Handle -> [Text] -> IO ()
showMatchColor o t = (hPrintColored (\h -> B.hPutStr h . encodeUtf8) o Term8 . fst . foldl' go (Value "" , False) $ t) *> B.hPutStrLn o ""
  where
    go (c , False) n = (c <> Value n , True)
    go (c , True ) n = (c <> Style Bold (Fg Blue (Value n)) , False)

defSearcher :: Handle -> Handle -> PipedSearcher
defSearcher ih oh = PipedSearcher ih oh 4096 (\r -> r ^. ocassion == QueryDone) showMatchColor

-- | Run an IO action that needs two handles to named pipes by creating two named pipes, opening the handles to them performing the action
--   and then cleaning up by closing the handles and deleting the named pipes created. The names of the pipes are printed on the stdout and are of the
--   form @\/tmp\/talash-input-pipe@ or @\/tmp\/talash-input-pipe\<n\>@ where n is an integer for the input-pipe and @\/tmp\/talash-output-pipe@ or
--   @\/tmp\/talash-output-pipe\<n\>@ for the output pipe. The integer @n@ will be the same for both.
withNamedPipes :: (IO () -> Handle -> Handle -> IO a) -> IO a
withNamedPipes f = doAct =<< openP =<< ifM ((||) <$> fileExist i <*> fileExist o) (go 1) ((,) <$> mkp i <*> mkp o)
  where
    i = "/tmp/talash-input-pipe"
    o = "/tmp/talash-output-pipe"
    doAct (fi , fo , ih , oh) = f (hClose ih *> hClose oh *> removeFile fi *> removeFile fo) ih oh
    openP (ip , op) = (\ih oh -> (ip , op , ih , oh)) <$> openFile ip ReadWriteMode <*> openFile op ReadWriteMode
    mkp p = createNamedPipe p stdFileMode *> print p  $> p
    go n = ifM ((||) <$> fileExist i' <*> fileExist o') (go $ n + 1) ((,) <$> mkp i' <*> mkp o')
      where
        i' = i <> show n
        o' = o <> show n

-- | Run search create a new session for the searcher to run in, forks a process in which the `searchLoop` is run in the background and exits.
runSearch :: KnownNat n => IO () -> SearchFunctions a Text -> PipedSearcher -> Chunks n -> IO ()
runSearch fin funcs searcher c = createSession *> forkProcess (search fin (searcher ^. inputHandle) =<< pipedEnv funcs searcher c) *> exitImmediately ExitSuccess

-- | Version of `runSearch` in which the vector of candidates is built by reading lines from stdin.
runSearchStdIn :: KnownNat n => Proxy n -> IO () -> SearchFunctions a Text -> PipedSearcher -> IO ()
runSearchStdIn p fin funcs searcher = runSearch fin funcs searcher . getCompact =<< compact . forceChunks =<< chunksFromHandle p stdin

-- | Version of `runSearchStdIn` which uses `showMatch` to put the output on the handle.
runSearchStdInDef :: SearchFunctions a Text -> IO ()
runSearchStdInDef funcs = withNamedPipes (\fin ih -> runSearchStdIn (Proxy :: Proxy 32) fin funcs . defSearcher ih) -- runSearchStdIn f showMatch

-- Send a query to the searcher by writing the text in the second argument to the named-pipe with path given by the first argument.
-- Does not check if the file is a named pipe.
send :: String -> Text -> IO ()
send i q = ifM (fileExist i) (withFile i WriteMode (`B.hPutStrLn` encodeUtf8 q)) (putStrLn $ "the named pipe" <> T.pack i <> " does not exist")

-- Read the results from the searcher from the named pipe with the path given as the argument. Does not check if the file exists or is a named pipe.
recieve :: String -> IO ()
recieve o = withFile o ReadMode (\h -> (go h . readMaybe . B.unpack =<< B.hGetLine h))
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
run' ["load"]               = runSearchStdInDef (orderlessFunctions IgnoreCase)
run' ["load" , "fuzzy"]     = runSearchStdInDef (fuzzyFunctions IgnoreCase)
run' ["load" , "orderless"] = runSearchStdInDef (orderlessFunctions IgnoreCase)
run' ["find" , x]           = askSearcher "/tmp/talash-input-pipe"  "/tmp/talash-output-pipe" . T.pack $ x
run' ["find" , n , x]       = askSearcher ("/tmp/talash-input-pipe" <> n)  ("/tmp/talash-output-pipe" <> n) . T.pack $ x
run' ["find" , i , o , x]   = askSearcher i o . T.pack $ x
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
