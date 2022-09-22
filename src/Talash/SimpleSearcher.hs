-- |

module Talash.SimpleSearcher where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import Data.Monoid.Colorful
import qualified Data.Text as T
import Data.Text.AhoCorasick.Automaton (CaseSensitivity(..))
import qualified Data.Vector as V
import GHC.TypeLits
import Lens.Micro
import qualified System.IO.Streams as I
import Talash.Chunked
import Talash.Core hiding (match , makeMatcher)
import Talash.Files
import Talash.Intro hiding (splitAt)
import Talash.ScoredMatch
import GHC.Compact (Compact , compact , getCompact)

data SimpleSearcher = SimpleSearcher {terms :: [Text] , sleepTime :: Int , matchesToPrint :: Int}

-- | Outputs a matching candidate for the terminal with the matches highlighted in blue. Uses the `Colored` `Text` monoid from `colorful-monoids` for coloring.
showMatchColor :: Handle -> [Text] -> IO ()
showMatchColor o t = (hPrintColored (\h -> B.hPutStr h . encodeUtf8) o Term8 . fst . foldl' go (Value "" , False) $ t) *> B.hPutStrLn o ""
  where
    go (c , False) n = (c <> Value n , True)
    go (c , True ) n = (c <> Style Bold (Fg Blue (Value n)) , False)

printMatches :: forall n m a. (KnownNat n , KnownNat m) => SearchFunctions a Text -> Chunks n -> SearchReport -> MatcherSized m a -> MatchSetSized m -> IO ()
printMatches funcs store r m s = when (o == QueryDone || o == NewQuery) (putStrLn $ (T.pack . show $ n) <> " Matches for this round.\n")
  where
    o = r ^. ocassion
    n = r ^. nummatches

printMatchesMvar :: forall n m a. (KnownNat n , KnownNat m) => SearchFunctions a Text -> MVar () -> Chunks n -> SearchReport -> MatcherSized m a -> MatchSetSized m -> IO ()
printMatchesMvar funcs v store r m s = when (r ^. ocassion == QueryDone) (putMVar v () *> putStrLn ((T.pack . show $ r ^. nummatches) <> " matches for this round.")
                                                  *> traverse_ (\(ScoredMatchSized _ c v) -> showMatchColor stdout . (funcs ^. display) (const id) m (store ! c) $ v) s)

simpleFuzzyEnv :: KnownNat n => Int -> Proxy n -> V.Vector Text -> IO (SearchEnv n MatchPart Text)
simpleFuzzyEnv n _ = searchEnv (fuzzyFunctions IgnoreCase) n (printMatches (fuzzyFunctions IgnoreCase)) . makeChunks

simpleFuzzyEnvM :: KnownNat n => MVar () -> Int -> Proxy n -> V.Vector Text -> IO (SearchEnv n MatchPart Text)
simpleFuzzyEnvM m n _ = searchEnv (fuzzyFunctions IgnoreCase) n (printMatchesMvar (fuzzyFunctions IgnoreCase) m) . makeChunks

simpleFuzzyEnvMI :: KnownNat n => MVar () -> Int -> Proxy n -> Chunks n -> IO (SearchEnv n MatchPart Text)
simpleFuzzyEnvMI m n _ = searchEnv (fuzzyFunctions IgnoreCase) n (printMatchesMvar (fuzzyFunctions IgnoreCase) m)

runSimpleSearcherWithEnv :: KnownNat n => SimpleSearcher -> SearchEnv n MatchPart Text -> IO ()
runSimpleSearcherWithEnv s env = forkIO (searchLoop env) *> traverse_ (doSearch . Just) (terms s) *> doSearch Nothing
  where
    doSearch term = putMVar (env ^. query) term *> threadDelay (sleepTime s)

runSimpleSearcher :: KnownNat n => Proxy n -> SimpleSearcher -> V.Vector Text -> IO ()
runSimpleSearcher p s v = runSimpleSearcherWithEnv s =<< simpleFuzzyEnv (matchesToPrint s) p v

runSimpleSearcherWithEnvM :: KnownNat n => SimpleSearcher -> MVar () -> SearchEnv n MatchPart Text -> IO ()
runSimpleSearcherWithEnvM s v env = forkIO (searchLoop env) *> traverse_ doSearch (terms s) *> putMVar (env ^. query) Nothing
  where
    doSearch term = unless (term == "") $ putMVar (env ^. query) (Just term) *> takeMVar v

runSimpleSearcherM :: KnownNat n => Proxy n -> SimpleSearcher -> V.Vector Text -> IO ()
runSimpleSearcherM p s v = (\mvar -> runSimpleSearcherWithEnvM s mvar =<< simpleFuzzyEnvM mvar (matchesToPrint s) p v) =<< newEmptyMVar

runSimpleSearcherMI :: KnownNat n => Proxy n -> SimpleSearcher -> Chunks n -> IO ()
runSimpleSearcherMI p s v = (\mvar -> runSimpleSearcherWithEnvM s mvar =<< simpleFuzzyEnvMI mvar (matchesToPrint s) p v) =<< newEmptyMVar

testVector :: IO (V.Vector Text)
testVector = I.toVector =<< I.decodeUtf8 =<< I.lines I.stdin

simpleSearcherTest :: IO ()
simpleSearcherTest = runSimpleSearcherMI (Proxy :: Proxy 64)
                                         (SimpleSearcher [ "m" , "ma" , "mal" , "malda" , "maldac" , "maldace" , "maldacen" , "maldacena" , "maldacenaf" , "maldacenafi" , "maldacenafiv" , "maldacenafive"
                                                         , "maldacenafiv"
                                                         , "w" , "wi" , "wit" , "witt" , "witte" , "witten" , "f" , "fr" , "fra" , "fran" , "franc" , "franco"
                                                         , "c" , "cl" , "clo" , "clos" , "closs" , "closse" , "closset" , "s" , "se" , "sen"
                                                        ] 25000 256)
                                         . forceChunks =<< chunksFromStream =<< I.decodeUtf8 =<< I.lines I.stdin
-- simpleSearcherTest = runSimpleSearcher (Proxy :: Proxy 32) (SimpleSearcher ["suse", "linux" , "binary" , "close" , "Witten" , "Maldacena" , "Franco" , "Closset"] 100000 1024) =<< testVector
