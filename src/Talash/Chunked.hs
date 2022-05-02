-- |
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Talash.Chunked where

import Brick.Widgets.List
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Data.Bit
import Data.Maybe (fromJust)
import qualified Data.Set as DS
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed.Sized as S
import GHC.TypeLits
import Intro hiding (splitAt)
import Lens.Micro
import System.IO (stdout)
import qualified System.IO.Streams as I
import Talash.Core hiding (match , makeMatcher)
import Talash.Files
import Talash.Internal hiding (makeMatcher)
import Talash.Piped (showMatchColor)
import Talash.ScoredMatch

newtype Chunks (n:: Nat) = Chunks {chunks ::  V.Vector (SV.Vector n Text)} deriving (Eq , Ord , Show)
newtype MatchSetG a = MatchSetG {matchset :: Set a} deriving (Eq , Ord , Show , Semigroup , Monoid , Foldable)
type MatchSetSized n = MatchSetG (ScoredMatchSized n)

instance Splittable MatchSetG where
  splitAt n = bimap MatchSetG MatchSetG . DS.splitAt n . matchset

data SearchStateSized (n :: Nat) a = SearchStateSized { _currentQuery :: {-# UNPACK #-} !Text
                                                      , _prevQuery :: {-# UNPACK #-} !Text
                                                      , _chunkNumber :: {-# UNPACK #-} !Int
                                                      , _canSend ::  !Bool
                                                      , _matchSet :: !(MatchSetSized n)}
makeLenses ''SearchStateSized

data SearchFunctions a = SearchFunctions { _makeMatcher :: Text -> Maybe (Matcher a)
                                         , _match :: forall n. KnownNat n => MatcherSized n a -> Text -> Maybe (MatchFull n)
                           -- | Given the matcher @m@, the matched string @t@ and the indices of matches in @t@ divide @t@ in alternating strings that are a matches
                           --   and the gap between these matches. The first of these is always a gap and can be empty. The rest should be non empty.
                                         , _display :: forall n. KnownNat n => MatcherSized n a -> Text -> S.Vector n Int -> [Text] }
makeLenses ''SearchFunctions

-- | The constant environment in which the search runs.
data SearchEnv n a = SearchEnv { _searchFunctions :: SearchFunctions a  -- ^ The functions used to find and display matches.
                               , _send :: forall n m. (KnownNat n , KnownNat m) => Bool -> Chunks n -> MatcherSized m a -> MatchSetSized m -> IO ()
                               , _maxMatches :: Int
                               , _candidates :: Chunks n
                               , _query :: MVar (Maybe Text)
                               , _allMatches :: M.IOVector (S.Vector n Bit) }
makeLenses ''SearchEnv

{-# INLINABLE  (!) #-}
(!) :: KnownNat n => Chunks n -> ChunkIndex -> Text
(!) (Chunks v) (ChunkIndex i j) = V.unsafeIndex (SV.fromSized $ V.unsafeIndex v i) j

{-# INLINABLE  union #-}
union :: Int -> MatchSetSized n -> MatchSetSized n -> MatchSetSized n
union n (MatchSetG s1) (MatchSetG s2) = MatchSetG (DS.take n $ DS.union s1 s2)

{-# INLINE  isNull #-}
isNull :: MatchSetSized n -> Bool
isNull (MatchSetG s) = DS.null s

{-# INLINE  getChunk #-}
getChunk :: Int -> Chunks n -> SV.Vector n Text
getChunk i (Chunks f) = V.unsafeIndex f i

{-# INLINEABLE  matchChunk #-}
matchChunk :: forall n m a. (KnownNat n , KnownNat m) => (MatcherSized n a -> Text -> Maybe (MatchFull n)) -> MatcherSized n a -> Int -> SV.Vector m Text
  -> S.Vector m Bit -> (S.Vector m Bit , MatchSetSized n)
matchChunk fun m = go
  where
    go ci v i = map (MatchSetG . DS.delete zero . DS.fromList . S.toList) . S.unzip . S.withVectorUnsafe (U.imap mtch) $ i
      where
        zero  = ScoredMatchSized 0 (ChunkIndex 0 0) $ S.replicate 0
        mtch _ (Bit False) = (Bit False , zero)
        mtch j (Bit True) = maybe (Bit False , zero) (conv j) . fun m . SV.unsafeIndex v $ j
        conv j (MatchFull k v) = (Bit True , ScoredMatchSized (Down k) (ChunkIndex ci j) v)

{-# INLINABLE resetMatches #-}
resetMatches :: forall n m a. KnownNat n => SearchEnv n a -> SearchStateSized m a -> IO ()
resetMatches env state
  | T.isInfixOf (state ^. prevQuery) (state ^. currentQuery) = pure ()
  | otherwise                                                = M.set (env ^. allMatches) (S.replicate 1)

{-# INLINABLE  searchNextChunk #-}
searchNextChunk :: (KnownNat n , KnownNat m) => SearchEnv n a -> MatcherSized m a -> SearchStateSized m a -> IO (SearchStateSized m a)
searchNextChunk env matcher state = nextstate . getMatches =<< M.read (env ^. allMatches) i
  where
    i          = state ^. chunkNumber
    getMatches = matchChunk (env ^. searchFunctions . match) matcher i (getChunk i (env ^. candidates))
    nextstate (js , mtchs) = M.write (env ^. allMatches) i js $> (over chunkNumber (+ 1) . updateAndSend . mergedMatches (state ^. matchSet) $ mtchs)
      where
        mergedMatches (MatchSetG curr) (MatchSetG new) = if not (DS.null new) && (DS.size curr < env ^. maxMatches || DS.lookupMax curr > DS.lookupMin new)
                                                         then Just . MatchSetG . DS.take (env ^. maxMatches) . DS.union curr $ new else Nothing
        updateAndSend = maybe (set canSend False state) (\mset -> set canSend True (set matchSet mset state))

matcherLoop :: (KnownNat n , KnownNat m) => SearchEnv n a -> Text -> Text -> MatcherSized m a -> IO (Maybe Text)
matcherLoop env qry prev matcher = resetMatches env initstate *> loop initstate
  where
    initstate = SearchStateSized qry prev 0 False (MatchSetG DS.empty)
    loop state = step =<< tryTakeMVar (env ^. query)
      where
        step x
          | Just Nothing <- x      = doSend True $> Nothing
          | inrange , Nothing <- x = doSend False *> (loop =<< searchNextChunk env matcher state)
          | Just (Just t) <- x     = doSend True $> Just t
          | otherwise              = doSend True *> takeMVar (env ^. query)
          where
            doSend b = when (state ^. canSend || b) $ (env ^. send) b (env ^. candidates) matcher (state ^. matchSet)
            inrange = state ^. chunkNumber < (V.length . chunks $ env ^. candidates)

searchEnv :: KnownNat n => SearchFunctions a -> Int -> (forall n m. (KnownNat n , KnownNat m) => Bool -> Chunks n -> MatcherSized m a -> MatchSetSized m -> IO ())
  -> Chunks n -> IO (SearchEnv n a)
searchEnv funs n sender chks = SearchEnv funs sender n chks <$> newEmptyMVar <*> M.replicate (V.length . chunks $ chks) (S.replicate 1)

searchLoop :: KnownNat n => SearchEnv n a -> IO ()
searchLoop env = maybe (pure ()) (`loop` "") =<< takeMVar (env ^. query)
  where
    loop qry prev
      | Nothing <- matcher = maybe (pure ()) (`loop` qry) =<< takeMVar (env ^. query)
      | Just (Matcher m) <- matcher = maybe (pure ()) (`loop` qry) =<< matcherLoop env qry prev m
      where
        matcher = (env ^. searchFunctions . makeMatcher) qry

fuzzyFunctions :: CaseSensitivity -> SearchFunctions MatchPart
fuzzyFunctions c = SearchFunctions (fuzzyMatcher c) fuzzyMatchSized fuzzyMatchParts

orderlessFunctions :: CaseSensitivity -> SearchFunctions Int
orderlessFunctions c = SearchFunctions (orderlessMatcher c) orderlessMatchSized orderlessMatchParts

makeChunks :: forall n. KnownNat n => V.Vector Text -> Chunks n
makeChunks v = Chunks $ V.generate numchunks chunk
  where
    n = fromIntegerUnsafe $ natVal (Proxy :: Proxy n)
    (numchunks , remainder) = bimap (+ 1) (+ 1) . divMod (length v - 1) $ n
    chunk i
      | i + 1 < numchunks  = fromJust . SV.toSized . V.slice (i*n) n $ v
      | otherwise          = fromJust . SV.toSized $ V.slice (i*n) remainder v <> V.replicate (n - remainder) ""

makeChunks5 :: V.Vector Text -> Chunks 5
makeChunks5 = makeChunks

data SimpleSearcher = SimpleSearcher {terms :: [Text] , sleepTime :: Int , matchesToPrint :: Int}

printMatches :: forall n m a. (KnownNat n , KnownNat m) => Int -> SearchFunctions a -> Bool -> Chunks n -> MatcherSized m a -> MatchSetSized m -> IO ()
printMatches n funcs b store m s = when b (putStrLn "\nMatches for this round.\n" *> go (fst . splitAt n $ s))
  where
    go = traverse_ (\(ScoredMatchSized _ c v) -> showMatchColor stdout $ (funcs ^. display) m (store ! c) v)

simpleFuzzyEnv :: KnownNat n => Int -> Proxy n -> V.Vector Text -> IO (SearchEnv n MatchPart)
simpleFuzzyEnv n _ = searchEnv (fuzzyFunctions IgnoreCase) n (printMatches n (fuzzyFunctions IgnoreCase)) . makeChunks

runSimpleSearcherWithEnv :: KnownNat n => SimpleSearcher -> SearchEnv n MatchPart -> IO ()
runSimpleSearcherWithEnv s env = forkIO (searchLoop env) *> traverse_ (doSearch . Just) (terms s) *> doSearch Nothing
  where
    doSearch term = putMVar (env ^. query) term *> threadDelay (sleepTime s)

runSimpleSearcher :: KnownNat n => Proxy n -> SimpleSearcher -> V.Vector Text -> IO ()
runSimpleSearcher p s v = runSimpleSearcherWithEnv s =<< simpleFuzzyEnv (matchesToPrint s) p v

testVector :: IO (V.Vector Text)
testVector = readVectorStdIn

simpleSearcherTest :: IO ()
simpleSearcherTest = runSimpleSearcher (Proxy :: Proxy 32)
                                       (SimpleSearcher ["" , "m" , "ma" , "mal" , "malda" , "maldac" , "maldace" , "maldacen" , "maldacena"
                                                       , "w" , "wi" , "wit" , "witt" , "witte" , "witten" , "f" , "fr" , "fra" , "fran" , "franc" , "franco"
                                                       , "c" , "cl" , "clo" , "clos" , "closs" , "closse" , "closset" , "s" , "se" , "sen"] 25000 16) =<< testVector
--simpleSearcherTest = runSimpleSearcher (Proxy :: Proxy 32) (SimpleSearcher ["suse", "linux" , "binary" , "close" , "Witten" , "Maldacena" , "Franco" , "Closset"] 100000 1024) =<< testVector
