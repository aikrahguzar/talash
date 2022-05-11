-- |
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}

module Talash.Chunked where

import Brick.Widgets.List
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import qualified Control.Monad.ST as ST
import Data.Bit
import qualified Data.ByteString.Char8 as B
import Data.Maybe (fromJust)
import Data.Monoid.Colorful
import qualified Data.Set as DS
import qualified Data.Text as T
import Data.Text.AhoCorasick.Automaton (CaseSensitivity(..))
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed.Mutable.Sized as MS
import qualified Data.Vector.Unboxed.Sized as S
import GHC.TypeLits
import Lens.Micro
import Lens.Micro.TH as Export ( makeLenses )
import System.IO.Streams (toVector)
import qualified System.IO.Streams as I
import Talash.Core hiding (match , makeMatcher)
import Talash.Files
import Talash.Intro hiding (splitAt)
import Talash.ScoredMatch

newtype Chunks (n:: Nat) = Chunks { chunks ::  V.Vector (SV.Vector n Text)} deriving (Eq , Ord , Show)
type MatchSetSized n = DS.Set (ScoredMatchSized n)

data Ocassion = ChunkSearched | QueryDone | NewQuery | SearchDone deriving (Eq , Ord , Show)

data SearchStateSized (n :: Nat) a = SearchStateSized { _currentQuery :: {-# UNPACK #-} !Text
                                                      , _prevQuery :: {-# UNPACK #-} !Text
                                                      , _chunkNumber :: {-# UNPACK #-} !Int
                                                      , _totalMatches :: {-# UNPACK #-} !Int
                                                      , _newMatches ::  !Bool
                                                      , _done :: !Bool
                                                      , _matchSet :: !(MatchSetSized n)}
makeLenses ''SearchStateSized

data SearchFunctions a b = SearchFunctions { _makeMatcher :: Text -> Matcher a
                                           , _match :: forall n. KnownNat n => MatcherSized n a -> Text -> Maybe (MatchFull n)
                           -- | Given the matcher @m@, the matched string @t@ and the indices of matches in @t@ divide @t@ in alternating strings that are a matches
                           --   and the gap between these matches. The first of these is always a gap and can be empty. The rest should be non empty.
                                           , _display :: forall n. KnownNat n => (Bool -> Text -> b) -> MatcherSized n a -> Text -> S.Vector n Int -> [b] }
makeLenses ''SearchFunctions

data SearchReport = SearchReport { _ocassion :: Ocassion , _hasNewMatches :: Bool , _nummatches :: Int , _searchedTerm :: Text}
makeLenses ''SearchReport

-- | The constant environment in which the search runs.
data SearchEnv n a b = SearchEnv { _searchFunctions :: SearchFunctions a b  -- ^ The functions used to find and display matches.
                                 , _send :: forall n m. (KnownNat n , KnownNat m) => Chunks n -> SearchReport -> MatcherSized m a -> MatchSetSized m -> IO ()
                                 , _maxMatches :: Int
                                 , _candidates :: Chunks n
                                 , _query :: MVar (Maybe Text)
                                 , _allMatches :: M.IOVector (S.Vector n Bit) }
makeLenses ''SearchEnv

{-# INLINABLE  (!) #-}
(!) :: KnownNat n => Chunks n -> ChunkIndex -> Text
(!) (Chunks v) (ChunkIndex i j) = V.unsafeIndex (SV.fromSized $ V.unsafeIndex v i) j

{-# INLINE  getChunk #-}
getChunk :: Int -> Chunks n -> SV.Vector n Text
getChunk i (Chunks f) = V.unsafeIndex f i

{-# INLINE matchChunk #-}
matchChunk :: forall n m a. (KnownNat n , KnownNat m) => (MatcherSized n a -> Text -> Maybe (MatchFull n)) -> MatcherSized n a -> Int -> SV.Vector m Text
  -> S.Vector m Bit -> (S.Vector m Bit , MatchSetSized n)
matchChunk fun m ci v i = ST.runST $ matchChunkM fun m ci v i

{-# INLINEABLE matchChunkM #-}
matchChunkM :: forall n m a s. (KnownNat n , KnownNat m) => (MatcherSized n a -> Text -> Maybe (MatchFull n)) -> MatcherSized n a -> Int -> SV.Vector m Text
  -> S.Vector m Bit -> ST.ST s (S.Vector m Bit , MatchSetSized n)
matchChunkM fun m = go
  where
    go ci v i = doMatching =<< MS.replicate (Bit False)
      where
        doMatching mbv = freezeAndDone =<< U.ifoldM' collectAndWrite DS.empty (S.fromSized i)
          where
            freezeAndDone mset = ( , mset) <$> S.unsafeFreeze mbv
            collectAndWrite x _ (Bit False) = pure x
            collectAndWrite x j (Bit True)
              | Nothing   <- res   = pure x
              | Just mtch <- res   = unsafeFlipBit umbv j $> DS.insert (conv mtch) x
              where
                umbv = MS.fromSized mbv
                res = fun m . SV.unsafeIndex v $ j
                conv (MatchFull k w) = ScoredMatchSized (Down k) (ChunkIndex ci j) w

{-# INLINABLE resetMatches #-}
resetMatches :: forall n m a b. KnownNat n => SearchEnv n a b -> SearchStateSized m a -> IO ()
resetMatches env state
  | T.isInfixOf (state ^. prevQuery) (state ^. currentQuery) = pure ()
  | otherwise                                                = M.set (env ^. allMatches) (S.replicate 1)

{-# INLINABLE  searchNextChunk #-}
searchNextChunk :: (KnownNat n , KnownNat m) => SearchEnv n a b -> MatcherSized m a -> SearchStateSized m a -> IO (SearchStateSized m a)
searchNextChunk env matcher state = nextstate . getMatches =<< M.read (env ^. allMatches) i
  where
    i          = state ^. chunkNumber
    getMatches = matchChunk (env ^. searchFunctions . match) matcher i (getChunk i (env ^. candidates))
    nextstate (js , mtchs) = M.write (env ^. allMatches) i js $> (over chunkNumber (+ 1) . updateAndSend . mergedMatches (state ^. matchSet) $ mtchs)
      where
        mergedMatches curr new = if not (DS.null new) && (DS.size curr < env ^. maxMatches || DS.lookupMax curr > DS.lookupMin new)
                                                         then Just . DS.take (env ^. maxMatches) . DS.union curr $ new else Nothing
        updateAndSend = over totalMatches (+ DS.size mtchs) . maybe (set newMatches False state) (\mset -> set newMatches True (set matchSet mset state))

matcherLoop :: (KnownNat n , KnownNat m) => SearchEnv n a b -> Text -> Text -> MatcherSized m a -> IO (Maybe Text)
matcherLoop env qry prev matcher = resetMatches env initstate *> loop initstate
  where
    initstate = SearchStateSized qry prev 0 0 False False DS.empty
    loop state = step =<< tryTakeMVar (env ^. query)
      where
        step x
          | Just Nothing <- x      = doSend SearchDone $> Nothing
          | inrange , Nothing <- x = doSend ChunkSearched *> (loop =<< searchNextChunk env matcher state)
          | Just (Just t) <- x     = doSend NewQuery $> Just t
          | otherwise              = doSend QueryDone *> takeMVar (env ^. query)
          where
            report b = SearchReport b (state ^. newMatches) (state ^. totalMatches) qry
            doSend b = (env ^. send) (env ^. candidates) (report b) matcher (state ^. matchSet)
            inrange = state ^. chunkNumber < (V.length . chunks $ env ^. candidates)

searchEnv :: KnownNat n => SearchFunctions a b -> Int -> (forall n m. (KnownNat n , KnownNat m) => Chunks n -> SearchReport -> MatcherSized m a -> MatchSetSized m -> IO ())
  -> Chunks n -> IO (SearchEnv n a b)
searchEnv funs n sender chks = SearchEnv funs sender n chks <$> newEmptyMVar <*> M.replicate (V.length . chunks $ chks) (S.replicate 1)

searchLoop :: KnownNat n => SearchEnv n a b -> IO ()
searchLoop env = maybe (pure ()) (`loop` "") =<< takeMVar (env ^. query)
  where
    loop qry prev
      | (Matcher m) <- (env ^. searchFunctions . makeMatcher) qry = maybe (pure ()) (`loop` qry) =<< matcherLoop env qry prev m

fuzzyFunctions :: CaseSensitivity -> SearchFunctions MatchPart b
fuzzyFunctions c = SearchFunctions (fuzzyMatcher c) fuzzyMatchSized fuzzyMatchPartsAs

orderlessFunctions :: CaseSensitivity -> SearchFunctions Int b
orderlessFunctions c = SearchFunctions (orderlessMatcher c) orderlessMatchSized orderlessMatchPartsAs

{-# INLINABLE  makeChunks #-}
makeChunks :: forall n. KnownNat n => V.Vector Text -> Chunks n
makeChunks v = Chunks $ V.generate numchunks chunk
  where
    n = fromInteger $ natVal (Proxy :: Proxy n)
    (numchunks , remainder) = bimap (+ 1) (+ 1) . divMod (length v - 1) $ n
    chunk i
      | i + 1 < numchunks  = fromJust . SV.toSized . V.slice (i*n) n $ v
      | otherwise          = fromJust . SV.toSized $ V.slice (i*n) remainder v <> V.replicate (n - remainder) ""

{-# INLINE makeChunksP #-}
makeChunksP :: KnownNat n => Proxy n -> V.Vector Text -> Chunks n
makeChunksP _ = makeChunks

{-# INLINE matchSetToVector #-}
matchSetToVector :: (a -> b) -> DS.Set a -> V.Vector b
matchSetToVector f s = ST.runST $ setToVectorST f s

{-# INLINE matchSetSizedToVector #-}
matchSetSizedToVector :: KnownNat n => MatchSetSized n -> U.Vector (ScoredMatchSized n)
matchSetSizedToVector = U.fromList . DS.toList

{-# INLINEABLE setToVectorST #-}
setToVectorST :: (a -> b) -> DS.Set a -> ST.ST s (V.Vector b)
setToVectorST f s = go =<< MV.unsafeNew (DS.size s)
  where
    go mv = foldM_ (\i e -> MV.unsafeWrite mv i (f e) $> i + 1) 0 s *> V.unsafeFreeze mv

startSearcher :: KnownNat n => SearchEnv n a b -> IO ()
startSearcher = void . forkIO . searchLoop

sendQuery :: KnownNat n => SearchEnv n a b -> Text -> IO ()
sendQuery env = putMVar (env ^. query) . Just

stopSearcher :: KnownNat n => SearchEnv n a b -> IO ()
stopSearcher env = putMVar (env ^. query) Nothing

concatChunks :: KnownNat n => Int -> Chunks n -> V.Vector Text
concatChunks i (Chunks c) =  V.concatMap SV.fromSized . V.take i $ c

forceChunks :: KnownNat n => Chunks n -> Chunks n
forceChunks (Chunks v) = Chunks . V.force $ v

chunksFromStream :: forall n. KnownNat n => I.InputStream Text -> IO (Chunks n)
chunksFromStream i = Chunks <$> (I.toVector =<< I.mapMaybe (\v -> map SV.force . SV.toSized $ v V.++ V.replicate (n - length v) "") =<< I.chunkVector n i)
  where
    n = fromInteger $ natVal (Proxy :: Proxy n)

{-# INLINE chunksFromStreamP #-}
chunksFromStreamP :: forall n. KnownNat n => Proxy n -> I.InputStream Text -> IO (Chunks n)
chunksFromStreamP _ = chunksFromStream

{-# INLINABLE  chunksFromHandle #-}
chunksFromHandle :: KnownNat n => Proxy n -> Handle -> IO (Chunks n)
chunksFromHandle p h = chunksFromStreamP p =<< I.decodeUtf8 =<< I.lines =<< I.handleToInputStream h

readVectorHandleWith :: (Text -> Text) -- ^ The function to transform the candidates.
  -> (V.Vector Text -> V.Vector Text) -- ^ The function to apply to the constructed vector before compacting.
  -> Handle -- ^ The handle to read from
  -> IO (V.Vector Text)
readVectorHandleWith f t h = map t $ I.toVector =<< I.map f =<< I.decodeUtf8 =<< I.lines =<< I.handleToInputStream h

fileNamesSorted :: Handle -> IO (V.Vector Text)
fileNamesSorted = readVectorHandleWith (T.takeWhileEnd (/= '/')) (V.uniq . V.modify sort)
