{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
-- | This modules provides the function `searchSome` for searching the candidates provided by `Vector` `Text`. The information about the location of matches
-- is stored in a length-tagged unboxed vector `S.Vector`. Such vectors have an `Unbox` instances which allows us to store the collection of such mathces in an
-- unboxed `U.Vector`. This significantly reduces the memory usage and pressure on garbage collector. As a result the matchers used by this function are tagged
-- with the number @n@ of needles need to be matched and are provided by `MatcherSized`. An unsized interface is provided by `Matcher` which is existenially
-- quantified over the number of needles. Functions for constructing matching and matchers have both a sized and unsized version.

module Talash.Core ( -- * Types
                     MatcherSized (..) , Matcher (..) , MatchState (..) , MatchPart (..) , MatchFull (..) , SearchSettings (..) , Indices
                     -- * Matchers and matching
                     , makeMatcher , emptyMatcher
                     -- ** Fuzzy style
                     , fuzzyMatcherSized , fuzzyMatcher , fuzzyMatchSized , fuzzyMatch , fuzzyMatchParts , fuzzyMatchPartsAs
                     -- ** Orderless Style
                     , orderlessMatcherSized , orderlessMatcher , orderlessMatchSized , orderlessMatch , orderlessMatchParts , orderlessMatchPartsAs
                     -- * Search
                     , fuzzySettings , orderlessSettings , parts , partsAs , partsOrderless , partsOrderlessAs , minify) where

import Control.Monad.ST (ST, runST)
import qualified Data.Text as T
import Data.Text.AhoCorasick.Automaton
import Data.Text.Utf8 hiding (indices)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M
import qualified Data.Vector.Unboxed.Mutable.Sized as MS
import qualified Data.Vector.Unboxed.Sized as S
import Talash.Intro
import GHC.TypeNats
import Prelude (fromIntegral)
import Lens.Micro (_1 , _2 , (^.))
import Debug.Trace (traceShow)

-- | The MatcherSized type consists of a state machine for matching a fixed number of needles. The number of matches needed is encoded in the Nat parameterzing
--   the type. Here the purpose is to improve the memory consumption by utlizing the `Unbox` instance for sized tagged unboxed vectors from
--   (vector-sized)[https://hackage.haskell.org/package/vector-sized] package. This significantly reduces the memory consumption. At least in the present
--   implementation there is no benefit for correctness and dealing with the length tag is occasionally annoying.
data MatcherSized (n :: Nat) a = MatcherSized {
                              caseSensitivity :: CaseSensitivity ,
                              -- | An AhoCorasick state machine from the alfred-margaret package which does the actual string matching
                              machina :: {-# UNPACK #-} !(AcMachine a) ,
                              -- | The sizes of the /basic/ needles in code unit indices. The Left Int case is for when the length of all the
                              -- needles is 1 with Int the number of needles.
                              sizes :: !(Either Int (S.Vector n Int))}

-- | The existential version of MatcherSized
data Matcher a      = forall n. KnownNat n => Matcher (MatcherSized n a)

-- | The matching process essentially takes the form of a fold with possible early termination over the matches produced. See the runLower from the
--   alfred-margaret. Here MatchState is the return type of this fold and essentially it records the positions of the matches. Here like in alfred-margaret
--   position is the code unit index of the first code unit beyond the match. We can't use the CodeUnitIndex here because it doesn't have an unbox instance.
data MatchState (n :: Nat) a = MatchState {
                                 -- | This is used to record the present extent of the match. What extent means is different to different matching styles.
                                 endLocation :: {-# UNPACK #-} !Int ,
                                 -- | The vector recording the position of the matches.
                                 partialMatch :: {-# UNPACK #-} !(S.Vector n Int) ,
                                 -- | Any auxiliary information needed to describe the state of the match.
                                 aux :: !a} deriving Show

data MatchPart = MatchPart {matchBegin :: {-# UNPACK #-} !Int , matchEnd :: {-# UNPACK #-} !Int} deriving Show

-- | The full match consisting of a score for the match and vector consisting of the positions of the match. The score is intended as for bucketing and as a
--   result shouldn't be two large and must be non-negative . For the fuzzy style in this module @n@ contiguous matches contribute @n-1@ to the score. The
--   scores thus range from @0@ to @n-1@ where @n@ is the length of the string to be matched. For orderless style this score is always @0@.
data MatchFull (n :: Nat) = MatchFull {scored :: {-# UNPACK #-} !Int , indices :: {-# UNPACK #-} !(S.Vector n Int)} deriving Show

-- | The configuration for a search style with n needles and matcher of type a
data SearchSettings a (n :: Nat) = SearchSettings {
                                     -- | Given the matcher and the candidate text, find a match or return Nothing if there is none.
                                     match :: a -> Text -> Maybe (MatchFull n) ,
                                     -- | The maximum score for a given matcher. It determines the number of buckets.
                                     fullscore :: a -> Int ,
                                     -- | Maximum number of matches with full score to produce.
                                     maxFullMatches :: Int ,
                                     -- | The ordering to sort the matches within a given bucket. It is run with two candidates and their corresponding matches.
                                     orderAs :: Text -> S.Vector n Int -> Text -> S.Vector n Int -> Ordering}

-- | Type synonym for the index of a candidate in the backing vector along with the positions of the matches for it.
type Indices (n :: Nat) = (Int , S.Vector n Int)

-- | Unsafe, use with care. eIndex i return 1 for Left and for Right the element at @i@-th position in the vector. The vector must have at least @i+1@ elements.
-- This uses unsafeIndex so no bound checks are performed.
{-# INLINE eIndex #-}
eIndex :: KnownNat n => Int -> Either a (S.Vector n Int) -> Int
eIndex i = either (const 1) (`S.unsafeIndex` i)

{-# INLINEABLE updateMatch #-}
updateMatch :: KnownNat n => Int -> Either Int (S.Vector n Int) -> MatchState n a -> Int -> Int -> a -> MatchState n a
updateMatch !c l (MatchState !f !m _) !b !e !a = MatchState e (S.withVectorUnsafe (U.modify doWrites) m) a
  where
    doWrites s = void $ foldlM (\d i -> M.unsafeWrite s i d $> d - eIndex i l) c [e , e-1 .. b]

-- | The score for a fuzzy match.
{-# INLINE matchScore #-}
matchScore :: KnownNat n => Either Int (S.Vector n Int) -> S.Vector n Int -> Int
matchScore u v
  | S.length v == 0       = 0
  | v' <- S.fromSized v   = U.ifoldl' (\ !s !i !cc -> if cc - U.unsafeIndex v' i == eIndex i u then s+1 else s ) 0 . U.tail $ v'

{-# INLINEABLE matchStepFuzzy #-}
matchStepFuzzy :: KnownNat n => Either Int (S.Vector n Int) -> MatchState n () -> Match MatchPart -> Next (MatchState n ())
matchStepFuzzy l s@(MatchState !f !m _) (Match (CodeUnitIndex !i) (MatchPart !b !e))
  | e - b == either id S.length l - 1                                                                           = Done $ updateMatch i l s b e ()
  | (b == 0 && f == (-1)) || (f + 1 == b && S.unsafeIndex m f + e <= i + b) || (f >= b && e > f && monotonic)   = Step $ updateMatch i l s b e ()
  | otherwise                                                                                                   = Step   s
  where
    monotonic = S.unsafeIndex m f + either (const $ e-f) (U.sum . U.slice f (e-f) . S.fromSized) l <= i

{-# INLINEABLE matchStepOrderless #-}
matchStepOrderless :: KnownNat n => Either Int (S.Vector n Int) -> MatchState n (Int , Int) -> Match Int -> Next (MatchState n (Int , Int))
matchStepOrderless !lv s@(MatchState r !m (!lm , !li)) (Match (CodeUnitIndex !c) !i)
  | S.unsafeIndex m i == 0 && c - eIndex i lv >= li       = go   $ MatchState (r+1) (S.withVectorUnsafe (U.modify (\mv -> M.unsafeWrite mv i c)) m) (i , c)
  | S.unsafeIndex m i == 0 && eIndex lm lv < eIndex i lv  = Step $ MatchState r (S.withVectorUnsafe (U.modify (\mv -> M.unsafeWrite mv i c *> M.write mv lm 0)) m) (i , c)
  | otherwise                                             = Step s
  where
    go = if r == S.length m - 1 then Done else Step

kConsecutive :: Int ->  Text -> [Text]
kConsecutive k t = map (T.take k) . take (1 + T.length t - k) . T.tails $ t

-- | A general function to construct a Matcher. Returns Nothing if the string is empty or if the number of needles turns out to be non-positive
makeMatcher :: forall a. CaseSensitivity -> (Text -> Int) -- ^ The function to determine the number of needles from the query string.
                                                          -- The proxy argument is instantiated at the resulting value.
                    -> (forall n. KnownNat n => Proxy n -> CaseSensitivity -> Text -> MatcherSized n a) -- ^ The functions for constructing the matcher
                    -> Text -- ^ The query string
                    -> Matcher a -- ^ Nothing if the string is empty or if the number of needles turns out to be non-positive
makeMatcher c lenf matf t
  | SomeNat p <- someNatVal . fromIntegral. lenf $ t    = Matcher . matf p c $ t

{-# INLINE withSensitivity #-}
withSensitivity :: CaseSensitivity -> Text -> Text
withSensitivity IgnoreCase    = lowerUtf8
withSensitivity CaseSensitive = id

-- | Constructs the matcher for fuzzy matching. The needles are all possible contigous subtrings of the string being matched. The Nat @n@ must be instantiated at the
--  length @n@ of the query string. They are n choose 2 such substrings, so to the complexity of matching is \(O(m + n^2)\) where @m@ is the length of candidate string.
--  This is a rough (and probably wrong) estimate as the updating the matchstate for each found match is not a constant time operation. Not sure if Aho Corasick is
--  the optimal way for this kind of matching but in practice it seems fast enough.
fuzzyMatcherSized :: KnownNat n => p n -> CaseSensitivity -> Text -> MatcherSized n MatchPart
fuzzyMatcherSized _ c t = MatcherSized {caseSensitivity = c , machina = build . concatMap go $ [T.length t , T.length t - 1 .. 1]
                                       , sizes = if S.sum sz == S.length sz then Left (S.length sz) else Right sz }
  where
    sz      = fromMaybe (S.replicate 1) . S.fromList . map (length . unpackUtf8 . withSensitivity c . T.singleton)  . T.unpack  $ t
    go !k   = zipWith (\t' l -> (withSensitivity c t' , MatchPart l (l + k -1))) (kConsecutive k t) [0 ..]

-- | Unsized version of fuzzyMatcherSized
fuzzyMatcher :: CaseSensitivity -> Text -> Matcher MatchPart
fuzzyMatcher c  = makeMatcher c T.length fuzzyMatcherSized

{-# INLINE emptyMatcher #-}
emptyMatcher :: MatcherSized 0 a
emptyMatcher = MatcherSized IgnoreCase (build []) (Left 0)

-- | Constructs the matcher for orderless matching, the needles are the words from the query string and the proxy argument should be instantiated at the
--  number of words.
orderlessMatcherSized :: KnownNat n => p n -> CaseSensitivity -> Text -> MatcherSized n Int
orderlessMatcherSized _ c t = MatcherSized {caseSensitivity = c , machina = build . zip wrds $ [0 ..]
                               , sizes = Right . fromMaybe (S.replicate 1) . S.fromList . map (codeUnitIndex . lengthUtf8) $ wrds }
  where
    wrds = withSensitivity c <$> T.words t

-- | Unsized version of orderlessMatcherSized
orderlessMatcher :: CaseSensitivity -> Text -> Matcher Int
orderlessMatcher c = makeMatcher c (length . T.words) orderlessMatcherSized

{-# INLINEABLE fuzzyMatchSized#-}
fuzzyMatchSized :: KnownNat n => MatcherSized n MatchPart -> Text -> Maybe (MatchFull n)
fuzzyMatchSized (MatcherSized c m l) = full . runWithCase c (MatchState (-1) (S.replicate 0) ()) (matchStepFuzzy l) m
  where
    full s@(MatchState !e !u _) = if e + 1 == S.length u then Just $ MatchFull (matchScore l u) u else Nothing

fuzzyMatch :: Matcher MatchPart -> Text -> Maybe [Text]
fuzzyMatch (Matcher m) t = parts (S.fromSized <$> sizes m) t . S.fromSized . indices <$> fuzzyMatchSized m t

fuzzyMatchParts :: KnownNat n => MatcherSized n MatchPart -> Text -> S.Vector n Int -> [Text]
fuzzyMatchParts m t = parts (S.fromSized <$> sizes m) t . S.fromSized

fuzzyMatchPartsAs :: KnownNat n => (Bool -> Text -> a) -> MatcherSized n MatchPart -> Text -> S.Vector n Int -> [a]
fuzzyMatchPartsAs f m t = partsAs f (S.fromSized <$> sizes m) t . S.fromSized

{-# INLINEABLE orderlessMatchSized#-}
orderlessMatchSized :: KnownNat n => MatcherSized n Int -> Text -> Maybe (MatchFull n)
orderlessMatchSized (MatcherSized c m l) = full . runWithCase c (MatchState 0 (S.replicate 0) (0,0)) (matchStepOrderless l) m
  where
    ln = either id S.length l
    full u = if endLocation u == ln then Just $ MatchFull 0 (partialMatch u) else Nothing

orderlessMatch :: Matcher Int -> Text -> Maybe [Text]
orderlessMatch (Matcher m) t = partsOrderless (S.fromSized <$> sizes m) t . S.fromSized . indices <$> orderlessMatchSized m t

orderlessMatchParts :: KnownNat n => MatcherSized n Int -> Text -> S.Vector n Int -> [Text]
orderlessMatchParts m t = partsOrderless (S.fromSized <$> sizes m) t . S.fromSized

orderlessMatchPartsAs :: KnownNat n => (Bool -> Text -> a) -> MatcherSized n Int -> Text -> S.Vector n Int -> [a]
orderlessMatchPartsAs f m t = partsOrderlessAs f (S.fromSized <$> sizes m) t . S.fromSized

-- | The parts of a string resulting from a match using the fuzzy matcher.
parts :: Either Int (U.Vector Int) -- ^ The information about the lengths of different needles.
  -> Text -- ^ The candidate string that has been matched
  -> U.Vector Int -- ^ The vector recording the positions of the needle in the matched string.
  -> [Text] -- ^ The candidate string split up according to  the match
parts v t u
  | U.null u  = [t]
  |otherwise  =  done . foldl' cut ([] , lengthUtf8 t) . minify v $ u
  where
    done (ms , cp) = unsafeSliceUtf8 0 cp t : ms
    cut  (!ms , !cp) !cc  = (unsafeSliceUtf8 cc (cp - cc) t : ms , cc )

partsAs :: (Bool -> Text -> a) -> Either Int (U.Vector Int) -> Text -> U.Vector Int -> [a]
partsAs f = go
  where
    go v t u
      | U.null u  = [f False t]
      |otherwise  =  done . foldl' cut ([] , lengthUtf8 t , False) . minify v $ u
      where
        done (ms , cp, b) = f b (unsafeSliceUtf8 0 cp t) : ms
        cut  (!ms , !cp , !b) !cc  = (f b (unsafeSliceUtf8 cc (cp - cc) t) : ms , cc , not b)

-- | The parts of a string resulting from a match using the orderless matcher. See parts for an explanation of arguments.
partsOrderless :: Either Int (U.Vector Int) -> Text -> U.Vector Int -> [Text]
partsOrderless v t u = parts (map (`U.backpermute` fst up) v) t (snd up)
  where
    up = U.unzip . U.modify (V.sortBy (comparing snd)) . U.imap (,) $ u

partsOrderlessAs :: (Bool -> Text -> a) -> Either Int (U.Vector Int) -> Text -> U.Vector Int -> [a]
partsOrderlessAs f v t u = partsAs f (map (`U.backpermute` fst up) v) t (snd up)
  where
    up = U.unzip . U.modify (V.sortBy (comparing snd)) . U.imap (,) $ u

{-# INLINE eIndexU #-}
eIndexU :: Int -> Either a (U.Vector Int) -> Int
eIndexU i = either (const 1) (`U.unsafeIndex` i)

-- | Shorten a match by collapsing the contiguous sub-matches together.
minify :: Either Int (U.Vector Int) -> U.Vector Int -> [CodeUnitIndex]
minify v s
  | U.length s == 1       = map CodeUnitIndex [a , a - eIndexU 0 v]
  | otherwise             = map CodeUnitIndex . (U.last s :) . snd . U.ifoldl' go (a , [a - eIndexU 0 v]) . U.tail $ s
  where
    a = U.unsafeHead s
    go (!l , s) !i !c = if c - l <= eIndexU (i+1) v then (c , s) else (c , c - eIndexU (i+1) v : l : s )

-- | The default ordering used in this module to sort matches within a given bucket. Prefers the matche for which the last part is closest to the end. To tie
-- break prefers the shorter matched string.
defOrdering :: Text -> S.Vector n Int -> Text -> S.Vector n Int -> Ordering
defOrdering t1 s1 t2 s2
  | el == EQ   = compare (T.length t1) (T.length t2)
  | otherwise  = el
  where
    el = compare (T.length t1 - U.maximum (S.fromSized s1)) (T.length t2 - U.maximum (S.fromSized s2))

-- | Search functions suitable for fuzzy matching. The candidate @c@ will match query @s@ if @c@ contains all the characters in @s@ in order. In general there
--   can be several ways of matching. This tries to find a match with minimum number of parts of. It does not find the minimum number of parts, if that requires
--   reducing the extent of the partial match during search. E.g. matching @"as"@ against @"talash"@ the split will be @["tal","as","h"]@ and not
--   @["t","a","la","s","h"]@. While matching @"talash best match testing hat"@ against @"tea"@ will not result in @["talash best match ","te","sting h","a","t"]@ since
--   @"te"@ occurs only after we have match all three letters and we can't know if we will find the @"a"@ without going through the string.
fuzzySettings :: KnownNat n => Int -> SearchSettings (MatcherSized n MatchPart) n
fuzzySettings !m = SearchSettings { match = fuzzyMatchSized , fullscore = \t -> either id S.length (sizes t) - 1 , maxFullMatches = m , orderAs = defOrdering}

-- | Search functions that match the words in i.e. space separated substring in any order. @"talash best"@ will match @"be as"@ with the split
--   @["tal","as","h","be","st"]@ but @"talash best"@ will not match @"bet"@.
orderlessSettings :: KnownNat n => Int -> SearchSettings (MatcherSized n Int) n
orderlessSettings n = SearchSettings {match = orderlessMatchSized , fullscore = const 0, maxFullMatches = n , orderAs = defOrdering}
