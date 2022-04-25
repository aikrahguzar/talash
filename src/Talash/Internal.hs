{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Talash.Internal ( -- * Search
                     SearchFunction , CaseSensitivity (..) , makeMatcher , lister , displayer ,  searchWithMatcher
                   , searchFunctionFuzzy , searchFunctionOL , searchFunctionFuzzyCustom  , searchFunctionOLCustom
                     -- * Help for reading vectors
                   , readVectorStdIn , fileNamesSorted , readVectorHandle , readVectorHandleWith, emptyIndices
                     -- * Exports
                   , module Export ) where

import Control.Exception as Export (finally , catch, bracket , AsyncException)
import qualified Data.Text as T
import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))
import qualified Data.Text.IO as T
import Data.Vector (Vector , (!), force , generate , take, singleton , convert, enumFromN, unfoldr, unfoldrM , uniq , modify, concat)
import Data.Vector.Algorithms.Intro (sort)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as S
import GHC.Compact (Compact , compact , getCompact)
import GHC.TypeNats
import Intro hiding (sort, take , modify)
import Lens.Micro as Export (ASetter' , over, set, (^.) , _1 , _2 , _3 , (.~) , (?~) , (%~))
import Lens.Micro.TH as Export ( makeLenses )
import System.IO as Export ( Handle , hIsEOF , isEOF, hClose, stdin)
import Talash.Core hiding (makeMatcher)

type SearchFunction = Vector Text -> Maybe Text -> U.Vector Int -> (U.Vector Int  ,  (Int , Vector [Text]))

data SearchFunctions a = SearchFunctions {
                           -- | Construct the matcher given the string to match.
                           _makeMatcher :: Text -> Maybe (Matcher a) ,
                           -- | Obtain the result for searching.
                           _lister :: forall n. KnownNat n => MatcherSized n a --  The n will be determined by the SomeMatcher constructed above
                                                                -> Vector Text --  The vector holding the candidates
                                                                -> U.Vector Int -- An unboxed vector of indices. Only these indices will be matched against the matcher
                                                                -> (U.Vector Int , U.Vector (Indices n)) ,
                           -- | Given the matcher @m@, the matched string @t@ and the indices of matches in @t@ divide @t@ in alternating strings that are a matches
                           --   and the gap between these matches. The first of these is always a gap and can be empty. The rest should be non empty.
                           _displayer :: forall n. KnownNat n => MatcherSized n a -> Text -> S.Vector n Int -> [Text] }
makeLenses ''SearchFunctions

emptyIndices :: Int -> U.Vector  (Indices 0)
emptyIndices n = U.generate n ( , S.empty)

-- | searchWithMatcher carries out one step of the search. Note that the search can stops before going through the whole vector of text. In that case the returned
--   vector of indices should contain not only the indices matched candidates but also the indices of candidates that weren't tested for a match.
searchWithMatcher :: SearchFunctions a -- ^ The configuration to use to carry out the search.
  -> Vector Text -- ^ The vector @v@ of candidates.
  -> Maybe Text -- ^ The query string
  -> U.Vector  Int -- ^ The subset of indices of @v@ to search against. If input changes from @talas@ to @talash@ we only search among candidates that matched @talas@.
  -> (U.Vector Int , (Int , Vector [Text])) -- ^ The indices of the matched candidates (see the note above) and the matched candidates broken up according to the match.
searchWithMatcher fs v t s = maybe nc go ((fs ^. makeMatcher) =<< t)
      where
        nc  = (U.enumFromN 0 (length v) , (0 , force . map (\i -> [v ! (i ^. _1)]) . convert . emptyIndices . min 512 . length $ v))
        go (Matcher  f') = (iv , (U.length iv , force . map (\i -> (fs ^. displayer) f' (v ! (i ^. _1)) (i ^. _2)) . convert $ mv))
          where
            (iv , mv) = (fs ^. lister) f' v s

-- | Read a vector of newline separated candidates from the stdin.
readVectorStdIn :: IO (Vector Text)
readVectorStdIn  = finally (unfoldrM (const . ifM isEOF (pure Nothing) . map (\ !l -> Just (l  , ())) $ T.getLine) ()) (hClose stdin)

-- | Read a vector of newline separated candidates from a handle.
readVectorHandle :: Handle -> IO (Vector Text)
readVectorHandle h = finally (unfoldrM (const . ifM (hIsEOF h) (pure Nothing) . map (\ !l -> Just (l  , ())) $ T.hGetLine h) ()) (hClose h)

-- | A generalized version of readVectorHandle allowing for the transformation of candidates and the resulting vector. See fileNamesSorted for an example of use.
readVectorHandleWith :: (Text -> Text) -- ^ The function to transform the candidates.
  -> (Vector Text -> Vector Text) -- ^ The function to apply to the constructed vector before compacting.
  -> Handle -- ^ The handle to read from
  -> IO (Vector Text)
readVectorHandleWith f t h = finally (t <$> unfoldrM (const . ifM (hIsEOF h) (pure Nothing) . map (\ !l -> Just (f $! l  , ())) $ T.hGetLine h) ())
                                     (hClose h)

-- | Read a vector of filenames from the handle, get the basename by removing the path of the directory. Finally sort and deduplicate the resulting filenames.
--   Useful to get the list of executables from PATH for example.
fileNamesSorted :: Handle -> IO (Vector Text)
fileNamesSorted = readVectorHandleWith (T.takeWhileEnd (/= '/')) (uniq . modify sort)

makeSearchFunction :: (forall n. KnownNat n => SearchSettings (MatcherSized n a) n) -> (Text -> Maybe (Matcher a))
                                -> (Either Int (U.Vector Int) -> Text -> U.Vector Int -> [Text]) -> SearchFunction
makeSearchFunction s f g = searchWithMatcher $ SearchFunctions f (searchSome s)  (\m t -> g (S.fromSized <$> sizes m) t . S.fromSized)

-- | Search functions suitable for fuzzy matching. The candidate @c@ will match the query @s@ if @c@ contains all the characters in @s@ in order. In general there
--   can be several ways of matching. This tries to find a match with the minimum number of parts. It does not find the minimum number of parts, if that requires
--   reducing the extent of the partial match during search. E.g. matching @"as"@ against @"talash"@ the split will be @["tal","as","h"]@ and not
--   @["t","a","la","s","h"]@. While matching @"talash best match testing hat"@ against @"tea"@ will not result in @["talash best match ","te","sting h","a","t"]@ since
--   @"te"@ occurs only after we have match all three letters and we can't know if we will find the @"a"@ without going through the string.
searchFunctionFuzzy :: CaseSensitivity -> SearchFunction
searchFunctionFuzzy c = makeSearchFunction (fuzzySettings 512) (fuzzyMatcher c) parts

searchFunctionFuzzyCustom :: (forall n. KnownNat n => SearchSettings (MatcherSized n MatchPart) n) -> CaseSensitivity -> SearchFunction
searchFunctionFuzzyCustom f c = makeSearchFunction f (fuzzyMatcher c) parts

-- | Search functions that match the words in i.e. space separated substring in any order. @"talash best"@ will match @"be as"@ with the split
--   @["tal","as","h","be","st"]@ but "talash best" will not match @"bet"@.
searchFunctionOL :: CaseSensitivity -> SearchFunction
searchFunctionOL c = makeSearchFunction (orderlessSettings 512) (orderlessMatcher c) partsOrderless -- searchWithMatcher $ SearchFunctions (orderlessMatcher IgnoreCase) (searchSome (orderlessSettings 512)) (\m t -> partsOrderless (S.fromSized <$> sizes m) t . S.fromSized)

searchFunctionOLCustom :: (forall n. KnownNat n => SearchSettings (MatcherSized n Int) n) -> CaseSensitivity -> SearchFunction
searchFunctionOLCustom f c = makeSearchFunction f (orderlessMatcher c) partsOrderless

-- testSearch :: IO ()
-- testSearch = (\v -> traverse_ print . take 64 . snd . snd . searchWithMatcher searchFunctionsFuzzy v (Just "figrun") . U.enumFromN  0 . length $ v) . getCompact
--               =<< readVectorStdIn
