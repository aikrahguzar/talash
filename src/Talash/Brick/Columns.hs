{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | This module is a quick hack to enable representation of data with columns of text. We use the fact the since the candidates are supposed to fit in a line,
-- they can't have a newlines but text with newlines can otherwise be searched normally. We use this here to separate columns by newlines. Like in
-- "Talash.Brick" the candidates comes from vector of text. Each such text consists of a fixed number of lines each representing a column. We match against such
-- text and `partsColumns` then uses the newlines to reconstruct the columns and the parts of the match within each column. This trick of using newline saves us
-- from dealing with the partial state of the match when we cross a column but there is probably a better way . The function `runApp` , `selected` and
-- `selectedIndex` hide this and instead take as argument a `Vector` [`Text`] with each element of the list representing a column. Each list must have the same
-- length. Otherwise this module provides a reduced version of the functions in "Talash.Brick".

-- This module hasn't been tested on large data and will likely be slow.
module Talash.Brick.Columns (-- * Types
                     Searcher (..) , SearchEvent (..) , SearchEnv (..) ,  EventHooks (..) , AppTheme (..) , AppSettings (..) , SearchFunctions , CaseSensitivity (..)
                     -- * The Brick App and Helpers
                    , searchApp , defSettings , searchFunctionFuzzy , searchFunctionOL , searchFunctionFuzzyCustom ,  selected , selectedIndex , runApp
                     -- * Lenses
                     -- ** Searcher
                    , query , prevQuery , allMatches , matches , numMatches , wait
                     -- ** SearchEvent
                    , matchedTop , totalMatches , term
                     -- ** SearchEnv
                    , searchFunctions , candidates , eventSource
                     -- ** SearchFunctions
                    , makeMatcher , lister , displayer
                     -- ** AppTheme
                    , prompt , themeAttrs , borderStyle
                     -- ** SearchSettings
                    , theme , hooks
                     -- * Exposed Internals
                    , makeQuery , haltQuit , handleKeyEvent , handleSearch , editStep , replaceSearch , search , searcherWidget , initialSearcher
                    , searchWithMatcher , partsColumns , emptyIndices , runApp' , selected' , selectedIndex' ) where

import Control.Concurrent(forkIO , killThread, ThreadId)
import Control.Exception (finally , catch, AsyncException)
import Data.IORef (IORef , newIORef , atomicModifyIORef' , atomicWriteIORef)
import qualified Data.Text as T
import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))
import qualified Data.Text.IO as T
import Data.Vector (Vector , (!), force , generate , take, singleton , convert, enumFromN, unfoldrM, indexed  , elemIndex)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as S
import GHC.Compact (Compact , compact , getCompact)
import GHC.TypeNats
import Intro hiding (on ,replicate , take)
import System.Environment (getArgs)
import System.IO ( Handle , hIsEOF , isEOF, hClose, stdin)
import Talash.Brick.Internal
import Talash.Core hiding (makeMatcher)

type SearchFunction = Vector Text -> Maybe Text -> U.Vector Int -> (U.Vector Int  ,  (Int , Vector [[Text]]))

data SearchFunctions a = SearchFunctions { _makeMatcher :: Text -> Maybe (Matcher a)
                                         , _lister :: forall n. KnownNat n => MatcherSized n a -> Vector Text -> U.Vector Int -> (U.Vector Int , U.Vector (Indices n))
                                         , _displayer :: forall n. KnownNat n => MatcherSized n a -> Text -> S.Vector n Int -> [[Text]] }
makeLenses ''SearchFunctions

data Searcher a = Searcher { -- | The editor to get the query from.
                           _query :: Editor Text Bool
                           -- | The last query which is saved to check if we should only search among the matches for it or all the candidates.
                           , _prevQuery :: Maybe Text
                           -- | Each outer list reprsents a column. The inner list is the text for that column split up as an alternating sequences of match
                           --   substrings and the gap between them. The first substring is always a gap and can be empty, the rest should be no empty.
                           , _matches :: List Bool [[Text]]
                           -- | The (maximum possible) number of matches. This is the length of vector stored in `_allMatches` which also contains the indices of
                           --   which weren't matched in case enough matches were found before going through all the candidates.
                           , _numMatches :: Int
                           -- | ThreadId of the thread currently computing matches. Nothing if there is no such thread.
                           , _wait :: Maybe ThreadId
                           -- | Unused by default but can be used store extra state needed for any extension to the functionality. For example to have multiple
                           --   selections this can be set to a `Vector` that stores them.
                           , _extension :: a} deriving Functor
makeLenses ''Searcher

data SearchEvent = SearchEvent {
                     -- | The matches received.
                     _matchedTop :: Vector [[Text]] ,
                     -- | The (maximum possible) number of matches. See the note on `_numMatches`.
                     _totalMatches :: Int ,
                     -- | The term which was searched for.
                     _term :: Maybe Text}
makeLenses ''SearchEvent

-- | The constant environment in which they search app runs.
data SearchEnv = SearchEnv { _searchFunctions :: SearchFunction  -- ^ The functions used to find and display matches.
                           , _candidates :: Vector Text -- ^ The vector of candidates.
                           , _eventSource :: BChan SearchEvent -- ^ The BChan from which the app receives search events.
                           -- | An IORef containing the indices of the filtered candidates. These are in an IORef to make it easier to deal with them in a different thread
                           --   than the UI of the app.
                           , _allMatches :: IORef (U.Vector Int)
                           }
makeLenses ''SearchEnv

-- | Event hooks are almost direct translations of the events from vty i.e. see `Event`.
data EventHooks a = EventHooks { keyHook :: Key -> [Modifier] -> a -> EventM Bool (Next a)
                               , pasteHook :: ByteString -> a -> EventM Bool (Next a)
                               , resizeHook :: Int -> Int -> a -> EventM Bool (Next a)
                               , mouseDownHook :: Int -> Int -> Button -> [Modifier] -> a -> EventM Bool (Next a)
                               , mouseUpHook   :: Int -> Int -> Maybe Button -> a -> EventM Bool (Next a)
                               , focusLostHook :: a -> EventM Bool (Next a)
                               , focusGainedHook :: a -> EventM Bool (Next a)}

data AppTheme = AppTheme { _prompt :: Text -- ^ The prompt to display next to the editor.
                         , _columnAttrs :: [AttrName] -- ^ The attrNames to use for each column. Must have the same length or greater length than the number of columns.
                         , _columnLimits :: [Int] -- ^ The area to limit each column to. This has a really naive and unituitive implementation. Each Int
                                                  -- must be between 0 and 100 and refers to the percentage of the width the widget for a column will occupy
                                                  -- from the space left over after all the columns before it have been rendered.
                         , _themeAttrs :: [(AttrName, Attr)]  -- ^ This is used to construct the `attrMap` for the app. By default the used attarNmaes are
                                                              --  `listSelectedAttr` , `borderAttr` , @"Prompt"@ , @"Highlight"@ and @"Stats"@
                         , _borderStyle :: BorderStyle -- ^ The border style to use. By default `unicodeRounded`
                         }
makeLenses ''AppTheme

data AppSettings b = AppSettings { _theme :: AppTheme
                                 , _hooks :: ReaderT SearchEnv EventHooks (Searcher b) -- ^ The event hooks which can make use of the search environment.
                                 }
makeLenses ''AppSettings

data ColumnsIso a = ColumnsIso { _toColumns :: a -> [Text] , _fromColumns :: [Text] -> a}
makeLenses ''ColumnsIso

defHooks :: EventHooks a
defHooks = EventHooks (const . const continue) (const continue) (const . const continue) (const . const . const . const continue)
                                (const . const . const continue) continue continue

emptyIndices :: Int -> U.Vector  (Indices 0)
emptyIndices n = U.generate n ( , S.empty)

-- | Get the current query from the editor of the searcher.
makeQuery :: Searcher a -> Maybe Text
makeQuery s = headMay . getEditContents $ s ^. query

-- | Handling of keypresses. The default bindings are
--  @Enter@ exits the app with the current selection.
--  @Esc@ exits without any selection
--  @Up@ , @Down@ , @PageUp@ and @PageDown@ move through the matches.
-- All others keys are used for editing the query. See `handleEditorEvent` for details.
handleKeyEvent :: SearchEnv ->  Key -> [Modifier] -> Searcher b -> EventM Bool (Next (Searcher b))
handleKeyEvent env k m s
  | k == KEnter                                && null m = halt s
  | k == KEsc                                  && null m = haltQuit s
  | elem k [KUp , KDown , KPageUp , KPageDown] && null m = continue =<< handleEventLensed s matches handleListEvent (EvKey  k m)
  | otherwise                                            = continue =<< liftIO . editStep env =<< handleEventLensed s query handleEditorEvent (EvKey k m)

-- | Handle a search event by updating `_numMatches` , `_matches` and `_wait`.
handleSearch :: Vector Text -> Searcher a ->  SearchEvent -> EventM Bool (Next (Searcher a))
handleSearch v s e = continue . (numMatches .~ e ^. totalMatches) . (matches %~ listReplace (e ^. matchedTop) Nothing) . (wait .~ Nothing) $ s

-- | The brick widget used to display the editor and the search result.
searcherWidget :: [AttrName] -> [Int] -> Text -> Text -> Searcher a -> Widget Bool
searcherWidget as ls p n s = joinBorders . border $    searchWidgetAux True p (s ^. query) (withAttr "Stats" . txt $ show (s ^. numMatches) <>  "/" <> n)
                                             <=> hBorder  <=> joinBorders (columnsListWithHighlights "➜ " id as ls False (s ^. matches))

-- | Quit without any selection.
haltQuit :: Searcher a -> EventM n (Next (Searcher a))
haltQuit = halt . ((matches . listSelectedL) .~ Nothing)

-- | Handle the editing of the query by starting the computation of the matches in a new thread and storing the `ThreadId` in `_wait`.
-- If the new query contains the last query then doesn't try to match the candidates that didn't match the last query, otherwise search among all the candidates.
-- Might be possible to make the performance better by storing the indices of the filtered candidates for more than one previous query.
editStep :: SearchEnv -> Searcher b -> IO (Searcher b)
editStep env s
  | makeQuery s == (s ^. prevQuery)      = pure s
  | otherwise                            = (\w -> set wait (Just w) s') <$> replaceSearch isBigger env s'
  where
    isBigger = fromMaybe False $ T.isInfixOf <$> (s ^. prevQuery) <*> (headMay . getEditContents $ s ^. query)
    s'       = set prevQuery (makeQuery s) s

-- | The functions for generating a search event.  It is executed in a separate thread via `forkIO` in `replaceSearch`.
search :: SearchFunction -> Vector Text -> Maybe Text -> IORef (U.Vector Int) -> IO SearchEvent
search fs v t r = (\(l , tm) -> SearchEvent tm l t)  <$> atomicModifyIORef' r (fs v t)

-- | searchWithMatcher carries out one step of the search. Note that the search can stops before going through the whole vector of text. In that case the returned
--   vector of indices should contain not only the indices matched candidates but also the indices of candidates that weren't tested for a match.
searchWithMatcher :: SearchFunctions a -> SearchFunction -- Vector Text -> Maybe Text -> U.Vector  Int -> (U.Vector Int , (Int , Vector [[Text]]))
searchWithMatcher fs v t s = maybe nc go ((fs ^. makeMatcher) =<< t)
      where
        nc  = (U.enumFromN 0 (length v) , (0 , force . map (\i -> map (:[]) . T.lines $ v ! (i ^. _1)) . convert . emptyIndices . min 512 . length $ v))
        go (Matcher  f') = (iv , (U.length iv , force . map (\i -> (fs ^. displayer) f' (v ! (i ^. _1)) (i ^. _2)) . convert $ mv))
          where
            (iv , mv) = (fs ^. lister) f' v s

-- | This function dispatches the computation of matches to a new thread and returns the new threadId. It also tries to kill the thread in which a previous computation
--   was going on (Not sure if it actually accomplishes that, my understanding of exceptions is not good enough).
replaceSearch :: Bool -> SearchEnv -> Searcher b -> IO ThreadId
replaceSearch ib (SearchEnv fs v b am) s = finally (forkIO . catch wrtev $ \ (_ :: AsyncException) -> pure ()) (maybe (pure ()) killThread (s ^. wait))
  where
    wrtev = writeBChan b =<< search fs v (s ^. prevQuery) =<< mtchs
    mtchs = if ib then pure am else atomicWriteIORef am (U.enumFromN 0 $ length v) $> am

defThemeAttrs :: [(AttrName, Attr)]
defThemeAttrs = [ (listSelectedAttr, withStyle (bg white) bold) , ("Prompt" , withStyle (white `on` blue) bold)
           , ("Highlight" , withStyle (fg blue) bold) ,  ("Stats" , fg blue) ,  (borderAttr , fg cyan)]

defTheme ::AppTheme
defTheme = AppTheme {_prompt = "Find: " , _columnAttrs = repeat mempty , _columnLimits = repeat 50 , _themeAttrs = defThemeAttrs
                    , _borderStyle = unicodeRounded}

-- | Default settings. Uses blue for various highlights and cyan for borders. All the hooks except keyHook which is `handleKeyEvent` are trivial.
defSettings :: AppSettings b
defSettings = AppSettings defTheme (ReaderT (\e -> defHooks {keyHook = handleKeyEvent e}))

-- | Tha app itself.  `selected` and the related functions are probably more convenient for embedding into a larger program.
searchApp :: AppSettings b -> SearchEnv -> App (Searcher b) SearchEvent Bool
searchApp (AppSettings th hks) env@(SearchEnv fs v b _) = App {appDraw = ad , appChooseCursor = showFirstCursor , appHandleEvent = he , appStartEvent = pure , appAttrMap = am}
  where
    ad                = (:[]) . withBorderStyle (th ^. borderStyle) . searcherWidget (th ^. columnAttrs) (th ^. columnLimits) (th ^. prompt) (show . length $ v)
    am                                    = const $ attrMap defAttr (th ^. themeAttrs)
    hk                                    = runReaderT hks env
    he s (VtyEvent (EvKey k m))           = keyHook hk k m s
    he s (VtyEvent (EvMouseDown i j b m)) = mouseDownHook   hk i j b m s
    he s (VtyEvent (EvMouseUp   i j b  )) = mouseUpHook     hk i j b   s
    he s (VtyEvent (EvPaste     b      )) = pasteHook       hk     b   s
    he s (VtyEvent  EvGainedFocus       ) = focusGainedHook hk         s
    he s (VtyEvent  EvLostFocus         ) = focusLostHook   hk         s
    he s (AppEvent e)                     = if e ^. term == s ^. prevQuery then handleSearch v s e else continue s
    he s _                                = continue s

-- | The initial state of the searcher.
initialSearcher :: a -> Vector Text -> Searcher a
initialSearcher e v = Searcher { _query = editorText True (Just 1) "" , _prevQuery = Nothing  , _wait = Nothing
                               , _matches = list False (map (:[]) . T.lines <$> take 512 v) 0 , _numMatches = length v , _extension = e}

-- | This function reconstructs the columns from the parts returned by the search by finding the newlines.
partsColumns :: [Text] -> [[Text]]
partsColumns = initDef [] . unfoldr (\l -> if null l then Nothing else Just . go $ l)
  where
    go x = bimap (f <>) (maybe s' (: s')) hs
      where
        (f , s) = break (T.isInfixOf "\n") x
        s'      = tailDef [] s
        hs      = maybe ([] , Nothing) (bimap (:[]) (T.stripPrefix "\n") . T.breakOn "\n") . headMay $ s

makeSearchFunction :: (forall n. KnownNat n => SearchSettings (MatcherSized n a) n) -> (Text -> Maybe (Matcher a))
                                -> (Either Int (U.Vector Int) -> Text -> U.Vector Int -> [Text]) -> SearchFunction
makeSearchFunction s f g = searchWithMatcher $ SearchFunctions f (searchSome s)  (\m t -> partsColumns . g (S.fromSized <$> sizes m) t . S.fromSized)

-- | Search functions suitable for fuzzy matching. The candidate @c@ will match the query @s@ if @c@ contains all the characters in @s@ in order. In general there
--   can be several ways of matching. This tries to find a match with minimum number of parts of. It does not find the minimum number of parts, if that requires
--   reducing the extent of the partial match during search. E.g. matching @"as"@ against @"talash"@ the split will be @["tal","as","h"]@ and not
--   @["t","a","la","s","h"]@. While matching @"talash best match testing hat"@ against @"tea"@ will not result in @["talash best match ","te","sting h","a","t"]@ since
--   @"te"@ occurs only after we have match all three letters and we can't know if we will find the @"a"@ without going through the string.
searchFunctionFuzzy :: SearchFunction
searchFunctionFuzzy = makeSearchFunction (fuzzySettings 512) (fuzzyMatcher IgnoreCase)  parts -- (\m t -> partsColumns . parts (S.fromSized <$> sizes m) t . S.fromSized) (searchSome (fuzzySettings 512))

searchFunctionFuzzyCustom :: (forall n. KnownNat n => SearchSettings (MatcherSized n MatchPart) n) -> CaseSensitivity -> SearchFunction
searchFunctionFuzzyCustom f c = makeSearchFunction f (fuzzyMatcher c) parts

-- | Search functions that match the words in i.e. space separated substring in any order. "talash best" will match "be as" with the split
--   ["tal","as","h","be","st"] but "talash best" will not match "bet".
searchFunctionOL :: SearchFunction
searchFunctionOL = makeSearchFunction (orderlessSettings 512) (orderlessMatcher IgnoreCase) partsOrderless

searchFunctionOLCustom :: (forall n. KnownNat n => SearchSettings (MatcherSized n Int) n) -> CaseSensitivity -> SearchFunction
searchFunctionOLCustom f c = makeSearchFunction f (orderlessMatcher c) partsOrderless

-- | The \'raw\' version of `runApp` taking a vector of text with columns separated by newlines.
runApp' :: b -> AppSettings b -> SearchFunction -> Vector Text -> IO (Searcher b)
runApp' e s f v = (\b -> (\r -> theMain (searchApp s (SearchEnv f v b r)) b . initialSearcher e $ v) =<< newIORef (U.enumFromN 0 . length $ v)) =<< newBChan 8

-- | Run app with given settings and return the final Searcher state.
runApp :: b -> AppSettings b  -> SearchFunction -> Vector [Text] -> IO (Searcher b)
runApp e s f = runApp' e s f . map T.unlines

-- | The \'raw\' version of `selected` taking a vector of text with columns separated by newlines.
selected' :: AppSettings () -> SearchFunction -> Vector Text -> IO (Maybe [Text])
selected' s f  = map (map (map mconcat . snd) . listSelectedElement . (^. matches)) . runApp' () s f . getCompact <=< compact . force

-- | Run app and return the the selection if there is one else Nothing.
selected :: AppSettings () -> SearchFunction -> Vector [Text] -> IO (Maybe [Text])
selected s f  = selected' s f . map T.unlines

selectedIso :: ColumnsIso a -> AppSettings () -> SearchFunction -> Vector a -> IO (Maybe a)
selectedIso (ColumnsIso from to) s f = map (map to) . selected' s f . map (T.unlines . from)

-- | The \'raw\' version of `selectedIndex` taking a vector of text with columns separated by newlines.
selectedIndex' :: AppSettings () -> SearchFunction -> Vector Text -> IO (Maybe Int)
selectedIndex' s f v = ((`elemIndex` v) . T.unlines =<<) <$> selected' s f v

-- | Returns the index of selected candidate in the vector of candidates. Note: it uses `elemIndex` which is O\(N\).
selectedIndex :: AppSettings () -> SearchFunction -> Vector [Text] -> IO (Maybe Int)
selectedIndex s f = selectedIndex' s f . map T.unlines
