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
module Talash.Brick.Columns (-- -- * Types
                      Searcher (..) , SearchEvent (..) , SearchEnv (..) ,  EventHooks (..) , AppTheme (..)
                    , AppSettings (..) , AppSettingsG (..) , SearchFunctions , CaseSensitivity (..)
                     -- * The Brick App and Helpers
                    , searchApp , defSettings , selected , selectedIndex , runApp
                     -- * Lenses
                     -- ** Searcher
                    , query , prevQuery , allMatches , matches , numMatches
                     -- ** SearchEvent
                    , matchedTop , totalMatches , term
                     -- ** SearchEnv
                    , searchFunctions , candidates , eventSource
                     -- ** AppTheme
                    , prompt , themeAttrs , borderStyle
                     -- ** SearchSettings
                    , theme , hooks
                     -- * Exposed Internals
                    , haltQuit , handleKeyEvent , handleSearch , searcherWidget , initialSearcher , partsColumns , runApp' , selected' , selectedIndex') where

import qualified Data.Text as T
import Data.Text.AhoCorasick.Automaton (CaseSensitivity (..))
import Data.Vector (Vector , force , generate , take, singleton , convert, enumFromN, unfoldrM, indexed  , elemIndex)
import GHC.Compact (Compact , compact , getCompact)
import GHC.TypeLits
import Talash.Brick.Internal
import Talash.Chunked
import Talash.Core hiding (makeMatcher)
import Talash.Intro hiding (on ,replicate , take)
import Talash.ScoredMatch (ScoredMatchSized(chunkIndex, matchData))

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

type AppSettings n a = AppSettingsG n a Text AppTheme

data ColumnsIso a = ColumnsIso { _toColumns :: a -> [Text] , _fromColumns :: [Text] -> a}
makeLenses ''ColumnsIso

-- | The brick widget used to display the editor and the search result.
searcherWidget :: (KnownNat n , KnownNat m) => AppTheme -> SearchEnv n a Text -> SearcherSized m a -> Widget Bool
searcherWidget t env s = joinBorders . border $    searchWidgetAux True (t ^. prompt) (s ^. queryEditor) (withAttr "Stats" . txt $ (T.pack . show $ s ^. numMatches))
                                        <=> hBorder  <=> joinBorders (makeColumns env "➜ " (t ^. columnAttrs) (t ^. columnLimits) (s ^. matcher) False (s ^. matches))

defThemeAttrs :: [(AttrName, Attr)]
defThemeAttrs = [ (listSelectedAttr, withStyle (bg white) bold) , ("Prompt" , withStyle (white `on` blue) bold)
           , ("Highlight" , withStyle (fg blue) bold) ,  ("Stats" , fg blue) ,  (borderAttr , fg cyan)]

defTheme ::AppTheme
defTheme = AppTheme {_prompt = "Find: " , _columnAttrs = repeat mempty , _columnLimits = repeat 50 , _themeAttrs = defThemeAttrs
                    , _borderStyle = unicodeRounded}

-- | Default settings. Uses blue for various highlights and cyan for borders. All the hooks except keyHook which is `handleKeyEvent` are trivial.
{-# INLINE defSettings#-}
defSettings :: KnownNat n => AppSettings n a
defSettings = AppSettings defTheme (ReaderT (\e -> defHooks {keyHook = handleKeyEvent e})) Proxy 1024 (\r -> r ^. ocassion == QueryDone)

-- | Tha app itself.  `selected` and the related functions are probably more convenient for embedding into a larger program.
searchApp :: KnownNat n => AppSettings n a -> SearchEnv n a Text -> App (Searcher a) (SearchEvent a) Bool
searchApp (AppSettings th hks _ _ _) env  = App {appDraw = ad , appChooseCursor = showFirstCursor , appHandleEvent = he , appStartEvent = as , appAttrMap = am}
  where
    ad (Searcher s)                                  = (:[]) . withBorderStyle (th ^. borderStyle) . searcherWidget th env $ s
    as s                                             = liftIO (sendQuery env "") $> s
    am                                               = const $ attrMap defAttr (th ^. themeAttrs)
    hk                                               = runReaderT hks env
    he s (VtyEvent (EvKey k m))                      = keyHook hk k m s
    he s (VtyEvent (EvMouseDown i j b m))            = mouseDownHook   hk i j b m s
    he s (VtyEvent (EvMouseUp   i j b  ))            = mouseUpHook     hk i j b   s
    he s (VtyEvent (EvPaste     b      ))            = pasteHook       hk     b   s
    he s (VtyEvent  EvGainedFocus       )            = focusGainedHook hk         s
    he s (VtyEvent  EvLostFocus         )            = focusLostHook   hk         s
    he s@(Searcher s') (AppEvent e@(SearchEvent e')) = if e' ^. term == getQuery s' then handleSearch s e else continue s
    he s _                                           = continue s

-- | This function reconstructs the columns from the parts returned by the search by finding the newlines.
partsColumns :: [Text] -> [[Text]]
partsColumns = initDef [] . unfoldr (\l -> if null l then Nothing else Just . go $ l)
  where
    go x = bimap (f <>) (maybe s' (: s')) hs
      where
        (f , s) = break (T.isInfixOf "\n") x
        s'      = tailDef [] s
        hs      = maybe ([] , Nothing) (bimap (:[]) (T.stripPrefix "\n") . T.breakOn "\n") . headMay $ s

makeColumns :: (Ord n , Show n , KnownNat m , KnownNat l) => SearchEnv l a Text -> Text -> [AttrName] -> [Int]
                                                         -> MatcherSized m a -> Bool -> GenericList n MatchSetG (ScoredMatchSized m) -> Widget n
makeColumns env c as ls m = renderList (columnsWithHighlights c mp as ls)
  where
    mp s = partsColumns $ (env ^. searchFunctions . display) (const id) m ((env ^. candidates) ! chunkIndex s) (matchData s)

-- | The \'raw\' version of `runApp` taking a vector of text with columns separated by newlines.
runApp' :: KnownNat n => AppSettings n a -> SearchFunctions a Text -> Chunks n -> IO (Searcher a)
runApp' s f c =     (\b -> (\env -> startSearcher env *> finally (theMain (searchApp s env) b . Searcher . initialSearcher env $ b) (stopSearcher env))
                 =<< searchEnv f (s ^. maximumMatches) (generateSearchEvent f (s ^. eventStrategy) b) c) =<< newBChan 8

-- -- | Run app with given settings and return the final Searcher state.
runApp :: KnownNat n => AppSettings n a  -> SearchFunctions a  Text -> Vector [Text] -> IO (Searcher a)
runApp s f = runApp' s f . makeChunks . map T.unlines

-- | The \'raw\' version of `selected` taking a vector of text with columns separated by newlines.
selected' :: KnownNat n => AppSettings n a -> SearchFunctions a Text -> Chunks n -> IO (Maybe [Text])
selected' s f = (\c -> map (map T.lines . selectedElement c) . runApp' s f $ c) . getCompact <=< compact . forceChunks

-- | Run app and return the the selection if there is one else Nothing.
selected :: KnownNat n => AppSettings n a -> SearchFunctions a Text -> Vector [Text] -> IO (Maybe [Text])
selected s f  = selected' s f . makeChunks . map T.unlines

selectedIso :: KnownNat n => ColumnsIso a -> AppSettings n b -> SearchFunctions b Text -> Vector a -> IO (Maybe a)
selectedIso (ColumnsIso from to) s f = map (map to) . selected' s f . makeChunks . map (T.unlines . from)

-- | The \'raw\' version of `selectedIndex` taking a vector of text with columns separated by newlines.
selectedIndex' :: KnownNat n => AppSettings n a -> SearchFunctions a Text -> Vector Text -> IO (Maybe Int)
selectedIndex' s f v = ((`elemIndex` v) . T.unlines =<<) <$> selected' s f (makeChunks v)

-- | Returns the index of selected candidate in the vector of candidates. Note: it uses `elemIndex` which is O\(N\).
selectedIndex :: KnownNat n => AppSettings n a -> SearchFunctions a Text -> Vector [Text] -> IO (Maybe Int)
selectedIndex s f = selectedIndex' s f . map T.unlines
