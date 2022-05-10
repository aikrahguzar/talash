{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Talash.Brick.Internal2 (twoColumnText , columns , searchWidget , searchWidgetAux , headingAndBody , listWithHighlights , columnsListWithHighlights
                             , Searcher (..) , SearchEvent (..) , SearchEnv (..) ,  EventHooks (..) , AppSettingsG (..) , MatchVectorSized (..)
                             , MatchVector (..) , WrappedMatchVector (..) , MatchList (..) , SearchEventSized (..) , SearcherSized (..)
                             , queryEditor , matches , matcher , numMatches , eventSource , theme , hooks , chunkSize , maximumMatches , eventStrategy
                             , matchedTop , totMatches , term , getQuery , defHooks , theMain , haltQuit , initialSearcher , generateSearchEvent
                             , handleKeyEvent , handleSearch , module Export) where

import Brick as Export
import Brick.BChan as Export (BChan , newBChan , writeBChan)
import Brick.Widgets.Border as Export (border, vBorder, hBorder, borderAttr)
import Brick.Widgets.Border.Style as Export
import Brick.Widgets.Center as Export (vCenter, center)
import Brick.Widgets.Edit  as Export (editor , editorText, renderEditor, Editor, handleEditorEvent, getEditContents, applyEdit )
import Brick.Widgets.List as Export (List, list ,handleListEvent, handleListEventVi, listAttr, listSelectedAttr, listSelectedElement , listSelectedL
                                    ,listReplace , listElements, GenericList (listElements, listSelected) , listMoveUp , listMoveDown)
import qualified Brick.Widgets.List as L
import Data.Vector (Vector , force , take , unsafeIndex , elemIndex)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as S
import GHC.TypeLits
import Graphics.Vty as Export (defAttr, cyan, white, blue, withStyle, bold, brightMagenta, black, magenta, brightBlue, Attr, defaultConfig, mkVty, green, standardIOConfig)
import Graphics.Vty.Config (Config(inputFd))
import Graphics.Vty.Input.Events as Export
import Lens.Micro as Export (ASetter' , over, set, (^.) , _1 , _2 , _3 , (.~) , (?~) , (%~) , to)
import Lens.Micro.TH as Export ( makeLenses )
import System.Posix.IO
import System.Posix.Terminal
import Talash.Chunked
import Talash.Intro
import Data.MonoTraversable as Export
import Talash.ScoredMatch (ScoredMatchSized (ScoredMatchSized))
import Talash.Core hiding (makeMatcher)

newtype MatchVectorSized (n::Nat) = MatchVectorSized (U.Vector (ScoredMatchSized n)) deriving (Eq, Ord, Show, MonoFunctor, MonoFoldable)
type instance Element (MatchVectorSized n) = ScoredMatchSized n

data MatchVector = forall n. KnownNat n => MatchVector {-# UNPACK #-} !(MatchVectorSized n)

newtype WrappedMatchVector (n :: Nat) a = WrappedMatchVector (WrappedMono (MatchVectorSized n) a) deriving (MonoFunctor, MonoFoldable , Foldable)
type instance Element (WrappedMatchVector (n :: Nat) a) = ScoredMatchSized n
instance KnownNat n => L.Splittable (WrappedMatchVector n) where
  splitAt n (WrappedMatchVector (WrappedMono (MatchVectorSized v))) = (WrappedMatchVector (WrappedMono (MatchVectorSized x))
                                                                      , WrappedMatchVector (WrappedMono (MatchVectorSized y)))
    where
      (x,y) = U.splitAt n v

data MatchList a = forall n. KnownNat n => MatchList {getList :: !((WrappedMatchVector n) a)}

-- data MatchListSized n t e =
--     List { listElements :: !(t e)
--          -- ^ The list's sequence of elements.
--          , listSelected :: !(Maybe Int)
--          -- ^ The list's selected element index, if any.
--          , listItemHeight :: Int
--          -- ^ The height of an individual item in the list.
--          } deriving (Functor, Foldable, Traversable, Show, Generic)

data SearchEventSized n a = SearchEventSized  { -- | The matches received.
                                                _matchedTop :: {-# UNPACK #-} !(MatchVectorSized n)
                                              -- | The (maximum possible) number of matches. See the note on `_numMatches`.
                                              , _matcherEv :: {-# UNPACK #-} !(MatcherSized n a)
                                              , _totMatches :: {-# UNPACK #-} !Int
                                              -- | The term which was searched for.
                                              , _term :: {-# UNPACK #-} !Text}
makeLenses ''SearchEventSized

data SearchEvent a = forall n. KnownNat n => SearchEvent (SearchEventSized n a)

data SearcherSized n a = SearcherSized { -- | The editor to get the query from.
                                    _queryEditor :: Editor Text Bool
                              -- | The matches received split up as alternating sequences of match substrings and the gap between them. The first substring is always a gap
                              --   and can be empty, the rest should be no empty.
                                  , _matches :: GenericList Bool (WrappedMatchVector n) (ScoredMatchSized n)
                                  , _matcher :: MatcherSized n a
                                  , _numMatches :: Int
                              -- | The (maximum possible) number of matches. This is the length of vector stored in `_allMatches` which also contains the indices of
                              --   which weren't matched in case enough matches were found before going through all the candidates.
                                 , _eventSource :: BChan (SearchEvent a) -- ^ The BChan from which the app receives search events.
                                 }
makeLenses ''SearcherSized

data Searcher a = forall n. KnownNat n => Searcher {getSearcher :: SearcherSized n a}

{-# INLINE generateSearchEvent #-}
generateSearchEvent :: forall n m a. (KnownNat n , KnownNat m) => SearchFunctions a -> (SearchReport -> Bool) -> BChan (SearchEvent a) -> SearchReport -> Chunks n
                                                                                -> MatcherSized m a -> MatchSetSized m -> IO ()
generateSearchEvent f p = go
  where
    go b r c m s = when (p r) . writeBChan b $ event
      where
        event = SearchEvent $ SearchEventSized (MatchVectorSized (matchSetSizedToVector s)) m (r ^. nummatches) (r ^. searchedTerm)

-- | Event hooks are almost direct translations of the events from vty i.e. see `Event`.
data EventHooks a = EventHooks { keyHook :: Key -> [Modifier] -> a -> EventM Bool (Next a)
                               , pasteHook :: ByteString -> a -> EventM Bool (Next a)
                               , resizeHook :: Int -> Int -> a -> EventM Bool (Next a)
                               , mouseDownHook :: Int -> Int -> Button -> [Modifier] -> a -> EventM Bool (Next a)
                               , mouseUpHook   :: Int -> Int -> Maybe Button -> a -> EventM Bool (Next a)
                               , focusLostHook :: a -> EventM Bool (Next a)
                               , focusGainedHook :: a -> EventM Bool (Next a)}

data AppSettingsG (n :: Nat) a t   =   AppSettings { _theme :: t
                                                   , _hooks :: ReaderT (SearchEnv n a) EventHooks (Searcher a) -- ^ The event hooks which can make use of the search environment.
                                                   , _chunkSize :: Proxy n
                                                   , _maximumMatches :: Int
                                                   , _eventStrategy :: SearchReport -> Bool}
makeLenses ''AppSettingsG

defHooks :: EventHooks a
defHooks = EventHooks (const . const continue) (const continue) (const . const continue) (const . const . const . const continue)
                                (const . const . const continue) continue continue
-- | Quit without any selection.
haltQuit :: Searcher a -> EventM m (Next (Searcher a))
haltQuit s@(Searcher s') = halt . Searcher . ((matches . listSelectedL) .~ Nothing) $ s'

getQuery :: SearcherSized n a -> Text
getQuery s = fromMaybe "" . listToMaybe . getEditContents $ s ^. queryEditor

twoColumnText :: Int -> Text -> Text -> Widget n
twoColumnText n t1 t2 =  joinBorders . vLimit 1 $ go n t1 <+> go 100 t2
  where
    go m t = hLimitPercent m $ padLeftRight 2 (txt t) <+> fill ' '

columns :: (a -> Widget n) -> [AttrName] -> [Int] -> [a] -> Widget n
columns f as ls = vLimit 1 . hBox . zipWith3 (\a l t -> hLimitPercent l $ (padRight (Pad 2) . withAttr a . f $ t) <+> fill ' ') as ls

searchWidget :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n
searchWidget b p e = withAttr "Prompt" (padLeftRight 2 . txt $ p) <+> padLeftRight 2 (renderEditor (txt . fromMaybe "" . listToMaybe) b e)

searchWidgetAux :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n -> Widget n
searchWidgetAux b p e w = withAttr "Prompt" (padLeftRight 2 . txt $ p) <+> padLeftRight 2 (renderEditor (txt . fromMaybe "" . listToMaybe) b e) <+> w

highlightAlternate :: Foldable f => (a -> Widget n) -> f a -> Widget n
highlightAlternate f = fst . foldl' (\(!w , !b) !n  -> (w <+> if b then withAttr "Highlight" (f n) else f n , not b)) (emptyWidget , False)

headingAndBody :: Text -> Text -> Widget n
headingAndBody h b = withAttr "Heading" (txt h) <=> txtWrap b

listWithHighlights :: (Ord n , Show n , KnownNat m , KnownNat l) => SearchEnv l a -> Text
                                                        -> MatcherSized m a -> Bool -> GenericList n (WrappedMatchVector m) (ScoredMatchSized m) -> Widget n
listWithHighlights env c m = renderList (\s e -> vLimit 1 . (txt (if s then c else "  ") <+>) . (<+> fill ' ') . highlightAlternate txt . go $! e)
  where
    go (ScoredMatchSized _ i v) = (env ^. searchFunctions . display) m ((env ^. candidates) ! i) v

columnsListWithHighlights :: (Foldable f , Ord n , Show n) => Text -> (a -> [f Text]) -> [AttrName] -> [Int] -> Bool -> List n a -> Widget n
columnsListWithHighlights c f as ls = renderList (\s e -> (txt (if s then c else "  ") <+>) . columns (highlightAlternate txt) as ls . f $! e)

theMain :: Ord n => App b e n -> BChan e -> b -> IO b
theMain a b s = (\v -> customMain v (pure v) (Just b) a s) =<< mkVty =<<(\c -> (\fd -> c {inputFd = Just fd}) <$> termFd) =<< standardIOConfig
  where
    termFd = (\f -> openFd f ReadOnly Nothing (OpenFileFlags False False False False False)) =<< getControllingTerminalName

drawListElements :: (Ord n, Show n, Foldable t, L.Splittable t) => Bool -> GenericList n t a -> (Int -> Bool -> a -> Widget n) -> Widget n
drawListElements foc l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext
        let es = L.slice start (numPerHeight * 2) (l^.L.listElementsL)
            idx = fromMaybe 0 (l^.listSelectedL)
            start = max 0 $ idx - numPerHeight + 1
            numPerHeight = 1 + (c^.availHeightL - 1) `div` (l^. L.listItemHeightL)
            off = start * (l^. L.listItemHeightL)
            drawElement j e =
                let isSelected = Just j == l^.listSelectedL
                    elemWidget = drawElem j isSelected e
                    selItemAttr = if foc
                                  then withDefAttr L.listSelectedFocusedAttr
                                  else withDefAttr listSelectedAttr
                    makeVisible = if isSelected
                                  then visible . selItemAttr
                                  else id
                in makeVisible elemWidget
        render $ viewport (l^.L.listNameL) Vertical $
                 translateBy (Location (0, off)) $
                 vBox $ zipWith drawElement [start .. ] . toList $ es

{-# INLINE renderListWithIndex #-}
renderListWithIndex :: (Ord n, Show n, Foldable t, L.Splittable t) => (Int -> Bool -> a -> Widget n) -> Bool -> GenericList n t a -> Widget n
renderListWithIndex drawElem foc l = withDefAttr listAttr $ drawListElements foc l drawElem

renderList :: (Foldable t, L.Splittable t, Ord n, Show n) => (Bool -> e -> Widget n) -> Bool -> GenericList n t e -> Widget n
renderList drawElem = renderListWithIndex $ const drawElem

-- | Handling of keypresses. The default bindings are
--  @Enter@ exits the app with the current selection.
--  @Esc@ exits without any selection
--  @Up@ , @Down@ , @PageUp@ and @PageDown@ move through the matches.
-- All others keys are used for editing the query. See `handleEditorEvent` for details.
{-# INLINE handleKeyEvent #-}
handleKeyEvent :: (KnownNat n) => SearchEnv n a -> Key -> [Modifier] -> Searcher b -> EventM Bool (Next (Searcher b))
handleKeyEvent env = go
  where
    go k m s@(Searcher s')
      | k == KEnter                                  , null m = halt s
      | k == KEsc                                    , null m = haltQuit s
      | k == KChar '\t'                              , null m = continue . Searcher . over matches listMoveDown $ s'
      | k == KBackTab                                , null m = continue . Searcher . over matches listMoveUp $ s'
      | k `elem` [KUp , KDown , KPageUp , KPageDown] , null m = continue . Searcher =<< handleEventLensed s' matches handleListEvent (EvKey k m)
      | otherwise                                             = continue =<< liftIO . editStep . Searcher =<< handleEventLensed s' queryEditor handleEditorEvent (EvKey k m)
      where
        editStep ns@(Searcher ns')
          | nq /= getQuery s'    = sendQuery env nq $> ns
          | otherwise            = pure ns
          where
            nq = getQuery ns'

-- | The initial state of the searcher. The editor is empty.
initialSearcher :: SearchEnv n a -> BChan (SearchEvent a) -> SearcherSized 0 a
initialSearcher env = SearcherSized (editorText True (Just 1) "") (list False (WrappedMatchVector (WrappedMono (MatchVectorSized U.empty))) 0) emptyMatcher 0

{-# INLINE  handleSearchSized #-}
handleSearchSized :: (KnownNat n , KnownNat m) => SearcherSized n a -> SearchEventSized m a -> SearcherSized m a
handleSearchSized s e = SearcherSized (s ^. queryEditor) (list False (WrappedMatchVector (WrappedMono (e ^. matchedTop))) 0)
                                      (e ^. matcherEv) (e ^. totMatches) (s ^. eventSource)

{-# INLINE  handleSearch #-}
handleSearch :: Searcher a -> SearchEvent a -> EventM Bool (Next (Searcher a))
handleSearch (Searcher s) (SearchEvent e) = continue . Searcher $ handleSearchSized s e