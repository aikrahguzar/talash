{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Talash.Brick.Internal (twoColumnText , columns , searchWidget , searchWidgetAux , headingAndBody , listWithHighlights , columnsWithHighlights
                             , MatchSetG (..) , Searcher (..) , SearchEvent (..) , SearchEnv (..) ,  EventHooks (..) , AppSettingsG (..)
                             , SearchEventSized (..) , SearcherSized (..)
                             , queryEditor , matches , matcher , numMatches , eventSource , theme , hooks , chunkSize , maximumMatches , eventStrategy
                             , matchedTop , totMatches , term , getQuery , defHooks , theMain , initialSearcher , generateSearchEvent
                             , handleKeyEvent , handleSearch , selectedElement , renderList , module Export) where

import Brick as Export
import Brick.BChan as Export (BChan , newBChan , writeBChan)
import Brick.Widgets.Border as Export (border, vBorder, hBorder, borderAttr)
import Brick.Widgets.Border.Style as Export
import Brick.Widgets.Center as Export (vCenter, center)
import Brick.Widgets.Edit  as Export (editor , editorText, renderEditor, Editor, handleEditorEvent, getEditContents, applyEdit )
import Brick.Widgets.List as Export (Splittable (..) , List, list ,handleListEvent, handleListEventVi, listAttr, listSelectedAttr, listSelectedElement , listSelectedL
                                    , listReplace , listElements, GenericList (listElements, listSelected) , listMoveUp , listMoveDown , slice)
import qualified Brick.Widgets.List as L
import qualified Data.Set as DS
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as S
import Graphics.Vty as Export (defAttr, cyan, white, blue, withStyle, bold, brightMagenta, black, magenta, brightBlue, Attr, defaultConfig, mkVty, green, standardIOConfig)
import qualified Graphics.Vty as V
import Graphics.Vty.Config (inputFd)
import Graphics.Vty.Input.Events as Export
import Lens.Micro (_head , (^?))
import Lens.Micro.TH as Export ( makeLenses )
import System.Posix.IO
import System.Posix.Terminal
import Talash.Chunked as Export
import Talash.Intro

newtype MatchSetG a = MatchSetG (DS.Set a) deriving Foldable

instance Splittable MatchSetG where
  splitAt n (MatchSetG s) = (MatchSetG p , MatchSetG q)
    where
      (p,q) = DS.splitAt n s

data SearchEventSized n a = SearchEventSized  { _matcherEv :: {-# UNPACK #-} !(MatcherSized n a)
                                                -- | The (maximum possible) number of matches. See the note on `_numMatches`.
                                              , _totMatches :: {-# UNPACK #-} !Int
                                              -- | The term which was searched for.
                                              , _term :: {-# UNPACK #-} !Text
                                              , -- | The matches received.
                                                _matchedTop :: !(MatchSetSized n)}
makeLenses ''SearchEventSized

data SearchEvent a = forall n. KnownNat n => SearchEvent (SearchEventSized n a)

data SearcherSized n a = SearcherSized { -- | The editor to get the query from.
                                         _queryEditor :: Editor Text Bool
                                       , _matches :: GenericList Bool MatchSetG (ScoredMatchSized n)
                                       , _matcher :: MatcherSized n a
                                       , _numMatches :: Int
                                       , _eventSource :: BChan (SearchEvent a) -- ^ The BChan from which the app receives search events.
                                       }
makeLenses ''SearcherSized

data Searcher a = forall n. KnownNat n => Searcher {getSearcher :: SearcherSized n a}

{-# INLINE generateSearchEvent #-}
generateSearchEvent :: forall m a b c. KnownNat m => (SearchReport -> Bool) -> BChan (SearchEvent a) -> c -> SearchReport -> MatcherSized m a -> MatchSetSized m -> IO ()
generateSearchEvent p b _ = go
  where
    go r m = when (p r) . writeBChan b . SearchEvent . SearchEventSized m (r ^. nummatches) (r ^. searchedTerm)

-- | Event hooks are almost direct translations of the events from vty i.e. see `Event`.
data EventHooks a = EventHooks { keyHook :: Key -> [Modifier] -> EventM Bool a ()
                               , pasteHook :: ByteString -> EventM Bool a ()
                               , resizeHook :: Int -> Int -> EventM Bool a ()
                               , mouseDownHook :: Int -> Int -> Button -> [Modifier] -> EventM Bool a ()
                               , mouseUpHook   :: Int -> Int -> Maybe Button -> EventM Bool a ()
                               , focusLostHook :: EventM Bool a ()
                               , focusGainedHook :: EventM Bool a ()}

data AppSettingsG (n :: Nat) a b t   =   AppSettings { _theme :: t
                                                     , _hooks :: ReaderT (SearchEnv n a b) EventHooks (Searcher a) -- ^ The event hooks which can make use of the search environment.
                                                     , _chunkSize :: Proxy n
                                                     , _maximumMatches :: Int
                                                     , _eventStrategy :: SearchReport -> Bool}
makeLenses ''AppSettingsG

defHooks :: EventHooks a
defHooks = EventHooks (const . const (pure ())) (const (pure ())) (const . const (pure ())) (const . const . const . const (pure ()))
                                (const . const . const (pure ())) (pure ()) (pure ())

getQuery :: SearcherSized n a -> Text
getQuery s = fromMaybe "" . listToMaybe . getEditContents $ s ^. queryEditor

twoColumnText :: Int -> Text -> Text -> Widget n
twoColumnText n t1 t2 =  joinBorders . vLimit 1 $ go n t1 <+> go 100 t2
  where
    go m t = hLimitPercent m $ padLeftRight 2 (txt t) <+> fill ' '

columns :: (a -> Widget n) -> [AttrName] -> [Int] -> [a] -> Widget n
columns f as ls = vLimit 1 . hBox . zipWith3 (\a l t -> hLimitPercent l $ (padRight (Pad 2) . withAttr a . f $ t) <+> fill ' ') as ls

searchWidget :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n
searchWidget b p e = hBox [withAttr (attrName "Prompt") (padLeftRight 2 . txt $ p) , padLeftRight 2 (renderEditor (hBox . map txt) b e)]

searchWidgetAux :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n -> Widget n
searchWidgetAux b p e w = hBox [withAttr (attrName "Prompt") (padLeftRight 2 . txt $ p) , padLeftRight 2 (renderEditor (hBox . map txt) b e) , w]

highlightAlternate :: (a -> Widget n) -> [a] -> Widget n
highlightAlternate f = hBox . zipWith (\b e -> if b then withAttr (attrName "Highlight") (f e) else f e) (cycle [False , True])

headingAndBody :: Text -> Text -> Widget n
headingAndBody h b = withAttr (attrName "Heading") (txt h) <=> txtWrap b

listWithHighlights :: (Ord n , Show n , KnownNat m , KnownNat l) => SearchEnv l a (Widget n) -> Text
                                                        -> MatcherSized m a -> Bool -> GenericList n MatchSetG (ScoredMatchSized m) -> Widget n
listWithHighlights env c m = renderList (\s e -> hBox [txtLine (if s then c else "  ") , hBox . go $! e])
  where
    go (ScoredMatchSized _ i v) = (env ^. searchFunctions . display) (\b e -> if b then withAttr (attrName "Highlight") (txtLine e) else txtLine e) m ((env ^. candidates) ! i) v

columnsWithHighlights :: Text -> (a -> [[Text]]) -> [AttrName] -> [Int] -> Bool -> a -> Widget n
columnsWithHighlights c f as ls s e = (txt (if s then c else "  ") <+>) . columns (highlightAlternate txt) as ls . f $! e

theMain :: Ord n => App b e n -> BChan e -> b -> IO b
theMain a b s = (\v -> customMain v (pure v) (Just b) a s) =<< mkVty =<<(\c -> (\fd -> c {inputFd = Just fd}) <$> termFd) =<< standardIOConfig
  where
    termFd = (\f -> openFd f ReadOnly Nothing (OpenFileFlags False False False False False)) =<< getControllingTerminalName

drawListElements :: (Ord n, Show n, Foldable t, Splittable t) => (Int -> Bool -> a -> Widget n) -> Bool -> GenericList n t a  -> Widget n
drawListElements drawElem foc l =
    Widget Greedy Greedy $ do
        c <- getContext
        let es = slice start (numPerHeight * 2) (l^. L.listElementsL)
            idx = fromMaybe 0 (l^. L.listSelectedL)
            start = max 0 $ idx - numPerHeight + 1
            numPerHeight = 1 + (c^.availHeightL - 1) `div` (l^. L.listItemHeightL)
            off = start * (l^. L.listItemHeightL)
            drawElement j e =
                let isSelected = Just j == l^.listSelectedL
                    elemWidget = drawElem j isSelected e
                    selItemAttr = withDefAttr listSelectedAttr
                    makeVisible = if isSelected
                                  then visible . selItemAttr
                                  else id
                in makeVisible elemWidget
        render $ viewport (l^. L.listNameL) Vertical $
                 translateBy (Location (0, off)) $ vBox . zipWith drawElement [start ..] . toList $ es

selectedElementInternal :: (Foldable t, Splittable t) => GenericList n t a -> Maybe (Int, a)
selectedElementInternal l = do
    sel <- l^.listSelectedL
    let (_, xs) = L.splitAt sel (l ^. L.listElementsL)
    (sel,) <$> toList xs ^? _head

{-# INLINE renderListWithIndex #-}
renderListWithIndex :: (Ord n, Show n, Foldable t, Splittable t) => (Int -> Bool -> a -> Widget n) -> Bool -> GenericList n t a -> Widget n
renderListWithIndex drawElem foc = withDefAttr listAttr . drawListElements drawElem foc

renderList :: (Foldable t, Splittable t, Ord n, Show n) => (Bool -> e -> Widget n) -> Bool -> GenericList n t e -> Widget n
renderList drawElem = renderListWithIndex $ const drawElem

handleQuery :: KnownNat n => SearchEnv n a c -> Key -> [Modifier] -> Searcher b -> EventM Bool (Searcher b) ()
handleQuery env k m (Searcher s) = put . Searcher =<< editStep =<< (nestEventM' s . zoom queryEditor . handleEditorEvent . VtyEvent $ EvKey k m)
  where
    editStep ns = sendStep (getQuery ns) $> ns
    sendStep nq = unless (nq == getQuery s) $ liftIO (sendQuery env nq)

-- | Handling of keypresses. The default bindings are
--  @Enter@ exits the app with the current selection.
--  @Esc@ exits without any selection
--  @Up@ , @Down@ , @PageUp@ and @PageDown@ move through the matches.
-- All others keys are used for editing the query. See `handleEditorEvent` for details.
{-# INLINE handleKeyEvent #-}
handleKeyEvent :: (KnownNat n) => SearchEnv n a c -> Key -> [Modifier] -> EventM Bool (Searcher b) ()
handleKeyEvent env k m
  | k == KEnter                                  , null m = halt
  | k == KEsc                                    , null m = modify (\(Searcher s) -> Searcher . ((matches . listSelectedL) .~ Nothing) $ s) >> halt
  | k == KChar '\t'                              , null m = modify (\(Searcher s) -> Searcher . over matches listMoveDown $ s)
  | k == KBackTab                                , null m = modify (\(Searcher s) -> Searcher . over matches listMoveUp $ s)
  | k `elem` [KUp , KDown , KPageUp , KPageDown] , null m = (\(Searcher s) -> put . Searcher =<< (nestEventM' s . zoom matches . handleListEvent $ EvKey k m)) =<< get
  | otherwise                                             = handleQuery env k m =<< get

-- | The initial state of the searcher. The editor is empty.
initialSearcher :: SearchEnv n a c -> BChan (SearchEvent a) -> SearcherSized 0 a
initialSearcher env = SearcherSized (editorText True (Just 1) "") (list False (MatchSetG DS.empty) 0) emptyMatcher 0

{-# INLINE  handleSearchSized #-}
handleSearchSized :: (KnownNat n , KnownNat m) => SearcherSized n a -> SearchEventSized m a -> SearcherSized m a
handleSearchSized s e = SearcherSized (s ^. queryEditor) (list False (MatchSetG (e ^. matchedTop)) 0)
                                      (e ^. matcherEv) (e ^. totMatches) (s ^. eventSource)

{-# INLINE  handleSearch #-}
handleSearch :: SearchEvent a -> EventM Bool (Searcher a) ()
handleSearch (SearchEvent e) = modify (\(Searcher s) -> if (e ^. term == getQuery s) then Searcher (handleSearchSized s e) else Searcher s)

selectedElement :: KnownNat n => Chunks n -> Searcher a -> Maybe Text
selectedElement c (Searcher s) = (map ((c !) . chunkIndex . snd) . selectedElementInternal . (^. matches)) s

txtLine :: Text -> Widget n
txtLine s =
    Widget Fixed Fixed $ do
      c <- getContext
      return $ (imageL .~ (V.text' (c^.attrL) s)) emptyResult
