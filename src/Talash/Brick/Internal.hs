{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Talash.Brick.Internal (twoColumnText , columns , searchWidget , searchWidgetAux , headingAndBody , listWithHighlights , columnsListWithHighlights
                             , SearcherG (..) , SearchEventG (..) , SearchEnv (..) ,  EventHooks (..) , AppSettingsG (..)
                             , queryEditor , matches , eventSource , numMatches , theme , hooks , chunkSize , maximumMatches , eventStrategy
                             , matchedTop , totMatches , term , getQuery , defHooks , theMain , haltQuit
                             , handleKeyEvent , resetSearcher , handleSearch , module Export) where

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
import Lens.Micro as Export (ASetter' , over, set, (^.) , _1 , _2 , _3 , (.~) , (?~) , (%~))
import Lens.Micro.TH as Export ( makeLenses )
import System.Posix.IO
import System.Posix.Terminal
import Talash.Chunked
import Talash.Intro
import Data.MonoTraversable as M
import Talash.ScoredMatch (ScoredMatchSized)

data SearchEventG v a = SearchEvent  { -- | The matches received.
                                       _matchedTop :: v a
                                    -- | The (maximum possible) number of matches. See the note on `_numMatches`.
                                     , _totMatches :: Int
                                    -- | The term which was searched for.
                                     , _term :: Text}
makeLenses ''SearchEventG

data SearcherG v a = Searcher { -- | The editor to get the query from.
                             _queryEditor :: Editor Text Bool
                              -- | The matches received split up as alternating sequences of match substrings and the gap between them. The first substring is always a gap
                              --   and can be empty, the rest should be no empty.
                            , _matches :: GenericList Bool v a
                              -- | The (maximum possible) number of matches. This is the length of vector stored in `_allMatches` which also contains the indices of
                              --   which weren't matched in case enough matches were found before going through all the candidates.
                            , _eventSource :: BChan (SearchEventG v a) -- ^ The BChan from which the app receives search events.
                            , _numMatches :: Int
                            }
makeLenses ''SearcherG

-- | Event hooks are almost direct translations of the events from vty i.e. see `Event`.
data EventHooks a = EventHooks { keyHook :: Key -> [Modifier] -> a -> EventM Bool (Next a)
                               , pasteHook :: ByteString -> a -> EventM Bool (Next a)
                               , resizeHook :: Int -> Int -> a -> EventM Bool (Next a)
                               , mouseDownHook :: Int -> Int -> Button -> [Modifier] -> a -> EventM Bool (Next a)
                               , mouseUpHook   :: Int -> Int -> Maybe Button -> a -> EventM Bool (Next a)
                               , focusLostHook :: a -> EventM Bool (Next a)
                               , focusGainedHook :: a -> EventM Bool (Next a)}

data AppSettingsG (n :: Nat) v a b t = AppSettings { _theme :: t
                                                   , _hooks :: ReaderT (SearchEnv n a) EventHooks (SearcherG v b) -- ^ The event hooks which can make use of the search environment.
                                                   , _chunkSize :: Proxy n
                                                   , _maximumMatches :: Int
                                                   , _eventStrategy :: SearchReport -> Bool}
makeLenses ''AppSettingsG

newtype MatchVector (n::Nat) = MatchVector (U.Vector (ScoredMatchSized n)) deriving (Eq, Ord, Show, MonoFunctor, MonoFoldable)
type instance Element (MatchVector n) = ScoredMatchSized n

newtype WrappedMatchVector (n :: Nat) a = WrappedMatchVector (WrappedMono (MatchVector n) a) deriving (MonoFunctor, MonoFoldable)
type instance Element (WrappedMatchVector (n :: Nat) a) = ScoredMatchSized n
data MatchList a = forall n. MatchList (GenericList Bool (WrappedMatchVector n) a)

defHooks :: EventHooks a
defHooks = EventHooks (const . const continue) (const continue) (const . const continue) (const . const . const . const continue)
                                (const . const . const continue) continue continue
-- | Quit without any selection.
haltQuit :: SearcherG v a -> EventM n (Next (SearcherG v a))
haltQuit = halt . ((matches . listSelectedL) .~ Nothing)

getQuery :: SearcherG v a -> Text
getQuery s = fromMaybe "" . listToMaybe . getEditContents $ s ^. queryEditor

twoColumnText :: Int -> Text -> Text -> Widget n
twoColumnText n t1 t2 =  joinBorders . vLimit 1 $ go n t1 <+> go 100 t2
  where
    go m t = hLimitPercent m $ padLeftRight 2 (txt t) <+> fill ' '

columns :: (a -> Widget n) -> [AttrName] -> [Int] -> [a] -> Widget n
columns f as ls = vLimit 1 . hBox . zipWith3 (\a l t -> hLimitPercent l $ (padRight (Pad 2) . withAttr a . f $ t) <+> fill ' ') as ls

searchWidget :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n
searchWidget b p e = hBox [withAttr "Prompt" (padLeftRight 2 . txt $ p) , padLeftRight 2 (renderEditor (hBox . map txt) b e)]

searchWidgetAux :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n -> Widget n
searchWidgetAux b p e w = hBox [withAttr "Prompt" (padLeftRight 2 . txt $ p) , padLeftRight 2 (renderEditor (hBox . map txt) b e) , w]

highlightAlternate :: (a -> Widget n) -> [a] -> Widget n
highlightAlternate f = hBox . zipWith (\b e -> if b then withAttr "Highlight" (f e) else f e) (cycle [False , True])
  -- fst . foldl' (\(!w , !b) !n  -> (w <+> if b then withAttr "Highlight" (f n) else f n , not b)) (emptyWidget , False)

headingAndBody :: Text -> Text -> Widget n
headingAndBody h b = withAttr "Heading" (txt h) <=> txtWrap b

listWithHighlights :: (Ord n , Show n) => Text -> (a -> [Text]) -> Bool -> List n a -> Widget n
listWithHighlights c f = renderList (\s e -> vLimit 1 . hBox $ [txt (if s then c else "  ") , highlightAlternate txt . f $ e , fill ' '])

columnsListWithHighlights :: (Ord n , Show n) => Text -> (a -> [[Text]]) -> [AttrName] -> [Int] -> Bool -> List n a -> Widget n
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
                 translateBy (Location (0, off)) $ vBox . zipWith drawElement [start ..] . toList $ es

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
handleKeyEvent :: (KnownNat n) => (Text -> b) -> SearchEnv n a -> Key -> [Modifier] -> SearcherG Vector b -> EventM Bool (Next (SearcherG Vector b))
handleKeyEvent f env = go
  where
    go k m s
      | k == KEnter                                  , null m = halt s
      | k == KEsc                                    , null m = haltQuit s
      | k == KChar '\t'                              , null m = continue . over matches listMoveDown $ s
      | k == KBackTab                                , null m = continue . over matches listMoveUp $ s
      | k `elem` [KUp , KDown , KPageUp , KPageDown] , null m = continue =<< handleEventLensed s matches handleListEvent (EvKey k m)
      | otherwise                                             = continue =<< liftIO . editStep =<< handleEventLensed s queryEditor handleEditorEvent (EvKey k m)
      where
        editStep ns
          | nq /= getQuery s    = if nq == "" then pure $ resetSearcher f env ns else sendQuery env nq $> ns
          | otherwise           = pure ns
          where
            nq = getQuery ns

resetSearcher :: forall n a b. (KnownNat n) => (Text -> b) -> SearchEnv n a -> SearcherG Vector b -> SearcherG Vector b
resetSearcher f env = (numMatches .~ 0) . (matches .~ list False (f <$> concatChunks k (env ^. candidates)) 0)
  where
    n = natVal (Proxy :: Proxy n)
    k = 1 + (env ^. maxMatches) `div` fromInteger n

{-# INLINE  handleSearch #-}
handleSearch :: (Foldable v , L.Splittable v) => SearcherG v a -> SearchEventG v a -> EventM Bool (Next (SearcherG v a))
handleSearch s !e = continue . (numMatches .~ e ^. totMatches) . (matches %~ listReplace (e ^. matchedTop) (Just 0)) $ s
