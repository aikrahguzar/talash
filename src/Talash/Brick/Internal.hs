module Talash.Brick.Internal (twoColumnText , columns , searchWidget , searchWidgetAux , headingAndBody , listWithHighlights , columnsListWithHighlights
                ,  theMain , module Export) where

import Brick as Export
import Brick.BChan as Export (BChan , newBChan , writeBChan)
import Brick.Widgets.Border as Export (border, vBorder, hBorder, borderAttr)
import Brick.Widgets.Border.Style as Export
import Brick.Widgets.Center as Export (vCenter, center)
import Brick.Widgets.Edit  as Export (editor , editorText, renderEditor, Editor, handleEditorEvent, getEditContents, applyEdit )
import Brick.Widgets.List as Export (List, list ,handleListEvent, handleListEventVi, listAttr, listSelectedAttr, listSelectedElement , listSelectedL
                                    ,listReplace , renderListWithIndex, renderList , listElements, GenericList (listElements, listSelected))
import Graphics.Vty as Export (defAttr, cyan, white, blue, withStyle, bold, brightMagenta, black, magenta, brightBlue, Attr, defaultConfig, mkVty, green, standardIOConfig)
import Graphics.Vty.Config (Config(inputFd))
import Graphics.Vty.Input.Events as Export 
import Intro
import Lens.Micro as Export (ASetter' , over, set, (^.) , _1 , _2 , _3 , (.~) , (?~) , (%~))
import Lens.Micro.TH as Export ( makeLenses )
import System.Posix.IO
import System.Posix.Terminal

twoColumnText :: Int -> Text -> Text -> Widget n
twoColumnText n t1 t2 =  joinBorders . vLimit 1 $ go n t1 <+> go 100 t2
  where
    go m t = hLimitPercent m $ padLeftRight 2 (txt t) <+> fill ' '

columns :: (a -> Widget n) -> [AttrName] -> [Int] -> [a] -> Widget n
columns f as ls = vLimit 1 . hBox . zipWith3 (\a l t -> hLimitPercent l $ (padRight (Pad 2) . withAttr a . f $ t) <+> fill ' ') as ls

searchWidget :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n
searchWidget b p e = withAttr "Prompt" (padLeftRight 2 . txt $ p) <+> padLeftRight 2 (renderEditor (hBox . map txt) b e)

searchWidgetAux :: (Ord n , Show n) => Bool -> Text -> Editor Text n -> Widget n -> Widget n
searchWidgetAux b p e w = withAttr "Prompt" (padLeftRight 2 . txt $ p) <+> padLeftRight 2 (renderEditor (hBox . map txt) b e) <+> w

highlightAlternate :: Foldable f => (a -> Widget n) -> f a -> Widget n
highlightAlternate f = fst . foldl' (\(!w , !b) !n  -> (w <+> if b then withAttr "Highlight" (f n) else f n , not b)) (emptyWidget , False)

headingAndBody :: Text -> Text -> Widget n
headingAndBody h b = withAttr "Heading" (txt h) <=> txtWrap b

listWithHighlights :: (Foldable f , Ord n , Show n) => Text -> (a -> f Text) -> Bool -> List n a -> Widget n
listWithHighlights c f = renderList (\s e -> vLimit 1 . (txt (if s then c else "  ") <+>) . (<+> fill ' ') . highlightAlternate txt . f $! e)

columnsListWithHighlights :: (Foldable f , Ord n , Show n) => Text -> (a -> [f Text]) -> [AttrName] -> [Int] -> Bool -> List n a -> Widget n
columnsListWithHighlights c f as ls = renderList (\s e -> (txt (if s then c else "  ") <+>) . columns (highlightAlternate txt) as ls . f $! e)

theMain :: Ord n => App b e n -> BChan e -> b -> IO b
theMain a b s = (\v -> customMain v (pure v) (Just b) a s) =<< mkVty =<<(\c -> (\fd -> c {inputFd = Just fd}) <$> termFd) =<< standardIOConfig
  where
    termFd = (\f -> openFd f ReadOnly Nothing (OpenFileFlags False False False False False)) =<< getControllingTerminalName
