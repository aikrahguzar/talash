{-# LANGUAGE TemplateHaskell #-}

-- | A simple brick app to search among the candidates from a vector of text and get the selection. By default the app doesn't do anything except
-- return a single selection but more complicated actions can be performed by using the `_hooks` which allow abitrary IO actions (due to `EventM` being a `MonadIO`)
-- in response to input events. The most convenient function to use the brick app are `selected` and related functions. `runApp` provides some more flexibility.
module Talash.Brick (-- * Types
                     Searcher (..) , SearchEvent (..) , SearchEnv (..) ,  EventHooks (..) , AppTheme (..) , AppSettings (..) , AppSettingsG (..) , CaseSensitivity (..)
                     -- * The Brick App and Helpers
                    , searchApp , defSettings , fuzzyFunctions , orderlessFunctions , runApp , runAppFromHandle , selected , selectedFromHandle
                    , selectedFromHandleWith , selectedFromFileNamesSorted , selectedFromFiles , selectedUsing , runSearch , makeChunks
                    -- * Default program
                    , run , run'
                     -- * Lenses
                     -- ** Searcher
                    , queryEditor , allMatches , matches , numMatches
                     -- ** SearchEvent
                    , matchedTop , totalMatches , term
                     -- ** SearchEnv
                    , candidates , eventSource
                     -- ** SearchFunctions
                    , makeMatcher , match , display
                     -- ** AppTheme
                    , prompt , themeAttrs , borderStyle
                     -- ** SearchSettings
                    , theme , hooks , defTheme , defHooks
                     -- * Exposed Internals
                    , handleKeyEvent , handleSearch , searcherWidget , initialSearcher , readVectorHandleWith)
where

import Data.Monoid.Colorful as C
import Data.Vector (unsafeIndex , elemIndex)
import GHC.Compact (Compact , compact , getCompact)
import qualified System.IO.Streams as I
import Talash.Brick.Internal
import Talash.Files
import Talash.Intro hiding (sort, on , take)

data AppTheme = AppTheme { _prompt :: Text -- ^ The prompt to display next to the editor.
                         , _themeAttrs :: [(AttrName, Attr)]  -- ^ This is used to construct the `attrMap` for the app. By default the used attarNmaes are
                                                              --  `listSelectedAttr` , `borderAttr` , \"Prompt\" , \"Highlight\" and \"Stats\"
                         , _borderStyle :: BorderStyle -- ^ The border style to use. By default `unicodeRounded`
                         }
makeLenses ''AppTheme

type AppSettings n a = AppSettingsG n a (Widget Bool) AppTheme

-- | The brick widget used to display the editor and the search result.
searcherWidget :: (KnownNat n , KnownNat m) => SearchEnv n a (Widget Bool) -> Text -> SearcherSized m a -> Widget Bool
searcherWidget env p s = joinBorders . border $ vBox [searchWidgetAux True p (s ^. queryEditor) (withAttr (attrName "Stats") . txt  $ pack (show $ s ^. numMatches))
                                                     , hBorder , listWithHighlights env "➜ " (s ^. matcher) False (s ^. matches)]

defThemeAttrs :: [(AttrName, Attr)]
defThemeAttrs = [ (listSelectedAttr, withStyle (bg white) bold) , (attrName "Prompt" , withStyle (white `on` blue) bold)
                , (attrName "Highlight" , withStyle (fg blue) bold) ,  (attrName "Stats" , fg blue) ,  (borderAttr , fg cyan)]

defTheme ::AppTheme
defTheme = AppTheme {_prompt = "Find: " , _themeAttrs = defThemeAttrs , _borderStyle = unicodeRounded}

-- | Default settings. Uses blue for various highlights and cyan for borders. All the hooks except keyHook which is `handleKeyEvent` are trivial.
{-# INLINE defSettings#-}
defSettings :: KnownNat n => AppSettings n a
defSettings = AppSettings defTheme (ReaderT (\e -> defHooks {keyHook = handleKeyEvent e})) Proxy 4096
                          (\r -> r ^. ocassion == QueryDone)

-- | Tha app itself. `selected` and the related functions are probably more convenient for embedding into a larger program.
searchApp ::KnownNat n => AppSettings n a -> SearchEnv n a (Widget Bool) -> App (Searcher a) (SearchEvent a) Bool
searchApp (AppSettings th hks _ _ _) env  = App {appDraw = ad , appChooseCursor = showFirstCursor , appHandleEvent = he , appStartEvent = as , appAttrMap = am}
  where
    ad (Searcher s)                                  = (:[]) . withBorderStyle (th ^. borderStyle) . searcherWidget env (th ^. prompt) $ s
    as                                               = liftIO (sendQuery env "")
    am                                               = const $ attrMap defAttr (th ^. themeAttrs)
    hk                                               = runReaderT hks env
    he (VtyEvent (EvKey k m))                      = keyHook hk k m
    he (VtyEvent (EvMouseDown i j b m))            = mouseDownHook   hk i j b m
    he (VtyEvent (EvMouseUp   i j b  ))            = mouseUpHook     hk i j b
    he (VtyEvent (EvPaste     b      ))            = pasteHook       hk     b
    he (VtyEvent  EvGainedFocus       )            = focusGainedHook hk
    he (VtyEvent  EvLostFocus         )            = focusLostHook   hk
    he (AppEvent e@(SearchEvent e'))               = handleSearch e
    he _                                           = pure ()

-- | Run app with given settings and return the final Searcher state.
runApp :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> Chunks n -> IO (Searcher a)
runApp s f c =     (\b -> (\env -> startSearcher env *> finally (theMain (searchApp s env) b . Searcher . initialSearcher env $ b) (stopSearcher env))
               =<< searchEnv f (s ^. maximumMatches) (generateSearchEvent (s ^. eventStrategy) b) c) =<< newBChan 8

-- | Run app with a vector that contains lines read from a handle and return the final Searcher state.
runAppFromHandle :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> Handle -> IO (Searcher a)
runAppFromHandle s f = runApp s f . getCompact <=< compact . forceChunks <=< chunksFromHandle (s ^. chunkSize)

-- | Run app with a vector that contains lines read from a handle and return the final Searcher state.
runAppFromVector :: KnownNat n =>  AppSettings n a -> SearchFunctions a (Widget Bool) -> Vector Text -> IO (Searcher a)
runAppFromVector s f = runApp s f . getCompact <=< compact . forceChunks . makeChunks

-- | Run app and return the text of the selection if there is one else Nothing.
selected :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> Chunks n -> IO (Maybe Text)
selected s f = (\c -> map (selectedElement c) . runApp s f $ c) . getCompact <=< compact . forceChunks

-- | Same as `selected` but reads the vector from the supplied handle.
selectedFromHandle :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> Handle -> IO (Maybe Text)
selectedFromHandle s f = selected s f <=< chunksFromHandle (s ^. chunkSize)

-- | Same as `selectedFromHandle` but allows for transforming the lines read and the final vector with supplied functions. See also `readVectorHandleWith`.
selectedFromHandleWith :: KnownNat n => (Text -> Text) -> (Vector Text -> Vector Text) -> AppSettings n a -> SearchFunctions a (Widget Bool) -> Handle -> IO (Maybe Text)
selectedFromHandleWith w t s f = selected s f . makeChunks <=< readVectorHandleWith w t

-- | Another variation on `selectedFromHandle`. See `fileNamesSorted` for what happens to read vector.
selectedFromFileNamesSorted :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> Handle -> IO (Maybe Text)
selectedFromFileNamesSorted s f = selected s f .  makeChunks <=< fileNamesSorted

-- | Version of `selected` for file search using a simple implementation of searching file trees from "Talash.Files". Better to use either other
-- libraries like @unix-recursive@ or external programs like @fd@ for more complicated tasks.
selectedFromFiles :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> [FindInDirs] -> IO (Maybe Text)
selectedFromFiles s f = selected s f . forceChunks . makeChunks . (flatten =<<) <=< findFilesInDirs

selectedUsing :: KnownNat n => AppSettings n a -> SearchFunctions a (Widget Bool) -> (a -> Text) -> Vector a -> IO (Maybe a)
selectedUsing s f t v = map (map (unsafeIndex v) . (`elemIndex` w) =<<) . selected s f . makeChunks $ w
  where
    w = map t v

-- | A version of `selected` that puts the selected text on the stdout.
runSearch :: AppSettings 64 a -> SearchFunctions a (Widget Bool) -> IO ()
runSearch s f = maybe (pure ()) putStrLn =<< selected s f =<< chunksFromStream =<< I.decodeUtf8 =<< I.lines I.stdin

-- | The backend for `run`
run' :: [String] -> IO ()
run' []                 = runSearch defSettings (orderlessFunctions IgnoreCase)
run' ["fuzzy"]          = runSearch defSettings (fuzzyFunctions IgnoreCase)
run' ["orderless"]      = runSearch defSettings (orderlessFunctions IgnoreCase)
run' xs                 = (\t -> C.printColored putStr t usageString) =<< C.getTerm

usageString :: Colored Text
usageString =    "talash tui is a set of command for a tui searcher/selector interface. It reads the input from the stdin to generate candidates to search for,"
              <> " one from each line and outputs the selected candidate (if there is one) on the stdout.\n"
              <> C.Fg C.Blue "talash tui" <> ": Run the tui with the default orderless style of searching.\n"
              <> C.Fg C.Blue "talash tui fuzzy" <> ": Run the tui with fuzzy style for searching.\n"
              <> C.Fg C.Blue "talash tui orderless" <>  ": Run the tui with the default orderless style of searching.\n"

-- | Defualt program for the brick app that reads candidates from stdin and prints the selected text to the stdout. Can be called from the executable with
-- @talash tui@ which uses the orderless style. The search style can be set explicitly by calling @talash tui fuzzy@ or @talash tui orderless@
run :: IO ()
run = run' =<< getArgs
