{-# LANGUAGE TemplateHaskell #-}

-- | A simple brick app to search among the candidates from a vector of text and get the selection. By default the app doesn't do anything except
-- return a single selection but more complicated actions can be performed by using the `_hooks` which allow abitrary IO actions (due to `EventM` being a `MonadIO`)
-- in response to input events. The most convenient function to use the brick app are `selected` and related functions. `runApp` provides some more flexibility.
module Talash.Brick (-- * Types
                     Searcher (..) , SearchEvent (..) , SearchEnv (..) , SearchFunction ,  EventHooks (..) , AppTheme (..) , AppSettings (..) , CaseSensitivity (..)
                     -- * The Brick App and Helpers
                    , searchApp , defSettings , searchFunctionFuzzy , searchFunctionOL , searchFunctionFuzzyCustom , searchFunctionOLCustom , runApp
                    , runAppFromHandle , selected , selectedFromHandle , selectedFromHandleWith , selectedFromFileNamesSorted , selectedFromFiles , selectedUsing , runSearch
                    -- * Default program
                    , run , run'
                     -- * Lenses
                     -- ** Searcher
                    , query , prevQuery , allMatches , matches , numMatches , wait
                     -- ** SearchEvent
                    , matchedTop , totalMatches , term
                     -- ** SearchEnv
                    , searchFunction , candidates , eventSource
                     -- ** SearchFunctions
                    , makeMatcher , lister , displayer
                     -- ** AppTheme
                    , prompt , themeAttrs , borderStyle
                     -- ** SearchSettings
                    , theme , hooks
                     -- * Exposed Internals
                    , makeQuery , haltQuit , handleKeyEvent , handleSearch , editStep , replaceSearch , search , searcherWidget , initialSearcher
                    , searchWithMatcher , readVectorStdIn , readVectorHandle , readVectorHandleWith , emptyIndices) where

import Control.Concurrent (forkIO , killThread, ThreadId)
import Data.IORef (IORef , newIORef , atomicModifyIORef' , atomicWriteIORef)
import qualified Data.Text as T
import Data.Vector (Vector , force , take , unsafeIndex , elemIndex)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as S
import GHC.Compact (Compact , compact , getCompact)
import Intro hiding (sort, on , take)
import System.Environment (getArgs)
import Talash.Brick.Internal
import Talash.Core hiding (makeMatcher)
import Talash.Files
import Talash.Internal
import Data.Monoid.Colorful as C
import Brick.Widgets.List (listMoveDown, listMoveUp)

data Searcher a = Searcher { -- | The editor to get the query from.
                             _query :: Editor Text Bool
                           -- | The last query which is saved to check if we should only search among the matches for it or all the candidates.
                           , _prevQuery :: Maybe Text
                           -- | The matches received split up as alternating sequences of match substrings and the gap between them. The first substring is always a gap
                           --   and can be empty, the rest should be no empty.
                           , _matches :: List Bool [Text]
                           -- | The (maximum possible) number of matches. This is the length of vector stored in `_allMatches` which also contains the indices of
                           --   which weren't matched in case enough matches were found before going through all the candidates.
                           , _numMatches :: Int
                           -- | ThreadId of the thread currently computing matches. Nothing if there is no such thread.
                           , _wait :: Maybe ThreadId
                           -- | Unused by default but can be used store extra state needed for any extension to the functionality. For example to have multiple
                           --   selections this can be set to a `Vector` that stores them.
                           , _extension :: a} deriving (Functor)
makeLenses ''Searcher

data SearchEvent = SearchEvent { -- | The matches received.
                                 _matchedTop :: Vector [Text]
                                 -- | The (maximum possible) number of matches. See the note on `_numMatches`.
                               , _totalMatches :: Int
                                 -- | The term which was searched for.
                               ,  _term :: Maybe Text}
makeLenses ''SearchEvent

-- | The constant environment in which the search app runs.
data SearchEnv = SearchEnv { _searchFunction :: SearchFunction  -- ^ The functions used to find and display matches.
                           , _candidates :: Vector Text -- ^ The vector of candidates.
                           , _eventSource :: BChan SearchEvent -- ^ The BChan from which the app receives search events.
                           -- | An IORef containing the indices of the filtered candidates. These are in an IORef to make it easier to deal with them in a different thread
                           --   than the UI of the app.
                           , _allMatches :: IORef (U.Vector Int) }
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
                         , _themeAttrs :: [(AttrName, Attr)]  -- ^ This is used to construct the `attrMap` for the app. By default the used attarNmaes are
                                                              --  `listSelectedAttr` , `borderAttr` , \"Prompt\" , \"Highlight\" and \"Stats\"
                         , _borderStyle :: BorderStyle -- ^ The border style to use. By default `unicodeRounded`
                         }
makeLenses ''AppTheme

data AppSettings b = AppSettings { _theme :: AppTheme
                                 , _hooks :: ReaderT SearchEnv EventHooks (Searcher b) -- ^ The event hooks which can make use of the search environment.
                                 }
makeLenses ''AppSettings

defHooks :: EventHooks a
defHooks = EventHooks (const . const continue) (const continue) (const . const continue) (const . const . const . const continue)
                                (const . const . const continue) continue continue
-- | Get the current query from the editor of the searcher.
makeQuery :: Searcher a -> Maybe Text
makeQuery s = headMay . getEditContents $ s ^. query

-- | Quit without any selection.
haltQuit :: Searcher a -> EventM n (Next (Searcher a))
haltQuit = halt . ((matches . listSelectedL) .~ Nothing)

-- | Handling of keypresses. The default bindings are
--  @Enter@ exits the app with the current selection.
--  @Esc@ exits without any selection
--  @Up@ , @Down@ , @PageUp@ and @PageDown@ move through the matches.
-- All others keys are used for editing the query. See `handleEditorEvent` for details.
handleKeyEvent :: SearchEnv -> Key -> [Modifier] -> Searcher b -> EventM Bool (Next (Searcher b))
handleKeyEvent e@(SearchEnv fs v b _) k m s
  | k == KEnter                                  , null m = halt s
  | k == KEsc                                    , null m = haltQuit s
  | k == KChar '\t'                              , null m = continue . over matches listMoveDown $ s
  | k == KBackTab                                , null m = continue . over matches listMoveUp $ s
  | k `elem` [KUp , KDown , KPageUp , KPageDown] , null m = continue =<< handleEventLensed s matches handleListEvent (EvKey k m)
  | otherwise                                             = continue =<< liftIO . editStep e =<< handleEventLensed s query handleEditorEvent (EvKey k m)

-- | Handle a search event by updating `_numMatches` , `_matches` and `_wait`.
handleSearch :: Vector Text -> Searcher a -> SearchEvent -> EventM Bool (Next (Searcher a))
handleSearch v s e = continue . (numMatches .~ e ^. totalMatches) . (matches %~ listReplace (e ^. matchedTop) (Just 0)) . (wait .~ Nothing) $ s

-- | The brick widget used to display the editor and the search result.
searcherWidget :: Text -> Text -> Searcher a -> Widget Bool
searcherWidget p n s = joinBorders . border $    searchWidgetAux True p (s ^. query) (withAttr "Stats" . txt $ show (s ^. numMatches) <>  "/" <> n)
                                             <=> hBorder  <=> joinBorders (listWithHighlights "➜ " id False (s ^. matches))

-- | Handle the editing of the query by starting the computation of the matches in a new thread and storing the `ThreadId` in `_wait`.
-- If the new query contains the last query then doesn't try to match the candidates that didn't match the last query, otherwise search among all the candidates.
-- Might be possible to make the performance better by storing the indices of the filtered candidates for more than one previous query.
editStep :: SearchEnv -> Searcher b -> IO (Searcher b)
editStep e@(SearchEnv f v b _) s
  | makeQuery s == (s ^. prevQuery)      = pure s
  | otherwise                            = (\w -> set wait (Just w) s') <$> replaceSearch isBigger e s'
  where
    isBigger = fromMaybe False $ T.isInfixOf <$> (s ^. prevQuery) <*> (headMay . getEditContents $ s ^. query)
    s'       = set prevQuery (makeQuery s) s

-- | The functions for generating a search event.  It is executed in a separate thread via `forkIO` in `replaceSearch`.
search :: forall a. SearchFunction -> Vector Text -> Maybe Text -> IORef (U.Vector Int) -> IO SearchEvent
search fs v t r = (\(l , tm) -> SearchEvent tm l t)  <$> atomicModifyIORef' r (fs v t)

-- | This function dispatches the computation of matches to a new thread and returns the new threadId. It also tries to kill the thread in which a previous computation
--   was going on (Not sure if it actually accomplishes that, my understanding of exceptions is not good enough).
replaceSearch :: Bool -- ^ If True then search among all matches by writing a vector of all the indices into `_allMatches`. If False use `_allMatches` as is.
                        -> SearchEnv -> Searcher b -> IO ThreadId
replaceSearch ib (SearchEnv fs v b am) s = finally (forkIO . catch wrtev $ \ (_ :: AsyncException) -> pure ()) (maybe (pure ()) killThread (s ^. wait))
  where
    wrtev = writeBChan b =<< search fs v (s ^. prevQuery) =<< mtchs
    mtchs = if ib then pure am else atomicWriteIORef am (U.enumFromN 0 $ length v) $> am

defThemeAttrs :: [(AttrName, Attr)]
defThemeAttrs = [ (listSelectedAttr, withStyle (bg white) bold) , ("Prompt" , withStyle (white `on` blue) bold)
           , ("Highlight" , withStyle (fg blue) bold) ,  ("Stats" , fg blue) ,  (borderAttr , fg cyan)]

defTheme ::AppTheme
defTheme = AppTheme {_prompt = "Find: " , _themeAttrs = defThemeAttrs , _borderStyle = unicodeRounded}

-- | Default settings. Uses blue for various highlights and cyan for borders. All the hooks except keyHook which is `handleKeyEvent` are trivial.
defSettings :: AppSettings b
defSettings = AppSettings defTheme (ReaderT (\e -> defHooks {keyHook = handleKeyEvent e}))

-- | Tha app itself. `selected` and the related functions are probably more convenient for embedding into a larger program.
searchApp :: AppSettings b -> SearchEnv -> App (Searcher b) SearchEvent Bool
searchApp (AppSettings th hks) env@(SearchEnv _ v _ _) = App {appDraw = ad , appChooseCursor = showFirstCursor , appHandleEvent = he , appStartEvent = pure , appAttrMap = am}
  where
    ad                                    = (:[]) . withBorderStyle (th ^. borderStyle) . searcherWidget (th ^. prompt) (show . length $ v)
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

-- | The initial state of the searcher. The editor is empty, the first @512@ elements of the vector are disaplyed as matches.
initialSearcher :: a -> Vector Text ->  Searcher a
initialSearcher e v = Searcher { _query = editorText True (Just 1) "" , _prevQuery = Nothing , _wait = Nothing
                               , _matches = list False ((:[]) <$> take 512 v) 0, _numMatches = length v , _extension =  e}

-- | Run app with given settings and return the final Searcher state.
runApp :: b -> AppSettings b -> SearchFunction -> Vector Text -> IO (Searcher b)
runApp e s f v = (\b -> (\r -> theMain (searchApp s (SearchEnv f v b r)) b . initialSearcher e $ v) =<< newIORef (U.enumFromN 0 . length $ v)) =<< newBChan 8

-- | Run app with a vector that contains lines read from a handle and return the final Searcher state.
runAppFromHandle :: b -> AppSettings b -> SearchFunction  -> Handle -> IO (Searcher b)
runAppFromHandle e s f = runApp e s f . getCompact <=< compact . force <=< readVectorHandle

-- | Run app and return the text of the selection if there is one else Nothing.
selected :: AppSettings () -> SearchFunction -> Vector Text -> IO (Maybe Text)
selected s f  = map (map (mconcat . snd) . listSelectedElement . (^. matches)) . runApp () s f . getCompact <=< compact . force

-- | Same as `selected` but reads the vector from the supplied handle.
selectedFromHandle :: AppSettings () -> SearchFunction -> Handle -> IO (Maybe Text)
selectedFromHandle s f = selected s f <=< readVectorHandle

-- | Same as `selectedFromHandle` but allows for transforming the lines read and the final vector with supplied functions. See also `readVectorHandleWith`.
selectedFromHandleWith :: (Text -> Text) -> (Vector Text -> Vector Text) -> AppSettings () -> SearchFunction -> Handle -> IO (Maybe Text)
selectedFromHandleWith w t s f = selected s f <=< readVectorHandleWith w t

-- | Another variation on `selectedFromHandle`. See `fileNamesSorted` for what happens to read vector.
selectedFromFileNamesSorted :: AppSettings () -> SearchFunction -> Handle -> IO (Maybe Text)
selectedFromFileNamesSorted s f = selected s f <=< fileNamesSorted

-- | Version of `selected` for file search using a simple implementation of searching file trees from "Talash.Files". Better to use either other
-- libraries like @unix-recursive@ or external programs like @fd@ for more complicated tasks.
selectedFromFiles ::  AppSettings () -> SearchFunction -> [FindInDirs] -> IO (Maybe Text)
selectedFromFiles s f = selected s f . (flatten =<<) <=< findFilesInDirs

selectedUsing :: AppSettings () -> SearchFunction -> (a -> Text) -> Vector a -> IO (Maybe a)
selectedUsing s f t v = map (map (unsafeIndex v) . (`elemIndex` w) =<<) . selected s f $ w
  where
    w = map t v

-- | A version of `selected` that puts the selected text on the stdout.
runSearch :: AppSettings () -> SearchFunction -> IO ()
runSearch s f = maybe (pure ()) putStrLn =<< selected s f =<< readVectorStdIn

-- | The backend for `run`
run' :: [String] -> IO ()
run' []                 = runSearch defSettings (searchFunctionOL IgnoreCase)
run' ["fuzzy"]          = runSearch defSettings (searchFunctionFuzzy IgnoreCase)
run' ["orderless"]      = runSearch defSettings (searchFunctionOL IgnoreCase)
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
