{-# LANGUAGE TemplateHaskell #-}

-- | A simple brick app to search among the candidates from a vector of text and get the selection. By default the app doesn't do anything except
-- return a single selection but more complicated actions can be performed by using the `_hooks` which allow abitrary IO actions (due to `EventM` being a `MonadIO`)
-- in response to input events. The most convenient function to use the brick app are `selected` and related functions. `runApp` provides some more flexibility.
module Talash.Brick.Chunked -- (-- * Types
                    --  SearchUI (..) , SearchEvent (..) , SearchEnv (..) , SearchFunction ,  EventHooks (..) , AppTheme (..) , AppSettings (..) , CaseSensitivity (..)
                    --  -- * The Brick App and Helpers
                    -- , searchApp , defSettings , searchFunctionFuzzy , searchFunctionOL , searchFunctionFuzzyCustom , searchFunctionOLCustom , runApp
                    -- , runAppFromHandle , selected , selectedFromHandle , selectedFromHandleWith , selectedFromFileNamesSorted , selectedFromFiles , selectedUsing , runSearch
                    -- -- * Default program
                    -- , run , run'
                    --  -- * Lenses
                    --  -- ** SearchUI
                    -- , queryEditor , prevQueryEditor , allMatches , matches , numMatches , wait
                    --  -- ** SearchEvent
                    -- , matchedTop , totalMatches , term
                    --  -- ** SearchEnv
                    -- , searchFunction , candidates , eventSource
                    --  -- ** SearchFunctions
                    -- , makeMatcher , lister , displayer
                    --  -- ** AppTheme
                    -- , prompt , themeAttrs , borderStyle
                    --  -- ** SearchSettings
                    -- , theme , hooks
                    --  -- * Exposed Internals
                    -- , makeQueryEditor , haltQuit , handleKeyEvent , handleSearch , editStep , replaceSearch , search , searcherWidget , initialSearchUI
                    -- , searchWithMatcher , readVectorStdIn , readVectorHandle , readVectorHandleWith , emptyIndices)
where

import qualified Data.Text as T
import Data.Vector (Vector , force , take , unsafeIndex , elemIndex)
import GHC.Compact (Compact , compact , getCompact)
import System.Environment (getArgs)
import Talash.Brick.Internal
import Talash.Core hiding (makeMatcher)
import Talash.Files
import Talash.Intro hiding (sort, on , take)
import Data.Monoid.Colorful as C
import Talash.Chunked
import GHC.TypeLits
import Talash.ScoredMatch (ScoredMatchSized(chunkIndex, matchData))
import Data.Text.AhoCorasick.Automaton (CaseSensitivity(..))
import qualified System.IO.Streams as I
import Brick.Widgets.List (listClear)

data SearchEvent = SearchEvent { -- | The matches received.
                                 _matchedTop :: Vector [Text]
                                 -- | The (maximum possible) number of matches. See the note on `_numMatches`.
                               , _totMatches :: Int
                                 -- | The term which was searched for.
                               ,  _term :: Text}
makeLenses ''SearchEvent

data SearchUI a = SearchUI { -- | The editor to get the query from.
                             _queryEditor :: Editor Text Bool
                           -- | The matches received split up as alternating sequences of match substrings and the gap between them. The first substring is always a gap
                           --   and can be empty, the rest should be no empty.
                           , _matches :: List Bool [Text]
                           -- | The (maximum possible) number of matches. This is the length of vector stored in `_allMatches` which also contains the indices of
                           --   which weren't matched in case enough matches were found before going through all the candidates.
                           , _eventSource :: BChan SearchEvent -- ^ The BChan from which the app receives search events.
                           , _numMatches :: Int
                           -- | ThreadId of the thread currently computing matches. Nothing if there is no such thread.
                           -- | Unused by default but can be used store extra state needed for any extension to the functionality. For example to have multiple
                           --   selections this can be set to a `Vector` that stores them.
                           , _extension :: a} deriving (Functor)
makeLenses ''SearchUI

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

data AppSettings (n :: Nat) a b = AppSettings { _theme :: AppTheme
                                              , _hooks :: ReaderT (SearchEnv n a) EventHooks (SearchUI b) -- ^ The event hooks which can make use of the search environment.
                                              , _chunkSize :: Proxy n
                                              , _maximumMatches :: Int}
makeLenses ''AppSettings

defHooks :: EventHooks a
defHooks = EventHooks (const . const continue) (const continue) (const . const continue) (const . const . const . const continue)
                                (const . const . const continue) continue continue
-- | Get the current queryEditor from the editor of the searcher.
makeQueryEditor :: SearchUI a -> Maybe Text
makeQueryEditor s = listToMaybe . getEditContents $ s ^. queryEditor

-- | Quit without any selection.
haltQuit :: SearchUI a -> EventM n (Next (SearchUI a))
haltQuit = halt . ((matches . listSelectedL) .~ Nothing)

getQuery :: SearchUI a -> Maybe Text
getQuery s = listToMaybe . getEditContents $ s ^. queryEditor

-- | Handling of keypresses. The default bindings are
--  @Enter@ exits the app with the current selection.
--  @Esc@ exits without any selection
--  @Up@ , @Down@ , @PageUp@ and @PageDown@ move through the matches.
-- All others keys are used for editing the query. See `handleEditorEvent` for details.
handleKeyEvent :: KnownNat n => SearchEnv n a -> Key -> [Modifier] -> SearchUI b -> EventM Bool (Next (SearchUI b))
handleKeyEvent env k m s
  | k == KEnter                                  , null m = halt s
  | k == KEsc                                    , null m = haltQuit s
  | k == KChar '\t'                              , null m = continue . over matches listMoveDown $ s
  | k == KBackTab                                , null m = continue . over matches listMoveUp $ s
  | k `elem` [KUp , KDown , KPageUp , KPageDown] , null m = continue =<< handleEventLensed s matches handleListEvent (EvKey k m)
  | otherwise                                             = continue =<< liftIO . editStep =<< handleEventLensed s queryEditor handleEditorEvent (EvKey k m)
  where
    editStep ns
      | nq /= getQuery s , Just x <- nq    = sendQuery env x $> ns
      | otherwise                          = pure ns
      where
        nq = getQuery ns

-- | Handle a search event by updating `_numMatches` , `_matches` and `_wait`.
handleSearch :: SearchUI a -> SearchEvent -> EventM Bool (Next (SearchUI a))
handleSearch s e = continue . (numMatches .~ e ^. totMatches) . (matches %~ listReplace (e ^. matchedTop) (Just 0)) $ s

-- | The brick widget used to display the editor and the search result.
searcherWidget :: Text -> SearchUI a -> Widget Bool
searcherWidget p s = joinBorders . border $     searchWidgetAux True p (s ^. queryEditor) (withAttr "Stats" . txt  $ T.pack (show $ s ^. numMatches))
                                            <=> hBorder  <=> joinBorders (listWithHighlights "➜ " id False (s ^. matches))

generateSearchEvent :: forall n m a. (KnownNat n , KnownNat m) => SearchFunctions a -> BChan SearchEvent -> SearchReport -> Chunks n
                                                                                -> MatcherSized m a -> MatchSetSized m -> IO ()
generateSearchEvent f b r c m s = when (r ^. ocassion == QueryDone) $ writeBChan b event
  where
    event = SearchEvent (matchSetToVector (\mtch -> (f ^. display) m (c ! chunkIndex mtch) (matchData mtch)) s) (r ^. nummatches) (r ^. searchedTerm)

defThemeAttrs :: [(AttrName, Attr)]
defThemeAttrs = [ (listSelectedAttr, withStyle (bg white) bold) , ("Prompt" , withStyle (white `on` blue) bold)
           , ("Highlight" , withStyle (fg blue) bold) ,  ("Stats" , fg blue) ,  (borderAttr , fg cyan)]

defTheme ::AppTheme
defTheme = AppTheme {_prompt = "Find: " , _themeAttrs = defThemeAttrs , _borderStyle = unicodeRounded}

-- | Default settings. Uses blue for various highlights and cyan for borders. All the hooks except keyHook which is `handleKeyEvent` are trivial.
defSettings :: KnownNat n => AppSettings n a b
defSettings = AppSettings defTheme (ReaderT (\e -> defHooks {keyHook = handleKeyEvent e})) Proxy 1024

-- | Tha app itself. `selected` and the related functions are probably more convenient for embedding into a larger program.
searchApp :: AppSettings n a b -> SearchEnv n a -> App (SearchUI b) SearchEvent Bool
searchApp (AppSettings th hks _ _) env = App {appDraw = ad , appChooseCursor = showFirstCursor , appHandleEvent = he , appStartEvent = pure , appAttrMap = am}
  where
    ad                                    = (:[]) . withBorderStyle (th ^. borderStyle) . searcherWidget (th ^. prompt)
    am                                    = const $ attrMap defAttr (th ^. themeAttrs)
    hk                                    = runReaderT hks env
    he s (VtyEvent (EvKey k m))           = keyHook hk k m s
    he s (VtyEvent (EvMouseDown i j b m)) = mouseDownHook   hk i j b m s
    he s (VtyEvent (EvMouseUp   i j b  )) = mouseUpHook     hk i j b   s
    he s (VtyEvent (EvPaste     b      )) = pasteHook       hk     b   s
    he s (VtyEvent  EvGainedFocus       ) = focusGainedHook hk         s
    he s (VtyEvent  EvLostFocus         ) = focusLostHook   hk         s
    he s (AppEvent e)                     = if e ^. term == fromMaybe "" (getQuery s) then handleSearch s e else continue s
    he s _                                = continue s

-- | The initial state of the searcher. The editor is empty, the first @512@ elements of the vector are disaplyed as matches.
initialSearchUI :: forall n a b. KnownNat n => SearchEnv n a -> BChan SearchEvent -> b -> SearchUI b
initialSearchUI env source = SearchUI (editorText True (Just 1) "") (list False ((:[]) <$> concatChunks k (env ^. candidates)) 0) source 0
  where
    n = natVal (Proxy :: Proxy n)
    k = 1 + (env ^. maxMatches) `div` fromInteger n

-- | Run app with given settings and return the final SearchUI state.
runApp :: KnownNat n => b -> AppSettings n a b -> SearchFunctions a -> Chunks n -> IO (SearchUI b)
runApp e s f c =     (\b -> (\env -> startSearcher env *> finally (theMain (searchApp s env) b . initialSearchUI env b $ e) (stopSearcher env))
                 =<< searchEnv f (s ^. maximumMatches) (generateSearchEvent f b) c) =<< newBChan 8

-- | Run app with a vector that contains lines read from a handle and return the final SearchUI state.
runAppFromHandle :: KnownNat n => b -> AppSettings n a b -> SearchFunctions a -> Handle -> IO (SearchUI b)
runAppFromHandle e s f = runApp e s f . getCompact <=< compact . forceChunks <=< chunksFromHandle (s ^. chunkSize)

-- | Run app with a vector that contains lines read from a handle and return the final SearchUI state.
runAppFromVector :: KnownNat n => b -> AppSettings n a b -> SearchFunctions a -> Vector Text -> IO (SearchUI b)
runAppFromVector e s f = runApp e s f . getCompact <=< compact . forceChunks . makeChunks

-- | Run app and return the text of the selection if there is one else Nothing.
selected :: KnownNat n => AppSettings n a () -> SearchFunctions a -> Chunks n -> IO (Maybe Text)
selected s f  = map (map (mconcat . snd) . listSelectedElement . (^. matches)) . runApp () s f . getCompact <=< compact . forceChunks

-- | Same as `selected` but reads the vector from the supplied handle.
selectedFromHandle :: KnownNat n => AppSettings n a () -> SearchFunctions a -> Handle -> IO (Maybe Text)
selectedFromHandle s f = selected s f <=< chunksFromHandle (s ^. chunkSize)

-- | Same as `selectedFromHandle` but allows for transforming the lines read and the final vector with supplied functions. See also `readVectorHandleWith`.
selectedFromHandleWith :: KnownNat n => (Text -> Text) -> (Vector Text -> Vector Text) -> AppSettings n a () -> SearchFunctions a -> Handle -> IO (Maybe Text)
selectedFromHandleWith w t s f = selected s f . makeChunks <=< readVectorHandleWith w t

-- | Another variation on `selectedFromHandle`. See `fileNamesSorted` for what happens to read vector.
selectedFromFileNamesSorted :: KnownNat n => AppSettings n a () -> SearchFunctions a -> Handle -> IO (Maybe Text)
selectedFromFileNamesSorted s f = selected s f . makeChunks <=< fileNamesSorted

-- | Version of `selected` for file search using a simple implementation of searching file trees from "Talash.Files". Better to use either other
-- libraries like @unix-recursive@ or external programs like @fd@ for more complicated tasks.
selectedFromFiles :: KnownNat n => AppSettings n a () -> SearchFunctions a -> [FindInDirs] -> IO (Maybe Text)
selectedFromFiles s f = selected s f . makeChunks . (flatten =<<) <=< findFilesInDirs

selectedUsing :: KnownNat n => AppSettings n a () -> SearchFunctions a -> (a -> Text) -> Vector a -> IO (Maybe a)
selectedUsing s f t v = map (map (unsafeIndex v) . (`elemIndex` w) =<<) . selected s f . makeChunks $ w
  where
    w = map t v

-- | A version of `selected` that puts the selected text on the stdout.
runSearch :: AppSettings 64 a () -> SearchFunctions a -> IO ()
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
