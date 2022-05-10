
-- | A simple brick app to search among the candidates from a vector of text and get the selection. By default the app doesn't do anything except
-- return a single selection but more complicated actions can be performed by using the `_hooks` which allow abitrary IO actions (due to `EventM` being a `MonadIO`)
-- in response to input events. The most convenient function to use the brick app are `selected` and related functions. `runApp` provides some more flexibility.
module Talash.Brick.Chunked (-- * Types
                     Searcher (..) , SearchEvent (..) , SearchEnv (..) ,  EventHooks (..) , AppTheme (..) , AppSettings (..) , CaseSensitivity (..)
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
                    , haltQuit , handleKeyEvent , handleSearch , searcherWidget , initialSearcher , readVectorHandleWith)
where

import qualified Data.Text as T
import Data.Vector (Vector , force , take , unsafeIndex , elemIndex)
import GHC.Compact (Compact , compact , getCompact)
import System.Environment (getArgs)
import Talash.Brick.Internal
import Talash.Core hiding (makeMatcher , match)
import Talash.Files
import Talash.Intro hiding (sort, on , take)
import Data.Monoid.Colorful as C
import Talash.Chunked
import GHC.TypeLits
import Talash.ScoredMatch (ScoredMatchSized(chunkIndex, matchData))
import Data.Text.AhoCorasick.Automaton (CaseSensitivity(..))
import qualified System.IO.Streams as I

-- | Handling of keypresses. The default bindings are
--  @Enter@ exits the app with the current selection.
--  @Esc@ exits without any selection
--  @Up@ , @Down@ , @PageUp@ and @PageDown@ move through the matches.
-- All others keys are used for editing the query. See `handleEditorEvent` for details.
{-# INLINE handleKeyEvent #-}
handleKeyEvent :: KnownNat n => SearchEnv n a -> Key -> [Modifier] -> Searcher -> EventM Bool (Next (Searcher))
handleKeyEvent env k m s
  | k == KEnter                                  , null m = halt s
  | k == KEsc                                    , null m = haltQuit s
  | k == KChar '\t'                              , null m = continue . over matches listMoveDown $ s
  | k == KBackTab                                , null m = continue . over matches listMoveUp $ s
  | k `elem` [KUp , KDown , KPageUp , KPageDown] , null m = continue =<< handleEventLensed s matches handleListEvent (EvKey k m)
  | otherwise                                             = continue =<< liftIO . editStep =<< handleEventLensed s queryEditor handleEditorEvent (EvKey k m)
  where
    editStep ns
      | nq /= getQuery s    = if nq == "" then pure $ resetSearcher env ns else sendQuery env nq $> ns
      | otherwise           = pure ns
      where
        nq = getQuery ns

handleSearch :: Searcher -> SearchEvent -> EventM Bool (Next (Searcher))
handleSearch s e = continue . (numMatches .~ e ^. totMatches) . (matches %~ listReplace (e ^. matchedTop) (Just 0)) $ s

-- | The brick widget used to display the editor and the search result.
searcherWidget :: Text -> Searcher -> Widget Bool
searcherWidget p s = joinBorders . border $     searchWidgetAux True p (s ^. queryEditor) (withAttr "Stats" . txt  $ T.pack (show $ s ^. numMatches))
                                            <=> hBorder  <=> joinBorders (listWithHighlights "➜ " id False (s ^. matches))

{-# INLINE generateSearchEvent #-}
generateSearchEvent :: forall n m a. (KnownNat n , KnownNat m) => SearchFunctions a -> (SearchReport -> Bool) -> BChan SearchEvent -> SearchReport -> Chunks n
                                                                                -> MatcherSized m a -> MatchSetSized m -> IO ()
generateSearchEvent f p = go
  where
    go b r c m s = when (p r) $ writeBChan b event
      where
        event = SearchEvent (matchSetToVector (\mtch -> (f ^. display) m (c ! chunkIndex mtch) (matchData mtch)) s) (r ^. nummatches) (r ^. searchedTerm)

defThemeAttrs :: [(AttrName, Attr)]
defThemeAttrs = [ (listSelectedAttr, withStyle (bg white) bold) , ("Prompt" , withStyle (white `on` blue) bold)
                , ("Highlight" , withStyle (fg blue) bold) ,  ("Stats" , fg blue) ,  (borderAttr , fg cyan)]

defTheme ::AppTheme
defTheme = AppTheme {_prompt = "Find: " , _themeAttrs = defThemeAttrs , _borderStyle = unicodeRounded}

-- | Default settings. Uses blue for various highlights and cyan for borders. All the hooks except keyHook which is `handleKeyEvent` are trivial.
{-# INLINE defSettings#-}
defSettings :: KnownNat n => AppSettings n a
defSettings = AppSettings defTheme (ReaderT (\e -> defHooks {keyHook = handleKeyEvent e})) Proxy 1024 (\r -> r ^. ocassion == QueryDone)

-- | Tha app itself. `selected` and the related functions are probably more convenient for embedding into a larger program.
searchApp :: AppSettings n a -> SearchEnv n a -> App (Searcher) SearchEvent Bool
searchApp (AppSettings th hks _ _ _) env  = App {appDraw = ad , appChooseCursor = showFirstCursor , appHandleEvent = he , appStartEvent = pure , appAttrMap = am}
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
    he s (AppEvent e)                     = if e ^. term == getQuery s then handleSearch s e else continue s
    he s _                                = continue s

-- | The initial state of the searcher. The editor is empty, the first @512@ elements of the vector are disaplyed as matches.
initialSearcher :: forall n a. KnownNat n => SearchEnv n a -> BChan SearchEvent -> Searcher
initialSearcher env source = Searcher (editorText True (Just 1) "") (list False ((:[]) <$> concatChunks k (env ^. candidates)) 0) source 0
  where
    n = natVal (Proxy :: Proxy n)
    k = 1 + (env ^. maxMatches) `div` fromInteger n

resetSearcher :: forall n a b. KnownNat n => SearchEnv n a -> Searcher -> Searcher
resetSearcher env = (numMatches .~ 0) . (matches .~ list False ((:[]) <$> concatChunks k (env ^. candidates)) 0)
  where
    n = natVal (Proxy :: Proxy n)
    k = 1 + (env ^. maxMatches) `div` fromInteger n

-- | Run app with given settings and return the final Searcher state.
runApp :: KnownNat n => AppSettings n a -> SearchFunctions a -> Chunks n -> IO (Searcher)
runApp s f c =     (\b -> (\env -> startSearcher env *> finally (theMain (searchApp s env) b . initialSearcher env $ b) (stopSearcher env))
               =<< searchEnv f (s ^. maximumMatches) (generateSearchEvent f (s ^. eventStrategy) b) c) =<< newBChan 8

-- | Run app with a vector that contains lines read from a handle and return the final Searcher state.
runAppFromHandle :: KnownNat n => AppSettings n a -> SearchFunctions a -> Handle -> IO (Searcher)
runAppFromHandle s f = runApp s f . getCompact <=< compact . forceChunks <=< chunksFromHandle (s ^. chunkSize)

-- | Run app with a vector that contains lines read from a handle and return the final Searcher state.
runAppFromVector :: KnownNat n =>  AppSettings n a -> SearchFunctions a -> Vector Text -> IO (Searcher)
runAppFromVector s f = runApp s f . getCompact <=< compact . forceChunks . makeChunks

-- | Run app and return the text of the selection if there is one else Nothing.
selected :: KnownNat n => AppSettings n a -> SearchFunctions a -> Chunks n -> IO (Maybe Text)
selected s f  = map (map (mconcat . snd) . listSelectedElement . (^. matches)) . runApp s f . getCompact <=< compact . forceChunks

-- | Same as `selected` but reads the vector from the supplied handle.
selectedFromHandle :: KnownNat n => AppSettings n a -> SearchFunctions a -> Handle -> IO (Maybe Text)
selectedFromHandle s f = selected s f <=< chunksFromHandle (s ^. chunkSize)

-- | Same as `selectedFromHandle` but allows for transforming the lines read and the final vector with supplied functions. See also `readVectorHandleWith`.
selectedFromHandleWith :: KnownNat n => (Text -> Text) -> (Vector Text -> Vector Text) -> AppSettings n a -> SearchFunctions a -> Handle -> IO (Maybe Text)
selectedFromHandleWith w t s f = selected s f . makeChunks <=< readVectorHandleWith w t

-- | Another variation on `selectedFromHandle`. See `fileNamesSorted` for what happens to read vector.
selectedFromFileNamesSorted :: KnownNat n => AppSettings n a -> SearchFunctions a -> Handle -> IO (Maybe Text)
selectedFromFileNamesSorted s f = selected s f .  makeChunks <=< fileNamesSorted

-- | Version of `selected` for file search using a simple implementation of searching file trees from "Talash.Files". Better to use either other
-- libraries like @unix-recursive@ or external programs like @fd@ for more complicated tasks.
selectedFromFiles :: KnownNat n => AppSettings n a -> SearchFunctions a -> [FindInDirs] -> IO (Maybe Text)
selectedFromFiles s f = selected s f . forceChunks . makeChunks . (flatten =<<) <=< findFilesInDirs

selectedUsing :: KnownNat n => AppSettings n a -> SearchFunctions a -> (a -> Text) -> Vector a -> IO (Maybe a)
selectedUsing s f t v = map (map (unsafeIndex v) . (`elemIndex` w) =<<) . selected s f . makeChunks $ w
  where
    w = map t v

-- | A version of `selected` that puts the selected text on the stdout.
runSearch :: AppSettings 64 a -> SearchFunctions a -> IO ()
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
