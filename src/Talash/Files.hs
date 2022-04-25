-- | A potentially too simple interface for getting candidates for search from file trees. Doesn't follow symbolic links. For a better solution to this use
-- [unix-recursive](https://hackage.haskell.org/package/unix-recursive).
module Talash.Files (-- * Types
                     Conf (..) , FindConf (..) , FindInDirs (..) , FileTree (..)
                     -- * File Collection
                    , defConf , withExts , ignoreExts , findWithExts , findFilesInDirs , executables
                     -- * Internal Details
                    , dirContentsWith , fileTreeWith , minify , flatten , ext) where

import Control.Exception
import qualified Data.ByteString.Char8 as B
import qualified Data.HashSet as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as V
import Intro
import System.Posix.Directory.ByteString
import System.Posix.Env.ByteString
import System.Posix.Files.ByteString

-- Configruation for the search when recursivley constructing the file tree.
data Conf = Conf {
              -- | Test for whether to include a file in the file tree. The second argument is the base name of the file.
              includeFile :: FileStatus -> ByteString -> IO Bool ,
              -- | Test used to determine whether to enter a directory to search for files.
              filterPath :: ByteString -> Bool }

-- | A simple type to represent a search either for a specific set of extensions or esle for excluding a specific set of extensions. An extension here
-- is just the part of the filename after the last '.' i.e this module doesn't handle multiple extensions.
data FindConf = Find !(S.HashSet ByteString) | Ignore !(S.HashSet ByteString) deriving Show

data FindInDirs = FindInDirs {
                    -- | The configuration of finding or excluding the extensions for this set of directories.
                    confLocal :: FindConf ,
                    -- | The list of directories to which this configuration should apply.
                    dirsLocal :: [ByteString]}

data FileTree a = Dir { rootDir  :: a -- ^ The root directory
                      , dirFiles :: V.Vector a -- ^ The files in the root directory that are not subdirectories
                      , subDirs  :: V.Vector (FileTree a)} -- ^ The vector of trees formed by subdirectories
                        deriving (Eq , Show)

-- | Default configuration, include every file and search directory.
defConf :: Conf
defConf = Conf (const . const $ pure True) (const True)

-- | Given the configuration and a directory returns a vector where the Left elements are the files in the directory that pass the `includeFile` test while
--   the Right elements are subdirectories that pass the `filterPath` test.
{-# INLINEABLE dirContentsWith #-}
dirContentsWith :: Conf -> ByteString -> IO (V.Vector (Either ByteString ByteString))
dirContentsWith c d = bracket (openDirStream d) closeDirStream (\s -> V.unfoldrM (map (map ( , s)) . go) s)
  where
    go s = nm =<< readDirStream s
      where
        nm f
          | f == ""                                         = pure Nothing
          | otherwise                                       = hr =<< getSymbolicLinkStatus f
          where
            hr fs = det (isDirectory fs && filterPath c f && f /= "." && f /= "..") =<< includeFile c fs f
            det True  _    = pure . Just . Right $ f
            det False True = pure . Just . Left  $ f
            det _     _    = go s

-- | Constructs the file tree with the given the second argument at the root according to the given configuration.
{-# INLINEABLE fileTreeWith #-}
fileTreeWith :: Conf -> ByteString -> IO (FileTree Text)
fileTreeWith c d = bracket getWorkingDirectory changeWorkingDirectory (const . catch (changeWorkingDirectory d *> (go =<< dirContentsWith c ".")) $ cex)
  where
    go v = (\(a , b) -> Dir (T.decodeUtf8 d) a <$!> V.mapM (fileTreeWith c) b) . V.partitionWith (first T.decodeUtf8) $ v
    cex (_ :: SomeException) = pure $ Dir (T.decodeUtf8 d) mempty mempty

-- | Collapses the directories with only subdirectory and no other files.
{-# INLINEABLE minify #-}
minify :: FileTree Text -> FileTree Text
minify (Dir d f t)
  | f == V.empty && V.length t == 1  = (\(Dir d' f' t') -> Dir (d <> d') f' t') (V.unsafeHead t)
  | otherwise                        = Dir d f . V.map minify $ t

-- | Flattens the fileTree by completing the paths of the file relative to that of root directory.
{-# INLINEABLE flatten #-}
flatten :: FileTree Text -> V.Vector Text
flatten (Dir d f t) = V.concatMap go t <> V.map ((d <> "/") <>) f
  where
    go (Dir d' !f' t') = flatten (Dir (d <> "/" <> d') f' t')

{-# INLINABLE withExts #-}
withExts :: [ByteString] -- ^ The set of extensions to search for
  -> FindConf
withExts = Find . S.fromList

{-# INLINABLE ignoreExts #-}
ignoreExts :: [ByteString] -- ^ The set of extensions to ignore.
  -> FindConf
ignoreExts = Ignore . S.fromList

-- | The last extension of a file. Returns empty bytestring if there is none.
{-# INLINABLE ext #-}
ext :: ByteString -> ByteString
ext c = if e == c then mempty else e
  where
    e = snd . B.spanEnd (/= '.') $ c

-- | Find files in the given set of directories that either have a specific extension (`Find` case) or else excluding a certain set of extensiosn (`Ignore` case).
{-# INLINE findWithExts #-}
findWithExts :: FindInDirs -> IO (V.Vector (FileTree Text))
findWithExts (FindInDirs c d) = V.mapM (fileTreeWith ch) . V.fromList $ d
  where
    ch
      | Find   es <- c     = defConf {includeFile = \ !s !n -> pure $ isRegularFile s && S.member (ext n) es}
      | Ignore es <- c     = defConf {includeFile = \ !s !n -> pure $ isRegularFile s && not (S.member (ext n) es)}

-- | Like `findWithExts` but applied to mutliple lists of directories each with their own configuration of extensions.
{-# INLINABLE findFilesInDirs #-}
findFilesInDirs :: [FindInDirs] -> IO (V.Vector (FileTree Text))
findFilesInDirs = foldr (\a t -> t <> findWithExts a) (pure mempty)

-- | Find all the executables in PATH
executables :: IO (V.Vector Text)
executables = map (V.uniq . V.modify V.sort) . foldr merge (pure V.empty) . B.split ':' =<< getEnvDefault "PATH" ""
  where
    cl = defConf { filterPath = const False , includeFile = \ s p ->  map ((isRegularFile s || isSymbolicLink s) &&) . fileAccess p False False $ True}
    merge a t = t <> map (V.map (T.takeWhileEnd (/= '/')) . flatten) (fileTreeWith cl a)
