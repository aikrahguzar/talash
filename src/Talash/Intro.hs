-- |

module Talash.Intro (map , module Export) where

import Control.Monad as Export
import Control.Monad.Extra as Export
import Control.Monad.Reader as Export
import Data.Bifunctor as Export
import Data.ByteString as Export (ByteString)
import Data.Either as Export
import Data.Foldable as Export
import Data.Functor as Export
import Data.Maybe as Export
import Data.Ord as Export
import Data.Proxy as Export
import Data.Text as Export (Text)
import Data.Text.Encoding as Export
import Data.Text.IO as Export (putStr , putStrLn)
import Prelude as Export hiding ((>>) , map , putStr , putStrLn)
import Safe as Export
import Data.List as Export (unfoldr)
import Text.Read as Export hiding (lift , list)
import System.IO as Export hiding (putStr , putStrLn)
import Control.Exception as Export

{-# INLINE map #-}
map :: Functor f => (a -> b) -> f a -> f b
map = fmap
