-- |

module Talash.Intro (map , module Export) where

import Control.Exception as Export
import Control.Monad as Export
import Control.Monad.Extra as Export
import Control.Monad.Reader as Export
import Control.Monad.ST as Export (ST, runST)
import Data.Bifunctor as Export
import Data.ByteString as Export (ByteString)
import Data.Either as Export
import Data.Foldable as Export
import Data.Functor as Export
import Data.List as Export (unfoldr)
import Data.Maybe as Export
import Data.Ord as Export
import Data.Proxy as Export
import Data.Text as Export (Text , pack , unpack)
import Data.Text.Encoding as Export
import Data.Text.IO as Export (putStr , putStrLn)
import Data.Vector as Export (Vector)
import GHC.TypeLits as Export hiding (TypeError)
import Lens.Micro as Export (ASetter' , over, set, (^.) , _1 , _2 , _3 , (.~) , (?~) , (%~) , to)
import Prelude as Export hiding ((>>) , map , putStr , putStrLn)
import Safe as Export
import System.Environment as Export
import System.IO as Export hiding (putStr , putStrLn)
import Text.Read as Export hiding (lift , list , get)

{-# INLINE map #-}
map :: Functor f => (a -> b) -> f a -> f b
map = fmap
