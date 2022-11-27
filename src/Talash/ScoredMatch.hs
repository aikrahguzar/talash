-- |
{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE  MultiParamTypeClasses #-}


module Talash.ScoredMatch (ChunkIndex (..) , ScoredMatchSized (..) , emptyMatch) where

import Talash.Intro
import GHC.TypeNats
import qualified Data.Vector.Unboxed.Sized as S

data ChunkIndex = ChunkIndex {number :: {-# UNPACK #-} !Int , index :: {-# UNPACK #-} !Int} deriving (Eq , Ord , Show)

data ScoredMatchSized (n::Nat) = ScoredMatchSized { score :: {-# UNPACK #-} !(Down Int), chunkIndex :: {-# UNPACK #-} !ChunkIndex
                                                  , matchData :: {-# UNPACK #-} !(S.Vector n Int)} deriving (Show)

instance Eq (ScoredMatchSized n) where
  (ScoredMatchSized s i _) == (ScoredMatchSized t j _) = s == t && i == j

instance Ord (ScoredMatchSized n) where
  compare (ScoredMatchSized s i _) (ScoredMatchSized t j _) = compare (s,i) (t,j)

emptyMatch :: Int -> Int -> ScoredMatchSized 0
emptyMatch i j = ScoredMatchSized 0 (ChunkIndex i j) S.empty
