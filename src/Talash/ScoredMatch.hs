-- |
{-# LANGUAGE  GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE  MultiParamTypeClasses #-}


module Talash.ScoredMatch where

import Talash.Intro
import Data.Vector.Unboxed.Deriving
import GHC.TypeNats
import qualified Data.Vector.Unboxed.Sized as S

data ChunkIndex = ChunkIndex {number :: {-# UNPACK #-} !Int , index :: {-# UNPACK #-} !Int} deriving (Eq , Ord , Show)

derivingUnbox "ChunkIndex"
    [t| ChunkIndex -> (Int , Int) |]
    [|\(ChunkIndex i j) -> (i,j)|]
    [|\(i,j) -> ChunkIndex i j|]

data ScoredMatchSized (n::Nat) = ScoredMatchSized { score :: {-# UNPACK #-} !(Down Int), chunkIndex :: {-# UNPACK #-} !ChunkIndex
                                                  , matchData :: {-# UNPACK #-} !(S.Vector n Int)} deriving (Show)

derivingUnbox "ScoredMatchSized"
    [t| forall (n::Nat). KnownNat n => ScoredMatchSized n -> (Int , Int , Int , S.Vector n Int) |]
    [| \(ScoredMatchSized (Down s) (ChunkIndex i j) m) -> (s, i , j , m) |]
    [| \(s, i , j , m) -> ScoredMatchSized (Down s) (ChunkIndex i j) m |]

instance Eq (ScoredMatchSized n) where
  (ScoredMatchSized s i _) == (ScoredMatchSized t j _) = s == t && i == j

instance Ord (ScoredMatchSized n) where
  compare (ScoredMatchSized s i _) (ScoredMatchSized t j _) = compare (s,i) (t,j)
