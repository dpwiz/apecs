{-# LANGUAGE TypeFamilies #-}

module Control.ECS.Immutable where

import qualified Data.IntSet as S
import qualified Data.IntMap as M
import Control.Monad.State
import Control.Lens

import Control.ECS.Types

newtype SimpleMap c = SimpleMap c deriving (Eq, Show)
instance Component (SimpleMap c) where

  type Repr    (SimpleMap c) = Maybe c
  type Storage (SimpleMap c) = M.IntMap c

  empty = Store mempty

  slice = Slice . M.keysSet . _unStore <$> get
  retrieve (Entity e) = Reads . M.lookup e . _unStore <$> get

  store (Entity e) (Writes (Just x)) = unStore %= M.insert e x
  store (Entity e) (Writes Nothing)  = unStore %= M.delete e

data SimpleFlag
instance Component SimpleFlag where

  type Repr    SimpleFlag = Bool
  type Storage SimpleFlag = S.IntSet

  empty = Store mempty

  slice = Slice . _unStore <$> get
  retrieve (Entity e) = Reads . S.member e . _unStore <$> get

  store (Entity e) (Writes True) = unStore %= S.insert e
  store (Entity e) (Writes True) = unStore %= S.delete e

data EntityCounter
instance Component EntityCounter where

  type Repr    EntityCounter = Int
  type Storage EntityCounter = Int

  empty = Store 0

  slice = return . Slice . S.singleton $ -1
  retrieve _ = Reads . _unStore <$> get
  store _ (Writes count) = unStore .= count
