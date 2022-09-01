{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

import           Control.Monad
import           Criterion
import qualified Criterion.Main  as C
import           Criterion.Types
import           Linear

import           Apecs
import           Apecs.Stores (StorableSparseSet)
import           Foreign.Storable (Storable)

-- pos_vel
newtype ECSPos = ECSPos (V2 Float) deriving (Eq, Show)
instance Component ECSPos where type Storage ECSPos = Cache 1000000 (Map ECSPos)

newtype ECSVel = ECSVel (V2 Float) deriving (Eq, Show)
instance Component ECSVel where type Storage ECSVel = Cache 100000 (Map ECSVel)

makeWorld "PosVel" [''ECSPos, ''ECSVel]

posVelInit :: System PosVel ()
posVelInit = do
  replicateM_ 100000 $ newEntity (ECSPos 0, ECSVel 1)
  replicateM_ 900000 $ newEntity (ECSPos 0)

posVelStep :: System PosVel ()
posVelStep = cmap $ \(ECSVel v, ECSPos p) -> ECSPos (p+v)

-- sparseset.storable
newtype SSSPos = SSSPos (V2 Float) deriving (Eq, Show, Storable)
instance Component SSSPos where type Storage SSSPos = StorableSparseSet SSSPos

newtype SSSVel = SSSVel (V2 Float) deriving (Eq, Show, Storable)
instance Component SSSVel where type Storage SSSVel = StorableSparseSet SSSVel

makeWorld "SSS" [''SSSPos, ''SSSVel]

posVelInitSS :: System SSS ()
posVelInitSS = do
  replicateM_ 100000 $ newEntity (SSSPos 0, SSSVel 1)
  replicateM_ 900000 $ newEntity (SSSPos 0)

posVelStepSS :: System SSS ()
posVelStepSS = cmap $ \(SSSVel v, SSSPos p) -> SSSPos (p+v)

main :: IO ()
main = C.defaultMainWith (C.defaultConfig {timeLimit = 10})
  [ bgroup "pos_vel / cached map"
    [ bench "init" $ whnfIO (initPosVel >>= runSystem posVelInit)
    , bench "step" $ whnfIO (initPosVel >>= runSystem (posVelInit >> posVelStep))
    ]
  , bgroup "pos_vel / SparseSet.Storable"
    [ bench "init" $ whnfIO (initSSS >>= runSystem posVelInitSS)
    , bench "step" $ whnfIO (initSSS >>= runSystem (posVelInitSS >> posVelStepSS))
    ]
  ]

