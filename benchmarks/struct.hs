{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all #-}
{-# LANGUAGE DataKinds, BangPatterns #-}
import Criterion.Main
import Control.Monad
import Control.Monad.ST
import Data.Extensible
import Data.Extensible.Struct
import Data.Functor.Identity
import Data.IORef

main = do
  s <- newRepeat undefined :: IO (Struct RealWorld Identity '[Int])
  ref <- newIORef undefined
  defaultMain
    [ bgroup "basic"
      [ bench "atomicModify'_" $ whnfIO $ do
        set s membership (Identity (0 :: Int))
        replicateM 512 $ atomicModify' s membership (\(Identity x) -> let !z = x + 1 :: Int in (Identity z, z))
      , bench "IORef" $ whnfIO $ do
        writeIORef ref 0
        replicateM 512 $ atomicModifyIORef' ref (\x -> let !z = x + 1 in (z, z))
      ]
    ]
