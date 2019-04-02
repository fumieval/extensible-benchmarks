{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE PackageImports #-}
import Data.Void

-- extensible-effects
import qualified Control.Eff as ExtEff
import qualified Control.Eff.Reader.Strict as ExtEff
import qualified Control.Eff.Writer.Strict as ExtEff
import qualified Control.Eff.State.Strict as ExtEff

-- freer-simple
import qualified Control.Monad.Freer as Freer
import qualified Control.Monad.Freer.Reader as Freer
import qualified Control.Monad.Freer.Writer as Freer
import qualified Control.Monad.Freer.State as Freer

-- fused
import qualified "fused-effects" Control.Effect as Fused
import qualified "fused-effects" Control.Effect.Reader as Fused
import qualified "fused-effects" Control.Effect.Writer as Fused
import qualified "fused-effects" Control.Effect.State as Fused

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Control.Monad.RWS.Strict

import Data.Extensible.Effect
import Data.Extensible.Effect.Default

import Gauge.Main

testExtEff :: (ExtEff.Member (ExtEff.Reader Int) r
  , ExtEff.Member (ExtEff.State Int) r
  , ExtEff.Member (ExtEff.Writer (Sum Int)) r)
  => ExtEff.Eff r ()
testExtEff = replicateM_ 100 $ do
  r :: Int <- ExtEff.ask
  s <- ExtEff.get
  ExtEff.tell (Sum s)
  ExtEff.put $! s + r

runExtEff :: ExtEff.Eff
  ( ExtEff.Reader Int
  ': ExtEff.State Int
  ': ExtEff.Writer (Sum Int)
  ': '[]) a -> ((a, Int), Sum Int)
runExtEff = ExtEff.run
  . ExtEff.runMonoidWriter
  . ExtEff.runState 0
  . ExtEff.runReader 1

testFreer :: (Freer.Member (Freer.Reader Int) r
  , Freer.Member (Freer.State Int) r
  , Freer.Member (Freer.Writer (Sum Int)) r)
  => Freer.Eff r ()
testFreer = replicateM_ 100 $ do
  r :: Int <- Freer.ask
  s <- Freer.get
  Freer.tell (Sum s)
  Freer.put $! s + r

runFreer :: Freer.Eff '[Freer.Reader Int, Freer.State Int, Freer.Writer (Sum Int)] a
  -> ((a, Int), Sum Int)
runFreer = Freer.run
  . Freer.runWriter
  . Freer.runState 0
  . Freer.runReader 1

testFused :: (Fused.Member (Fused.Reader Int) sig
  , Fused.Member (Fused.State Int) sig
  , Fused.Member (Fused.Writer (Sum Int)) sig
  , Fused.Carrier sig m)
  => m ()
testFused = replicateM_ 100 $ do
  r :: Int <- Fused.ask
  s <- Fused.get
  Fused.tell (Sum s)
  Fused.put $! s + r

runFused :: Fused.ReaderC Int (Fused.StateC Int (Fused.WriterC (Sum Int) Fused.PureC)) a
  -> (Sum Int, (Int, a))
runFused = Fused.run
  . Fused.runWriter
  . Fused.runState 0
  . Fused.runReader 1

testMTL :: (MonadReader Int m, MonadState Int m, MonadWriter (Sum Int) m)
  => m ()
testMTL = replicateM_ 100 $ do
  r <- ask
  s <- get
  tell (Sum s)
  put $! s + r

runMTL :: ReaderT Int (StateT Int (Writer (Sum Int))) a -> ((a, Int), Sum Int)
runMTL = runWriter
  . flip runStateT 0
  . flip runReaderT 1

runExtensible :: Eff '[ReaderDef Int, StateDef Int, WriterDef (Sum Int)] a
  -> ((a, Int), Sum Int)
runExtensible = leaveEff
  . runWriterDef
  . flip runStateDef 0
  . flip runReaderDef 1

main = defaultMain
  [ bgroup "rws"
    [ bench "mtl" $ nf runMTL testMTL
    , bench "mtl-RWS" $ nf (\m -> runRWS m 0 1) testMTL
    , bench "extensible" $ nf runExtensible testMTL
    , bench "exteff" $ nf runExtEff testExtEff
    , bench "freer-simple" $ nf runFreer testFreer
    , bench "fused-effects" $ nf runFused testFused
    ]
  ]
