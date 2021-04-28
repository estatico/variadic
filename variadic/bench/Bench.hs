{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Criterion.Main
import Data.Maybe
import Control.Monad.Morph
import qualified Control.Variadic
import qualified Control.Variadic.Bench.NoReader

#define BENCH(M) \
  [ bench "vhoist" $ nf id $ \
      let go = M.vhoist listToMaybe f \
      in run go \
  , bench "monad" $ nf id $ \
      let go = M.fromVariadicT do { \
            x <- M.toVariadicT f; \
            y <- M.toVariadicT g; \
            z <- lift [x, y]; \
            pure z; } \
        in run go \
  ]

main :: IO ()
main = defaultMain
  [ bgroup "Variadic" (BENCH(Control.Variadic))
  , bgroup "NoReader" (BENCH(Control.Variadic.Bench.NoReader))
  ]
  where
  f  x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 x31 x32 x33 x34 x35 =
    [x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x30,x31,x32,x33,x34,x35]

  g  x35 x34 x33 x32 x31 x30 x29 x28 x27 x26 x25 x24 x23 x22 x21 x20 x19 x18 x17 x16 x15 x14 x13 x12 x11 x10 x9 x8 x7 x6 x5 x4 x3 x2 x1 x0 =
    [x35,x34,x33,x32,x31,x30,x29,x28,x27,x26,x25,x24,x23,x22,x21,x20,x19,x18,x17,x16,x15,x14,x13,x12,x11,x10,x9,x8,x7,x6,x5,x4,x3,x2,x1,x0]

  run h =
    h '0' '1' '2' '3' '4' '5' '6' '7' '8' '9' 'a' 'b' 'c' 'd' 'e' 'f' 'g' 'h' 'i' 'j' 'k' 'l' 'm' 'n' 'o' 'p' 'q' 'r' 's' 't' 'u' 'v' 'w' 'x' 'y' 'z'
