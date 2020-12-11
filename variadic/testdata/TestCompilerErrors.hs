{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
module TestCompilerErrors where

import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Variadic.Generic (ghoist, ghoist', ghoist0)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics (Generic)

data Handle1 f = Handle1
  { a1 :: f ()
  , b1 :: IO ()
  } deriving stock (Generic)

readerHandle1 :: Handle1 (ReaderT () IO)
readerHandle1 = Handle1
  { a1 = pure ()
  , b1 = pure ()
  }

ioHandle1 :: Handle1 IO
ioHandle1 = ghoist (flip runReaderT ()) readerHandle1

data Handle2 f = Handle2
  { a2 :: f ()
  , b2 :: f ()
  } deriving stock (Generic)

readerHandle2 :: Handle2 (ReaderT () IO)
readerHandle2 = Handle2
  { a1 = pure ()
  , b2 = pure ()
  }

ioHandle2 :: Handle2 IO
ioHandle2 = ghoist' (Proxy @'["b2"]) (flip runReaderT ()) readerHandle2

data Handle3 f = Handle3
  { a3 :: f ()
  , b3 :: IO ()
  } deriving stock (Generic)

readerHandle3 :: Handle3 (ReaderT () IO)
readerHandle3 = Handle3
  { a3 = pure ()
  , b3 = pure ()
  }

ioHandle3 :: Handle3 IO
ioHandle3 = ghoist0 (flip runReaderT ()) readerHandle3
