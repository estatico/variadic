{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
module Test.Infra.Handle where

import GHC.Generics (Generic)

-- | An example handle for interacting with a database.
-- We'll be using it as a test case for @ghoist@.
data Handle (f :: * -> *) = Handle
  { insert :: String -> String -> f Int
  , get :: Int -> f (Maybe (String, String))
  , delete :: Int -> f Bool
  , close :: IO ()
  } deriving stock (Generic)
