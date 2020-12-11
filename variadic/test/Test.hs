{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Variadic
import Control.Variadic.Generic (ghoist)
import Data.Char (chr, ord)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.Map.Strict (Map)
import Data.Maybe (isJust, listToMaybe)
import Test.Hspec (HasCallStack, SpecWith, describe, hspec)
import Test.Hspec.Expectations.Lifted (shouldBe, shouldReturn)
import Test.Infra.Handle (Handle(Handle))
import Test.ShouldNotCompile (shouldNotCompile)
import qualified Data.Map.Strict as Map
import qualified Test.Hspec as Hspec
import qualified Test.Infra.Handle

-- | Specialized 'Hspec.it' body to @IO ()@ to make type inference
-- easier for @hspec-expectations-lifted@.
it :: (HasCallStack) => String -> IO () -> SpecWith ()
it = Hspec.it

main :: IO ()
main = hspec do
  describe "Variadic" do
    describe "arity" do
      it "0" do
        let v = toVariadic ioA
        fromVariadic v `shouldReturn` 'a'
      it "1" do
        let v = toVariadic ioCharToInt
        fromVariadic v 'a' `shouldReturn` 97
      it "2" do
        let v = toVariadic ioPlusChars
        fromVariadic v 'a' 2 `shouldReturn` 'c'

  describe "VariadicT" do
    it "*>" do
      let v1 = toVariadicT ioCharToInt
      let v2 = toVariadicT ioIncChar
      let v3 = v1 *> v2
      fromVariadicT v3 'a' `shouldReturn` 'b'

  describe "Functions" do
    it "...*>" do
      ref <- newIORef '\0'
      let f = writeIORef ref ...*> ioIncChar
      f 'a' `shouldReturn` 'b'
      readIORef ref `shouldReturn` 'a'

    it "vhoist" do
      let f = vhoist listToMaybe twoList
      f 1 2 `shouldBe` Just 1

  describe "Generic" do
    it "errors" do
      shouldNotCompile "TestCompilerErrors"

    it "ghoist" do
      -- Keep track of how many times we call 'close'.
      closeCounter <- newIORef (0 :: Int)

      -- A starting implementation of 'Handle'.
      let readerHandle :: Handle (ReaderT DB IO)
          readerHandle = Handle
            { insert = \name descr -> do
                db <- ask
                liftIO $ atomicModifyIORef' db \rows ->
                  let k = maybe 1 (+1) $ fmap fst $ Map.lookupMax rows
                   in ( Map.insert k (name, descr) rows
                      , k
                      )

            , get = \k -> do
                db <- ask
                rows <- liftIO $ readIORef db
                pure $ Map.lookup k rows

            , delete = \k -> do
                db <- ask
                liftIO $ atomicModifyIORef' db \rows ->
                  let (oldValue, rows') = Map.updateLookupWithKey mempty k rows
                   in (rows', isJust oldValue)

            , close = atomicModifyIORef' closeCounter \i -> (i + 1, ())
            }

      -- Create the initial database.
      db :: DB <- newIORef mempty

      -- Tests for the @readerHandle@.
      flip runReaderT db do
        let Handle { insert, get, delete, close } = readerHandle
        get 1 `shouldReturn` Nothing
        insert "foo" "bar" `shouldReturn` 1
        get 1 `shouldReturn` Just ("foo", "bar")
        get 2 `shouldReturn` Nothing
        insert "spam" "eggs" `shouldReturn` 2
        get 2 `shouldReturn` Just ("spam", "eggs")
        delete 2 `shouldReturn` True
        get 2 `shouldReturn` Nothing
        delete 2 `shouldReturn` False
        liftIO do
          readIORef closeCounter `shouldReturn` 0
          close
          readIORef closeCounter `shouldReturn` 1

      -- Used to keep track of how many times our @runner@ is invoked.
      runCounter <- newIORef (0 :: Int)

      -- Natural transformation for converting our @readerHandle@ to @ioHandle@.
      let runner :: ReaderT DB IO x -> IO x
          runner action = do
            atomicModifyIORef' runCounter \i -> (i + 1, ())
            runReaderT action db

      -- Example usage of 'ghoist'.
      let ioHandle :: Handle IO
          ioHandle = ghoist runner readerHandle

      -- Tests for the @ioHandle@.
      do
        let Handle { insert, get, delete, close } = ioHandle
        readIORef runCounter `shouldReturn` 0
        get 1 `shouldReturn` Just ("foo", "bar")
        readIORef runCounter `shouldReturn` 1
        get 2 `shouldReturn` Nothing
        readIORef runCounter `shouldReturn` 2
        k <- insert "baz" "quux"
        readIORef runCounter `shouldReturn` 3
        k `shouldBe` 2
        get 2 `shouldReturn` Just ("baz", "quux")
        readIORef runCounter `shouldReturn` 4
        delete 2 `shouldReturn` True
        readIORef runCounter `shouldReturn` 5
        get 2 `shouldReturn` Nothing
        readIORef runCounter `shouldReturn` 6
        -- Calling 'close' should not increment the @runCounter@
        -- but should still increment @closeCounter@.
        readIORef closeCounter `shouldReturn` 1
        close
        readIORef closeCounter `shouldReturn` 2
        readIORef runCounter `shouldReturn` 6

ioA :: IO Char
ioA = pure 'a'

ioCharToInt :: Char -> IO Int
ioCharToInt c = pure (ord c)

ioIncChar :: Char -> IO Char
ioIncChar c = pure (chr (ord c + 1))

ioPlusChars :: Char -> Int -> IO Char
ioPlusChars c n = pure (chr (ord c + n))

twoList :: Int -> Int -> [Int]
twoList x y = [x, y]

-- | A simple in-memory database.
type DB = IORef (Map Int (String, String))
