-- | Re-implementation of @Control.Variadic@ which does not use @ReaderT@
-- and, as such, does not pack the argument list into @Varargs@. While
-- this may seem to be a more efficient encoding, the benchmarks
-- don't seem to prove this out.
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Variadic.Bench.NoReader where

import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Control.Monad.Morph (MFunctor(hoist), MMonad(embed), MonadTrans(lift))

type family ToVariadicArgs x :: [Type] where
  ToVariadicArgs (a -> x) = a ': ToVariadicArgs x
  ToVariadicArgs a = '[]

type family ToVariadicReturn x :: Type where
  ToVariadicReturn (a -> x) = ToVariadicReturn x
  ToVariadicReturn a = a

type family Signature (args :: [Type]) r where
  Signature '[] r = r
  Signature (x ': xs) r = x -> Signature xs r

type IsVariadic x args r =
  ( ToVariadicArgs x ~ args
  , ToVariadicReturn x ~ r
  , Coercible x (Variadic args r)
  , Signature args r ~ x
  )

toVariadic
  :: (IsVariadic x args a)
  => x -> Variadic args a
toVariadic = coerce

type IsVariadicT x args f r =
  ( ToVariadicArgs x ~ args
  , ToVariadicReturn x ~ f r
  , Coercible x (VariadicT args f r)
  , Signature args (f r) ~ x
  )

toVariadicT
  :: (IsVariadicT x args f a)
  => x -> VariadicT args f a
toVariadicT = coerce

newtype Variadic (args :: [Type]) (a :: Type) = Variadic
  { runVariadic :: Signature args a
  }

newtype VariadicT (args :: [Type]) (f :: Type -> Type) (a :: Type) = VariadicT
  { runVariadicT :: Signature args (f a)
  }

-- Added to make compatible with the Control.Variadic module.
fromVariadicT :: VariadicT args f a -> Signature args (f a)
fromVariadicT = runVariadicT

-- Added to make compatible with the Control.Variadic module.
fromVariadic :: Variadic args a -> Signature args a
fromVariadic = runVariadic

instance (Functor f) => Functor (VariadicT '[] f) where
  fmap f (VariadicT x) = VariadicT $ fmap f x
  {-# INLINE fmap #-}

instance (Functor (VariadicT args f)) => Functor (VariadicT (arg ': args) f) where
  fmap f (VariadicT x) =
    VariadicT \arg -> runVariadicT $
      fmap f $ VariadicT @args @f $ x arg
  {-# INLINE fmap #-}

instance (Applicative f) => Applicative (VariadicT '[] f) where
  pure a = VariadicT $ pure a
  {-# INLINE pure #-}

  VariadicT x <*> VariadicT y = VariadicT $ x <*> y
  {-# INLINE (<*>) #-}

instance (Applicative (VariadicT args f)) => Applicative (VariadicT (arg ': args) f) where
  pure a = VariadicT \_ -> runVariadicT $ pure @(VariadicT args f) a
  {-# INLINE pure #-}

  (<*>)
    :: forall a b.
       VariadicT (arg ': args) f (a -> b)
    -> VariadicT (arg ': args) f a
    -> VariadicT (arg ': args) f b
  VariadicT x <*> VariadicT y =
    VariadicT \arg -> runVariadicT $
      VariadicT @args @f @(a -> b) (x arg) <*> VariadicT @args @f @a (y arg)
  {-# INLINE (<*>) #-}

instance (Monad m) => Monad (VariadicT '[] m) where
  VariadicT x >>= f =
    VariadicT $ x >>= \a -> runVariadicT $ f a

instance (Monad (VariadicT args m)) => Monad (VariadicT (arg ': args) m) where
  (>>=)
    :: forall a b.
       VariadicT (arg ': args) m a
    -> (a -> VariadicT (arg ': args) m b)
    -> VariadicT (arg ': args) m b
  VariadicT x >>= f =
    VariadicT \arg -> runVariadicT $
      VariadicT @args @m @a (x arg) >>= \a ->
        VariadicT @args @m @b $ runVariadicT (f a) arg

instance MFunctor (VariadicT '[]) where
  hoist f (VariadicT x) = VariadicT $ f x
  {-# INLINE hoist #-}

instance (MFunctor (VariadicT args)) => MFunctor (VariadicT (arg ': args)) where
  hoist
    :: forall m n b. (Monad m)
    => (forall a. m a -> n a)
    -> VariadicT (arg ': args) m b
    -> VariadicT (arg ': args) n b
  hoist f (VariadicT x) =
    VariadicT \arg -> runVariadicT $
      hoist f $ VariadicT @args @m @b $ x arg
  {-# INLINE hoist #-}

instance MMonad (VariadicT '[]) where
  embed f (VariadicT x) = f x
  {-# INLINE embed #-}

instance (MMonad (VariadicT args)) => MMonad (VariadicT (arg ': args)) where
  embed
    :: forall m n b. (Monad n)
    => (forall a. m a -> VariadicT (arg ': args) n a)
    -> VariadicT (arg ': args) m b
    -> VariadicT (arg ': args) n b
  embed f (VariadicT x) =
    VariadicT \arg ->
      runVariadicT $
        embed
        (\ma -> VariadicT @args @n (runVariadicT (f ma) arg))
        (VariadicT @args @m @b (x arg))
  {-# INLINE embed #-}

instance MonadTrans (VariadicT '[]) where
  lift ma = VariadicT ma
  {-# INLINE lift #-}

instance (MonadTrans (VariadicT args)) => MonadTrans (VariadicT (arg ': args)) where
  lift ma = VariadicT \_ -> runVariadicT @args $ lift ma
  {-# INLINE lift #-}

-- | Analogous to '*>' for 'VariadicT' but works on vanilla functions.
(...*>)
  :: forall va vb args m a b.
     ( Applicative (VariadicT args m)
     , IsVariadic va args (m a)
     , IsVariadic vb args (m b)
     )
  => va -> vb -> vb
va ...*> vb = fromVariadicT $ toVariadicT @va @args va *> toVariadicT @vb @args vb

-- | Analogous to 'hoist' for 'VariadicT' but works on vanilla functions.
vhoist
  :: ( Monad f
     , MFunctor (VariadicT args)
     , IsVariadicT vf args f a
     , IsVariadicT vg args g a
     )
  => (forall x. f x -> g x) -> vf -> vg
vhoist f = runVariadicT . hoist f . toVariadicT
