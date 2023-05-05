{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Variadic where

import Control.Monad.Morph (MFunctor(hoist), MMonad, MonadTrans)
import Control.Monad.Reader (ReaderT(ReaderT))
import Control.Variadic.Varargs (Varargs(Cons, Nil))
import Data.Functor (void)
import Data.Kind (Type)

-- | Same as 'Variadic' but captures the higher-kinded type parameter in the
-- return type. Useful so we can use 'Monad' and friends with 'Variadic'
-- functions.
newtype VariadicT args (m :: Type -> Type) a = VariadicT
  { unVariadicT :: Variadic args (m a)
  } deriving (Functor, Applicative, Monad) via ReaderT (Varargs args) m
    deriving (MFunctor, MMonad, MonadTrans) via ReaderT (Varargs args)

-- | Converts a function to a 'VariadicT'. Analogous to 'toVariadic'.
toVariadicT
  :: ( ToVariadic x
     , args ~ ToVariadicArgs x
     , m r ~ ToVariadicReturn x
     )
  => x -> VariadicT args m r
toVariadicT = VariadicT . toVariadic

-- | Converts a 'VariadicT' to a normal function. Analogous to 'fromVariadic'.
fromVariadicT
  :: ( FromVariadic args (m r)
     )
  => VariadicT args m r -> FromVariadicSignature args (m r)
fromVariadicT = fromVariadic . unVariadicT

-- | A function whose argument list is collapsed into 'Varargs'
-- and shows its return type.
newtype Variadic args a = Variadic
  { runVariadic :: Varargs args -> a
  }

-- | Resolves the argument list for a function of arbitrary arity.
type family ToVariadicArgs x :: [Type] where
  ToVariadicArgs (i -> o) = i ': ToVariadicArgs o
  ToVariadicArgs a = '[]

-- | Resolves the return type for a function of arbitrary arity.
type family ToVariadicReturn x :: Type where
  ToVariadicReturn (i -> o) = ToVariadicReturn o
  ToVariadicReturn a = a

-- | Converts a function of arbitrary arity to 'Variadic'.
class ToVariadic x where
  toVariadic :: x -> Variadic (ToVariadicArgs x) (ToVariadicReturn x)

instance {-# OVERLAPPING #-}
  ( ToVariadicArgs a ~ '[]
  , ToVariadicReturn a ~ a
  ) => ToVariadic a where
  toVariadic a = Variadic \_ -> a

instance {-# OVERLAPS #-}
  ( ToVariadic o
  , ToVariadicArgs (i -> o) ~ (i ': args)
  , ToVariadicArgs o ~ args
  , ToVariadicReturn (i -> o) ~ ToVariadicReturn o
  ) => ToVariadic (i -> o)
  where
  toVariadic f =
    Variadic \(arg `Cons` args) ->
      runVariadic (toVariadic (f arg)) args

-- | Builds a function signature given the @args@ and return type @r@.
type family FromVariadicSignature (args :: [Type]) (r :: Type) :: Type where
  FromVariadicSignature '[] r = r
  FromVariadicSignature (arg ': args) r = arg -> FromVariadicSignature args r

-- | Converts a 'Variadic' to a normal function.
class FromVariadic args r where
  fromVariadic :: Variadic args r -> FromVariadicSignature args r

instance FromVariadic '[] a where
  fromVariadic v = runVariadic v Nil

instance (FromVariadic args a) => FromVariadic (arg ': args) a where
  fromVariadic v arg =
    fromVariadic $ Variadic \args ->
      runVariadic v (arg `Cons` args)

-- | Convenience constraint enabling variadic.
--
-- @x@ is the Haskell function type,
-- @args@ is a type-level list of arguments,
-- @r@ is the return type.
--
-- Usually you'll want to use these type arguments polymorphically, e.g. -
--
-- > (...*>)
-- >   :: ( Applicative m
-- >      , IsVariadic va args (m a)
-- >      , IsVariadic vb args (m b)
-- >      )
-- >   => va -> vb -> vb
-- > va ...*> vb = fromVariadicT $ toVariadicT va *> toVariadicT vb
type IsVariadic x args r =
  ( ToVariadic x
  , args ~ ToVariadicArgs x
  , r ~ ToVariadicReturn x
  , x ~ FromVariadicSignature args r
  , FromVariadic args r
  )

-- | Analogous to 'fmap' for 'VariadicT' but works on vanilla functions.
vmap
  :: ( Functor f
     , IsVariadic va args (f a)
     , IsVariadic vb args (f b)
     )
  => (a -> b) -> va -> vb
vmap f va = fromVariadicT $ fmap f $ toVariadicT va

-- | Analogous to 'fmap' for 'VariadicT' but works on vanilla functions.
(<$>...)
  :: ( Functor f
     , IsVariadic va args (f a)
     , IsVariadic vb args (f b)
     )
  => (a -> b) -> va -> vb
(<$>...) = vmap

-- | Analogous to 'void' for 'VariadicT' but works on vanilla functions.
vvoid
  :: ( Functor f
     , IsVariadic va args (f a)
     , IsVariadic vu args (f ())
     )
  => va -> vu
vvoid va = fromVariadicT $ void $ toVariadicT va

-- | Analogous to '*>' for 'VariadicT' but works on vanilla functions.
(...*>)
  :: ( Applicative m
     , IsVariadic va args (m a)
     , IsVariadic vb args (m b)
     )
  => va -> vb -> vb
va ...*> vb = fromVariadicT $ toVariadicT va *> toVariadicT vb

-- | Analogous to '<*' for 'VariadicT' but works on vanilla functions.
(<*...)
  :: ( Applicative m
     , IsVariadic va args (m a)
     , IsVariadic vb args (m b)
     )
  => va -> vb -> va
va <*... vb = fromVariadicT $ toVariadicT va <* toVariadicT vb

-- | Analogous to '>>=' for 'VariadicT' but works on vanilla functions.
(...>>=)
  :: ( Monad m
     , IsVariadic va args (m a)
     , IsVariadic vb args (m b)
     )
  => va -> (a -> vb) -> vb
va ...>>= f = fromVariadicT $ toVariadicT va >>= \a -> toVariadicT (f a)

-- | Analogous to '=<<' for 'VariadicT' but works on vanilla functions.
(=<<...)
  :: ( Monad m
     , IsVariadic va args (m a)
     , IsVariadic vb args (m b)
     )
  => (a -> vb) -> va -> vb
(=<<...) = flip (...>>=)

-- | Analogous to 'hoist' for 'VariadicT' but works on vanilla functions.
vhoist
  :: ( Monad f
     , IsVariadic vf args (f a)
     , IsVariadic vg args (g a)
     )
  => (forall x. f x -> g x) -> vf -> vg
vhoist f = fromVariadicT . hoist f . toVariadicT
