{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Control.Variadic.Bench.NoReader.Generic.Internal where

import Control.Monad.Morph (MFunctor)
import Control.Variadic.Bench.NoReader
import Data.Kind (Constraint)
import Data.Proxy (Proxy(Proxy))
import GHC.Generics
import GHC.TypeLits

-- | Runs @hoist@ on the return values each field of @r@
-- with the given natural transformation function, ignoring
-- the @close@ field, if it exists.
ghoist
  :: ( Generic (r f)
     , Generic (r g)
     , GHoist (Rep (r f)) (Rep (r g)) f g '["close"]
     )
  => (forall x. f x -> g x)
  -> r f
  -> r g
ghoist = ghoist' (Proxy @'["close"])

-- | Runs @hoist@ on the return values each field of @r@
-- with the given natural transformation function; no fields
-- are ignored.
ghoist0
  :: ( Generic (r f)
     , Generic (r g)
     , GHoist (Rep (r f)) (Rep (r g)) f g '[]
     )
  => (forall x. f x -> g x)
  -> r f
  -> r g
ghoist0 = ghoist' (Proxy @'[])

-- | Runs @hoist@ on the return values each field of @r@
-- with the given natural transformation function.
-- A supplied of @ignored@ fields is provided to signal which
-- fields should not be transformed.
ghoist'
  :: ( Generic (r f)
     , Generic (r g)
     , GHoist (Rep (r f)) (Rep (r g)) f g ignored
     )
  => proxy ignored
  -> (forall x. f x -> g x)
  -> r f
  -> r g
ghoist' proxy f = to . gghoist proxy f . from

class GHoist (i :: * -> *) (o :: * -> *) (f :: * -> *) (g :: * -> *) (ignored :: [Symbol]) where
  gghoist :: proxy ignored -> (forall x. f x -> g x) -> i p -> o p

instance (GHoist i o f g ignored) => GHoist (M1 D c i) (M1 D c o) f g ignored where
  gghoist proxy f (M1 i) = M1 (gghoist proxy f i)

instance (GHoist i o f g ignored) => GHoist (M1 C c i) (M1 C c o) f g ignored where
  gghoist proxy f (M1 i) = M1 (gghoist proxy f i)

instance {-# OVERLAPPING #-}
  ( VerifyIgnored n a ignored
  ) => GHoist
        (M1 S ('MetaSel ('Just n) su ss ds) (K1 R a))
        (M1 S ('MetaSel ('Just n) su ss ds) (K1 R a))
        f g ignored
  where
  gghoist _proxy _f (M1 i) = M1 i

instance
  ( GHoist (K1 R i) (K1 R o) f g ignored
  , VerifyNotIgnored n i ignored
  ) => GHoist
        (M1 S ('MetaSel ('Just n) su ss ds) (K1 R i))
        (M1 S ('MetaSel ('Just n) su ss ds) (K1 R o))
        f g ignored
  where
  gghoist proxy f (M1 i) = M1 (gghoist proxy f i)

instance
  ( GHoist i1 o1 f g ignored
  , GHoist i2 o2 f g ignored
  ) => GHoist (i1 :*: i2) (o1 :*: o2) f g ignored
  where
  gghoist proxy f (i1 :*: i2) = gghoist proxy f i1 :*: gghoist proxy f i2

instance
  ( Monad f
  , MFunctor (VariadicT args)
  , IsVariadicT vf args f a
  , IsVariadicT vg args g a
  ) => GHoist (K1 R vf) (K1 R vg) f g ignored
  where
  gghoist _proxy f (K1 vf) = K1 (vhoist f vf)

type VerifyIgnored e a es = VerifyIgnoredGo e a es es

type family VerifyIgnoredGo e a es orig :: Constraint where
  VerifyIgnoredGo x a (x ': xs) orig = ()
  VerifyIgnoredGo y a (x ': xs) orig = VerifyIgnoredGo y a xs orig
  VerifyIgnoredGo x a '[] orig       =
    TypeError
      ( 'Text "Field:"
          ':$$: 'Text "  "
          ':<>: 'Text x
          ':<>: 'Text " :: "
          ':<>: 'ShowType a
          ':$$: 'Text "cannot be ghoist-ed with the supplied "
          ':<>: 'Text "function and was not in the ignored fields list: "
          ':<>: 'ShowType orig
      )

type VerifyNotIgnored e a es = VerifyNotIgnoredGo e a es es

type family VerifyNotIgnoredGo e a es orig :: Constraint where
  VerifyNotIgnoredGo x a (x ': xs) orig =
    TypeError
      ( 'Text "Field:"
          ':$$: 'Text "  "
          ':<>: 'Text x
          ':<>: 'Text " :: "
          ':<>: 'ShowType a
          ':$$: 'Text "must be ghoist-ed but was present in the ignored "
          ':<>: 'Text "fields list: "
          ':<>: 'ShowType orig
      )

  VerifyNotIgnoredGo y a (x ': xs) orig = VerifyNotIgnoredGo y a xs orig
  VerifyNotIgnoredGo x a '[] orig = ()
