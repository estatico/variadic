{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Variadic.Varargs where

import Data.Kind (Type)

-- | Glorified HList representing variadic arguments.
data family Varargs (l :: [Type])
data instance Varargs '[] = Nil
data instance Varargs (x ': xs) = x `Cons` Varargs xs
infixr 2 `Cons`
