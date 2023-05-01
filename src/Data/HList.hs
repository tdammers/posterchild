{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Data.HList
where

import Numeric.Natural
import GHC.TypeLits ( type (-) )
import Data.Proxy
import Data.Kind

-- * HList type

data HList a where
  HNil :: HList '[]
  HCons :: x -> HList xs -> HList (x ': xs)

-- * Type-level naturals
-- | The reason we are not using plain 'Natural' is because this leads to
-- illegal type family overlaps when distinguishing between zero and non-zero.

data HNat where
  HZero :: HNat
  HSucc :: HNat -> HNat

-- | Converting 'Nat's to 'HNat's
type family MkHNat (n :: Natural) where
  MkHNat 0 = HZero
  MkHNat n = HSucc (MkHNat (n - 1))

-- | Demoting type-level 'HNat' to term-level 'Natural'
class KnownHNat (a :: HNat) where
  hnatVal :: proxy a -> Natural

instance KnownHNat HZero where
  hnatVal _ = 0

instance KnownHNat n => KnownHNat (HSucc n) where
  hnatVal _ = succ $ hnatVal (Proxy @n)

-- * List size

class HSized a where
  type HSize a :: HNat

instance HSized (HList '[]) where
  type HSize (HList '[]) = HZero

instance HSized (HList xs) => HSized (HList (x ': xs)) where
  type HSize (HList (x ': xs)) = HSucc (HSize (HList xs))

hsize :: forall list. KnownHNat (HSize list) => list -> Natural
hsize _ = hnatVal (Proxy @(HSize list))

-- * Empty and non-empty Lists

class HEmpty a where

instance HEmpty (HList '[]) where

class HNonEmpty a where
  type HHead a :: Type
  type HTail a :: Type
  hhead :: a -> HHead a
  htail :: a -> HTail a

instance HNonEmpty (HList (x ': xs)) where
  type HHead (HList (x ': _)) = x
  type HTail (HList (_ ': xs)) = HList xs
  hhead (HCons x _) = x
  htail (HCons _ xs) = xs

class HIndexed (i :: HNat) (a :: Type) where
  type HNth i a :: Type
  type HTake' i a :: [Type]
  type HDrop' i a :: [Type]
  hnth :: forall proxy. proxy i -> a -> HNth i a
  htake :: forall proxy. proxy i -> a -> HTake i a
  hdrop :: forall proxy. proxy i -> a -> HDrop i a

type HTake i a = HList (HTake' i a)
type HDrop i a = HList (HDrop' i a)

instance HIndexed HZero (HList (x : xs)) where
  type HNth HZero (HList (x : _)) = x
  type HTake' HZero (HList _) = '[]
  type HDrop' HZero (HList (x : xs)) = x : xs

  hnth _ (HCons x _) = x
  htake _ _ = HNil
  hdrop _ xs = xs

instance HIndexed n (HList xs) => HIndexed (HSucc n) (HList (x : xs)) where
  type HNth (HSucc n) (HList (_ : xs)) = HNth n (HList xs)
  type HTake' (HSucc n) (HList (x : xs)) = x : HTake' n (HList xs)
  type HDrop' (HSucc n) (HList (x : xs)) = HDrop' n (HList xs)

  hnth _ (HCons _ xs) = hnth (Proxy @n) xs
  htake _ (HCons x xs) = HCons x (htake (Proxy @n) xs)
  hdrop _ (HCons _ xs) = hdrop (Proxy @n) xs
