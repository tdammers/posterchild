{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Database.Posterchild.Driver.Class
where

import Data.HList

class DatabaseDriver driver where
  type DriverParams driver
  type DriverResultRow driver

  driverQuery :: driver -> String -> DriverParams driver -> IO [DriverResultRow driver]

typedQuery :: ( DatabaseDriver driver
              , FromTyped typedParams (DriverParams driver)
              , FromUntyped (DriverResultRow driver) typedResultRow
              )
           => driver
           -> String
           -> typedParams
           -> IO [typedResultRow]
typedQuery driver queryString typedParams =
  fromUntypedM =<< driverQuery driver queryString (fromTyped typedParams)

-- * Conversions between typed values and \"untyped\" values
-- Untyped values are represented by a unitype (such as HDBC's @SqlValue@), and
-- that unitype should generally cover all possible typed values that may be
-- converted to the unitype, hence the 'FromTyped' class does not support
-- conversion failures, whereas 'FromUntyped' does.

class FromTyped typed untyped where
  fromTyped :: typed -> untyped

class FromUntyped untyped typed where
  fromUntyped :: untyped -> Either String typed

fromUntypedM :: (MonadFail m, FromUntyped untyped typed)
             => untyped -> m typed
fromUntypedM untyped =
  case fromUntyped untyped of
    Left err -> fail err
    Right res -> return res

instance FromTyped t u => FromTyped [t] [u] where
  fromTyped = map fromTyped

instance FromUntyped u t => FromUntyped [u] [t] where
  fromUntyped = mapM fromUntyped

instance FromTyped (HList '[]) [a] where
  fromTyped _ = []

instance (FromTyped x y, FromTyped (HList xs) [y]) => FromTyped (HList (x : xs)) [y] where
  fromTyped (HCons x xs) = fromTyped x : fromTyped xs

instance FromUntyped [a] (HList '[]) where
  fromUntyped [] = return HNil
  fromUntyped _ = Left "Excessive elements at end of list"

instance (FromUntyped [a] (HList xs), FromUntyped a x) => FromUntyped [a] (HList (x : xs)) where
  fromUntyped [] = Left "Premature end of list"
  fromUntyped (x : xs) =
    HCons <$> fromUntyped x <*> fromUntyped xs
