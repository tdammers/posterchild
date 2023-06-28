{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.Posterchild.Syntax.SqlTy
where

import Data.BitArray
import Data.ByteString (ByteString)
import Data.Decimal
import Data.Int
import Data.Kind
import Data.Text (Text)
import Data.Time
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.Word
import Numeric.Natural
import GHC.Float (float2Double)
import GHC.TypeLits

data SqlIntervalFields
  = AllIntervalFields
  | YearIntervalField
  | MonthIntervalField
  | DayIntervalField
  | HourIntervalField
  | MinuteIntervalField
  | SecondIntervalField
  | YearToMonthIntervalFields
  | DayToHourIntervalFields
  | DayToMinuteIntervalFields
  | DayToSecondIntervalFields
  | HourToMinuteIntervalFields
  | HourToSecondIntervalFields
  | MinuteToSecondIntervalFields
  deriving (Show, Read, Eq, Ord)

data SqlTy
  -- Numeric types
  = SqlSmallIntT
    -- ^ 2-byte int
  | SqlIntegerT
    -- ^ 4-byte int
  | SqlBigIntT
    -- ^ 8-byte int
  | SqlNumericT Natural Natural
    -- ^ numeric, a.k.a. decimal, with given precision and scale
  | SqlRealT
    -- ^ 4-byte float
  | SqlDoubleT
    -- ^ 8-byte float
  | SqlSmallSerialT
    -- ^ 2-byte autoincrementing int
  | SqlSerialT
    -- ^ 4-byte autoincrementing int
  | SqlBigSerialT
    -- ^ 8-byte autoincrementing int
  | SqlMoneyT
    -- ^ 8-byte exact monetary value, locale-defined precision and scale

  -- Character types
  | SqlVarCharT Natural
    -- ^ Variable-length with limit
  | SqlCharT Natural
    -- ^ Fixed-length, blank-padded
  | SqlTextT
    -- ^ Variable-length, unlimited

  -- Binary types
  | SqlBlobT
    -- ^ Variable-length, unlimited (@bytea@ in PostgreSql)

  -- Date/time types
  | SqlTimestampT Natural
    -- ^ Date+time without time zone, with precision (0-6)
  | SqlTimestampWithTimeZoneT Natural
    -- ^ Date+time with time zone, with precision (0-6)
  | SqlDateT
    -- ^ Date only (no time of day)
  | SqlTimeT Natural
    -- ^ Time of day (no date) without time zone, with precision (0-6)
  | SqlTimeWithTimeZoneT Natural
    -- ^ Time of day (no date) with time zone, with precision (0-6)
  | SqlIntervalT SqlIntervalFields Natural
    -- ^ Interval, with optional restriction on stored fields and precision (0-6)

  -- Boolean type
  | SqlBooleanT

  -- Enum types
  | SqlNamedEnumT
    -- ^ Enum type must be defined separately using @CREATE TYPE@

  -- Geometric types
  | SqlPointT
  | SqlLineT
  | SqlLSegT
  | SqlBoxT
  | SqlPathT
  | SqlPolygonT
  | SqlCircleT

  -- Network address types
  | SqlCIDRT
    -- ^ IPv4 and IPv8 networks
  | SqlINETT
    -- ^ IPv4 and IPv8 hosts and networks
  | SqlMacAddrT
    -- ^ 6-byte MAC address
  | SqlMacAddr8T
    -- ^ 8-byte MAC address

  -- Bit string types
  | SqlBitT Natural
    -- ^ fixed-length
  | SqlBitVaryingT (Maybe Natural)
    -- ^ variable-length with optional limit

  -- Text search types
  | SqlTextSearchVectorT
  | SqlTextSearchQueryT

  -- UUID type
  | SqlUUIDT
    -- ^ 128-bit UUID (a.k.a. GUID)

  -- XML type
  | SqlXMLT

  -- JSON types
  | SqlJSONT
    -- ^ JSON source stored as text
  | SqlJSONBT
    -- ^ parsed JSON stored as an efficient binary representation

  -- Arrays
  | SqlArrayT SqlTy (Maybe Natural)

  -- Unknown type
  | SqlAnyT
  deriving (Show, Read, Eq, Ord)

-- * Value type

data SqlValue (t :: SqlTy) where
  SqlSmallInt :: !Int16 -> SqlValue SqlSmallIntT
  SqlInteger :: !Int32 -> SqlValue SqlIntegerT
  SqlBigInt :: !Int64 -> SqlValue SqlBigIntT
  SqlNumeric :: forall p s. !Decimal -> SqlValue (SqlNumericT p s)
  SqlReal :: !Float -> SqlValue SqlRealT
  SqlDouble :: !Double -> SqlValue SqlDoubleT
  SqlSmallSerial :: !Word16 -> SqlValue SqlSmallSerialT
  SqlSerial :: !Word32 -> SqlValue SqlSerialT
  SqlBigSerial :: !Word64 -> SqlValue SqlBigSerialT
  SqlMoney :: !Decimal -> SqlValue SqlMoneyT
  SqlVarChar :: forall l. !Text -> SqlValue (SqlVarCharT (l :: Natural))
  SqlChar :: forall l. !Text -> SqlValue (SqlCharT (l :: Natural))
  SqlText :: !Text -> SqlValue SqlTextT
  SqlBlob :: !ByteString -> SqlValue SqlBlobT
  SqlTimestamp :: forall p. !LocalTime -> SqlValue (SqlTimestampT (p :: Natural))
  SqlTimestampWithTimeZone :: forall p. !LocalTimeWithTimezone -> SqlValue (SqlTimestampWithTimeZoneT (p :: Natural))
  SqlDate :: !Day -> SqlValue SqlDateT
  SqlTime :: forall p. !TimeOfDay -> SqlValue (SqlTimeT (p :: Natural))
  SqlTimeWithTimeZone :: forall p. !TimeOfDayWithTimezone -> SqlValue (SqlTimeWithTimeZoneT (p :: Natural))
  SqlInterval :: forall fields p. SqlValue (SqlIntervalT (fields :: SqlIntervalFields) (p :: Natural))
  SqlBoolean :: !Bool -> SqlValue SqlBooleanT
  SqlNamedEnum :: !Int -> SqlValue SqlNamedEnumT

  SqlBit :: forall l. !BitArray -> SqlValue (SqlBitT (l :: Natural))
  SqlBitVarying :: forall l. !BitArray -> SqlValue (SqlBitVaryingT (l :: Maybe Natural))

  -- TODO: deal with these
  -- SqlPoint :: -> SqlValue SqlPointT
  -- SqlLine :: -> SqlValue SqlLineT
  -- SqlLSeg :: -> SqlValue SqlLSegT
  -- SqlBox :: -> SqlValue SqlBoxT
  -- SqlPath :: -> SqlValue SqlPathT
  -- SqlPolygon :: -> SqlValue SqlPolygonT
  -- SqlCircle :: -> SqlValue SqlCircleT
  -- SqlCIDR :: -> SqlValue SqlCIDRT
  -- SqlINET :: -> SqlValue SqlINETT
  -- SqlMacAddr :: -> SqlValue SqlMacAddrT
  -- SqlMacAddr8 :: -> SqlValue SqlMacAddr8T
  -- SqlTextSearchVector :: -> SqlValue SqlTextSearchVectorT
  -- SqlTextSearchQuery :: -> SqlValue SqlTextSearchQueryT
  -- SqlUUID :: -> SqlValue SqlUUIDT
  -- SqlXML :: -> SqlValue SqlXMLT
  -- SqlJSON :: -> SqlValue SqlJSONT
  -- SqlJSONB :: -> SqlValue SqlJSONBT

  SqlArray :: forall (a :: SqlTy) l. !(Vector (SqlValue a)) -> SqlValue (SqlArrayT a (l :: Maybe Natural))

  -- TODO: can we represent this?
  -- SqlAny :: ? -> SqlValue SqlAnyT

data LocalTimeWithTimezone =
  LocalTimeWithTimezone
    { localTimeWithoutTimeZone :: !LocalTime
    , localTimeTimeZone :: !TimeZone
    }
    deriving (Show, Eq)

data TimeOfDayWithTimezone =
  TimeOfDayWithTimezone
    { timeOfDayWithoutTimeZone :: !TimeOfDay
    , timeOfDayTimeZone :: !TimeZone
    }
    deriving (Show, Eq)

deriving instance Show (SqlValue (a :: SqlTy))
deriving instance Eq (SqlValue (a :: SqlTy))

-- * Subtypes

isSubtypeOf :: SqlTy -> SqlTy -> Bool
a `isSubtypeOf` b | a == b = True
_ `isSubtypeOf` SqlAnyT = True

SqlBooleanT `isSubtypeOf` SqlSmallIntT = True
SqlBooleanT `isSubtypeOf` SqlIntegerT = True
SqlBooleanT `isSubtypeOf` SqlBigIntT = True

SqlSmallIntT `isSubtypeOf` SqlIntegerT = True
SqlSmallIntT `isSubtypeOf` SqlBigIntT = True
SqlIntegerT `isSubtypeOf` SqlBigIntT = True

SqlRealT `isSubtypeOf` SqlDoubleT = True

SqlSmallSerialT `isSubtypeOf` SqlSerialT = True
SqlSmallSerialT `isSubtypeOf` SqlBigSerialT = True
SqlSerialT `isSubtypeOf` SqlBigSerialT = True

SqlBooleanT `isSubtypeOf` SqlNumericT p s =
  p - s >= 1
SqlSmallIntT `isSubtypeOf` SqlNumericT p s =
  p - s >= 5
SqlIntegerT `isSubtypeOf` SqlNumericT p s =
  p - s >= 10
SqlBigIntT `isSubtypeOf` SqlNumericT p s =
  p - s >= 19
SqlSmallSerialT `isSubtypeOf` SqlNumericT p s =
  p - s >= 5
SqlSerialT `isSubtypeOf` SqlNumericT p s =
  p - s >= 10
SqlBigSerialT `isSubtypeOf` SqlNumericT p s =
  p - s >= 19
SqlRealT `isSubtypeOf` SqlNumericT p s =
  p >= 40 && s >= 20
SqlDoubleT `isSubtypeOf` SqlNumericT p s =
  p >= 616 && s >= 308
SqlNumericT p s `isSubtypeOf` SqlNumericT q t =
  p <= q && s <= t

SqlVarCharT _ `isSubtypeOf` SqlTextT = True
SqlCharT _ `isSubtypeOf` SqlTextT = True
SqlVarCharT s `isSubtypeOf` SqlVarCharT t =
  s <= t
SqlCharT s `isSubtypeOf` SqlCharT t =
  s <= t
SqlVarCharT s `isSubtypeOf` SqlCharT t =
  s <= t
SqlCharT s `isSubtypeOf` SqlVarCharT t =
  s <= t

SqlBitT s `isSubtypeOf` SqlBitT t =
  s <= t
SqlBitVaryingT (Just s) `isSubtypeOf` SqlBitVaryingT (Just t) =
  s <= t
SqlBitVaryingT _ `isSubtypeOf` SqlBitVaryingT Nothing =
  True
SqlBitT s `isSubtypeOf` SqlBitVaryingT (Just t) =
  s <= t
SqlBitT _ `isSubtypeOf` SqlBitVaryingT Nothing =
  True

SqlArrayT e _ `isSubtypeOf` SqlArrayT f Nothing =
  e `isSubtypeOf` f
SqlArrayT e (Just s) `isSubtypeOf` SqlArrayT f (Just t) =
  (e `isSubtypeOf` f) && (s <= t)

_ `isSubtypeOf` _ = False


class IsSubtypeOf (c :: Type) (p :: Type) where
  upcast :: c -> p

instance IsSubtypeOf a a where
  upcast = id

instance IsSubtypeOf (SqlValue SqlBooleanT) (SqlValue SqlSmallIntT) where
  upcast (SqlBoolean v) = SqlSmallInt (fromIntegral $ fromEnum v)
instance IsSubtypeOf (SqlValue SqlBooleanT) (SqlValue SqlIntegerT) where
  upcast (SqlBoolean v) = SqlInteger (fromIntegral $ fromEnum v)
instance IsSubtypeOf (SqlValue SqlBooleanT) (SqlValue SqlBigIntT) where
  upcast (SqlBoolean v) = SqlBigInt (fromIntegral $ fromEnum v)
instance IsSubtypeOf (SqlValue SqlBooleanT) (SqlValue SqlSerialT) where
  upcast (SqlBoolean v) = SqlSerial (fromIntegral $ fromEnum v)
instance IsSubtypeOf (SqlValue SqlBooleanT) (SqlValue SqlBigSerialT) where
  upcast (SqlBoolean v) = SqlBigSerial (fromIntegral $ fromEnum v)

instance IsSubtypeOf (SqlValue SqlSmallIntT) (SqlValue SqlIntegerT) where
  upcast (SqlSmallInt v) = SqlInteger (fromIntegral v)
instance IsSubtypeOf (SqlValue SqlSmallIntT) (SqlValue SqlBigIntT) where
  upcast (SqlSmallInt v) = SqlBigInt (fromIntegral v)

instance IsSubtypeOf (SqlValue SqlIntegerT) (SqlValue SqlBigIntT) where
  upcast (SqlInteger v) = SqlBigInt (fromIntegral v)

instance IsSubtypeOf (SqlValue SqlRealT) (SqlValue SqlDoubleT) where
  upcast (SqlReal v) = SqlDouble (float2Double v)

instance IsSubtypeOf (SqlValue SqlSmallSerialT) (SqlValue SqlSerialT) where
  upcast (SqlSmallSerial v) = SqlSerial (fromIntegral v)
instance IsSubtypeOf (SqlValue SqlSmallSerialT) (SqlValue SqlBigSerialT) where
  upcast (SqlSmallSerial v) = SqlBigSerial (fromIntegral v)

instance IsSubtypeOf (SqlValue SqlSerialT) (SqlValue SqlBigSerialT) where
  upcast (SqlSerial v) = SqlBigSerial (fromIntegral v)

instance (p <= q, s <= t) =>
          IsSubtypeOf
            (SqlValue (SqlNumericT p s))
            (SqlValue (SqlNumericT q t))
        where
  upcast (SqlNumeric n) = SqlNumeric n

instance IsSubtypeOf
          (SqlValue (SqlVarCharT a))
          (SqlValue SqlTextT)
        where
    upcast (SqlVarChar v) = SqlText v
instance IsSubtypeOf
          (SqlValue (SqlCharT a))
          (SqlValue SqlTextT)
        where
    upcast (SqlChar v) = SqlText v
instance (s <= t) =>
          IsSubtypeOf
            (SqlValue (SqlVarCharT s))
            (SqlValue (SqlCharT t))
        where
    upcast (SqlVarChar v) = SqlChar v
instance (s <= t) =>
          IsSubtypeOf
            (SqlValue (SqlCharT t))
            (SqlValue (SqlVarCharT s))
        where
    upcast (SqlChar v) = SqlVarChar v

instance (s <= t) =>
          IsSubtypeOf
            (SqlValue (SqlBitT s))
            (SqlValue (SqlBitT t))
        where
    upcast (SqlBit v) = SqlBit v

instance (s <= t) =>
          IsSubtypeOf
            (SqlValue (SqlBitVaryingT ('Just s)))
            (SqlValue (SqlBitVaryingT ('Just t)))
        where
    upcast (SqlBitVarying v) = SqlBitVarying v

instance IsSubtypeOf
            (SqlValue (SqlBitVaryingT smay))
            (SqlValue (SqlBitVaryingT 'Nothing))
        where
    upcast (SqlBitVarying v) = SqlBitVarying v

instance (s <= t) =>
          IsSubtypeOf
            (SqlValue (SqlBitT s))
            (SqlValue (SqlBitVaryingT ('Just t)))
        where
    upcast (SqlBit v) = SqlBitVarying v

instance IsSubtypeOf
            (SqlValue (SqlBitT s))
            (SqlValue (SqlBitVaryingT 'Nothing))
        where
    upcast (SqlBit v) = SqlBitVarying v

instance (s <= t, SqlValue a `IsSubtypeOf` SqlValue b) =>
          IsSubtypeOf
            (SqlValue (SqlArrayT a ('Just s)))
            (SqlValue (SqlArrayT b ('Just t)))
        where
    upcast (SqlArray v) = SqlArray (Vector.map upcast v)

instance (SqlValue a `IsSubtypeOf` SqlValue b) =>
          IsSubtypeOf
            (SqlValue (SqlArrayT a smay))
            (SqlValue (SqlArrayT b 'Nothing))
        where
    upcast (SqlArray v) = SqlArray (Vector.map upcast v)

-- * Comparisons

-- | Heterogenous 'Eq'.
class HEq (a :: Type) (b :: Type) where
  heq :: a -> b -> Bool

instance Eq a => HEq a a where
  heq = (==)

-- | Helper for implementing 'heq' for types that are subtypes of one another.
heqViaUpcast :: forall a b.
                     ( HEq b b
                     , a `IsSubtypeOf` b
                     )
                  => a -> b -> Bool
heqViaUpcast a = heq (upcast a :: b)

instance HEq (SqlValue SqlBooleanT) (SqlValue SqlSmallIntT) where
  heq = heqViaUpcast
instance HEq (SqlValue SqlSmallIntT) (SqlValue SqlBooleanT) where
  heq = flip heqViaUpcast
instance HEq (SqlValue SqlBooleanT) (SqlValue SqlIntegerT) where
  heq = heqViaUpcast
instance HEq (SqlValue SqlIntegerT) (SqlValue SqlBooleanT) where
  heq = flip heqViaUpcast
instance HEq (SqlValue SqlBooleanT) (SqlValue SqlBigIntT) where
  heq = heqViaUpcast
instance HEq (SqlValue SqlBigIntT) (SqlValue SqlBooleanT) where
  heq = flip heqViaUpcast
instance HEq (SqlValue SqlSmallIntT) (SqlValue SqlIntegerT) where
  heq = heqViaUpcast
instance HEq (SqlValue SqlIntegerT) (SqlValue SqlSmallIntT) where
  heq = flip heqViaUpcast
instance HEq (SqlValue SqlSmallIntT) (SqlValue SqlBigIntT) where
  heq = heqViaUpcast
instance HEq (SqlValue SqlBigIntT) (SqlValue SqlSmallIntT) where
  heq = flip heqViaUpcast
instance HEq (SqlValue SqlIntegerT) (SqlValue SqlBigIntT) where
  heq = heqViaUpcast
instance HEq (SqlValue SqlBigIntT) (SqlValue SqlIntegerT) where
  heq = flip heqViaUpcast

-- TODO: Draw the rest of the fucking owl.

-- | Heterogenous 'Ord'.
class HCompare (a :: Type) (b :: Type) where
  hcompare :: a -> b -> Ordering

-- | Helper for implementing 'hcompare' for types that are subtypes of one
-- another.
hcompareViaUpcast :: forall a b.
                     ( HCompare b b
                     , a `IsSubtypeOf` b
                     )
                  => a -> b -> Ordering
hcompareViaUpcast a = hcompare (upcast a :: b)

flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering GT = LT
flipOrdering EQ = EQ

flipOrd :: (a -> b -> Ordering) -> b -> a -> Ordering
flipOrd f a b = flipOrdering $ f b a

instance HCompare (SqlValue SqlBooleanT) (SqlValue SqlBooleanT) where
  hcompare (SqlBoolean a) (SqlBoolean b) = compare a b
instance HCompare (SqlValue SqlSmallIntT) (SqlValue SqlSmallIntT) where
  hcompare (SqlSmallInt a) (SqlSmallInt b) = compare a b
instance HCompare (SqlValue SqlIntegerT) (SqlValue SqlIntegerT) where
  hcompare (SqlInteger a) (SqlInteger b) = compare a b
instance HCompare (SqlValue SqlBigIntT) (SqlValue SqlBigIntT) where
  hcompare (SqlBigInt a) (SqlBigInt b) = compare a b

instance HCompare (SqlValue SqlBooleanT) (SqlValue SqlSmallIntT) where
  hcompare = hcompareViaUpcast
instance HCompare (SqlValue SqlSmallIntT) (SqlValue SqlBooleanT) where
  hcompare = flipOrd hcompareViaUpcast
instance HCompare (SqlValue SqlBooleanT) (SqlValue SqlIntegerT) where
  hcompare = hcompareViaUpcast
instance HCompare (SqlValue SqlIntegerT) (SqlValue SqlBooleanT) where
  hcompare = flipOrd hcompareViaUpcast
instance HCompare (SqlValue SqlBooleanT) (SqlValue SqlBigIntT) where
  hcompare = hcompareViaUpcast
instance HCompare (SqlValue SqlBigIntT) (SqlValue SqlBooleanT) where
  hcompare = flipOrd hcompareViaUpcast
instance HCompare (SqlValue SqlSmallIntT) (SqlValue SqlIntegerT) where
  hcompare = hcompareViaUpcast
instance HCompare (SqlValue SqlIntegerT) (SqlValue SqlSmallIntT) where
  hcompare = flipOrd hcompareViaUpcast
instance HCompare (SqlValue SqlSmallIntT) (SqlValue SqlBigIntT) where
  hcompare = hcompareViaUpcast
instance HCompare (SqlValue SqlBigIntT) (SqlValue SqlSmallIntT) where
  hcompare = flipOrd hcompareViaUpcast
instance HCompare (SqlValue SqlIntegerT) (SqlValue SqlBigIntT) where
  hcompare = hcompareViaUpcast
instance HCompare (SqlValue SqlBigIntT) (SqlValue SqlIntegerT) where
  hcompare = flipOrd hcompareViaUpcast

-- TODO: Draw the rest of the fucking owl.
