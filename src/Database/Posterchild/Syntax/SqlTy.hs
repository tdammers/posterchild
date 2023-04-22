{-# LANGUAGE DerivingStrategies #-}

module Database.Posterchild.Syntax.SqlTy
where

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
  | SqlNumericT Int Int
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
  | SqlVarCharT Int
    -- ^ Variable-length with limit
  | SqlCharT Int
    -- ^ Fixed-length, blank-padded
  | SqlTextT
    -- ^ Variable-length, unlimited

  -- Binary types
  | SqlBlobT
    -- ^ Variable-length, unlimited (@bytea@ in PostgreSql)

  -- Date/time types
  | SqlTimestampT Int
    -- ^ Date+time without time zone, with precision (0-6)
  | SqlTimestampWithTimeZoneT Int
    -- ^ Date+time with time zone, with precision (0-6)
  | SqlDateT
    -- ^ Date only (no time of day)
  | SqlTimeT Int
    -- ^ Time of day (no date) without time zone, with precision (0-6)
  | SqlTimeWithTimeZoneT Int
    -- ^ Time of day (no date) with time zone, with precision (0-6)
  | SqlIntervalT SqlIntervalFields Int
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
  | SqlBitT Int
    -- ^ fixed-length
  | SqlBitVaryingT (Maybe Int)
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
  | SqlArrayT SqlTy (Maybe Int)

  -- Unknown type
  | SqlAnyT
  deriving (Show, Read, Eq, Ord)

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
SqlSerialT `isSubtypeOf` SqlBigSerialT = True

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


