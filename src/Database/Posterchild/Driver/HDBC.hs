{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Database.Posterchild.Driver.HDBC
where

import Database.HDBC as HDBC
import Database.HDBC.PostgreSQL
import qualified Data.Text as Text
import GHC.Float (float2Double, double2Float)
import Data.Time

import Database.Posterchild.Driver.Class
import Database.Posterchild.Syntax as Syntax

newtype HDBCDriver =
  HDBCDriver
    { hdbcConn :: Connection
    }

instance DatabaseDriver HDBCDriver where
  type DriverParams HDBCDriver = [HDBC.SqlValue]
  type DriverResultRow HDBCDriver = [HDBC.SqlValue]
  driverQuery (HDBCDriver conn) = HDBC.quickQuery' conn

-- ** FromTyped

instance FromTyped (Syntax.SqlValue Syntax.SqlBooleanT) HDBC.SqlValue where
  fromTyped (Syntax.SqlBoolean b) = HDBC.SqlBool b

instance FromTyped (Syntax.SqlValue Syntax.SqlSmallIntT) HDBC.SqlValue where
  fromTyped (Syntax.SqlSmallInt i) = HDBC.SqlInt32 (fromIntegral i)

instance FromTyped (Syntax.SqlValue Syntax.SqlIntegerT) HDBC.SqlValue where
  fromTyped (Syntax.SqlInteger i) = HDBC.SqlInt32 i

instance FromTyped (Syntax.SqlValue Syntax.SqlBigIntT) HDBC.SqlValue where
  fromTyped (Syntax.SqlBigInt i) = HDBC.SqlInt64 i

instance FromTyped (Syntax.SqlValue Syntax.SqlSmallSerialT) HDBC.SqlValue where
  fromTyped (Syntax.SqlSmallSerial i) = HDBC.SqlWord32 (fromIntegral i)

instance FromTyped (Syntax.SqlValue Syntax.SqlSerialT) HDBC.SqlValue where
  fromTyped (Syntax.SqlSerial i) = HDBC.SqlWord32 i

instance FromTyped (Syntax.SqlValue Syntax.SqlBigSerialT) HDBC.SqlValue where
  fromTyped (Syntax.SqlBigSerial i) = HDBC.SqlWord64 i

instance FromTyped (Syntax.SqlValue (Syntax.SqlNumericT a b)) HDBC.SqlValue where
  fromTyped (Syntax.SqlNumeric i) = HDBC.SqlRational $ toRational i

instance FromTyped (Syntax.SqlValue Syntax.SqlMoneyT) HDBC.SqlValue where
  fromTyped (Syntax.SqlMoney i) = HDBC.SqlRational $ toRational i

instance FromTyped (Syntax.SqlValue Syntax.SqlRealT) HDBC.SqlValue where
  fromTyped (Syntax.SqlReal i) = HDBC.SqlDouble (float2Double i)

instance FromTyped (Syntax.SqlValue Syntax.SqlDoubleT) HDBC.SqlValue where
  fromTyped (Syntax.SqlDouble i) = HDBC.SqlDouble i

instance FromTyped (Syntax.SqlValue Syntax.SqlTextT) HDBC.SqlValue where
  fromTyped (Syntax.SqlText t) = HDBC.SqlString (Text.unpack t)

instance FromTyped (Syntax.SqlValue (Syntax.SqlCharT n)) HDBC.SqlValue where
  fromTyped (Syntax.SqlChar t) = HDBC.SqlString (Text.unpack t)

instance FromTyped (Syntax.SqlValue (Syntax.SqlVarCharT n)) HDBC.SqlValue where
  fromTyped (Syntax.SqlVarChar t) = HDBC.SqlString (Text.unpack t)

instance FromTyped (Syntax.SqlValue Syntax.SqlBlobT) HDBC.SqlValue where
  fromTyped (Syntax.SqlBlob b) = HDBC.SqlByteString b

instance FromTyped (Syntax.SqlValue (Syntax.SqlTimestampT p)) HDBC.SqlValue where
  fromTyped (Syntax.SqlTimestamp t) = HDBC.SqlLocalTime t

instance FromTyped (Syntax.SqlValue (Syntax.SqlTimestampWithTimeZoneT p)) HDBC.SqlValue where
  fromTyped (Syntax.SqlTimestampWithTimeZone (LocalTimeWithTimezone t z)) =
    HDBC.SqlZonedTime (ZonedTime t z)

instance FromTyped a HDBC.SqlValue =>
         FromTyped (Maybe a) HDBC.SqlValue where
  fromTyped (Just x) = fromTyped x
  fromTyped Nothing = HDBC.SqlNull

-- ** FromUntyped

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlBooleanT) where
  fromUntyped (HDBC.SqlBool b) = Right (Syntax.SqlBoolean b)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlSmallIntT) where
  fromUntyped (HDBC.SqlInt32 i) = Right (Syntax.SqlSmallInt $ fromIntegral i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlIntegerT) where
  fromUntyped (HDBC.SqlInt32 i) = Right (Syntax.SqlInteger i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlBigIntT) where
  fromUntyped (HDBC.SqlInt64 i) = Right (Syntax.SqlBigInt i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlSmallSerialT) where
  fromUntyped (HDBC.SqlWord32 i) = Right (Syntax.SqlSmallSerial $ fromIntegral i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlSerialT) where
  fromUntyped (HDBC.SqlWord32 i) = Right (Syntax.SqlSerial i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlBigSerialT) where
  fromUntyped (HDBC.SqlWord64 i) = Right (Syntax.SqlBigSerial i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue (Syntax.SqlNumericT a b)) where
  fromUntyped (HDBC.SqlRational i) = Right (Syntax.SqlNumeric $ fromRational i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlMoneyT) where
  fromUntyped (HDBC.SqlRational i) = Right (Syntax.SqlMoney $ fromRational i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlDoubleT) where
  fromUntyped (HDBC.SqlDouble i) = Right (Syntax.SqlDouble i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlRealT) where
  fromUntyped (HDBC.SqlDouble i) = Right (Syntax.SqlReal $ double2Float i)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlTextT) where
  fromUntyped (HDBC.SqlString t) = Right (Syntax.SqlText $ Text.pack t)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue (Syntax.SqlCharT n)) where
  fromUntyped (HDBC.SqlString t) = Right (Syntax.SqlChar $ Text.pack t)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue (Syntax.SqlVarCharT n)) where
  fromUntyped (HDBC.SqlString t) = Right (Syntax.SqlVarChar $ Text.pack t)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue Syntax.SqlBlobT) where
  fromUntyped (HDBC.SqlByteString b) = Right (Syntax.SqlBlob b)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue (Syntax.SqlTimestampT p)) where
  fromUntyped (HDBC.SqlLocalTime t) = Right (Syntax.SqlTimestamp t)
  fromUntyped x = Left $ "Invalid value: " ++ show x

instance FromUntyped HDBC.SqlValue (Syntax.SqlValue (Syntax.SqlTimestampWithTimeZoneT p)) where
  fromUntyped (HDBC.SqlZonedTime (ZonedTime t z)) =
    Right (Syntax.SqlTimestampWithTimeZone (LocalTimeWithTimezone t z))
  fromUntyped x = Left $ "Invalid value: " ++ show x


instance FromUntyped HDBC.SqlValue a =>
         FromUntyped HDBC.SqlValue (Maybe a) where
  fromUntyped HDBC.SqlNull = pure Nothing
  fromUntyped x = Just <$> fromUntyped x
