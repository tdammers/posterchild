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

instance FromTyped (Syntax.SqlValue Syntax.SqlTextT) HDBC.SqlValue where
  fromTyped (Syntax.SqlText t) = HDBC.SqlString (Text.unpack t)

instance FromTyped (Syntax.SqlValue (Syntax.SqlCharT n)) HDBC.SqlValue where
  fromTyped (Syntax.SqlChar t) = HDBC.SqlString (Text.unpack t)

instance FromTyped (Syntax.SqlValue (Syntax.SqlVarCharT n)) HDBC.SqlValue where
  fromTyped (Syntax.SqlVarChar t) = HDBC.SqlString (Text.unpack t)

instance FromTyped (Syntax.SqlValue Syntax.SqlBlobT) HDBC.SqlValue where
  fromTyped (Syntax.SqlBlob b) = HDBC.SqlByteString b

instance FromTyped a HDBC.SqlValue =>
         FromTyped (Maybe a) HDBC.SqlValue where
  fromTyped (Just x) = fromTyped x
  fromTyped Nothing = HDBC.SqlNull

instance FromUntyped HDBC.SqlValue a =>
         FromUntyped HDBC.SqlValue (Maybe a) where
  fromUntyped HDBC.SqlNull = pure Nothing
  fromUntyped x = Just <$> fromUntyped x
