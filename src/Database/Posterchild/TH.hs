{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Database.Posterchild.TH
where

import Database.Posterchild.Parser.Select
import Database.Posterchild.SchemaConstraints
import Database.Posterchild.Syntax
import Database.Posterchild.TyCheck
import Database.Posterchild.Driver.Class
import Data.HList
import GHC.Stack (HasCallStack)

import qualified Data.Map as Map
import qualified Data.Text as Text
import Language.Haskell.TH
import Text.Casing (pascal)
import Numeric.Natural
import Data.Proxy

tableNameLit :: TableName -> Q Type
tableNameLit (TableName tname) = litT (strTyLit (Text.unpack tname))

columnNameLit :: ColumnName -> Q Type
columnNameLit (ColumnName cname) = litT (strTyLit (Text.unpack cname))

paramName :: ParamName -> Name
paramName (ParamName pname) =
  mkName ("p" ++ Text.unpack pname)

paramNameLit :: ParamName -> Q Type
paramNameLit pname =
  varT $ paramName pname

tyToType :: Name -> Ty -> Q Type
tyToType _ NullTy =
  varT '()
tyToType _ (MonoTy sqlTy) =
  conT ''SqlValue `appT` sqlTyToType sqlTy
tyToType sname (ColumnRefTy tname cname) =
  conT ''TableColumnTy
    `appT` (conT ''SchemaTableTy `appT` varT sname `appT` tableNameLit tname)
    `appT` columnNameLit cname
tyToType _ (ParamRefTy pname) =
  paramNameLit pname

sqlTyToType :: SqlTy -> Q Type
sqlTyToType SqlSmallIntT = conT 'SqlSmallIntT
sqlTyToType SqlIntegerT = conT 'SqlIntegerT
sqlTyToType SqlBigIntT = conT 'SqlBigIntT
sqlTyToType (SqlNumericT p s) = conT 'SqlNumericT `appT` litT (numTyLit $ fromIntegral p) `appT` litT (numTyLit $ fromIntegral s)
sqlTyToType SqlRealT = conT 'SqlRealT 
sqlTyToType SqlDoubleT = conT 'SqlDoubleT
sqlTyToType SqlSmallSerialT = conT 'SqlSmallSerialT
sqlTyToType SqlSerialT = conT 'SqlSerialT
sqlTyToType SqlBigSerialT = conT 'SqlBigSerialT
sqlTyToType SqlMoneyT = conT 'SqlMoneyT
sqlTyToType _ = error "Not implemented"

-- sqlTyToType (SqlVarChar :: forall l. !Text -> SqlValue (SqlVarCharT (l :: Natural))
-- sqlTyToType (SqlChar :: forall l. !Text -> SqlValue (SqlCharT (l :: Natural))
-- sqlTyToType (SqlText :: !Text -> SqlValue SqlTextT
-- sqlTyToType (SqlBlob :: !ByteString -> SqlValue SqlBlobT
-- sqlTyToType (SqlTimestamp :: forall p. !LocalTime -> SqlValue (SqlTimestampT (p :: Natural))
-- sqlTyToType (SqlTimestampWithTimeZone :: forall p. !LocalTimeWithTimezone -> SqlValue (SqlTimestampWithTimeZoneT (p :: Natural))
-- sqlTyToType (SqlDate :: !Day -> SqlValue SqlDateT
-- sqlTyToType (SqlTime :: forall p. !TimeOfDay -> SqlValue (SqlTimeT (p :: Natural))
-- sqlTyToType (SqlTimeWithTimeZone :: forall p. !TimeOfDayWithTimezone -> SqlValue (SqlTimeWithTimeZoneT (p :: Natural))
-- sqlTyToType (SqlInterval :: forall fields p. SqlValue (SqlIntervalT (fields :: SqlIntervalFields) (p :: Natural))
-- sqlTyToType (SqlBoolean :: !Bool -> SqlValue SqlBooleanT
-- sqlTyToType (SqlNamedEnum :: !Int -> SqlValue SqlNamedEnumT
-- sqlTyToType (
-- sqlTyToType (SqlBit :: forall l. !BitArray -> SqlValue (SqlBitT (l :: Natural))
-- sqlTyToType (SqlBitVarying :: forall l. !BitArray -> SqlValue (SqlBitVaryingT (l :: Maybe Natural))
-- sqlTyToType (
-- sqlTyToType (SqlArray :: forall (a :: SqlTy) l. !(Vector (SqlValue a)) -> SqlValue (SqlArrayT a (l :: Maybe Natural))
-- sqlTyToType (
-- sqlTyToType (-- TODO: deal with these
-- sqlTyToType (-- SqlPoint :: -> SqlValue SqlPointT
-- sqlTyToType (-- SqlLine :: -> SqlValue SqlLineT
-- sqlTyToType (-- SqlLSeg :: -> SqlValue SqlLSegT
-- sqlTyToType (-- SqlBox :: -> SqlValue SqlBoxT
-- sqlTyToType (-- SqlPath :: -> SqlValue SqlPathT
-- sqlTyToType (-- SqlPolygon :: -> SqlValue SqlPolygonT
-- sqlTyToType (-- SqlCircle :: -> SqlValue SqlCircleT
-- sqlTyToType (-- SqlCIDR :: -> SqlValue SqlCIDRT
-- sqlTyToType (-- SqlINET :: -> SqlValue SqlINETT
-- sqlTyToType (-- SqlMacAddr :: -> SqlValue SqlMacAddrT
-- sqlTyToType (-- SqlMacAddr8 :: -> SqlValue SqlMacAddr8T
-- sqlTyToType (-- SqlTextSearchVector :: -> SqlValue SqlTextSearchVectorT
-- sqlTyToType (-- SqlTextSearchQuery :: -> SqlValue SqlTextSearchQueryT
-- sqlTyToType (-- SqlUUID :: -> SqlValue SqlUUIDT
-- sqlTyToType (-- SqlXML :: -> SqlValue SqlXMLT
-- sqlTyToType (-- SqlJSON :: -> SqlValue SqlJSONT
-- sqlTyToType (-- SqlJSONB :: -> SqlValue SqlJSONBT
-- sqlTyToType (
-- sqlTyToType (-- TODO: can we represent this?
-- sqlTyToType (-- SqlAny :: ? -> SqlValue SqlAnyT


mkQueryConstraint :: Name -> QueryConstraint -> Q [Type]
mkQueryConstraint sname (TableExists tname) =
  sequence
    [ conT ''SchemaHasTable `appT` varT sname `appT` tableNameLit tname
    ]
mkQueryConstraint sname (ColumnExists (ColumnRef tname cname)) =
  sequence
    [ conT ''TableHasColumn
        `appT`
          ( conT ''SchemaTableTy
            `appT` varT sname
            `appT` tableNameLit tname
          )
        `appT` columnNameLit cname
    ]
mkQueryConstraint sname (SubtypeOf a b) =
  sequence
    [ conT ''IsSubtypeOf
        `appT` (tyToType sname a)
        `appT` (tyToType sname b)
    ]
mkQueryConstraint sname (EqTypes a b) =
  sequence
    [ conT ''HEq
        `appT` (tyToType sname a)
        `appT` (tyToType sname b)
    ]
mkQueryConstraint sname (ComparableTypes a b) =
  sequence
    [ conT ''HCompare
        `appT` (tyToType sname a)
        `appT` (tyToType sname b)
    ]

{- HLINT "ignore Move brackets" -}
mkQueryTyDec :: Name -> Name -> SelectQueryTy -> DecQ
mkQueryTyDec n sname sqt = do
  sigD n $ mkQueryTy sname sqt

mkQueryTy :: Name -> SelectQueryTy -> TypeQ
mkQueryTy sname sqt = do
  let paramNames = map (paramName . fst) $ selectQueryParamsTy sqt
  let dname = mkName "driver"
  let paramsT = mkQueryParamsT $ selectQueryParamsTy sqt
      resultRowT = mkQueryResultRowT sname $ selectQueryResultTy sqt
      resultsT = mkQueryResultsT sname $ selectQueryResultTy sqt
  forallT [PlainTV fn SpecifiedSpec | fn <- dname : sname : paramNames ]
    (concat <$> mapM (mkQueryConstraint sname) (selectQueryConstraintsTy sqt))
    [t|
      DatabaseDriver $(varT dname)
      => FromTyped $(paramsT) (DriverParams $(varT dname))
      => FromUntyped (DriverResultRow $(varT dname)) $(resultRowT)
      => Proxy $(varT sname)
      -> $(varT dname)
      -> $(paramsT)
      -> $(resultsT)
      |]

mkHListT :: ((a, Ty) -> Q Type) -> [(a, Ty)] -> Q Type
mkHListT _ [] = conT '[]
mkHListT mkElemT (t : xs) =
  conT '(:)
    `appT` (mkElemT t)
    `appT` mkHListT mkElemT xs

mkQueryParamsT :: [(ParamName, Ty)] -> Q Type
mkQueryParamsT xs = conT ''HList `appT` mkHListT mkQueryParamT xs

mkQueryParamT :: (ParamName, Ty) -> Q Type
mkQueryParamT (n, _) = paramNameLit n

mkQueryResultsT :: Name -> [(ColumnName, Ty)] -> Q Type
mkQueryResultsT sname columns =
  [t| IO [ $(mkQueryResultRowT sname columns) ] |]

mkQueryResultRowT :: Name -> [(ColumnName, Ty)] -> Q Type
mkQueryResultRowT sname xs =
  conT ''HList `appT` mkHListT (tyToType sname . snd) xs

mkSelectQueryBodyExp :: String -> ExpQ
mkSelectQueryBodyExp queryString =
  [e| \_ driver -> typedQuery driver $(litE $ StringL queryString)
    |]

mkSelectQueryDec :: HasCallStack => String -> String -> Q [Dec]
mkSelectQueryDec fnameStr queryString = do
  sq <- parseSelect "<<TH>>" queryString
  sqt <- either (error . show) return $ runTC $ tcSelectQuery sq
  let fname = mkName fnameStr
  sname <- newName "schema"
  sequence
    [ mkQueryTyDec fname sname sqt
    , valD (varP fname) (normalB $ mkSelectQueryBodyExp queryString) []
    ]

schemaTyName :: Schema -> Name
schemaTyName schema =
  mkName $ pascal (Text.unpack . schemaNameText . schemaName $ schema)

mkSchema :: Schema -> Q [Dec]
mkSchema schema = do
  let schemaN = schemaTyName schema
  schemaPhantomDec <- dataD (pure []) schemaN [] Nothing [normalC schemaN []] []
  tablePhantomDecs <- mkSchemaTablePhantoms schema
  return $ schemaPhantomDec : tablePhantomDecs

mkSchemaTablePhantoms :: Schema -> Q [Dec]
mkSchemaTablePhantoms schema =
  concat <$> mapM (\(tableName, table) -> mkTable schema tableName table) (Map.toList $ schemaTables schema)

mkTable :: Schema -> TableName -> Table -> Q [Dec]
mkTable schema tableName table = do
  let schemaN = schemaTyName schema
  let tableN = mkName $ pascal (Text.unpack . tableNameText $ tableName)
  let tableTySym = litT . strTyLit . Text.unpack . tableNameText $ tableName
  tablePhantomDec <- dataD (pure []) tableN [] Nothing [normalC tableN []] []
  schemaHasTableDec <- [d|
    instance SchemaHasTable $(conT schemaN) $(tableTySym) where
      type SchemaTableTy $(conT schemaN) $(tableTySym) = $(conT tableN)
    |]
  tableColumnDecs <- concat <$> mapM (mkTableColumnDec tableN) (tableColumns table)
  return $ tablePhantomDec : (schemaHasTableDec ++ tableColumnDecs)

mkTableColumnDec :: Name -> Column -> Q [Dec]
mkTableColumnDec tableN column = do
  let columnTySym = litT . strTyLit . Text.unpack . columnNameText $ columnName column
  [d| instance TableHasColumn $(conT tableN) $(columnTySym) where
        type TableColumnTy $(conT tableN) $(columnTySym) =
          SqlValue $( mkSqlTy (columnType column) )
    |]

liftNat :: Natural -> Q Type
liftNat n = litT . numTyLit . fromIntegral $ n

liftMaybe :: (a -> Q Type) -> Maybe a -> Q Type
liftMaybe _ Nothing = conT 'Nothing
liftMaybe f (Just x) = conT 'Just `appT` f x

liftMaybeNat :: Maybe Natural -> Q Type
liftMaybeNat = liftMaybe liftNat

mkSqlTy :: SqlTy -> Q Type
mkSqlTy SqlAnyT = conT 'SqlAnyT
mkSqlTy (SqlArrayT t ml) = conT 'SqlArrayT `appT` mkSqlTy t `appT` liftMaybeNat ml
mkSqlTy SqlBigIntT = conT 'SqlBigIntT
mkSqlTy SqlBigSerialT = conT 'SqlBigSerialT 
mkSqlTy (SqlBitT n) = conT 'SqlBitT `appT` liftNat n
mkSqlTy (SqlBitVaryingT ml) = conT 'SqlBitVaryingT `appT` liftMaybeNat ml
mkSqlTy SqlBlobT = conT 'SqlBlobT 
mkSqlTy SqlBooleanT = conT 'SqlBooleanT 
mkSqlTy SqlBoxT = conT 'SqlBoxT 
mkSqlTy (SqlCharT s) = conT 'SqlCharT `appT` liftNat s
mkSqlTy SqlCIDRT = conT 'SqlCIDRT 
mkSqlTy SqlCircleT = conT 'SqlCircleT 
mkSqlTy SqlDateT = conT 'SqlDateT 
mkSqlTy SqlDoubleT = conT 'SqlDoubleT 
mkSqlTy SqlINETT = conT 'SqlINETT 
mkSqlTy SqlIntegerT = conT 'SqlIntegerT 
mkSqlTy (SqlIntervalT fields p) = conT 'SqlIntervalT `appT` liftIntervalFields fields `appT` liftNat p
mkSqlTy SqlJSONBT = conT 'SqlJSONBT 
mkSqlTy SqlJSONT = conT 'SqlJSONT 
mkSqlTy SqlLineT = conT 'SqlLineT 
mkSqlTy SqlLSegT = conT 'SqlLSegT 
mkSqlTy SqlMacAddr8T = conT 'SqlMacAddr8T 
mkSqlTy SqlMacAddrT = conT 'SqlMacAddrT 
mkSqlTy SqlMoneyT = conT 'SqlMoneyT 
mkSqlTy SqlNamedEnumT = conT 'SqlNamedEnumT 
mkSqlTy (SqlNumericT p s) = conT 'SqlNumericT `appT` liftNat p `appT` liftNat s
mkSqlTy SqlPathT = conT 'SqlPathT 
mkSqlTy SqlPointT = conT 'SqlPointT 
mkSqlTy SqlPolygonT = conT 'SqlPolygonT 
mkSqlTy SqlRealT = conT 'SqlRealT 
mkSqlTy SqlSerialT = conT 'SqlSerialT 
mkSqlTy SqlSmallIntT = conT 'SqlSmallIntT 
mkSqlTy SqlSmallSerialT = conT 'SqlSmallSerialT 
mkSqlTy SqlTextSearchQueryT = conT 'SqlTextSearchQueryT 
mkSqlTy SqlTextSearchVectorT = conT 'SqlTextSearchVectorT 
mkSqlTy SqlTextT = conT 'SqlTextT 
mkSqlTy (SqlTimestampT p) = conT 'SqlTimestampT `appT` liftNat p
mkSqlTy (SqlTimestampWithTimeZoneT p) = conT 'SqlTimestampWithTimeZoneT `appT` liftNat p
mkSqlTy (SqlTimeT p) = conT 'SqlTimeT `appT` liftNat p
mkSqlTy (SqlTimeWithTimeZoneT p) = conT 'SqlTimeWithTimeZoneT `appT` liftNat p
mkSqlTy SqlUUIDT = conT 'SqlUUIDT 
mkSqlTy (SqlVarCharT s) = conT 'SqlVarCharT `appT` liftNat s
mkSqlTy SqlXMLT = conT 'SqlXMLT 

liftIntervalFields :: SqlIntervalFields -> Q Type
liftIntervalFields AllIntervalFields = conT 'AllIntervalFields
liftIntervalFields YearIntervalField = conT 'YearIntervalField
liftIntervalFields MonthIntervalField = conT 'MonthIntervalField
liftIntervalFields DayIntervalField = conT 'DayIntervalField
liftIntervalFields HourIntervalField = conT 'HourIntervalField
liftIntervalFields MinuteIntervalField = conT 'MinuteIntervalField
liftIntervalFields SecondIntervalField = conT 'SecondIntervalField
liftIntervalFields YearToMonthIntervalFields = conT 'YearToMonthIntervalFields
liftIntervalFields DayToHourIntervalFields = conT 'DayToHourIntervalFields
liftIntervalFields DayToMinuteIntervalFields = conT 'DayToMinuteIntervalFields
liftIntervalFields DayToSecondIntervalFields = conT 'DayToSecondIntervalFields
liftIntervalFields HourToMinuteIntervalFields = conT 'HourToMinuteIntervalFields
liftIntervalFields HourToSecondIntervalFields = conT 'HourToSecondIntervalFields
liftIntervalFields MinuteToSecondIntervalFields = conT 'MinuteToSecondIntervalFields
