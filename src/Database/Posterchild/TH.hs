{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Database.Posterchild.TH
where

import Database.Posterchild.Parser
import Database.Posterchild.SchemaConstraints
import Database.Posterchild.Syntax
import Database.Posterchild.TyCheck
import Database.Posterchild.Driver.Class
import Data.HList
import GHC.Stack (HasCallStack)

import qualified Data.Map as Map
import qualified Data.Set as Set
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
paramName pname =
  mkName ("p" ++ Text.unpack (paramNameText pname))

paramNameLit :: ParamName -> Q Type
paramNameLit pname =
  varT $ paramName pname

tyToType :: Name -> Ty -> Q Type
tyToType _sname NullTy =
  varT '()
tyToType _sname (MonoTy sqlTy) =
  conT ''SqlValue `appT` mkSqlTy sqlTy
tyToType sname (ColumnRefTy tname cname) =
  conT ''TableColumnTy
    `appT` (conT ''SchemaTableTy `appT` varT sname `appT` tableNameLit tname)
    `appT` columnNameLit cname
tyToType _sname (ParamRefTy pname) =
  paramNameLit pname
tyToType sname (SumTy a b) =
  conT ''Either `appT` tyToType sname a `appT` tyToType sname b

mkSqlTy :: SqlTy -> Q Type
mkSqlTy SqlSmallIntT = conT 'SqlSmallIntT
mkSqlTy SqlIntegerT = conT 'SqlIntegerT
mkSqlTy SqlBigIntT = conT 'SqlBigIntT
mkSqlTy (SqlNumericT p s) = conT 'SqlNumericT `appT` liftNat p `appT` liftNat s
mkSqlTy SqlRealT = conT 'SqlRealT 
mkSqlTy SqlDoubleT = conT 'SqlDoubleT
mkSqlTy SqlSmallSerialT = conT 'SqlSmallSerialT
mkSqlTy SqlSerialT = conT 'SqlSerialT
mkSqlTy SqlBigSerialT = conT 'SqlBigSerialT
mkSqlTy SqlMoneyT = conT 'SqlMoneyT
mkSqlTy SqlBooleanT = conT 'SqlBooleanT

mkSqlTy (SqlVarCharT l) = conT 'SqlVarCharT `appT` liftNat l
mkSqlTy (SqlCharT l) = conT 'SqlCharT `appT` liftNat l
mkSqlTy SqlTextT = conT 'SqlTextT
mkSqlTy SqlBlobT = conT 'SqlBlobT
mkSqlTy (SqlTimestampT p) = conT 'SqlTimestampT `appT` liftNat p
mkSqlTy (SqlTimestampWithTimeZoneT p) = conT 'SqlTimestampWithTimeZoneT `appT` liftNat p
mkSqlTy SqlDateT = conT 'SqlDateT
mkSqlTy (SqlTimeT p) = conT 'SqlTimeT `appT` liftNat p
mkSqlTy (SqlTimeWithTimeZoneT p) = conT 'SqlTimeWithTimeZoneT `appT` liftNat p
mkSqlTy (SqlIntervalT fields p) =
  conT 'SqlIntervalT `appT` liftIntervalFields fields `appT` liftNat p
mkSqlTy SqlNamedEnumT = conT 'SqlNamedEnumT

mkSqlTy (SqlBitT l) = conT 'SqlBitT `appT` liftNat l
mkSqlTy (SqlBitVaryingT l) = conT 'SqlBitVaryingT `appT` liftMaybeNat l

mkSqlTy (SqlArrayT t l) = conT 'SqlArrayT `appT` mkSqlTy t `appT` liftMaybeNat l
mkSqlTy SqlPointT = conT 'SqlPointT
mkSqlTy SqlLineT = conT 'SqlLineT
mkSqlTy SqlLSegT = conT 'SqlLSegT
mkSqlTy SqlBoxT = conT 'SqlBoxT
mkSqlTy SqlPathT = conT 'SqlPathT
mkSqlTy SqlPolygonT = conT 'SqlPolygonT
mkSqlTy SqlCircleT = conT 'SqlCircleT
mkSqlTy SqlCIDRT = conT 'SqlCIDRT
mkSqlTy SqlINETT = conT 'SqlINETT
mkSqlTy SqlMacAddrT = conT 'SqlMacAddrT
mkSqlTy SqlMacAddr8T = conT 'SqlMacAddr8T
mkSqlTy SqlTextSearchVectorT = conT 'SqlTextSearchVectorT
mkSqlTy SqlTextSearchQueryT = conT 'SqlTextSearchQueryT
mkSqlTy SqlUUIDT = conT 'SqlUUIDT
mkSqlTy SqlXMLT = conT 'SqlXMLT
mkSqlTy SqlJSONT = conT 'SqlJSONT
mkSqlTy SqlJSONBT = conT 'SqlJSONBT
mkSqlTy SqlAnyT = conT 'SqlAnyT

-- mkSqlTy t = error $ "Not implemented: " ++ show t

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
    (concat <$> mapM (mkQueryConstraint sname) (Set.toList $ selectQueryConstraintsTy sqt))
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
  sq <- either error return $ parseSelect "<<TH>>" (Text.pack queryString)
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
