{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}

module Database.Posterchild.TH
where

import Database.Posterchild.Syntax.Abstract
import Database.Posterchild.Syntax.SqlTy
import Database.Posterchild.TyCheck
import Database.Posterchild.Parser.Select
import Database.Posterchild.SchemaConstraints
import Language.Haskell.TH
import Data.Text (Text)
import qualified Data.Text as Text
import Data.List (foldl')

tableNameLit :: TableName -> Q Type
tableNameLit (TableName tname) = litT (strTyLit (Text.unpack tname))

columnNameLit :: ColumnName -> Q Type
columnNameLit (ColumnName cname) = litT (strTyLit (Text.unpack cname))

paramName :: ParamName -> Name
paramName (ParamName pname) =
  mkName ("param_" ++ Text.unpack pname)

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
    [ conT ''SchemaHasTable `appT` varT sname `appT` tableNameLit tname
    , conT ''TableHasColumn `appT` tableNameLit tname `appT` columnNameLit cname
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
  let paramNames = map (paramName . fst) $ selectQueryParamsTy sqt
  sigD n $
    forallT [PlainTV fn SpecifiedSpec | fn <- sname : paramNames ]
      (concat <$> mapM (mkQueryConstraint sname) (selectQueryConstraintsTy sqt))
      (arrowT
        `appT` (mkQueryParamsT $ selectQueryParamsTy sqt)
        `appT` (mkQueryResultsT sname $ selectQueryResultTy sqt)
      )

mkQueryParamsT :: [(ParamName, Ty)] -> Q Type
mkQueryParamsT [] = conT '()
mkQueryParamsT [x] = mkQueryParamT x
mkQueryParamsT xs = do
  ts <- mapM mkQueryParamT xs
  return $ foldl' AppT (TupleT $ length ts) ts

mkQueryParamT :: (ParamName, Ty) -> Q Type
mkQueryParamT (n, _) = paramNameLit n

mkQueryResultsT :: Name -> [(ColumnName, Ty)] -> Q Type
mkQueryResultsT _ [] = conT '()
mkQueryResultsT sname [(_, t)] = tyToType sname t
mkQueryResultsT sname xs = do
  ts <- mapM (tyToType sname . snd) xs
  return $ foldl' AppT (TupleT $ length ts) ts

mkSelectQuery :: String -> String -> Q [Dec]
mkSelectQuery fnameStr queryString = do
  sq <- parseSelect "<<TH>>" queryString
  sqt <- either (error . show) return $ runTC $ tcSelectQuery sq
  let fname = mkName fnameStr
  sname <- newName "schema"
  sequence
    [ mkQueryTyDec fname sname sqt
    , valD (varP fname) (normalB $ varE 'undefined) []
    ]
