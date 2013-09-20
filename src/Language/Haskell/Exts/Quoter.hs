{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Exts.Quoter
  (
  -- * Short quasiquoters
    qe, qp, qt, qd
  -- * AST Quaiquoters
  , qModule
  , qExp
  , qPat
  , qType
  , qDecl
  , qDecls
  , qStmt
  , qStmts
  , qCon
  , qCons
  , qField
  , qFields
  , qModuleName
  -- * Conversion Utilities
  , toExp, toPat, toType, toQName, toName
  ) where

import Control.Applicative        ( (<$>) )
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Either ( EitherT(..), hoistEither )
import Data.Char                  ( isSpace, isUpper )
import Data.Either                ( rights )
import Data.Generics              ( Data, everywhereBut, extQ, extT )
import Data.Text                  ( Text, unpack )
import Text.Themplates            ( Chunk, substSplices, parseSplices
                                  , curlySplice, generateNames, dedentQuote
                                  )

import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Lift     as TH
import qualified Language.Haskell.TH.Quote    as TH
import qualified Language.Haskell.Exts.SrcLoc as Exts
import qualified Language.Haskell.Exts        as Exts
import qualified Language.Haskell.Meta        as Meta

{-
import Data.Either ( lefts )
import Debug.Trace

debug x = trace (show x) x
-}

{- TODO:
 * Eeew - make this code prettier
 * Remove haskell-src-meta dependency by using syntax-trees or using simpler
   antiquotes.
 * Don't reparse puns for default splices
 -}

-- Versions of the classic TH quasi-quoters.

qe, qp, qt, qd :: TH.QuasiQuoter
qe = astQuoter "qe" (extsParse :: EParser Exts.Exp)
qp = astQuoter "qp" (extsParse :: EParser Exts.Pat)
qt = astQuoter "qt" (extsParse :: EParser Exts.Type)
qd = astQuoter "qd" (extsParse :: EParser [Exts.Decl])

-- Quasi-quoters for other types

qModule, qExp, qPat, qType, qDecl, qDecls, qStmt, qStmts, qCon, qCons, qField,
  qFields, qModuleName :: TH.QuasiQuoter
qModule     = astQuoter "qModule"     (extsParse :: EParser Exts.Module)
qExp        = astQuoter "qExp"        (extsParse :: EParser Exts.Exp)
qPat        = astQuoter "qPat"        (extsParse :: EParser Exts.Pat)
qType       = astQuoter "qType"       (extsParse :: EParser Exts.Type)
qDecl       = astQuoter "qDecl"       (extsParse :: EParser Exts.Decl)
qDecls      = astQuoter "qDecls"      (extsParse :: EParser [Exts.Decl])
qStmt       = astQuoter "qStmt"       (extsParse :: EParser Exts.Stmt)
qStmts      = astQuoter "qStmts"      (extsParse :: EParser [Exts.Stmt])
qCon        = astQuoter "qCon"        (extsParse :: EParser Con)
qCons       = astQuoter "qCons"       (extsParse :: EParser [Con])
qField      = astQuoter "qFieldDecl"  (extsParse :: EParser FieldDecl)
qFields     = astQuoter "qFieldDecls" (extsParse :: EParser [FieldDecl])
qModuleName = astQuoter "qModuleName" (extsParse :: EParser Exts.ModuleName)

class ToExp        a where toExp        :: a -> Exts.Exp
class ToPat        a where toPat        :: a -> Exts.Pat
class ToType       a where toType       :: a -> Exts.Type
class ToDecl       a where toDecl       :: a -> Exts.Decl
class ToQName      a where toQName      :: a -> Exts.QName
class ToName       a where toName       :: a -> Exts.Name
class ToModuleName a where toModuleName :: a -> Exts.ModuleName
class ToCons       a where toCons       :: a -> [Exts.QualConDecl]

-- | Builds a quoter for an AST, given a few configurations.  This is the
--   function that is used to implement @e'@, @p'@, @t'@, and @d'@.
astQuoter :: forall a. TH.Lift a
          => String -> EParser a -> TH.QuasiQuoter
astQuoter name parser = TH.QuasiQuoter expr undefined undefined undefined
 where
  errEitherT e = either fail return =<< runEitherT e

  parse_err :: String -> Either String b -> ErrorQ b
  parse_err err = hoistEither . mapLeft ((err ++ ": \n") ++ )

  parse_ast :: (b -> TH.Q c) -> ErrorQ b -> ErrorQ c
  parse_ast lifter p = lift . lifter =<< p

  parse_splice :: Show c =>
                  (forall b. TH.Lift b => b -> TH.Q c)
               -> (TH.Name -> c -> c)
               -> (String -> ErrorQ c)
               -> (String, String)
               -> ErrorQ [(c, c)]
  parse_splice lifter conv p (pun, code) = do
    parsed <- p code
    {-
    lift $ do
      _ <- TH.runIO $ mapM print $ lefts results
      mapM_ (TH.runIO . print =<<) results'
    -}
    case results' of
      [] -> hoistEither
          $ Left $  "Parse error in fancy splice's pun expression: "
                 ++ pun ++ "\nHere are the different parse errors:\n"
                 ++ unlines (map (\(Left e) -> e) results)
      _ -> lift $ mapM (fmap $ \(cvar, pun') -> (pun', conv cvar parsed)) results'
   where
    results' = rights results
    results = [ fmap ('toExp, )        . lifter <$> (extsParse pun :: EError Exts.Exp)
              , fmap ('toPat, )        . lifter <$> (extsParse pun :: EError Exts.Pat)
              , fmap ('toType, )       . lifter <$> (extsParse pun :: EError Exts.Type)
              , fmap ('toQName, )      . lifter <$> (extsParse pun :: EError Exts.QName)
              , fmap ('toName, )       . lifter <$> (extsParse pun :: EError Exts.Name)
              , fmap ('toCons, )       . lifter <$> (extsParse pun :: EError [Con])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [FieldDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Decl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Stmt])
              , fmap ('toModuleName, ) . lifter <$> (extsParse pun :: EError Exts.ModuleName)
              ]

  expr :: String -> TH.ExpQ
  expr input = errEitherT $ do
    (chunks, free) <- parseChunks name input
    spliced <- substSplices fst ast_p splice_p chunks
    return $ if null free
             then spliced
             else TH.LamE (map (TH.VarP . TH.mkName) free) spliced
   where
    ast_p code
      = parse_ast ({- fmap debug . -} TH.lift)
      . parse_err ("Parse error in pun-substituted " ++ name ++ " AST literal:\n" ++ code)
      $ parser code
    splice_p splice
      = parse_splice TH.lift (\v -> TH.AppE (TH.VarE v))
        ( parse_err ("Parse error in " ++ name ++ " AST literal splice:\n" ++ show splice)
        . parseThExp
        )
        splice

  {-
  pat :: String -> Q Pat
  pat input = errEitherT $ do
    (chunks, _) <- parseAst input
    substSplices fst ast_p splice_p chunks
   where
    ast_p    = parse_ast lifter
             . parse_err ("Parse error in pun-substituted " ++ name ++ " AST match")
             . parser
    splice_p = parse_splice lifter (\v p -> if use_conv then ViewP v p else p)
             ( parse_err ("Parse error in " ++ name ++ " AST match splice")
             . parseThPat
             )
    lifter x = pat_exp <$> TH.lift x
    pat_exp = expToPatMap . M.fromList
            . map (second (\n [p] -> ViewP (VarE . mkName $ n) $ expToPat' p))
            $  prefix_all "Language.Haskell.TH.Syntax."
                 [ ("mkOccName", "occString"), ("mkPkgName", "pkgString"), ("mkModName", "modString")]
            ++ prefix_all "Language.Quasi.Convenience."
                 [ ("toExp", "fromExp"), ("toPat", "fromPat"), ("toType", "fromType") ]
    prefix_all pre = map ((pre++) *** (pre++))
  -}

parseChunks
  :: Monad m
  => String
  -> String
  -> EitherT String m ([Chunk String (String, String)], [String])
parseChunks qq input = do
  chunks <- hoistEither $ parseSplices curlySplice =<< dedentQuote qq input
  let named = zipWith giveName (generateNames "S" input) chunks

  return ( map processEmpty named
         , [x | s@(Right (x, _)) <- named, isEmpty s]
         )
 where
  giveName n (Right (Nothing, s)) = Right (n, s)
  giveName _ (Right (Just n, s))  = Right (n, s)
  giveName _ (Left str)           = Left str

  isEmpty = either (const False) (null . filter (not . isSpace) . snd)

  processEmpty s@(Right (n, _))
    | isEmpty s = Right (n, n)
    | otherwise = s
  processEmpty x = x

--------------------------------------------------------------------------------
-- Utils

type ErrorQ = EitherT String TH.Q

type EError a = Either String a
type EParser a = String -> Either String a

--TODO: move these to HSE?
type Con = Exts.QualConDecl
type FieldDecl = ([Exts.Name], Exts.BangType)

extsParse :: (Data a, Exts.Parseable a) => EParser a
extsParse = mapRight deLoc . Meta.parseResultToEither . Exts.parseWithMode parseMode

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.baseLanguage = Exts.Haskell98
  , Exts.parseFilename = ""
  , Exts.extensions = Exts.knownExtensions
  , Exts.ignoreLinePragmas = False
  , Exts.ignoreLanguagePragmas = False
  , Exts.fixities = Nothing
  }

deLoc :: Data a => a -> a
deLoc = everywhereBut ignoreStrings (id `extT` const Exts.noLoc)

-- | Used for SYB optimization: don't recurse down strings
ignoreStrings :: Data a => a -> Bool
ignoreStrings = const False `extQ` (const True :: String -> Bool)

parseThExp :: EParser TH.Exp
parseThExp = mapRight Meta.toExp . (extsParse :: EParser Exts.Exp)
{-
parseThPat :: EParser TH.Pat
parseThPat = mapRight Meta.toPat  . parsePat
parseThType :: EParser TH.Type
parseThType = mapRight Meta.toType . parseType
parseThDecs :: EParser [TH.Dec]
parseThDecs = mapRight Meta.toDecs . parseDecs
-}

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f g = either (Left . f) (Right . g)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft  f = mapBoth f id

mapRight :: (b -> d) -> Either a b -> Either a d
mapRight f = mapBoth id f

--------------------------------------------------------------------------------
-- Orphans
--TODO: make a package for them

$(TH.deriveLiftMany
  [ ''Exts.Module
  , ''Exts.ImportDecl
  , ''Exts.ModulePragma
  , ''Exts.WarningText
  , ''Exts.ImportSpec
  , ''Exts.ExportSpec
  , ''Exts.CName
  , ''Exts.Exp
  , ''Exts.XAttr
  , ''Exts.XName
  , ''Exts.Splice
  , ''Exts.Bracket
  , ''Exts.Decl
  , ''Exts.Kind
  , ''Exts.Name
  , ''Exts.GuardedAlts
  , ''Exts.PXAttr
  , ''Exts.SpecialCon
  , ''Exts.Type
  , ''Exts.QualStmt
  , ''Exts.FieldUpdate
  , ''Exts.Stmt
  , ''Exts.Alt
  , ''Exts.Binds
  , ''Exts.Pat
  , ''Exts.QOp
  , ''Exts.Literal
  , ''Exts.IPName
  , ''Exts.QName
  , ''Exts.Annotation
  , ''Exts.Activation
  , ''Exts.Rule
  , ''Exts.Safety
  , ''Exts.Rhs
  , ''Exts.Match
  , ''Exts.Op
  , ''Exts.Assoc
  , ''Exts.InstDecl
  , ''Exts.ClassDecl
  , ''Exts.FunDep
  , ''Exts.GadtDecl
  , ''Exts.QualConDecl
  , ''Exts.DataOrNew
  , ''Exts.CallConv
  , ''Exts.GuardedAlt
  , ''Exts.TyVarBind
  , ''Exts.IPBind
  , ''Exts.RPat
  , ''Exts.PatField
  , ''Exts.ModuleName
  , ''Exts.RuleVar
  , ''Exts.GuardedRhs
  , ''Exts.ConDecl
  , ''Exts.Asst
  , ''Exts.RPatOp
  , ''Exts.BangType
  , ''Exts.Tool
  , ''Exts.Boxed
  , ''Exts.SrcLoc
  ])
-- Stuff that's not a type syn in annotated
-- , ''Exts.Deriving
-- , ''Exts.Context
-- Stuff only in annotated
-- , ''Exts.ImportSpecList
-- , ''Exts.ModuleHead
-- , ''Exts.ExportSpecList
-- , ''Exts.DeclHead
-- , ''Exts.InstHead
-- , ''Exts.FieldDecl

--------------------------------------------------------------------------------
-- Convenience conversion instances

instance ToExp         Exts.Exp           where toExp         = id
instance ToPat         Exts.Pat           where toPat         = id
instance ToType        Exts.Type          where toType        = id
instance ToDecl        Exts.Decl          where toDecl        = id
instance ToQName       Exts.QName         where toQName       = id
instance ToName        Exts.Name          where toName        = id
instance ToModuleName  Exts.ModuleName    where toModuleName  = id
instance ToCons        [Exts.QualConDecl] where toCons        = id

isConName :: Exts.Name -> Bool
isConName (Exts.Ident n) = isUpper $ head n
isConName (Exts.Symbol (':':_)) = True
isConName _ = False

ifCon :: (Exts.QName -> a) -> (Exts.Name -> a) -> Exts.QName -> a
ifCon f g qn@(Exts.Qual _ n) = if isConName n then f qn else g n
ifCon f g qn@(Exts.UnQual n) = if isConName n then f qn else g n
ifCon f _ qn@(Exts.Special _) = f qn

ifConQ :: (Exts.QName -> a) -> (Exts.QName -> a) -> Exts.QName -> a
ifConQ f g qn@(Exts.Qual _ n) = if isConName n then f qn else g qn
ifConQ f g qn@(Exts.UnQual n) = if isConName n then f qn else g qn
ifConQ f _ qn@(Exts.Special _) = f qn

--NOTE: this ToPat sucks for constructors..

instance ToExp  Exts.QName where toExp  = ifConQ Exts.Con Exts.Var
instance ToPat  Exts.QName where toPat  = ifCon (`Exts.PApp` []) Exts.PVar
instance ToType Exts.QName where toType = ifCon Exts.TyCon Exts.TyVar

instance ToExp  Exts.Name where toExp  = toExp  . Exts.UnQual
instance ToPat  Exts.Name where toPat  = toPat  . Exts.UnQual
instance ToType Exts.Name where toType = toType . Exts.UnQual

instance ToExp        String where toExp   = toExp       . Exts.name
instance ToPat        String where toPat   = toPat       . Exts.name
instance ToType       String where toType  = toType      . Exts.name
instance ToQName      String where toQName = Exts.UnQual . Exts.name
instance ToName       String where toName  =               Exts.name
instance ToModuleName String where toModuleName = Exts.ModuleName
instance ToCons       String where
  toCons n =
    [ Exts.QualConDecl Exts.noLoc [] [] (Exts.ConDecl (Exts.name n) []) ]

instance ToExp        Text where toExp        = toExp        . unpack
instance ToPat        Text where toPat        = toPat        . unpack
instance ToType       Text where toType       = toType       . unpack
instance ToQName      Text where toQName      = toQName      . unpack
instance ToName       Text where toName       = toName       . unpack
instance ToModuleName Text where toModuleName = toModuleName . unpack
instance ToCons       Text where toCons       = toCons       . unpack

{-
-- this code comes from quasi-extras, and used to be used for TH AST quoting, so
-- these refer to TH types.  Stuff liek this might be useful in the future.

class ToBody     a where toBody     :: a -> Body
class ToStmt     a where toStmt     :: a -> Stmt
class ToMatch    a where toMatch    :: a -> Match
class ToGuard    a where toGuard    :: a -> Guard
class ToTyVarBndr a where toTyVarBndr :: a -> TyVarBndr

instance ToBody  Body  where toBody  = id
instance ToStmt  Stmt  where toStmt  = id
instance ToMatch Match where toMatch = id
instance ToGuard Guard where toGuard = id

instance ToBody      Exts.QName where toBody      = toBody . toExp
instance ToStmt      Exts.QName where toStmt      = toStmt . toExp
instance ToTyVarBndr Exts.QName where toTyVarBndr = PlainTV

instance ToBody      String where toBody      = toBody      . Exts.name
instance ToStmt      String where toStmt      = toStmt      . Exts.name
instance ToTyVarBndr String where toTyVarBndr = toTyVarBndr . Exts.name

instance ToBody Exp            where toBody = NormalB
--TODO: good idea?
instance ToBody [(Guard, Exp)] where toBody = GuardedB

instance ToGuard  Exp   where toGuard = NormalG
instance ToGuard  Stmt  where toGuard = PatG . (:[])
instance ToGuard [Stmt] where toGuard = PatG

instance ToStmt Exp where toStmt = NoBindS

--TODO: good idea?
instance ToTyVarBndr (Name, Kind) where toTyVarBndr = uncurry KindedTV
-- instance ToPred (Name, [Type])
-- instance ToPred (Type, Type)

instance (ToPat p, ToBody b, ToDec d) => ToMatch (p, b, [d]) where toMatch (p, b, ds) = Match (toPat p) (toBody b) (map toDec ds)
instance (ToPat p, ToBody b)          => ToMatch (p, b     ) where toMatch (p, b    ) = Match (toPat p) (toBody b)              []
-}
