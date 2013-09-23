{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Exts.Quoter
  (
  -- * Short quasiquoters
    qe, qp, qt, qd,
  -- * AST Quaiquoters
  qModule, qExp, qPat, qType, qDecl, qDecls, qStmt, qStmts, qQualConDecl,
  qQualConDecls, qField, qFields, qModuleName, qModuleHead, qAnnotation,
  qIPName, qQOp, qOp, qOps, qCName, qCNames, qExportSpecList, qExportSpec,
  qImportDecl, qImportDecls, qImportSpecList, qImportSpec, qBinds, qIPBind,
  qIPBinds, qConDecl, qFieldDecl, qFieldDecls, qGadtDecl, qGadtDecls,
  qClassDecl, qClassDecls, qInstDecl, qInstDecls, qBangType, qRhs, qGuardedRhs,
  qGuardedRhss, qTyVarBind, qTyVarBinds, qKind, qFunDep, qFunDeps, qLiteral,
  qXName, qSafety, qCallConv, qModulePragma, qModulePragmas, qActivation, qRule,
  qRules, qRuleVar, qRuleVars, qQualStmt, qQualStmts, qAlt, qGuardedAlt,
  qGuardedAlts, qQName, qQNames, qName, qNames
  -- * Conversion Utilities
  , toExp, toPat, toType, fromName
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

qModule, qExp, qPat, qType, qDecl, qDecls, qStmt, qStmts, qQualConDecl,
  qQualConDecls, qField, qFields, qModuleName, qModuleHead, qAnnotation,
  qIPName, qQOp, qOp, qOps, qCName, qCNames, qExportSpecList, qExportSpec,
  qImportDecl, qImportDecls, qImportSpecList, qImportSpec, qBinds, qIPBind,
  qIPBinds, qConDecl, qFieldDecl, qFieldDecls, qGadtDecl, qGadtDecls,
  qClassDecl, qClassDecls, qInstDecl, qInstDecls, qBangType, qRhs, qGuardedRhs,
  qGuardedRhss, qTyVarBind, qTyVarBinds, qKind, qFunDep, qFunDeps, qLiteral,
  qXName, qSafety, qCallConv, qModulePragma, qModulePragmas, qActivation, qRule,
  qRules, qRuleVar, qRuleVars, qQualStmt, qQualStmts, qAlt, qGuardedAlt,
  qGuardedAlts, qQName, qQNames, qName, qNames :: TH.QuasiQuoter

qModule         = astQuoter "qModule"         (extsParse :: EParser Exts.Module)
qExp            = astQuoter "qExp"            (extsParse :: EParser Exts.Exp)
qPat            = astQuoter "qPat"            (extsParse :: EParser Exts.Pat)
qType           = astQuoter "qType"           (extsParse :: EParser Exts.Type)
qDecl           = astQuoter "qDecl"           (extsParse :: EParser Exts.Decl)
qDecls          = astQuoter "qDecls"          (extsParse :: EParser [Exts.Decl])
qStmt           = astQuoter "qStmt"           (extsParse :: EParser Exts.Stmt)
qStmts          = astQuoter "qStmts"          (extsParse :: EParser [Exts.Stmt])
qQualConDecl    = astQuoter "qQualConDecl"    (extsParse :: EParser Exts.QualConDecl)
qQualConDecls   = astQuoter "qQualConDecls"   (extsParse :: EParser [Exts.QualConDecl])
qField          = astQuoter "qFieldDecl"      (extsParse :: EParser FieldDecl)
qFields         = astQuoter "qFieldDecls"     (extsParse :: EParser [FieldDecl])
qModuleName     = astQuoter "qModuleName"     (extsParse :: EParser Exts.ModuleName)
qModuleHead     = astQuoter "qModuleHead"     (extsParse :: EParser ModuleHead)
qAnnotation     = astQuoter "qAnnotation"     (extsParse :: EParser Exts.Annotation)
qIPName         = astQuoter "qIPName"         (extsParse :: EParser Exts.IPName)
qQOp            = astQuoter "qQOp"            (extsParse :: EParser Exts.QOp)
qOp             = astQuoter "qOp"             (extsParse :: EParser Exts.Op)
qOps            = astQuoter "qOps"            (extsParse :: EParser [Exts.Op])
qCName          = astQuoter "qCName"          (extsParse :: EParser Exts.CName)
qCNames         = astQuoter "qCNames"         (extsParse :: EParser Exts.CName)
qExportSpecList = astQuoter "qExportSpecList" (extsParse :: EParser [Exts.ExportSpec])
qExportSpec     = astQuoter "qExportSpec"     (extsParse :: EParser Exts.ExportSpec)
qImportDecl     = astQuoter "qImportDecl"     (extsParse :: EParser Exts.ImportDecl)
qImportDecls    = astQuoter "qImportDecls"    (extsParse :: EParser [Exts.ImportDecl])
qImportSpecList = astQuoter "qImportSpecList" (extsParse :: EParser ImportSpecList)
qImportSpec     = astQuoter "qImportSpec"     (extsParse :: EParser Exts.ImportSpec)
qBinds          = astQuoter "qBinds"          (extsParse :: EParser Exts.Binds)
qIPBind         = astQuoter "qIPBind"         (extsParse :: EParser Exts.IPBind)
qIPBinds        = astQuoter "qIPBinds"        (extsParse :: EParser [Exts.IPBind])
qConDecl        = astQuoter "qConDecl"        (extsParse :: EParser Exts.ConDecl)
qFieldDecl      = astQuoter "qFieldDecl"      (extsParse :: EParser FieldDecl)
qFieldDecls     = astQuoter "qFieldDecls"     (extsParse :: EParser [FieldDecl])
qGadtDecl       = astQuoter "qGadtDecl"       (extsParse :: EParser Exts.GadtDecl)
qGadtDecls      = astQuoter "qGadtDecls"      (extsParse :: EParser [Exts.GadtDecl])
qClassDecl      = astQuoter "qClassDecl"      (extsParse :: EParser Exts.ClassDecl)
qClassDecls     = astQuoter "qClassDecls"     (extsParse :: EParser [Exts.ClassDecl])
qInstDecl       = astQuoter "qInstDecl"       (extsParse :: EParser Exts.InstDecl)
qInstDecls      = astQuoter "qInstDecls"      (extsParse :: EParser [Exts.InstDecl])
qBangType       = astQuoter "qBangType"       (extsParse :: EParser Exts.BangType)
qRhs            = astQuoter "qRhs"            (extsParse :: EParser Exts.Rhs)
qGuardedRhs     = astQuoter "qGuardedRhs"     (extsParse :: EParser Exts.GuardedRhs)
qGuardedRhss    = astQuoter "qGuardedRhss"    (extsParse :: EParser [Exts.GuardedRhs])
qTyVarBind      = astQuoter "qTyVarBind"      (extsParse :: EParser Exts.TyVarBind)
qTyVarBinds     = astQuoter "qTyVarBinds"     (extsParse :: EParser [Exts.TyVarBind])
qKind           = astQuoter "qKind"           (extsParse :: EParser Exts.Kind)
qFunDep         = astQuoter "qFunDep"         (extsParse :: EParser Exts.FunDep)
qFunDeps        = astQuoter "qFunDeps"        (extsParse :: EParser [Exts.FunDep])
qLiteral        = astQuoter "qLiteral"        (extsParse :: EParser Exts.Literal)
qXName          = astQuoter "qXName"          (extsParse :: EParser Exts.XName)
qSafety         = astQuoter "qSafety"         (extsParse :: EParser Exts.Safety)
qCallConv       = astQuoter "qCallConv"       (extsParse :: EParser Exts.CallConv)
qModulePragma   = astQuoter "qModulePragma"   (extsParse :: EParser Exts.ModulePragma)
qModulePragmas  = astQuoter "qModulePragmas"  (extsParse :: EParser [Exts.ModulePragma])
qActivation     = astQuoter "qActivation"     (extsParse :: EParser Exts.Activation)
qRule           = astQuoter "qRule"           (extsParse :: EParser Exts.Rule)
qRules          = astQuoter "qRules"          (extsParse :: EParser [Exts.Rule])
qRuleVar        = astQuoter "qRuleVar"        (extsParse :: EParser Exts.RuleVar)
qRuleVars       = astQuoter "qRuleVars"       (extsParse :: EParser [Exts.RuleVar])
qQualStmt       = astQuoter "qQualStmt"       (extsParse :: EParser Exts.QualStmt)
qQualStmts      = astQuoter "qQualStmts"      (extsParse :: EParser [Exts.QualStmt])
qAlt            = astQuoter "qAlt"            (extsParse :: EParser Exts.Alt)
qGuardedAlt     = astQuoter "qGuardedAlt"     (extsParse :: EParser Exts.GuardedAlt)
qGuardedAlts    = astQuoter "qGuardedAlts"    (extsParse :: EParser Exts.GuardedAlts)
-- qGuardedAlts2   = astQuoter "qGuardedAlts2"    (extsParse :: EParser [Exts.GuardedAlt])
qQName          = astQuoter "qQName"          (extsParse :: EParser Exts.QName)
qQNames         = astQuoter "qQNames"         (extsParse :: EParser [Exts.QName])
qName           = astQuoter "qName"           (extsParse :: EParser Exts.Name)
qNames          = astQuoter "qNames"          (extsParse :: EParser [Exts.Name])

class ToExp        a where toExp        :: a -> Exts.Exp
class ToPat        a where toPat        :: a -> Exts.Pat
class ToType       a where toType       :: a -> Exts.Type
class ToDecl       a where toDecl       :: a -> Exts.Decl
class FromName   n a where fromName     :: n -> a

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
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError Exts.QName)
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError Exts.Name)
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError [Exts.Name])
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError Exts.ModuleName)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError ModuleHead)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Annotation)
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError Exts.IPName)
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError Exts.QOp)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.ExportSpec)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.ImportSpec)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Binds)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.ConDecl)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.BangType)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Rhs)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Kind)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Literal)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.XName)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Safety)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.CallConv)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Activation)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.Alt)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.GuardedAlt)
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError Exts.GuardedAlts)
              , fmap ('fromName, )     . lifter <$> (extsParse pun :: EError [Exts.QualConDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [FieldDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Decl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Stmt])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Stmt])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Op])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.CName])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.ExportSpec])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.ImportDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.IPBind])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.GadtDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.ClassDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.InstDecl])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.GuardedRhs])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.TyVarBind])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.FunDep])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.ModulePragma])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.Rule])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.RuleVar])
              , fmap ('id, )           . lifter <$> (extsParse pun :: EError [Exts.QualStmt])
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
type FieldDecl = ([Exts.Name], Exts.BangType)
type ImportSpecList = (Bool, [Exts.ImportSpec])
type ModuleHead = (Exts.ModuleName, Maybe Exts.WarningText, Maybe [Exts.ExportSpec])

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

instance ToExp  String where toExp  = toExp  . Exts.name
instance ToPat  String where toPat  = toPat  . Exts.name
instance ToType String where toType = toType . Exts.name

instance ToExp  Text where toExp  = toExp  . unpack
instance ToPat  Text where toPat  = toPat  . unpack
instance ToType Text where toType = toType . unpack

instance FromName String Exts.QName      where fromName = Exts.UnQual . Exts.name
instance FromName String Exts.Name       where fromName =               Exts.name
instance FromName String Exts.ModuleName where fromName = Exts.ModuleName

instance FromName String Exts.QualConDecl where
  fromName n =
    Exts.QualConDecl Exts.noLoc [] [] (Exts.ConDecl (Exts.name n) [])

-- Undecidable instances
instance FromName a a where fromName = id
instance FromName String a => FromName Text a where fromName = fromName . unpack
instance FromName String a => FromName String [a] where fromName = (:[]) . fromName

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
