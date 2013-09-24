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
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Language.Haskell.Quoter.HSE
  (
  -- * Short quasiquoters
  -- | These mimic the ast quoters used for TH.
  qe, qp, qt, qd,
  -- * AST Quaiquoters
  -- | Each 'QuasiQuoter' can be used to generate Haskell expressions.
  qModule, qExp, qExps, qPat, qPats, qType, qDecl, qDecls, qStmt, qStmts,
  qQualConDecl,   qQualConDecls, qField, qFields, qModuleName, qModuleHead,
  qAnnotation,   qIPName, qQOp, qOp, qOps, qCName, qCNames, qExportSpecList,
  qExportSpec,   qImportDecl, qImportDecls, qImportSpecList, qImportSpec,
  qBinds, qIPBind, qIPBinds, qConDecl, qFieldDecl, qFieldDecls, qGadtDecl,
  qGadtDecls, qClassDecl, qClassDecls, qInstDecl, qInstDecls, qBangType, qRhs,
  qGuardedRhs, qGuardedRhss, qTyVarBind, qTyVarBinds, qKind, qFunDep, qFunDeps,
  qLiteral, qXName, qSafety, qCallConv, qModulePragma, qModulePragmas,
  qActivation, qRule, qRules, qRuleVar, qRuleVars, qQualStmt, qQualStmts, qAlt,
  qGuardedAlt, qGuardedAlts, qQName, qQNames, qName, qNames,
  -- * Utilities
  stringLit,
  -- * Conversion Utilities
  -- | These utilities are inserted to make splicing expressions, patterns,
  --   types and names more convenient.
  toExp, toPat, toType, fromName,
  -- * Type Synonyms
  FieldDecl, ImportSpecList, ModuleHead
  ) where

import Control.Applicative        ( (<$>) )
import Data.Char                  ( isUpper )
import Data.Text                  ( Text, pack, unpack, splitOn, intercalate )

import Language.Haskell.Quoter.Internal (astQuoter, extsParse, EError, EParser)

import qualified Language.Haskell.TH          as TH
import qualified Language.Haskell.TH.Lift     as TH
import qualified Language.Haskell.TH.Quote    as TH
import qualified Language.Haskell.Exts.SrcLoc as Exts
import qualified Language.Haskell.Exts        as Exts

--TODO: move these to HSE?
type FieldDecl = ([Exts.Name], Exts.BangType)
type ImportSpecList = (Bool, [Exts.ImportSpec])
type ModuleHead =
  (Exts.ModuleName, Maybe Exts.WarningText, Maybe [Exts.ExportSpec])

-- Versions of the classic TH quasi-quoters.

qe, qp, qt, qd :: TH.QuasiQuoter
qe = extsQuoter "qe" (extsParse :: EParser Exts.Exp)
qp = extsQuoter "qp" (extsParse :: EParser Exts.Pat)
qt = extsQuoter "qt" (extsParse :: EParser Exts.Type)
qd = extsQuoter "qd" (extsParse :: EParser [Exts.Decl])

-- Quasi-quoters for other types

qModule, qExp, qExps, qPat, qPats, qType, qDecl, qDecls, qStmt, qStmts,
  qQualConDecl,   qQualConDecls, qField, qFields, qModuleName, qModuleHead,
  qAnnotation,   qIPName, qQOp, qOp, qOps, qCName, qCNames, qExportSpecList,
  qExportSpec,   qImportDecl, qImportDecls, qImportSpecList, qImportSpec,
  qBinds, qIPBind, qIPBinds, qConDecl, qFieldDecl, qFieldDecls, qGadtDecl,
  qGadtDecls, qClassDecl, qClassDecls, qInstDecl, qInstDecls, qBangType, qRhs,
  qGuardedRhs, qGuardedRhss, qTyVarBind, qTyVarBinds, qKind, qFunDep, qFunDeps,
  qLiteral, qXName, qSafety, qCallConv, qModulePragma, qModulePragmas,
  qActivation, qRule, qRules, qRuleVar, qRuleVars, qQualStmt, qQualStmts, qAlt,
  qGuardedAlt, qGuardedAlts, qQName, qQNames, qName, qNames :: TH.QuasiQuoter

qModule         = extsQuoter "qModule"         (extsParse :: EParser Exts.Module)
qExp            = extsQuoter "qExp"            (extsParse :: EParser Exts.Exp)
qExps           = extsQuoter "qExps"           (extsParse :: EParser [Exts.Exp])
qPat            = extsQuoter "qPat"            (extsParse :: EParser Exts.Pat)
qPats           = extsQuoter "qPats"           (extsParse :: EParser [Exts.Pat])
qType           = extsQuoter "qType"           (extsParse :: EParser Exts.Type)
qDecl           = extsQuoter "qDecl"           (extsParse :: EParser Exts.Decl)
qDecls          = extsQuoter "qDecls"          (extsParse :: EParser [Exts.Decl])
qStmt           = extsQuoter "qStmt"           (extsParse :: EParser Exts.Stmt)
qStmts          = extsQuoter "qStmts"          (extsParse :: EParser [Exts.Stmt])
qQualConDecl    = extsQuoter "qQualConDecl"    (extsParse :: EParser Exts.QualConDecl)
qQualConDecls   = extsQuoter "qQualConDecls"   (extsParse :: EParser [Exts.QualConDecl])
qField          = extsQuoter "qFieldDecl"      (extsParse :: EParser FieldDecl)
qFields         = extsQuoter "qFieldDecls"     (extsParse :: EParser [FieldDecl])
qModuleName     = extsQuoter "qModuleName"     (extsParse :: EParser Exts.ModuleName)
qModuleHead     = extsQuoter "qModuleHead"     (extsParse :: EParser ModuleHead)
qAnnotation     = extsQuoter "qAnnotation"     (extsParse :: EParser Exts.Annotation)
qIPName         = extsQuoter "qIPName"         (extsParse :: EParser Exts.IPName)
qQOp            = extsQuoter "qQOp"            (extsParse :: EParser Exts.QOp)
qOp             = extsQuoter "qOp"             (extsParse :: EParser Exts.Op)
qOps            = extsQuoter "qOps"            (extsParse :: EParser [Exts.Op])
qCName          = extsQuoter "qCName"          (extsParse :: EParser Exts.CName)
qCNames         = extsQuoter "qCNames"         (extsParse :: EParser Exts.CName)
qExportSpecList = extsQuoter "qExportSpecList" (extsParse :: EParser [Exts.ExportSpec])
qExportSpec     = extsQuoter "qExportSpec"     (extsParse :: EParser Exts.ExportSpec)
qImportDecl     = extsQuoter "qImportDecl"     (extsParse :: EParser Exts.ImportDecl)
qImportDecls    = extsQuoter "qImportDecls"    (extsParse :: EParser [Exts.ImportDecl])
qImportSpecList = extsQuoter "qImportSpecList" (extsParse :: EParser ImportSpecList)
qImportSpec     = extsQuoter "qImportSpec"     (extsParse :: EParser Exts.ImportSpec)
qBinds          = extsQuoter "qBinds"          (extsParse :: EParser Exts.Binds)
qIPBind         = extsQuoter "qIPBind"         (extsParse :: EParser Exts.IPBind)
qIPBinds        = extsQuoter "qIPBinds"        (extsParse :: EParser [Exts.IPBind])
qConDecl        = extsQuoter "qConDecl"        (extsParse :: EParser Exts.ConDecl)
qFieldDecl      = extsQuoter "qFieldDecl"      (extsParse :: EParser FieldDecl)
qFieldDecls     = extsQuoter "qFieldDecls"     (extsParse :: EParser [FieldDecl])
qGadtDecl       = extsQuoter "qGadtDecl"       (extsParse :: EParser Exts.GadtDecl)
qGadtDecls      = extsQuoter "qGadtDecls"      (extsParse :: EParser [Exts.GadtDecl])
qClassDecl      = extsQuoter "qClassDecl"      (extsParse :: EParser Exts.ClassDecl)
qClassDecls     = extsQuoter "qClassDecls"     (extsParse :: EParser [Exts.ClassDecl])
qInstDecl       = extsQuoter "qInstDecl"       (extsParse :: EParser Exts.InstDecl)
qInstDecls      = extsQuoter "qInstDecls"      (extsParse :: EParser [Exts.InstDecl])
qBangType       = extsQuoter "qBangType"       (extsParse :: EParser Exts.BangType)
qRhs            = extsQuoter "qRhs"            (extsParse :: EParser Exts.Rhs)
qGuardedRhs     = extsQuoter "qGuardedRhs"     (extsParse :: EParser Exts.GuardedRhs)
qGuardedRhss    = extsQuoter "qGuardedRhss"    (extsParse :: EParser [Exts.GuardedRhs])
qTyVarBind      = extsQuoter "qTyVarBind"      (extsParse :: EParser Exts.TyVarBind)
qTyVarBinds     = extsQuoter "qTyVarBinds"     (extsParse :: EParser [Exts.TyVarBind])
qKind           = extsQuoter "qKind"           (extsParse :: EParser Exts.Kind)
qFunDep         = extsQuoter "qFunDep"         (extsParse :: EParser Exts.FunDep)
qFunDeps        = extsQuoter "qFunDeps"        (extsParse :: EParser [Exts.FunDep])
qLiteral        = extsQuoter "qLiteral"        (extsParse :: EParser Exts.Literal)
qXName          = extsQuoter "qXName"          (extsParse :: EParser Exts.XName)
qSafety         = extsQuoter "qSafety"         (extsParse :: EParser Exts.Safety)
qCallConv       = extsQuoter "qCallConv"       (extsParse :: EParser Exts.CallConv)
qModulePragma   = extsQuoter "qModulePragma"   (extsParse :: EParser Exts.ModulePragma)
qModulePragmas  = extsQuoter "qModulePragmas"  (extsParse :: EParser [Exts.ModulePragma])
qActivation     = extsQuoter "qActivation"     (extsParse :: EParser Exts.Activation)
qRule           = extsQuoter "qRule"           (extsParse :: EParser Exts.Rule)
qRules          = extsQuoter "qRules"          (extsParse :: EParser [Exts.Rule])
qRuleVar        = extsQuoter "qRuleVar"        (extsParse :: EParser Exts.RuleVar)
qRuleVars       = extsQuoter "qRuleVars"       (extsParse :: EParser [Exts.RuleVar])
qQualStmt       = extsQuoter "qQualStmt"       (extsParse :: EParser Exts.QualStmt)
qQualStmts      = extsQuoter "qQualStmts"      (extsParse :: EParser [Exts.QualStmt])
qAlt            = extsQuoter "qAlt"            (extsParse :: EParser Exts.Alt)
qGuardedAlt     = extsQuoter "qGuardedAlt"     (extsParse :: EParser Exts.GuardedAlt)
qGuardedAlts    = extsQuoter "qGuardedAlts"    (extsParse :: EParser Exts.GuardedAlts)
-- qGuardedAlts2   = extsQuoter "qGuardedAlts2"    (extsParse :: EParser [Exts.GuardedAlt])
qQName          = extsQuoter "qQName"          (extsParse :: EParser Exts.QName)
qQNames         = extsQuoter "qQNames"         (extsParse :: EParser [Exts.QName])
qName           = extsQuoter "qName"           (extsParse :: EParser Exts.Name)
qNames          = extsQuoter "qNames"          (extsParse :: EParser [Exts.Name])

extsQuoter :: TH.Lift a => String -> EParser a -> TH.QuasiQuoter
extsQuoter = astQuoter antiquotes

antiquotes :: String -> [EError (TH.Q (TH.Name, TH.Exp))]
antiquotes pun =
  [ fmap ('toExp, )    . TH.lift <$> (extsParse pun :: EError Exts.Exp)
  , fmap ('toPat, )    . TH.lift <$> (extsParse pun :: EError Exts.Pat)
  , fmap ('toType, )   . TH.lift <$> (extsParse pun :: EError Exts.Type)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError Exts.QName)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError Exts.Name)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError [Exts.Name])
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError Exts.ModuleName)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError ModuleHead)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Annotation)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError Exts.IPName)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError Exts.QOp)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError Exts.ExportSpec)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.ImportSpec)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Binds)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.ConDecl)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.BangType)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Rhs)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Kind)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Literal)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.XName)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Safety)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.CallConv)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Activation)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.Alt)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.GuardedAlt)
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError Exts.GuardedAlts)
  , fmap ('fromName, ) . TH.lift <$> (extsParse pun :: EError [Exts.QualConDecl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [FieldDecl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.Exp])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.Pat])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.Decl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.Stmt])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.Op])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.CName])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.ExportSpec])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.ImportDecl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.IPBind])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.GadtDecl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.ClassDecl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.InstDecl])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.GuardedRhs])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.TyVarBind])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.FunDep])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.ModulePragma])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.Rule])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.RuleVar])
  , fmap ('id, )       . TH.lift <$> (extsParse pun :: EError [Exts.QualStmt])
  ]

--TODO: make this unnecessary.
stringLit :: String -> Exts.Exp
stringLit = Exts.Lit . Exts.String

--------------------------------------------------------------------------------
-- Convenience conversion instances

class ToExp        a where toExp        :: a -> Exts.Exp
class ToPat        a where toPat        :: a -> Exts.Pat
class ToType       a where toType       :: a -> Exts.Type
class ToDecl       a where toDecl       :: a -> Exts.Decl
class FromName   n a where fromName     :: n -> a

instance ToExp         Exts.Exp           where toExp         = id
instance ToPat         Exts.Pat           where toPat         = id
instance ToType        Exts.Type          where toType        = id
instance ToDecl        Exts.Decl          where toDecl        = id

--TODO: How do we work in validation, failure and such?

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

qname :: Text -> Exts.QName
qname qn
    | null prefix = Exts.UnQual suffix
    | otherwise = Exts.Qual (Exts.ModuleName prefix) suffix
  where
    components = splitOn "." qn
    prefix = unpack . intercalate "." $ init components
    suffix = Exts.name . unpack $ last components

--NOTE: this ToPat sucks for constructors..

instance ToExp  Exts.QName where toExp  = ifConQ Exts.Con Exts.Var
instance ToPat  Exts.QName where toPat  = ifCon (`Exts.PApp` []) Exts.PVar
instance ToType Exts.QName where toType = ifCon Exts.TyCon Exts.TyVar

instance ToExp  Exts.Name where toExp  = toExp  . Exts.UnQual
instance ToPat  Exts.Name where toPat  = toPat  . Exts.UnQual
instance ToType Exts.Name where toType = toType . Exts.UnQual

instance ToExp  String where toExp  = toExp  . qname . pack
instance ToPat  String where toPat  = toPat  . qname . pack
instance ToType String where toType = toType . qname . pack

instance ToExp  Text where toExp  = toExp  . qname
instance ToPat  Text where toPat  = toPat  . qname
instance ToType Text where toType = toType . qname

instance FromName String Exts.QName      where fromName = qname . pack
instance FromName String Exts.Name       where fromName = Exts.name
instance FromName String Exts.ModuleName where fromName = Exts.ModuleName

instance FromName String Exts.ExportSpec where
  fromName = ifConQ Exts.EAbs Exts.EVar . qname . pack

instance FromName String Exts.QualConDecl where
  fromName n =
    Exts.QualConDecl Exts.noLoc [] [] (Exts.ConDecl (Exts.name n) [])

--FIXME
-- Undecidable instances
instance FromName a a where fromName = id
instance FromName String a => FromName Text a where fromName = fromName . unpack
instance FromName String a => FromName String [a] where fromName = (:[]) . fromName

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
