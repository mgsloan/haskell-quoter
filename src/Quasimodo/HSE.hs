{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Quasimodo.HSE
  ( e', ec, p', pc, t', tc, d', dc
  , toExp, toPat, toType
  ) where

import Control.Applicative        ( (<$>) )
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Either ( EitherT(..), hoistEither )
import Data.Char                  ( isSpace, isUpper )
import Data.Either                ( rights )
import Text.Themplates            ( Chunk, substSplices, parseSplices
                                  , curlySplice, generateNames )

import qualified Language.Haskell.TH       as TH
import qualified Language.Haskell.TH.Lift  as TH
import qualified Language.Haskell.TH.Quote as TH
import qualified Language.Haskell.Exts     as Exts
import qualified Language.Haskell.Meta     as Meta

{- TODO:
 * Eeew - make this code prettier
 * Remove haskell-src-meta dependency by using syntax-trees or using simpler
   antiquotes.
 -}

e', ec, p', pc, t', tc, d', dc :: TH.QuasiQuoter
e' = astQuoter False "Exp"  parseExp'
ec = astQuoter True  "Exp"  parseExp'

p' = astQuoter False "Pat"  parsePat'
pc = astQuoter True  "Pat"  parsePat'

t' = astQuoter False "Type" parseType'
tc = astQuoter True  "Type" parseType'

d' = astQuoter False "Decs" parseDecs'
dc = astQuoter True  "Decs" parseDecs'

class ToExp      a where toExp      :: a -> Exts.Exp
class ToPat      a where toPat      :: a -> Exts.Pat
class ToType     a where toType     :: a -> Exts.Type
class ToDecl     a where toDecl     :: a -> Exts.Decl

-- | Builds a quoter for an AST, given a few configurations.  This is the
--   function that is used to implement @e'@, @p'@, @t'@, and @d'@.
astQuoter :: forall a. TH.Lift a
          => Bool
          -> String
          -> (String -> Either String a)
          -> TH.QuasiQuoter
astQuoter use_conv name parser = TH.QuasiQuoter expr undefined undefined undefined
 where
  errEitherT e = either fail return =<< runEitherT e

  parse_err :: String -> Either String b -> ErrorQ b
  parse_err err = hoistEither . mapLeft (err ++)

  parse_ast :: (b -> TH.Q c) -> ErrorQ b -> ErrorQ c
  parse_ast lifter p = lift . lifter =<< p

  parse_splice :: (forall b. TH.Lift b => b -> TH.Q c)
               -> (TH.Name -> c -> c)
               -> (String -> ErrorQ c)
               -> (String, String)
               -> ErrorQ [(c, c)]
  parse_splice lifter conv p (pun, code) = do
    parsed <- p code
    case results' of
      [] -> hoistEither
          $ Left $  "Parse error in fancy splice's pun expression: "
                 ++ pun ++ "\nHere are the different parse errors:\n"
                 ++ unlines (map (\(Left e) -> e) results)
      _ -> lift $ mapM (fmap $ \(cvar, pun') -> (pun', conv cvar parsed)) results'
   where
    results' = rights results
    results = [ fmap ('toExp, )  . lifter <$> parseExp'  pun
              , fmap ('toPat, )  . lifter <$> parsePat'  pun
              , fmap ('toType, ) . lifter <$> parseType' pun
              ]

  expr :: String -> TH.ExpQ
  expr input = errEitherT $ do
    (chunks, free) <- parseAst input
    spliced <- substSplices fst ast_p splice_p chunks
    return $ if null free
             then spliced
             else TH.LamE (map (TH.VarP . TH.mkName) free) spliced
   where
    ast_p    = parse_ast TH.lift
             . parse_err ("Parse error in pun-substituted " ++ name ++ " AST literal")
             . parser
    splice_p = parse_splice TH.lift (\v e -> if use_conv then TH.AppE (TH.VarE v) e else e)
             ( parse_err ("Parse error in " ++ name ++ " AST literal splice")
             . parseExp
             )

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
             . parsePat
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

type AstChunk = Chunk String (String, String)

parseAst :: Monad m => String -> EitherT String m ([AstChunk], [String])
parseAst input = do
  chunks <- hoistEither $ parseSplices curlySplice input
  let named = zipWith giveName (generateNames "s" input) chunks

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

parseExp'  :: String -> Either String Exts.Exp
parseExp'  = Meta.parseResultToEither
          . Exts.parseExpWithMode  parseMode
parsePat'  :: String -> Either String Exts.Pat
parsePat'  = Meta.parseResultToEither
          . Exts.parsePatWithMode  parseMode
parseType' :: String -> Either String Exts.Type
parseType' = Meta.parseResultToEither
          . Exts.parseTypeWithMode parseMode
parseDecs' :: String -> Either String [Exts.Decl]
parseDecs' = mapRight (\(Exts.Module _ _ _ _ _ _ x) -> x)
          . Meta.parseResultToEither
          . Exts.parseModuleWithMode parseMode
          . ("module Dummy where\n" ++)

parseExp  :: String -> Either String TH.Exp
parseExp  = mapRight Meta.toExp  . Meta.parseResultToEither
          . Exts.parseExpWithMode  parseMode
parsePat  :: String -> Either String TH.Pat
parsePat  = mapRight Meta.toPat  . Meta.parseResultToEither
          . Exts.parsePatWithMode  parseMode
parseType :: String -> Either String TH.Type
parseType = mapRight Meta.toType . Meta.parseResultToEither
          . Exts.parseTypeWithMode parseMode
parseDecs :: String -> Either String [TH.Dec]
parseDecs = mapRight (\(Exts.Module _ _ _ _ _ _ x) -> Meta.toDecs x)
          . Meta.parseResultToEither
          . Exts.parseModuleWithMode parseMode
          . ("module Dummy where\n" ++)

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.parseFilename = ""
  , Exts.extensions = Exts.glasgowExts ++ [Exts.TupleSections, Exts.BangPatterns, Exts.ViewPatterns]
  , Exts.ignoreLinePragmas = False
  , Exts.ignoreLanguagePragmas = False
  , Exts.fixities = Nothing
  }

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
-- Note that the commented out stuff refers to TH types because this code comes
-- from quasi-extras, and used to be used for TH AST quoting.

{-
class ToBody     a where toBody     :: a -> Body
class ToStmt     a where toStmt     :: a -> Stmt
class ToMatch    a where toMatch    :: a -> Match
class ToGuard    a where toGuard    :: a -> Guard
class ToTyVarBndr a where toTyVarBndr :: a -> TyVarBndr
-}

instance ToExp   Exts.Exp   where toExp   = id
instance ToPat   Exts.Pat   where toPat   = id
instance ToType  Exts.Type  where toType  = id
instance ToDecl  Exts.Decl  where toDecl  = id
{-
instance ToBody  Body  where toBody  = id
instance ToStmt  Stmt  where toStmt  = id
instance ToMatch Match where toMatch = id
instance ToGuard Guard where toGuard = id
-}

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

instance ToExp       Exts.QName where toExp       = ifConQ Exts.Con Exts.Var
instance ToPat       Exts.QName where toPat       = ifCon (`Exts.PApp` []) Exts.PVar
instance ToType      Exts.QName where toType      = ifCon Exts.TyCon Exts.TyVar
{-
instance ToBody      Exts.QName where toBody      = toBody . toExp
instance ToStmt      Exts.QName where toStmt      = toStmt . toExp
instance ToTyVarBndr Exts.QName where toTyVarBndr = PlainTV
-}

instance ToExp       Exts.Name where toExp       = toExp  . Exts.UnQual
instance ToPat       Exts.Name where toPat       = toPat  . Exts.UnQual
instance ToType      Exts.Name where toType      = toType . Exts.UnQual

instance ToExp       String where toExp       = toExp       . Exts.name
instance ToPat       String where toPat       = toPat       . Exts.name
instance ToType      String where toType      = toType      . Exts.name
{-
instance ToBody      String where toBody      = toBody      . Exts.name
instance ToStmt      String where toStmt      = toStmt      . Exts.name
instance ToTyVarBndr String where toTyVarBndr = toTyVarBndr . Exts.name
-}

{-
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
