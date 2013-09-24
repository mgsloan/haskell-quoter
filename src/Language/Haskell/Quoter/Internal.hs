{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.Quoter.Internal
  ( astQuoter, extsParse, EParser, EError
  ) where

import Control.Applicative        ( (<$>), (<*>) )
import Control.Monad.Trans.Class  ( lift )
import Control.Monad.Trans.Either ( EitherT(..), hoistEither )
import Data.Char                  ( isSpace, toLower )
import Data.Either                ( rights )
import Data.Generics              ( Data, extT, extQ, everywhereBut, everywhere' )
import Data.List                  ( isPrefixOf, stripPrefix, tails )
import qualified Data.Map as M
import Data.Maybe                 ( maybeToList )
import Text.Parsec
  ( Parsec, parse, try, eof, anyToken, noneOf, char, string, choice, (<|>)
  , lookAhead, anyChar, manyTill )

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
 * Ensure that each splice does get substituted.
 -}

type EError a = Either String a

type EParser a = String -> Either String a

type ErrorQ = EitherT String TH.Q

-- | Builds a quoter for an AST, given a few configurations.  This is the
--   function that is used to implement @e'@, @p'@, @t'@, and @d'@.
astQuoter :: forall a. TH.Lift a
          => (String -> [EError (TH.Q (TH.Name, TH.Exp))])
          -> String
          -> EParser a
          -> TH.QuasiQuoter
astQuoter antiquotes name parser =
  TH.QuasiQuoter expr undefined undefined undefined
 where
  parse_err :: String -> Either String b -> ErrorQ b
  parse_err err = hoistEither . mapLeft ((err ++ "\n\n") ++ )

  parse_ast :: (b -> TH.Q c) -> ErrorQ b -> ErrorQ c
  parse_ast lifter p = lift . lifter =<< p

  errEitherT e = either fail return =<< runEitherT e

  parse_splice
    :: Show c
    => (TH.Name -> c -> c)
    -> (String -> ErrorQ c)
    -> (String, String)
    -> ErrorQ [(TH.Exp, c)]
  parse_splice conv p (pun, code) = do
    parsed <- p code
    let results = antiquotes pun
        punParses = rights results
    {-
    lift $ do
      _ <- TH.runIO $ mapM print $ lefts results
      mapM_ (TH.runIO . print =<<) punParses
    -}
    case punParses of
      [] -> hoistEither
          $ Left $  "Parse error in splice's pun expression: "
                 ++ pun ++ "\nHere are the different parse errors:\n"
                 ++ unlines (map (\(Left e) -> e) results)
      _ -> lift $ mapM (fmap $ \(cvar, pun') -> (pun', conv cvar parsed)) punParses

  expr :: String -> TH.ExpQ
  expr input = errEitherT $ do
    (chunks, free) <- parseChunks name input
    spliced <- substSplices fst ast_p splice_p chunks
    return $ if null free
             then spliced
             else TH.LamE (map (TH.VarP . TH.mkName . decapitalize) free) spliced
   where
    ast_p code
      = parse_ast ({- fmap debug . -} TH.lift)
      . parse_err ("Parse error in pun-substituted " ++ name ++ " literal:\n" ++ code)
      $ parser code
    splice_p splice
      = parse_splice (\v -> TH.AppE (TH.VarE v))
        ( parse_err ("Parse error in " ++ name ++ " literal splice:\n" ++ show splice)
        . parseThExp
        )
        splice

  {-
  pat :: String -> Q Pat
  pat input = errEitherT $ do
    (chunks, _) <- parseChunks input
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

-- Utils

extsParse :: (Data a, Exts.Parseable a) => EParser a
extsParse = mapRight deLoc . Meta.parseResultToEither . Exts.parseWithMode parseMode

-- | Parse mode with all extensions and no fixities.
parseMode :: Exts.ParseMode
parseMode = Exts.ParseMode
  { Exts.baseLanguage = Exts.Haskell2010
  , Exts.parseFilename = ""
  , Exts.extensions =
    [ Exts.EnableExtension ext
    | Exts.EnableExtension ext <- Exts.knownExtensions
    ]
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
parseThPat = mapRight Meta.toPat  . (extsParse :: EParser Exts.Pat)
-}

mapBoth :: (a -> c) -> (b -> d) -> Either a b -> Either c d
mapBoth f g = either (Left . f) (Right . g)

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft  f = mapBoth f id

mapRight :: (b -> d) -> Either a b -> Either a d
mapRight f = mapBoth id f

decapitalize :: String -> String
decapitalize "" = ""
decapitalize (x:xs) = toLower x : xs

-- Quote Parser

parseChunks
  :: Monad m
  => String
  -> String
  -> EitherT String m ([Either String (String, String)], [String])
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
    | isEmpty s = Right (n, decapitalize n)
    | otherwise = s
  processEmpty x = x

substSplices :: forall t s e m r. (Monad m, Data r, Ord r)
             => (s -> [t])
             -> ([t] -> EitherT e m r)
             -> (s -> EitherT e m [(r, r)])
             -> [Either [t] s]
             -> EitherT e m r
substSplices placeholder parser subst xs = do
  subs <- sequence [subst s | Right s <- xs]

  let subs_map = M.fromList $ concat subs
      do_subst :: r -> r
      do_subst x
        | Just r <- M.lookup x subs_map = r
        | otherwise                     = x

  parsed <- parser
          $ concatMap (either id placeholder) xs

  return $ everywhere' (id `extT` do_subst) parsed

-- Utilities for parsing spliced stuff.

parseSplices :: forall t s. Show t
             => Parsec [t] () s
             -> [t]
             -> Either String [Either [t] s]
parseSplices splice =
  either (Left . show) Right . parse (spliceParser splice) ""

spliceParser :: forall t s. Show t
             => Parsec [t] () s
             -> Parsec [t] () [Either [t] s]
spliceParser parse_splice = do
  s <-  (Right         <$> try parse_splice)
    <|> (Left  . (:[]) <$> anyToken)
    <|> (eof >> return (Left  []))
  case s of
    c@(Left  []) -> return [c]
    _ -> do
      rest <- spliceParser parse_splice
      case (s, rest) of
        (Left  [c], Left  acc:rs) -> return $ Left  (c:acc) : rs
        _ -> return $ s : rest

{-
-- The same splice style as TH ast quoters.
thSplice :: Parsec String () (Maybe String, String)
thSplice = do
  _ <- try $ string "$("
  fancySplice (concat <$> nestParser (delimParser '(' ')')
                                     [try $ char ')' >> return ""])
-}

-- To be passed as the first parameter to parseSplices or spliceParser.
curlySplice :: Parsec String () (Maybe String, String)
curlySplice = do
  _ <- try $ string "{{"
  fancySplice (concat <$> nestParser (delimParser '{' '}')
                                     [try $ string "}}" >> return ""])

fancySplice :: Parsec String () s
            -> Parsec String () (Maybe String, s)
fancySplice code_parser = do
  c <- lookAhead anyChar
  case c of
    '<' -> do
      _ <- char '<'
      splice <- manyTill
        (escapeParser '\\' [('>', '>'), ('\\', '\\')])
        (char '>')
      code <- code_parser
      return (Just splice, code)
    _ ->  do
      code <- code_parser
      return (Nothing, code)

nestParser
  :: forall t r. Show t
  =>  Parsec [t] () (r, Maybe (Parsec [t] () r))
  -> [Parsec [t] () r]
  ->  Parsec [t] () [r]
nestParser open closers = case closers of
  [] -> return []
  (close:cs)
    -> ((:) <$> close <*> nestParser open cs)
   <|> (open >>= \(x, c) -> (x:) <$> nestParser open (maybeToList c ++ closers))
   <|> return []

escapeParser :: Char -> [(Char, Char)] -> Parsec String () Char
escapeParser c xs =
    (char c >> choice (map escape xs)) <|> noneOf [c]
  where
    escape (code, repl) = char code >> return repl

delimParser :: Char -> Char
            -> Parsec String () (String, Maybe (Parsec String () String))
delimParser start end = do
  r <- try (string [start]) <|> ((:[]) <$> noneOf [end])
  return (r, if r == [start] then Just (try $ string [end]) else Nothing)

-- | Dedents a qq based on the indentation of the first line that has content.
--   As a safety measure, if any of the lines have content but not the same
--   indentation, then an error is yielded.
dedentQuote :: String -> String -> Either String String
dedentQuote qq code =
    case lines code of
      (l0:l1:ls)
        | all isSpace l0 -> dedent $ zip ixs (l1:ls)
        | otherwise -> dedent $ zip ixs (l0':l1:ls)
          where
            l0' = replicate (length qq + 2) ' ' ++ l0
      _ -> Right $ dropWhile isSpace code
  where
    ixs :: [Int]
    ixs = [1..]
    dedent [] = Right ""
    dedent ls@((_, l):_) = unlines <$> mapM removePrefix ls
      where
        prefix = takeWhile isSpace l
        removePrefix (ix, x)
          | all isSpace x = Right x
          | Just x' <- stripPrefix prefix x = Right x'
          | otherwise = Left $ unwords
            [ "While dedenting a quote, line "
            , show ix
            , " had less indentation, or used tabs."
            ]

generateNames :: String -> String -> [String]
generateNames prefix input =
    [ prefix ++ s
    | s <- map show [(0::Int)..]
    , all (not . isPrefixOf s) avoids
    ]
  where
    avoids = [ drop (length prefix) t
             | t <- tails input
             , prefix `isPrefixOf` t
             ]
