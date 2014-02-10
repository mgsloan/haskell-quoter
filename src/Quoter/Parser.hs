module Quoter.Parser where

import Control.Applicative
import Data.Char
import Data.List (stripPrefix, find)

-- | Source errors explain issues encountered while compiling a quoter.
data SrcError = SrcError Span String
    deriving (Eq, Ord)

instance Show SrcError where
    show (SrcError sp err) = show sp ++ " " ++ err

-- | A line-and-column source span.
data Span = Span Loc Loc
    deriving (Eq, Ord)

instance Show Span where
    show (Span fr to) = show fr ++ "-" ++ show to

-- | A line-and-column source location.
data Loc = Loc Int Int
    deriving (Eq, Ord)

instance Show Loc where
    show (Loc l c) = show l ++ ":" ++ show c

-- | A 'LocString' is just like a 'String', except each 'Char' is paired with
--   the source location it originated from.
newtype LocString = LocString { unLocString :: [(Char, Loc)] }
    deriving (Eq)

instance Show LocString where
    show (LocString xs@((_, l):_)) = "locString (" ++ show l ++ ")" ++ show (map fst xs)

-- | Given a starting source location and a string, yields the string with
--   source locations added to each character.
locString :: Loc -> String -> LocString
locString loc = LocString . go loc
  where
    go loc@(Loc l c) (ch:xs)  =
        (ch, loc) : go loc' xs
      where
        loc' =
            case ch of
                '\n' -> Loc (l + 1) 1
                '\t' -> Loc l       ((c `div` 8 + 1) * 8)
                _    -> Loc l       (c + 1)

-- | A 'QuoterPiece' is a component of the result of parsing a quoter.  It
--   can either be a quote (the content of the template), or an antiquote
--   (the splices of the template).
data QuoterPiece
    = Quote LocString
    | Antiquote LocString
    deriving Show

-- | Parse a quoter into pieces, or yield a 'SrcError' when an antiquote
--   is unterminated.
parseQuoter :: LocString -> Either [SrcError] [QuoterPiece]
parseQuoter (LocString txt) = quote txt []
  where
    quote []                              acc = Right [Quote (LocString (reverse acc))]
    quote (         ('#',st):('{', _):xs) acc = (Quote (LocString (reverse acc)) :) <$> antiquote [] st xs
    quote (('\\',_):('#',l1):('{',l2):xs)    acc = quote (('{',l2):('#',l1):acc) xs
    quote (x:xs)                          acc = quote (x:acc) xs
    antiquote []                              st acc = Left [SrcError (Span st st) "Unterminated antiquote."]
    antiquote (         ('}', _):('#', _):xs) st acc = (Antiquote (LocString (reverse acc)) :) <$> quote [] xs
    antiquote (('\\',_):('}',l1):('#',l2):xs) st acc = antiquote (('#',l2):('}',l1):acc) st xs
    antiquote (x:xs)                          st acc = antiquote (x:acc) st xs

-- | Dedents a qq based on the indentation of the first line that has content.
--   As a safety measure, if any of the lines have content but not the same
--   indentation, an error is yielded.
dedentQuote :: LocString -> Either [SrcError] LocString
dedentQuote (LocString chars) =
    case find (not . isSpace . fst) chars of
        Nothing -> Right $ LocString []
        Just (_, Loc _ col) -> LocString <$> dedent chars
          where
            dedent ((ch, l@(Loc _ col')):xs) | col' < col =
                if isSpace ch
                    then dedent xs
                    else Left [SrcError (Span l l) "Line has less indent than the first line."]
            dedent (x:xs) = (x:) <$> dedent xs
