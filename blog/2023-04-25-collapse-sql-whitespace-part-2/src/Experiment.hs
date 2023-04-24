{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Experiment
  ( sqlCollapseWhitespace
  ) where

-- https://hackage.haskell.org/package/base
import Data.Char (isSpace)

------------------------------------------------------------------------------

-- | Collapse whitespace and remove comments from PostgreSQL queries
--
-- This function collapses consecutive whitespace and comments.  Consecutive
-- whitespace and comments after a statement is collapsed to a newline.  All
-- other consecutive whitespace and comments is collapsed to a space.
-- Collapsed queries have no space at the beginning or end of the string.
-- Note that unnecessary whitespace surrounding syntax such as parentheses is
-- /not/ removed; this function collapses whitespace but does not minimize it.
--
-- This function is total.  It does /not/ raise errors on invalid SQL.
--
-- Supported syntax:
--
-- * Standard SQL comments (double-dash) are supported.
-- * Nested C-style comments are supported.
-- * Double-quoted identifiers are supported.  Whitespace in identifiers is
--   not collapsed.
-- * Single-quoted string syntax is supported.  Multiple single-quoted string
--   constants that are only separated by whitespace with at least one newline
--   are are translated to a single string constant.
-- * PostgreSQL escape string syntax is supported.
-- * PostgreSQL Unicode escape string syntax is supported.
-- * Nested PostgreSQL dollar-quoted string syntax is supported.  Tags are
--   /not/ validated.
-- * Semicolons enclosed in parenthesis are not counted as the end of a
--   statement.  For example, a @CREATE RULE@ statement may be defined with
--   multiple commands that are separated by semicolons.
--
-- Reference:
--
-- * <https://www.postgresql.org/docs/current/sql-syntax-lexical.html>
-- * <https://www.postgresql.org/docs/current/sql-createrule.html>
sqlCollapseWhitespace :: String -> String
sqlCollapseWhitespace = goSpace 0 Nothing
  where
    dropStdComment :: String -> String
    dropStdComment = dropWhile (/= '\n')

    dropCStyleComment :: Int -> String -> String
    dropCStyleComment !count = \case
      ('*':'/':xs)
        | count < 1 -> xs
        | otherwise -> dropCStyleComment (count - 1) xs
      ('/':'*':xs) -> dropCStyleComment (count + 1) xs
      (_x:xs) -> dropCStyleComment count xs
      [] -> []

    goSpace :: Int -> Maybe Char -> String -> String
    goSpace !numParens mC xs = case dropWhile isSpace xs of
      ('-':'-':ys) -> goSpace numParens mC $ dropStdComment ys
      ('/':'*':ys) -> goSpace numParens mC $ dropCStyleComment 0 ys
      [] -> []
      ys -> case mC of
        Just c -> c : goSql numParens ys
        Nothing -> goSql numParens ys

    goSql :: Int -> String -> String
    goSql !numParens = \case
      ('"':xs) -> '"' : goIdentDQ numParens xs
      ('\'':xs) -> '\'' : goStringSQ numParens xs
      ('$':xs) -> '$' : goStringDSBegin numParens "" xs
      ('(':xs) -> '(' : goSql (numParens + 1) xs
      (')':xs) -> ')' : goSql (max 0 $ numParens - 1) xs
      (';':xs)
        | numParens > 0 -> ';' : goSql numParens xs
        | otherwise -> ';' : goSpace numParens (Just '\n') xs
      ('-':'-':xs) -> goSpace numParens (Just ' ') $ dropStdComment xs
      ('/':'*':xs) -> goSpace numParens (Just ' ') $ dropCStyleComment 0 xs
      ('E':'\'':xs) -> 'E' : '\'' : goStringE numParens xs
      (x:xs)
        | isSpace x -> goSpace numParens (Just ' ') xs
        | otherwise -> x : goSql numParens xs
      [] -> []

    goIdentDQ :: Int -> String -> String
    goIdentDQ !numParens = \case
      ('"':'"':xs) -> '"' : '"' : goIdentDQ numParens xs
      ('"':xs) -> '"' : goSql numParens xs
      (x:xs) -> x : goIdentDQ numParens xs
      [] -> []

    goStringSQ :: Int -> String -> String
    goStringSQ !numParens = \case
      ('\'':'\'':xs) -> '\'' : '\'' : goStringSQ numParens xs
      ('\'':xs) -> goStringSQEnd numParens False xs
      (x:xs) -> x : goStringSQ numParens xs
      [] -> []

    goStringSQEnd :: Int -> Bool -> String -> String
    goStringSQEnd !numParens hasSpace = \case
      ('\n':xs) -> goStringSQEndNewline numParens xs
      ('-':'-':xs) ->
        -- avoid unused @hasSpace@
        goStringSQEndNewline numParens $ dropStdComment xs
      ('/':'*':xs) -> goStringSQEnd numParens True $ dropCStyleComment 0 xs
      xs@(y:ys)
        | isSpace y -> goStringSQEnd numParens True ys
        | hasSpace -> '\'' : ' ' : goSql numParens xs
        | otherwise -> '\'' : goSql numParens xs
      [] -> "'"

    goStringSQEndNewline :: Int -> String -> String
    goStringSQEndNewline !numParens xs = case dropWhile isSpace xs of
      ('\'':ys) -> goStringSQ numParens ys
      ('-':'-':ys) -> goStringSQEndNewline numParens $ dropStdComment ys
      ('/':'*':ys) -> goStringSQEndNewline numParens $ dropCStyleComment 0 ys
      [] -> "'"
      ys -> '\'' : ' ' : goSql numParens ys

    goStringE :: Int -> String -> String
    goStringE !numParens = \case
      ('\\':x:xs) -> '\\' : x : goStringE numParens xs
      ('\'':'\'':xs) -> '\'' : '\'' : goStringE numParens xs
      ('\'':xs) -> '\'' : goSql numParens xs
      (x:xs) -> x : goStringE numParens xs
      [] -> []

    goStringDSBegin :: Int -> String -> String -> String
    goStringDSBegin !numParens acc = \case
      ('$':xs) -> '$' : goStringDS numParens (reverse acc) xs
      (x:xs) -> x : goStringDSBegin numParens (x : acc) xs
      [] -> []

    goStringDS :: Int -> String -> String -> String
    goStringDS !numParens tag = \case
      ('$':xs) -> '$' : goStringDSEnd numParens tag (tag, xs)
      (x:xs) -> x : goStringDS numParens tag xs
      [] -> []

    goStringDSEnd :: Int -> String -> (String, String) -> String
    goStringDSEnd !numParens tag = \case
      (t:ts, x:xs)
        | x == t -> x : goStringDSEnd numParens tag (ts, xs)
        | x == '$' -> x : goStringDSEnd numParens tag (tag, xs)
        | otherwise -> x : goStringDS numParens tag xs
      ([], '$':xs) -> '$' : goSql numParens xs
      (_ts, xs) -> goStringDS numParens tag xs
