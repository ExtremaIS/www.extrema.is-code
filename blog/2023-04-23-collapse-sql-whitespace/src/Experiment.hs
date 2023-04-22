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
-- whitespace and comments after a semicolon are collapsed to a newline.  All
-- other consecutive whitespace and comments are collapsed to a space.
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
--
-- Reference:
--
-- * <https://www.postgresql.org/docs/current/sql-syntax-lexical.html>
sqlCollapseWhitespace :: String -> String
sqlCollapseWhitespace = goSpace Nothing
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

    goSpace :: Maybe Char -> String -> String
    goSpace mC xs = case dropWhile isSpace xs of
      ('-':'-':ys) -> goSpace mC $ dropStdComment ys
      ('/':'*':ys) -> goSpace mC $ dropCStyleComment 0 ys
      [] -> []
      ys -> case mC of
        Just c -> c : goSql ys
        Nothing -> goSql ys

    goSql :: String -> String
    goSql = \case
      ('"':xs) -> '"' : goIdentDQ xs
      ('\'':xs) -> '\'' : goStringSQ xs
      ('$':xs) -> '$' : goStringDSBegin "" xs
      (';':xs) -> ';' : goSpace (Just '\n') xs
      ('-':'-':xs) -> goSpace (Just ' ') $ dropStdComment xs
      ('/':'*':xs) -> goSpace (Just ' ') $ dropCStyleComment 0 xs
      ('E':'\'':xs) -> 'E' : '\'' : goStringE xs
      (x:xs)
        | isSpace x -> goSpace (Just ' ') xs
        | otherwise -> x : goSql xs
      [] -> []

    goIdentDQ :: String -> String
    goIdentDQ = \case
      ('"':'"':xs) -> '"' : '"' : goIdentDQ xs
      ('"':xs) -> '"' : goSql xs
      (x:xs) -> x : goIdentDQ xs
      [] -> []

    goStringSQ :: String -> String
    goStringSQ = \case
      ('\'':'\'':xs) -> '\'' : '\'' : goStringSQ xs
      ('\'':xs) -> goStringSQEnd False xs
      (x:xs) -> x : goStringSQ xs
      [] -> []

    goStringSQEnd :: Bool -> String -> String
    goStringSQEnd hasSpace = \case
      ('\n':xs) -> goStringSQEndNewline xs
      ('-':'-':xs) ->
        -- avoid unused @hasSpace@
        goStringSQEndNewline $ dropStdComment xs
      ('/':'*':xs) -> goStringSQEnd True $ dropCStyleComment 0 xs
      xs@(y:ys)
        | isSpace y -> goStringSQEnd True ys
        | hasSpace -> '\'' : ' ' : goSql xs
        | otherwise -> '\'' : goSql xs
      [] -> "'"

    goStringSQEndNewline :: String -> String
    goStringSQEndNewline xs = case dropWhile isSpace xs of
      ('\'':ys) -> goStringSQ ys
      ('-':'-':ys) -> goStringSQEndNewline $ dropStdComment ys
      ('/':'*':ys) -> goStringSQEndNewline $ dropCStyleComment 0 ys
      [] -> "'"
      ys -> '\'' : ' ' : goSql ys

    goStringE :: String -> String
    goStringE = \case
      ('\\':x:xs) -> '\\' : x : goStringE xs
      ('\'':'\'':xs) -> '\'' : '\'' : goStringE xs
      ('\'':xs) -> '\'' : goSql xs
      (x:xs) -> x : goStringE xs
      [] -> []

    goStringDSBegin :: String -> String -> String
    goStringDSBegin acc = \case
      ('$':xs) -> '$' : goStringDS (reverse acc) xs
      (x:xs) -> x : goStringDSBegin (x : acc) xs
      [] -> []

    goStringDS :: String -> String -> String
    goStringDS tag = \case
      ('$':xs) -> '$' : goStringDSEnd tag (tag, xs)
      (x:xs) -> x : goStringDS tag xs
      [] -> []

    goStringDSEnd :: String -> (String, String) -> String
    goStringDSEnd tag = \case
      (t:ts, x:xs)
        | x == t -> x : goStringDSEnd tag (ts, xs)
        | x == '$' -> x : goStringDSEnd tag (tag, xs)
        | otherwise -> x : goStringDS tag xs
      ([], '$':xs) -> '$' : goSql xs
      (_ts, xs) -> goStringDS tag xs
