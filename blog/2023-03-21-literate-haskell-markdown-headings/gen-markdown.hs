{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- https://hackage.haskell.org/package/conduit
import qualified Conduit as C
import Data.Conduit ((.|))

-- https://hackage.haskell.org/package/literatex
import qualified LiterateX
import qualified LiterateX.Renderer as Renderer
import qualified LiterateX.Types.SourceFormat as SourceFormat

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T
import Data.Text (Text)

------------------------------------------------------------------------------

unescapeMarkdownHeadings :: Monad m => C.ConduitT Text Text m ()
unescapeMarkdownHeadings = C.awaitForever $ \case
    line
      | "\\#" `T.isPrefixOf` line -> C.yield $ T.tail line
      | otherwise -> C.yield line

main :: IO ()
main =
    LiterateX.runResource
      SourceFormat.LiterateHaskell
      (Renderer.defaultOptionsFor "haskell")
      (LiterateX.sourceFile "Demo.lhs")
      (unescapeMarkdownHeadings .| LiterateX.sinkFile "Demo.md")
