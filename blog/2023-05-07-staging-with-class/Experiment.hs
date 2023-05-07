{-# LANGUAGE TemplateHaskell #-}

module Experiment where

-- https://hackage.haskell.org/package/template-haskell
import Language.Haskell.TH.Syntax (Code, Quote)

------------------------------------------------------------------------------

qpower
  :: Quote m
  => Int
  -> Code m Int
  -> Code m Int
qpower 0 _n = [|| 1 ||]
qpower k  n = [|| $$n * $$(qpower (k - 1) n) ||]
