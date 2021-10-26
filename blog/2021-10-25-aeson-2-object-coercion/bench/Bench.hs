{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

-- https://hackage.haskell.org/package/base
import Data.Functor.Identity (runIdentity)

-- https://hackage.haskell.org/package/gauge
import qualified Gauge

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger
import Text.Ginger.Html (Html, htmlSource)

-- https://hackage.haskell.org/package/mtl
import Control.Monad.Writer.Lazy (Writer)

-- https://hackage.haskell.org/package/text
import Data.Text (Text)

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (experiment)
import qualified Experiment
import qualified Experiment2

------------------------------------------------------------------------------

type GingerM = Ginger.Run Ginger.SourcePos (Writer Html) Html

type GVal = Ginger.GVal GingerM

------------------------------------------------------------------------------

template :: Ginger.Template Ginger.SourcePos
template
    = either (error . show) id
    . runIdentity
    . Ginger.parseGinger (const $ pure Nothing) Nothing
    $ unlines
        [ "<html><head><title>experiment</title></head><body>"
        , "<h1>Numbers</h1>"
        , "{% for lang in languages %}"
        , "<div><h2 id=\"{{ lang.code }}\">{{ lang.label }}</h2><dl>"
        , "{% for num in lang.numbers %}"
        , "<dt>{{ num.number }}</dt><dd>{{ num.label }}</dd>"
        , "{% endfor %}"
        , "</dl></div>"
        , "{% endfor %}"
        , "</body></html>"
        ]

------------------------------------------------------------------------------

render :: GVal -> Text
render = htmlSource . (`Ginger.easyRender` template)

------------------------------------------------------------------------------

main :: IO ()
main = Gauge.defaultMain
    [ Gauge.env (Yaml.decodeFileThrow "l10n.yaml") $ \l10n ->
        Gauge.bgroup "l10n"
          [ Gauge.bench "ginger" $
              Gauge.nf (render . Ginger.toGVal) l10n
          , Gauge.bench "unsafeAesonToGVal (1)" $
              Gauge.nf (render . Experiment.unsafeAesonToGVal) l10n
          , Gauge.bench "unsafeAesonToGVal (2)" $
              Gauge.nf (render . Experiment2.unsafeAesonToGVal) l10n
          ]
    ]
