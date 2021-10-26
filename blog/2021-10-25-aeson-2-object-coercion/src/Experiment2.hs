{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Experiment2 (unsafeAesonToGVal) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
#endif

-- https://hackage.haskell.org/package/base
#if MIN_VERSION_aeson(2,0,0)
import Data.Bifunctor (bimap)
import Text.Read (readMaybe)
import Unsafe.Coerce (unsafeCoerce)
#endif

-- https://hackage.haskell.org/package/containers
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
#endif

-- https://hackage.haskell.org/package/data-default-class
#if MIN_VERSION_aeson(2,0,0)
import Data.Default.Class (def)
#endif

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger
#if MIN_VERSION_aeson(2,0,0)
import qualified Text.Ginger.Html as Html
#endif

-- https://hackage.haskell.org/package/scientific
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Scientific as Sci
import Data.Scientific (Scientific)
#endif

-- https://hackage.haskell.org/package/text
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
#endif

-- https://hackage.haskell.org/package/unordered-containers
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.HashMap.Strict as HashMap
import Data.HashMap.Strict (HashMap)
#endif

-- https://hackage.haskell.org/package/vector
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Vector as Vector
#endif

------------------------------------------------------------------------------

unsafeAesonToGVal :: forall m. A.Value -> Ginger.GVal m
#if MIN_VERSION_aeson(2,0,0)
unsafeAesonToGVal =
    case (AK.coercionToText, AKM.coercionToHashMap, AKM.coercionToMap) of
      (Just{}, Just{}, _mMapCoercion) -> goHashMapText
      (Just{}, _mHashMapCoercion, Just{}) -> goMapText
      (Nothing, Just{}, _mMapCoercion) -> goHashMap
      (_mTextCoercion, _mHashMapCoercion, _mMapCoercion) -> goMap
  where
    goHashMapText :: A.Value -> Ginger.GVal m
    goHashMapText v = setAsJSON v $ case v of
      A.Number n -> scientificToGVal n
      A.String s -> textToGVal s
      A.Bool b -> boolToGVal b
      A.Null -> def
      A.Array a -> listToGVal . map goHashMapText $ Vector.toList a
      A.Object o -> hashMapToGVal $
        HashMap.map goHashMapText (unsafeCoerce o :: HashMap Text A.Value)

    goMapText :: A.Value -> Ginger.GVal m
    goMapText v = setAsJSON v $ case v of
      A.Number n -> scientificToGVal n
      A.String s -> textToGVal s
      A.Bool b -> boolToGVal b
      A.Null -> def
      A.Array a -> listToGVal . map goMapText $ Vector.toList a
      A.Object o ->
        mapToGVal $ Map.map goMapText (unsafeCoerce o :: Map Text A.Value)

    goHashMap :: A.Value -> Ginger.GVal m
    goHashMap v = setAsJSON v $ case v of
      A.Number n -> scientificToGVal n
      A.String s -> textToGVal s
      A.Bool b -> boolToGVal b
      A.Null -> def
      A.Array a -> listToGVal . map goHashMap $ Vector.toList a
      A.Object o ->
        hashMapToGVal . HashMap.fromList . map (bimap AK.toText goHashMap) $
          AKM.toList o

    goMap :: A.Value -> Ginger.GVal m
    goMap v = setAsJSON v $ case v of
      A.Number n -> scientificToGVal n
      A.String s -> textToGVal s
      A.Bool b -> boolToGVal b
      A.Null -> def
      A.Array a -> listToGVal . map goMap $ Vector.toList a
      A.Object o ->
        mapToGVal . Map.fromList . map (bimap AK.toText goMap) $ AKM.toList o
#else
unsafeAesonToGVal = Ginger.toGVal
#endif

------------------------------------------------------------------------------

#if MIN_VERSION_aeson(2,0,0)

boolToGVal :: Bool -> Ginger.GVal m
boolToGVal b = def
    { Ginger.asHtml = Html.html $ if b then "1" else ""
    , Ginger.asText = if b then "1" else ""
    , Ginger.asBoolean = b
    , Ginger.asBytes = Just $ if b then "1" else "0"
    , Ginger.asNumber = Just $ if b then 1 else 0
    , Ginger.isNull = False
    , Ginger.asJSON = Just (A.Bool b)
    }
{-# INLINE boolToGVal #-}

hashMapToGVal :: HashMap Text (Ginger.GVal m) -> Ginger.GVal m
hashMapToGVal map' = def
    { Ginger.asHtml = mconcat . map Ginger.asHtml . HashMap.elems $ map'
    , Ginger.asText = mconcat . map Ginger.asText . HashMap.elems $ map'
    , Ginger.asBytes = mconcat . map Ginger.asBytes . HashMap.elems $ map'
    , Ginger.asBoolean = not . HashMap.null $ map'
    , Ginger.isNull = False
    , Ginger.asLookup = Just (`HashMap.lookup` map')
    , Ginger.asDictItems = Just $ HashMap.toList map'
    }
{-# INLINE hashMapToGVal #-}

listToGVal :: [Ginger.GVal m] -> Ginger.GVal m
listToGVal vs = def
    { Ginger.asHtml = mconcat . map Ginger.asHtml $ vs
    , Ginger.asText = mconcat . map Ginger.asText $ vs
    , Ginger.asBytes = mconcat . map Ginger.asBytes $ vs
    , Ginger.asBoolean = not . null $ vs
    , Ginger.isNull = False
    , Ginger.asList = Just vs
    , Ginger.length = Just $ length vs
    }
{-# INLINE listToGVal #-}

mapToGVal :: Map Text (Ginger.GVal m) -> Ginger.GVal m
mapToGVal map' = def
    { Ginger.asHtml = mconcat . map Ginger.asHtml . Map.elems $ map'
    , Ginger.asText = mconcat . map Ginger.asText . Map.elems $ map'
    , Ginger.asBytes = mconcat . map Ginger.asBytes . Map.elems $ map'
    , Ginger.asBoolean = not . Map.null $ map'
    , Ginger.isNull = False
    , Ginger.asLookup = Just (`Map.lookup` map')
    , Ginger.asDictItems = Just $ Map.toAscList map'
    }
{-# INLINE mapToGVal #-}

scientificToGVal :: Scientific -> Ginger.GVal m
scientificToGVal sci = def
    { Ginger.asHtml = Html.html $ scientificToText sci
    , Ginger.asText = scientificToText sci
    , Ginger.asBytes = Just . TE.encodeUtf8 . scientificToText $ sci
    , Ginger.asBoolean = sci /= 0
    , Ginger.asNumber = Just sci
    , Ginger.isNull = False
    }
{-# INLINE scientificToGVal #-}

scientificToText :: Scientific -> Text
scientificToText
    = T.pack
    . either (show @Double) (show @Integer)
    . Sci.floatingOrInteger
{-# INLINE scientificToText #-}

setAsJSON :: A.Value -> Ginger.GVal m -> Ginger.GVal m
setAsJSON v gv = gv { Ginger.asJSON = Just v }
{-# INLINE setAsJSON #-}

textToGVal :: Text -> Ginger.GVal m
textToGVal t = def
    { Ginger.asHtml = Html.html t
    , Ginger.asText = t
    , Ginger.asBytes = Just . TE.encodeUtf8 $ t
    , Ginger.asBoolean = not $ T.null t
    , Ginger.asNumber = readMaybe . T.unpack $ t
    , Ginger.isNull = False
    }
{-# INLINE textToGVal #-}

#endif
