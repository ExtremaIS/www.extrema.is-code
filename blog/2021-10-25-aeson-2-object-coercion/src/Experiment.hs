{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Experiment (unsafeAesonToGVal) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key as AK
import qualified Data.Aeson.KeyMap as AKM
#endif

-- https://hackage.haskell.org/package/base
#if MIN_VERSION_aeson(2,0,0)
import Data.Bifunctor (bimap)
import Unsafe.Coerce (unsafeCoerce)
#endif

-- https://hackage.haskell.org/package/containers
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
#endif

-- https://hackage.haskell.org/package/ginger
import qualified Text.Ginger as Ginger

-- https://hackage.haskell.org/package/text
#if MIN_VERSION_aeson(2,0,0)
import Data.Text (Text)
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
      A.Number n -> Ginger.toGVal n
      A.String s -> Ginger.toGVal s
      A.Bool b -> Ginger.toGVal b
      A.Null -> Ginger.toGVal ()
      A.Array a -> Ginger.toGVal . map goHashMapText $ Vector.toList a
      A.Object o -> Ginger.toGVal $
        HashMap.map goHashMapText (unsafeCoerce o :: HashMap Text A.Value)

    goMapText :: A.Value -> Ginger.GVal m
    goMapText v = setAsJSON v $ case v of
      A.Number n -> Ginger.toGVal n
      A.String s -> Ginger.toGVal s
      A.Bool b -> Ginger.toGVal b
      A.Null -> Ginger.toGVal ()
      A.Array a -> Ginger.toGVal . map goMapText $ Vector.toList a
      A.Object o -> Ginger.toGVal $
        Map.map goMapText (unsafeCoerce o :: Map Text A.Value)

    goHashMap :: A.Value -> Ginger.GVal m
    goHashMap v = setAsJSON v $ case v of
      A.Number n -> Ginger.toGVal n
      A.String s -> Ginger.toGVal s
      A.Bool b -> Ginger.toGVal b
      A.Null -> Ginger.toGVal ()
      A.Array a -> Ginger.toGVal . map goHashMap $ Vector.toList a
      A.Object o ->
        Ginger.toGVal . HashMap.fromList . map (bimap AK.toText goHashMap) $
          AKM.toList o

    goMap :: A.Value -> Ginger.GVal m
    goMap v = setAsJSON v $ case v of
      A.Number n -> Ginger.toGVal n
      A.String s -> Ginger.toGVal s
      A.Bool b -> Ginger.toGVal b
      A.Null -> Ginger.toGVal ()
      A.Array a -> Ginger.toGVal . map goMap $ Vector.toList a
      A.Object o ->
        Ginger.toGVal . Map.fromList . map (bimap AK.toText goMap) $
          AKM.toList o
#else
unsafeAesonToGVal = Ginger.toGVal
#endif

------------------------------------------------------------------------------

#if MIN_VERSION_aeson(2,0,0)

setAsJSON :: A.Value -> Ginger.GVal m -> Ginger.GVal m
setAsJSON v gv = gv { Ginger.asJSON = Just v }
{-# INLINE setAsJSON #-}

#endif
