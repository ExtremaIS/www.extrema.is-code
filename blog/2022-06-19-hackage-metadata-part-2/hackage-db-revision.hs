#!/usr/bin/env stack
{- stack
  script
  --resolver lts-19.11
  --package Cabal
  --package containers
  --package hackage-db
-}

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import qualified Data.List as List
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/Cabal
import qualified Distribution.Types.GenericPackageDescription as GPD
import qualified Distribution.PackageDescription as PD
import Distribution.Types.PackageName (PackageName)
import qualified Distribution.Types.Version as Version
import Distribution.Types.Version (Version)

-- https://hackage.haskell.org/package/containers
import qualified Data.Map.Strict as Map

-- https://hackage.haskell.org/package/hackage-db
import qualified Distribution.Hackage.DB as DB
import Distribution.Hackage.DB (HackageDB)

------------------------------------------------------------------------------

newtype Revision = Revision Int
  deriving (Eq, Ord, Show)

defaultRevision :: Revision
defaultRevision = Revision 0

lookupRevision
  :: HackageDB
  -> PackageName
  -> Version
  -> Either String Revision
lookupRevision db packageName version = do
    packageData <- maybe (Left "package not found") Right $
      Map.lookup packageName db
    versionData <- maybe (Left "version not found") Right $
      Map.lookup version packageData
    let mRevisionString
          = List.lookup revisionField
          . PD.customFieldsPD
          . GPD.packageDescription
          $ DB.cabalFile versionData
    case mRevisionString of
      Just revisionString -> case readMaybe revisionString of
        Just revision -> pure $ Revision revision
        Nothing -> Left $ "unable to parse revision: " ++ revisionString
      Nothing -> pure defaultRevision
  where
    revisionField :: String
    revisionField = "x-revision"

------------------------------------------------------------------------------

main :: IO ()
main = do
    db <- DB.readTarball Nothing =<< DB.hackageTarball
    print . lookupRevision db "bm" $ Version.mkVersion [0,1,0,2]
