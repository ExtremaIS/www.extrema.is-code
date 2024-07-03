------------------------------------------------------------------------------
-- |
-- Module      : Demo1.FeatureFlag
-- Description : Simple feature flags
------------------------------------------------------------------------------

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}

module Demo1.FeatureFlag
  ( -- * Current Feature Flags
    FeatureFlag(..)
    -- * Feature Flag Configuration
  , FeatureFlagMap
  , lookup
  , lookupT
  , lookupF
    -- * Global Storage
  , initialize
    -- * Usage API
  , MonadFeatureFlags(..)
  , lookupM
  , lookupTM
  , lookupFM
  , HasFeatureFlags(..)
  ) where

-- https://hackage.haskell.org/package/base
import qualified Control.Concurrent.MVar as MVar
import Control.Concurrent.MVar (MVar)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Prelude hiding (lookup, read)
import System.IO.Unsafe (unsafePerformIO)

-- https://hackage.haskell.org/package/containers
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Reader (ReaderT, asks)

------------------------------------------------------------------------------
-- Current Feature Flags
------------------------------------------------------------------------------

-- | Currently-supported feature flags
--
-- Note that the constructor names purposefully do not follow usual Haskell
-- conventions.  In this demo, there are two types of feature flags:
--
-- * Bug fixes have a @Fix@ prefix.
-- * Refactors have a @Ref@ prefix.
--
-- The constructors encode the month, a concise description, and (optionally)
-- an issue/ticket number.
data FeatureFlag
  = Fix_202407_SomeBug_42
  | Ref_202407_SomeBusinessLogic
  deriving (Eq, Ord, Show)

------------------------------------------------------------------------------
-- Feature Flag Configuration
------------------------------------------------------------------------------

-- | A snapshot of feature flag configuration
--
-- A 'Map' is used (instead of a set) so that it is possible to distinguish
-- when a feature flag is not configured.
type FeatureFlagMap = Map FeatureFlag Bool

-- | Lookup a specific feature flag
lookup :: FeatureFlag -> FeatureFlagMap -> Maybe Bool
lookup = Map.lookup

-- | Lookup a specific feature flag, returning 'True' if not found
lookupT :: FeatureFlag -> FeatureFlagMap -> Bool
lookupT = Map.findWithDefault True

-- | Lookup a specific feature flag, returning 'False' if not found
lookupF :: FeatureFlag -> FeatureFlagMap -> Bool
lookupF = Map.findWithDefault False

------------------------------------------------------------------------------
-- Global Storage
------------------------------------------------------------------------------

-- | Global storage for feature flag configuration
globalFeatureFlagMap :: MVar FeatureFlagMap
globalFeatureFlagMap = unsafePerformIO MVar.newEmptyMVar
{-# NOINLINE globalFeatureFlagMap #-}

-- | Initialize the global storage
--
-- The global storage is initialized exactly once in production.  To support
-- testing, however, this function replaces any existing configuration.
initialize :: MonadIO m => FeatureFlagMap -> m ()
initialize ffMap = liftIO $ do
    isSuccess <- MVar.tryPutMVar globalFeatureFlagMap ffMap
    unless isSuccess . void $ MVar.swapMVar globalFeatureFlagMap ffMap

------------------------------------------------------------------------------
-- Usage API
------------------------------------------------------------------------------

-- | Core API for feature flag usage
--
-- This abstraction allows consistent usage with implementations that vary
-- depending on the monad.
--
-- The default implementation is to read the current feature flag
-- configuration from the global storage.  It is implemented as a default so
-- that any other 'MonadIO' monads that should do this can declare an instance
-- trivially and consistently.
class Monad m => MonadFeatureFlags m where
  -- | Get the feature flag configuration
  readM :: m FeatureFlagMap

  default readM :: MonadIO m => m FeatureFlagMap
  readM = liftIO $ MVar.readMVar globalFeatureFlagMap

instance MonadFeatureFlags IO

-- | Lookup a specific feature flag
lookupM :: MonadFeatureFlags m => FeatureFlag -> m (Maybe Bool)
lookupM ff = lookup ff <$> readM

-- | Lookup a specific feature flag, returning 'True' if not found
lookupTM :: MonadFeatureFlags m => FeatureFlag -> m Bool
lookupTM ff = lookupT ff <$> readM

-- | Lookup a specific feature flag, returning 'False' if not found
lookupFM :: MonadFeatureFlags m => FeatureFlag -> m Bool
lookupFM ff = lookupF ff <$> readM

------------------------------------------------------------------------------

-- | Feature flags may be cached within a 'ReaderT' environment
--
-- When using a 'ReaderT' monad with an environment that has feature flags,
-- 'readM' returns those flags.
class HasFeatureFlags a where
  -- | Get the feature flags
  getFeatureFlags :: a -> FeatureFlagMap

instance HasFeatureFlags FeatureFlagMap where
  getFeatureFlags = id

instance (HasFeatureFlags env, Monad m)
    => MonadFeatureFlags (ReaderT env m) where
  readM = asks getFeatureFlags
