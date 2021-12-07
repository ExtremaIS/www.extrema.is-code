{-# LANGUAGE CPP #-}

module FFISync (sync) where

-- (ffi-sync)
#ifndef mingw32_HOST_OS
import FFISync.Unistd (c_sync)
#endif

------------------------------------------------------------------------------

sync :: IO ()
#ifdef mingw32_HOST_OS
sync = pure ()
#else
sync = c_sync
#endif
