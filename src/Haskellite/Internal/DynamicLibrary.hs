{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Haskellite.Internal.DynamicLibrary
  ( DynamicLibrary
  , closeDynamicLibrary
  , loadDynamicLibrary
  , loadSymbol
  ) where

import Foreign.Ptr (FunPtr)

#if defined(mingw32_HOST_OS)
import Control.Exception (throwIO)
import Foreign.C.String (CString, withCString, withCWString)
import Foreign.C.Types (CInt (..), CWchar)
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr)

newtype DynamicLibrary = DynamicLibrary (Ptr ())

foreign import stdcall unsafe "windows.h LoadLibraryW"
  cLoadLibraryW :: Ptr CWchar -> IO (Ptr ())

foreign import stdcall unsafe "windows.h GetProcAddress"
  cGetProcAddress :: Ptr () -> CString -> IO (FunPtr a)

foreign import stdcall unsafe "windows.h FreeLibrary"
  cFreeLibrary :: Ptr () -> IO CInt

loadDynamicLibrary :: FilePath -> IO DynamicLibrary
loadDynamicLibrary path = withCWString path $ \widePath -> do
  handle <- cLoadLibraryW widePath
  if handle == nullPtr
    then throwIO . userError $ "Could not load dynamic library: " <> path
    else pure (DynamicLibrary handle)

loadSymbol :: DynamicLibrary -> String -> IO (FunPtr a)
loadSymbol (DynamicLibrary handle) name = withCString name $ \symbolName -> do
  symbol <- cGetProcAddress handle symbolName
  if symbol == nullFunPtr
    then throwIO . userError $ "Dynamic library does not export " <> name
    else pure symbol

closeDynamicLibrary :: DynamicLibrary -> IO ()
closeDynamicLibrary (DynamicLibrary handle) = do
  _ <- cFreeLibrary handle
  pure ()
#else
import System.Posix.DynamicLinker
  ( DL
  , RTLDFlags (RTLD_GLOBAL, RTLD_NOW)
  , dlclose
  , dlopen
  , dlsym
  )

newtype DynamicLibrary = DynamicLibrary DL

loadDynamicLibrary :: FilePath -> IO DynamicLibrary
loadDynamicLibrary path = DynamicLibrary <$> dlopen path [RTLD_NOW, RTLD_GLOBAL]

loadSymbol :: DynamicLibrary -> String -> IO (FunPtr a)
loadSymbol (DynamicLibrary handle) = dlsym handle

closeDynamicLibrary :: DynamicLibrary -> IO ()
closeDynamicLibrary (DynamicLibrary handle) = dlclose handle
#endif
