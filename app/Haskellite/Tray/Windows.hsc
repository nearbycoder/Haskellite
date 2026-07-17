{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Tray.Windows
  ( SystemTray
  , startSystemTray
  , stopSystemTray
  ) where

#include <SDL.h>
#include <SDL_syswm.h>
#include <windows.h>
#include <shellapi.h>

import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, when)
import Data.Bits ((.|.))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word8, Word16, Word32)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign
  ( FunPtr
  , Ptr
  , allocaBytesAligned
  , castFunPtrToPtr
  , castPtr
  , castPtrToFunPtr
  , freeHaskellFunPtr
  , intPtrToPtr
  , nullPtr
  , peekByteOff
  , peekElemOff
  , plusPtr
  , pokeByteOff
  , pokeElemOff
  , ptrToIntPtr
  , wordPtrToPtr
  )
import Foreign.Ptr (IntPtr (..), WordPtr (..))
import Foreign.C.String (CWchar, withCWString)
import Foreign.C.Types (CInt (..), CUInt (..))
import Foreign.Marshal.Utils (fillBytes)
import qualified SDL
import SDL.Internal.Types (Window (..))

data SystemTray = SystemTray
  { trayWindowHandle :: Ptr ()
  , trayOldWindowProcedure :: FunPtr WindowProcedure
  , trayWindowProcedure :: FunPtr WindowProcedure
  }

type WindowProcedure = Ptr () -> Word32 -> WordPtr -> IntPtr -> IO IntPtr

startSystemTray :: SDL.Window -> IO () -> IO () -> IO (Either Text SystemTray)
startSystemTray (Window rawWindow) onShow onQuit = do
  outcome <- try $ do
    windowHandle <- getNativeWindow (castPtr rawWindow)
    oldProcedureRef <- newIORef Nothing
    callback <- makeWindowProcedure $ \window message wParam lParam -> do
      if message == trayCallbackMessage
        then do
          case fromIntegral lParam :: Word32 of
            event | event == #{const WM_LBUTTONUP} -> onShow
            event | event == #{const WM_RBUTTONUP} -> onShow
            event | event == #{const WM_MBUTTONUP} -> onQuit
            _ -> pure ()
          pure 0
        else do
          oldProcedure <- readIORef oldProcedureRef
          maybe (pure 0) (\procedure -> callWindowProcedure procedure window message wParam lParam) oldProcedure
    oldValue <- setWindowLongPointer windowHandle #{const GWLP_WNDPROC} (ptrToIntPtr $ castFunPtrToPtr callback)
    let oldProcedure = castPtrToFunPtr (intPtrToPtr oldValue)
    writeIORef oldProcedureRef (Just oldProcedure)
    added <- withNotifyData windowHandle $ shellNotifyIcon #{const NIM_ADD}
    when (added == 0) $ do
      _ <- setWindowLongPointer windowHandle #{const GWLP_WNDPROC} oldValue
      freeHaskellFunPtr callback
      ioError (userError "Windows could not add the notification-area icon")
    pure $ SystemTray windowHandle oldProcedure callback
  pure $ either (Left . Text.pack . displayException) Right (outcome :: Either SomeException SystemTray)

stopSystemTray :: SystemTray -> IO ()
stopSystemTray tray = do
  _ <- withNotifyData (trayWindowHandle tray) $ shellNotifyIcon #{const NIM_DELETE}
  _ <-
    setWindowLongPointer
      (trayWindowHandle tray)
      #{const GWLP_WNDPROC}
      (ptrToIntPtr . castFunPtrToPtr $ trayOldWindowProcedure tray)
  freeHaskellFunPtr (trayWindowProcedure tray)

getNativeWindow :: Ptr () -> IO (Ptr ())
getNativeWindow window =
  allocaBytesAligned sysWmInfoSize sysWmInfoAlignment $ \info -> do
    fillBytes info 0 sysWmInfoSize
    pokeByteOff info versionMajorOffset (#{const SDL_MAJOR_VERSION} :: Word8)
    pokeByteOff info versionMinorOffset (#{const SDL_MINOR_VERSION} :: Word8)
    pokeByteOff info versionPatchOffset (#{const SDL_PATCHLEVEL} :: Word8)
    ready <- getWindowWmInfo window info
    when (ready == 0) $ ioError (userError "SDL could not expose the native Windows window")
    peekByteOff info nativeWindowOffset

withNotifyData :: Ptr () -> (Ptr () -> IO a) -> IO a
withNotifyData windowHandle action =
  allocaBytesAligned notifyDataSize notifyDataAlignment $ \notifyData -> do
    fillBytes notifyData 0 notifyDataSize
    pokeByteOff notifyData notifySizeOffset (fromIntegral notifyDataSize :: Word32)
    pokeByteOff notifyData notifyWindowOffset windowHandle
    pokeByteOff notifyData notifyIdOffset (1 :: CUInt)
    pokeByteOff notifyData notifyFlagsOffset (#{const NIF_MESSAGE} .|. #{const NIF_ICON} .|. #{const NIF_TIP} :: CUInt)
    pokeByteOff notifyData notifyCallbackOffset (trayCallbackMessage :: Word32)
    icon <- loadIcon nullPtr (wordPtrToPtr 32512)
    pokeByteOff notifyData notifyIconOffset icon
    withCWString "Haskellite — local voice dictation" $ \source -> do
      let destination = castPtr (notifyData `plusPtr` notifyTipOffset) :: Ptr CWchar
      copyWideString destination source 127
    action notifyData

copyWideString :: Ptr CWchar -> Ptr CWchar -> Int -> IO ()
copyWideString destination source maximumLength = go 0
  where
    go index = do
      character <- peekElemOff source index
      pokeElemOff destination index character
      when (character /= 0 && index < maximumLength) $ go (index + 1)

trayCallbackMessage :: Word32
trayCallbackMessage = #{const WM_APP} + 42

sysWmInfoSize, sysWmInfoAlignment, versionMajorOffset, versionMinorOffset, versionPatchOffset, nativeWindowOffset :: Int
sysWmInfoSize = #{size SDL_SysWMinfo}
sysWmInfoAlignment = #{alignment SDL_SysWMinfo}
versionMajorOffset = #{offset SDL_SysWMinfo, version.major}
versionMinorOffset = #{offset SDL_SysWMinfo, version.minor}
versionPatchOffset = #{offset SDL_SysWMinfo, version.patch}
nativeWindowOffset = #{offset SDL_SysWMinfo, info.win.window}

notifyDataSize, notifyDataAlignment, notifySizeOffset, notifyWindowOffset, notifyIdOffset, notifyFlagsOffset, notifyCallbackOffset, notifyIconOffset, notifyTipOffset :: Int
notifyDataSize = #{size NOTIFYICONDATAW}
notifyDataAlignment = #{alignment NOTIFYICONDATAW}
notifySizeOffset = #{offset NOTIFYICONDATAW, cbSize}
notifyWindowOffset = #{offset NOTIFYICONDATAW, hWnd}
notifyIdOffset = #{offset NOTIFYICONDATAW, uID}
notifyFlagsOffset = #{offset NOTIFYICONDATAW, uFlags}
notifyCallbackOffset = #{offset NOTIFYICONDATAW, uCallbackMessage}
notifyIconOffset = #{offset NOTIFYICONDATAW, hIcon}
notifyTipOffset = #{offset NOTIFYICONDATAW, szTip}

foreign import ccall unsafe "SDL_GetWindowWMInfo"
  getWindowWmInfo :: Ptr () -> Ptr () -> IO CInt

foreign import stdcall "wrapper"
  makeWindowProcedure :: WindowProcedure -> IO (FunPtr WindowProcedure)

foreign import stdcall unsafe "CallWindowProcW"
  callWindowProcedure :: FunPtr WindowProcedure -> WindowProcedure

foreign import stdcall unsafe "SetWindowLongPtrW"
  setWindowLongPointer :: Ptr () -> CInt -> IntPtr -> IO IntPtr

foreign import stdcall unsafe "Shell_NotifyIconW"
  shellNotifyIcon :: Word32 -> Ptr () -> IO CInt

foreign import stdcall unsafe "LoadIconW"
  loadIcon :: Ptr () -> Ptr Word16 -> IO (Ptr ())
