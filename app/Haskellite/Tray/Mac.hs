{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Tray.Mac
  ( SystemTray
  , startSystemTray
  , stopSystemTray
  ) where

import Control.Exception (SomeException, displayException, try)
import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text qualified as Text
import Foreign (FunPtr, Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..), CDouble (..), CSize (..))
import qualified SDL

data SystemTray = SystemTray
  { trayStatusBar :: Ptr ()
  , trayStatusItem :: Ptr ()
  , trayTarget :: Ptr ()
  , trayCallback :: FunPtr TrayCallback
  }

type TrayCallback = Ptr () -> Ptr () -> Ptr () -> IO ()

startSystemTray :: SDL.Window -> IO () -> IO () -> IO (Either Text SystemTray)
startSystemTray _ onShow _onQuit = do
  outcome <- try $ do
    clickedSelector <- selector "haskelliteTrayClicked:"
    baseClass <- objcClass "NSObject"
    trayClass <- withCString "HaskelliteTrayTarget" $ \name -> objcAllocateClassPair baseClass name 0
    when (trayClass == nullPtr) $ ioError (userError "Could not create the macOS tray target")
    callback <- makeTrayCallback $ \_ _ _ -> onShow
    added <- withCString "v@:@" $ \encoding -> classAddMethod trayClass clickedSelector callback encoding
    unless (added /= 0) $ do
      freeHaskellFunPtr callback
      ioError (userError "Could not install the macOS tray action")
    objcRegisterClassPair trayClass
    target <- classCreateInstance trayClass 0
    statusBarClass <- objcClass "NSStatusBar"
    systemStatusBarSelector <- selector "systemStatusBar"
    statusBar <- msgSendPointer statusBarClass systemStatusBarSelector
    itemSelector <- selector "statusItemWithLength:"
    statusItem <- msgSendDouble statusBar itemSelector (-1)
    buttonSelector <- selector "button"
    button <- msgSendPointer statusItem buttonSelector
    title <- nsString "● H"
    tooltip <- nsString "Haskellite — local voice dictation"
    setTitle <- selector "setTitle:"
    setTooltip <- selector "setToolTip:"
    setTarget <- selector "setTarget:"
    setAction <- selector "setAction:"
    msgSendPointerArgument button setTitle title
    msgSendPointerArgument button setTooltip tooltip
    msgSendPointerArgument button setTarget target
    msgSendPointerArgument button setAction clickedSelector
    pure $ SystemTray statusBar statusItem target callback
  pure $ either (Left . Text.pack . displayException) Right (outcome :: Either SomeException SystemTray)

stopSystemTray :: SystemTray -> IO ()
stopSystemTray tray = do
  removeSelector <- selector "removeStatusItem:"
  msgSendPointerArgument (trayStatusBar tray) removeSelector (trayStatusItem tray)
  objectDispose (trayTarget tray)
  freeHaskellFunPtr (trayCallback tray)

objcClass :: String -> IO (Ptr ())
objcClass name = withCString name $ \pointer -> do
  value <- objcGetClass pointer
  when (value == nullPtr) $ ioError . userError $ "Missing macOS class: " <> name
  pure value

selector :: String -> IO (Ptr ())
selector name = withCString name selRegisterName

nsString :: String -> IO (Ptr ())
nsString value = do
  stringClass <- objcClass "NSString"
  constructor <- selector "stringWithUTF8String:"
  withCString value $ msgSendCString stringClass constructor

foreign import ccall "wrapper"
  makeTrayCallback :: TrayCallback -> IO (FunPtr TrayCallback)

foreign import ccall unsafe "objc_getClass"
  objcGetClass :: CString -> IO (Ptr ())

foreign import ccall unsafe "sel_registerName"
  selRegisterName :: CString -> IO (Ptr ())

foreign import ccall unsafe "objc_allocateClassPair"
  objcAllocateClassPair :: Ptr () -> CString -> CSize -> IO (Ptr ())

foreign import ccall unsafe "objc_registerClassPair"
  objcRegisterClassPair :: Ptr () -> IO ()

foreign import ccall unsafe "class_addMethod"
  classAddMethod :: Ptr () -> Ptr () -> FunPtr TrayCallback -> CString -> IO CBool

foreign import ccall unsafe "class_createInstance"
  classCreateInstance :: Ptr () -> CSize -> IO (Ptr ())

foreign import ccall unsafe "object_dispose"
  objectDispose :: Ptr () -> IO ()

foreign import ccall unsafe "objc_msgSend"
  msgSendPointer :: Ptr () -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "objc_msgSend"
  msgSendDouble :: Ptr () -> Ptr () -> CDouble -> IO (Ptr ())

foreign import ccall unsafe "objc_msgSend"
  msgSendPointerArgument :: Ptr () -> Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "objc_msgSend"
  msgSendCString :: Ptr () -> Ptr () -> CString -> IO (Ptr ())
