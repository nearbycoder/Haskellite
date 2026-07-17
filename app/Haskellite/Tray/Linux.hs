{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Haskellite.Tray.Linux
  ( SystemTray
  , startSystemTray
  , stopSystemTray
  ) where

import Control.Exception (SomeException, displayException, onException, try)
import Data.Int (Int32)
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import DBus
  ( formatBusName
  , methodCall
  , methodCallBody
  , methodCallDestination
  , toVariant
  )
import DBus.Client
  ( Client
  , Interface (..)
  , autoMethod
  , call_
  , connectSession
  , defaultInterface
  , disconnect
  , export
  , readOnlyProperty
  , requestName
  , unexport
  )
import qualified SDL
import System.Posix.Process (getProcessID)

newtype SystemTray = SystemTray Client

startSystemTray :: SDL.Window -> IO () -> IO () -> IO (Either Text SystemTray)
startSystemTray _ onShow onQuit = do
  outcome <- try $ do
    client <- connectSession
    (`onException` disconnect client) do
      processId <- getProcessID
      let serviceName = fromString $ "org.kde.StatusNotifierItem.haskellite.h" <> show processId
          objectPath = "/StatusNotifierItem"
          activate :: Int32 -> Int32 -> IO ()
          activate _ _ = onShow
          contextMenu :: Int32 -> Int32 -> IO ()
          contextMenu _ _ = onShow
          secondaryActivate :: Int32 -> Int32 -> IO ()
          secondaryActivate _ _ = onQuit
          interface =
            defaultInterface
              { interfaceName = "org.kde.StatusNotifierItem"
              , interfaceMethods =
                  [ autoMethod "Activate" activate
                  , autoMethod "ContextMenu" contextMenu
                  , autoMethod "SecondaryActivate" secondaryActivate
                  ]
              , interfaceProperties =
                  [ readOnlyProperty "Category" (pure ("ApplicationStatus" :: Text))
                  , readOnlyProperty "Id" (pure ("haskellite" :: Text))
                  , readOnlyProperty "Title" (pure ("Haskellite" :: Text))
                  , readOnlyProperty "Status" (pure ("Active" :: Text))
                  , readOnlyProperty "IconName" (pure ("audio-input-microphone-symbolic" :: Text))
                  , readOnlyProperty "ItemIsMenu" (pure False)
                  ]
              }
      _ <- requestName client serviceName []
      export client objectPath interface
      _ <-
        call_
          client
          (methodCall "/StatusNotifierWatcher" "org.kde.StatusNotifierWatcher" "RegisterStatusNotifierItem")
            { methodCallDestination = Just "org.kde.StatusNotifierWatcher"
            , methodCallBody = [toVariant $ formatBusName serviceName]
            }
      pure $ SystemTray client
  pure $ either (Left . Text.pack . displayException) Right (outcome :: Either SomeException SystemTray)

stopSystemTray :: SystemTray -> IO ()
stopSystemTray (SystemTray client) = do
  unexport client "/StatusNotifierItem"
  disconnect client
