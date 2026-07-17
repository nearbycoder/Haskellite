{-# LANGUAGE OverloadedStrings #-}

module Haskellite.SettingsSpec (settingsTests) where

import Data.Aeson (eitherDecode, encode)
import Haskellite.Types (Settings (..), defaultSettings)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

settingsTests :: TestTree
settingsTests =
  testGroup
    "settings migration"
    [ testCase "legacy settings default to toggle mode" $
        case eitherDecode "{}" of
          Left message -> assertFailure message
          Right settings -> holdHotkeyToTalk settings @?= False
    , testCase "current settings retain hold-to-talk mode" $
        let settings = defaultSettings {holdHotkeyToTalk = True}
         in eitherDecode (encode settings) @?= Right settings
    ]
