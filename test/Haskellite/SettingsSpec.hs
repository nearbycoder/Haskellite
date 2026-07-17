{-# LANGUAGE OverloadedStrings #-}

module Haskellite.SettingsSpec (settingsTests) where

import Data.Aeson (eitherDecode, encode)
import Haskellite.Types
  ( HotkeyKey (..)
  , HotkeyModifiers (..)
  , HotkeyPreset (..)
  , Settings (..)
  , defaultSettings
  , hotkeyBinding
  , validHotkeyBinding
  )
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
    , testCase "legacy shortcut presets still decode" $
        case eitherDecode "{\"activationHotkey\":\"FunctionKey9\"}" of
          Left message -> assertFailure message
          Right settings -> activationHotkey settings @?= FunctionKey9
    , testCase "current settings retain hold-to-talk mode" $
        let settings = defaultSettings {holdHotkeyToTalk = True}
         in eitherDecode (encode settings) @?= Right settings
    , testCase "custom shortcut settings round trip" $
        let modifiers = HotkeyModifiers True True False True
            settings = defaultSettings {activationHotkey = CustomHotkey modifiers HotkeyA}
         in eitherDecode (encode settings) @?= Right settings
    , testCase "preset shortcuts expose their bindings" $
        hotkeyBinding SuperShiftSpace
          @?= (HotkeyModifiers False True False True, HotkeySpace)
    , testCase "regular keys require a modifier" $ do
        validHotkeyBinding (HotkeyModifiers False False False False) HotkeyA @?= False
        validHotkeyBinding (HotkeyModifiers False False False False) HotkeyF8 @?= True
    ]
