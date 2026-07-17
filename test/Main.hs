module Main (main) where

import Haskellite.RuntimeSpec (runtimeTests)
import Haskellite.SettingsSpec (settingsTests)
import Haskellite.HistorySpec (historyTests)
import Haskellite.VADSpec (vadTests)
import Haskellite.WavSpec (wavTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Haskellite" [vadTests, wavTests, runtimeTests, historyTests, settingsTests]
