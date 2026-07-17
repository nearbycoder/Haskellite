module Main (main) where

import Haskellite.RuntimeSpec (runtimeTests)
import Haskellite.VADSpec (vadTests)
import Haskellite.WavSpec (wavTests)
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "Haskellite" [vadTests, wavTests, runtimeTests]
