module Test.Main where

import Effect (Effect)
import Prelude
import MainFrameTests as MainFrameTests
import Test.Unit.Main (runTest)

foreign import forDeps :: Effect Unit

main :: Effect Unit
main =
  runTest do
    MainFrameTests.all
