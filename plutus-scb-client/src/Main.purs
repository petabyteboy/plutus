module Main where

import Prelude
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import MainFrame (mkMainFrame)
import Types (HAction(..))

main :: Effect Unit
main = do
  mainFrame <- mkMainFrame
  runHalogenAff do
    body <- awaitBody
    driver <- runUI mainFrame Init body
    pure unit

onLoad :: Unit
onLoad = unsafePerformEffect main
