{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Plutus.SCB.Webserver.API
    ( API
    ) where

import           Plutus.SCB.Webserver.Types (FullReport)
import           Servant.API                ((:<|>), (:>), Get, JSON)

type API
     = "healthcheck" :> Get '[ JSON] ()
       :<|> "all" :> Get '[ JSON] FullReport
