{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

module Plutus.SCB.Webserver.Server
    ( main
    ) where

import           Control.Lens                  (view, _1, _2, _3)
import           Control.Monad.Except          (ExceptT (ExceptT))
import           Control.Monad.Freer.Extra.Log (logInfo)
import           Control.Monad.IO.Class        (liftIO)
import           Data.Bifunctor                (first)
import qualified Data.ByteString.Lazy.Char8    as LBS
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Data.Proxy                    (Proxy (Proxy))
import           Eventful                      (streamEventEvent)
import           Ledger                        (PubKeyHash)
import           Network.Wai.Handler.Warp      (run)
import           Plutus.SCB.App                (App, runApp)
import           Plutus.SCB.Arbitrary          ()
import           Plutus.SCB.Core               (runGlobalQuery)
import qualified Plutus.SCB.Query              as Query
import           Plutus.SCB.Types              (Config, WebserverConfig (..), scbWebserverConfig)
import           Plutus.SCB.Utils              (tshow)
import           Plutus.SCB.Webserver.API      (API)
import           Plutus.SCB.Webserver.Types
import           Servant                       ((:<|>) ((:<|>)), (:>), Application, Handler (Handler), err500, errBody,
                                                hoistServer, serve)
import           Servant.Client                (BaseUrl (baseUrlPort))
import           Wallet.Emulator.Wallet        (Wallet)
import qualified Wallet.Rollup                 as Rollup
import           Wallet.Rollup.Types           (AnnotatedTx)

asHandler :: Config -> App a -> Handler a
asHandler config action =
    Handler $
    ExceptT $
    fmap (first (\err -> err500 {errBody = LBS.pack $ show err})) $
    runApp config action

healthcheck :: Monad m => m ()
healthcheck = pure ()

fullReport :: App FullReport
fullReport = do
    latestContractStatus <- runGlobalQuery Query.latestContractStatus
    events <- fmap streamEventEvent <$> runGlobalQuery Query.pureProjection
    transactions <- runGlobalQuery Query.utxoIndexProjection
    let walletMap :: Map PubKeyHash Wallet = Map.empty -- TODO
    annotatedBlockchain :: [[AnnotatedTx]] <-
        Rollup.doAnnotateBlockchain $ view _1 transactions
    let transactionMap = view _2 transactions
    let utxoIndex = view _3 transactions
    pure
        FullReport
            { latestContractStatus
            , events
            , transactionMap
            , utxoIndex
            , annotatedBlockchain
            , walletMap
            }

app :: Config -> Application
app config =
    serve api $ hoistServer api (asHandler config) $ healthcheck :<|> fullReport
  where
    api = Proxy @("api" :> API)

main :: Config -> App ()
main config = do
    let port = baseUrlPort $ baseUrl $ scbWebserverConfig config
    logInfo $ "Starting SCB backend server on port: " <> tshow port
    liftIO $ run port $ app config
