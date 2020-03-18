module MainFrame
  ( mkMainFrame
  , handleAction
  , mkInitialState
  ) where

import Control.Monad.Except.Trans (ExceptT, runExceptT, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Control.Monad.State (class MonadState)
import Data.Lens (assign)
import Data.Maybe (Maybe(..))
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, hoist)
import Halogen as H
import Halogen.HTML (HTML)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Plutus.SCB.Webserver (SPParams_(SPParams_), getAll)
import Prelude
import Servant.PureScript.Ajax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Types (HAction(..), Query, State(..), _fullReport, WebData)
import View as View

mkInitialState :: forall m. Monad m => m State
mkInitialState = do
  pure
    $ State
        { fullReport: NotAsked }

------------------------------------------------------------
ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = defaultSettings $ SPParams_ { baseURL: "/api/" }

mkMainFrame ::
  forall m n.
  MonadThrow Error n =>
  MonadEffect n =>
  MonadAff m =>
  n (Component HTML Query HAction Void m)
mkMainFrame = do
  initialState <- mkInitialState
  pure $ hoist (flip runReaderT ajaxSettings)
    $ H.mkComponent
        { initialState: const initialState
        , render: View.render
        , eval:
          H.mkEval
            { handleAction: handleAction
            , handleQuery: const $ pure Nothing
            , initialize: Just Init
            , receive: const Nothing
            , finalize: Nothing
            }
        }

handleAction ::
  forall m.
  MonadState State m =>
  MonadAff m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  HAction -> m Unit
handleAction Init = handleAction LoadFullReport

handleAction LoadFullReport = do
  assign _fullReport Loading
  reportResult <- runAjax $ getAll
  assign _fullReport reportResult

runAjax ::
  forall m a.
  Monad m =>
  ExceptT AjaxError m a ->
  m (WebData a)
runAjax action = RemoteData.fromEither <$> runExceptT action
