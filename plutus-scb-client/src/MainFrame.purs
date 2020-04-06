module MainFrame
  ( mkMainFrame
  , handleAction
  , initialState
  ) where

import Prelude
import Chain.Eval (handleAction) as Chain
import Chain.Types (AnnotatedBlockchain(..))
import Chain.Types (initialState) as Chain
import Control.Monad.Except.Trans (ExceptT, runExceptT, class MonadThrow)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Control.Monad.State (class MonadState)
import Control.Monad.State.Extra (zoomStateT)
import Data.Lens (assign, to)
import Data.Lens.Extra (peruse)
import Data.Maybe (Maybe(..))
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen (Component, hoist)
import Halogen as H
import Halogen.HTML (HTML)
import MonadAnimate (class MonadAnimate)
import Network.RemoteData (RemoteData(..), _Success)
import Network.RemoteData as RemoteData
import Plutus.SCB.Webserver (SPParams_(SPParams_), getAll)
import Servant.PureScript.Ajax (AjaxError)
import Servant.PureScript.Settings (SPSettings_, defaultSettings)
import Types (HAction(..), Query, State(..), WebData, _annotatedBlockchain, _chainState, _fullReport)
import View as View

initialState :: State
initialState =
  State
    { fullReport: NotAsked
    , chainState: Chain.initialState
    }

------------------------------------------------------------
ajaxSettings :: SPSettings_ SPParams_
ajaxSettings = defaultSettings $ SPParams_ { baseURL: "/api/" }

mkMainFrame ::
  forall m n.
  MonadThrow Error n =>
  MonadEffect n =>
  MonadAff m =>
  n (Component HTML Query HAction Void m)
mkMainFrame =
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
  MonadAnimate m =>
  MonadAff m =>
  MonadAsk (SPSettings_ SPParams_) m =>
  HAction -> m Unit
handleAction Init = handleAction LoadFullReport

handleAction LoadFullReport = do
  assign _fullReport Loading
  reportResult <- runAjax $ getAll
  assign _fullReport reportResult

handleAction (ChainAction newFocus) = do
  mAnnotatedBlockchain <-
    peruse (_fullReport <<< _Success <<< _annotatedBlockchain <<< to AnnotatedBlockchain)
  zoomStateT _chainState $ Chain.handleAction newFocus mAnnotatedBlockchain

runAjax ::
  forall m a.
  Monad m =>
  ExceptT AjaxError m a ->
  m (WebData a)
runAjax action = RemoteData.fromEither <$> runExceptT action
