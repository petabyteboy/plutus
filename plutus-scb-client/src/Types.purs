module Types where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.RawJson (RawJson(..))
import Data.Symbol (SProxy(..))
import Network.RemoteData (RemoteData)
import Plutus.SCB.Types (ActiveContractState, PartiallyDecodedResponse)
import Plutus.SCB.Webserver.Types (FullReport)
import Servant.PureScript.Ajax (AjaxError)

data Query a

type WebData
  = RemoteData AjaxError

data HAction
  = Init
  | LoadFullReport

newtype State
  = State
  { fullReport :: WebData FullReport
  }

derive instance newtypeState :: Newtype State _

derive instance genericState :: Generic State _

_fullReport :: Lens' State (WebData FullReport)
_fullReport = _Newtype <<< prop (SProxy :: SProxy "fullReport")

_partiallyDecodedResponse :: Lens' ActiveContractState PartiallyDecodedResponse
_partiallyDecodedResponse = _Newtype <<< prop (SProxy :: SProxy "partiallyDecodedResponse")

_hooks :: Lens' PartiallyDecodedResponse RawJson
_hooks = _Newtype <<< prop (SProxy :: SProxy "hooks")
