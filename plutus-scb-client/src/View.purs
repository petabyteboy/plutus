module View (render) where

import Data.Tuple.Nested
import AjaxUtils (ajaxErrorPane)
import Bootstrap (cardBody_, cardHeader_, card_, col12_, col3_, col9_, container_, nbsp, row_)
import Data.Array as Array
import Data.Json.JsonMap (JsonMap, _JsonMap)
import Data.Json.JsonTuple (_JsonTuple)
import Data.Json.JsonUUID (JsonUUID, _JsonUUID)
import Data.Lens (_1, _2, to, traversed, view)
import Data.Lens.Extra (toArrayOf)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.RawJson (_RawJson)
import Data.Tuple (Tuple(..))
import Data.UUID as UUID
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML (ClassName(..), ComponentHTML, HTML, b_, code_, div, div_, h3_, pre_, span_, text)
import Halogen.HTML.Properties (class_)
import Icons (Icon(..), icon)
import Ledger.Index (UtxoIndex)
import Ledger.Tx (Tx)
import Ledger.TxId (TxId)
import Network.RemoteData (RemoteData(..))
import Plutus.SCB.Events (ChainEvent(..))
import Plutus.SCB.Events.Contract (RequestEvent, ResponseEvent)
import Plutus.SCB.Events.Node (NodeEvent(..))
import Plutus.SCB.Events.User (UserEvent(..))
import Plutus.SCB.Events.Wallet (WalletEvent)
import Plutus.SCB.Types (ActiveContract(..), ActiveContractState(..), Contract(..))
import Plutus.SCB.Webserver.Types (FullReport(..))
import Prelude (class Eq, class Show, otherwise, show, ($), (+), (<$>), (<<<), (<>), (==))
import Types (HAction, State(State), _hooks, _partiallyDecodedResponse)

render ::
  forall m slots.
  MonadAff m =>
  State -> ComponentHTML HAction slots m
render (State { fullReport }) =
  div
    [ class_ $ ClassName "main-frame" ]
    [ container_
        [ div_
            $ case fullReport of
                Success report -> [ fullReportPane report ]
                Failure error -> [ ajaxErrorPane error ]
                Loading -> [ icon Spinner ]
                NotAsked -> [ icon Spinner ]
        ]
    ]

fullReportPane :: forall p i. FullReport -> HTML p i
fullReportPane fullReport@(FullReport { events, latestContractStatus, utxoIndex }) =
  row_
    [ col9_ [ contractStatusPane latestContractStatus ]
    , col12_ [ eventsPane events ]
    , col9_ [ transactionPane (view (_JsonTuple <<< _1) utxoIndex) ]
    , col9_ [ utxoIndexPane (view (_JsonTuple <<< _2) utxoIndex) ]
    ]

contractStatusPane :: forall p i. JsonMap JsonUUID ActiveContractState -> HTML p i
contractStatusPane latestContractStatus =
  card_
    [ cardHeader_ [ text "Latest Contract Statuses" ]
    , cardBody_
        [ div_
            ( ( \(Tuple k v) ->
                  row_
                    [ col3_ [ h3_ [ text $ view (_JsonUUID <<< to UUID.toString) k ] ]
                    , col9_ [ text $ view (_partiallyDecodedResponse <<< _hooks <<< _RawJson) v ]
                    ]
              )
                <$> (Map.toUnfoldable $ unwrap latestContractStatus :: Array (Tuple JsonUUID ActiveContractState))
            )
        ]
    ]

transactionPane ::
  forall p i.
  JsonMap TxId Tx -> HTML p i
transactionPane txMap =
  card_
    [ cardHeader_ [ text "Txs" ]
    , cardBody_
        ( toArrayOf
            ( _JsonMap
                <<< traversed
                <<< to (\x -> div_ [ code_ [ text $ show x ] ])
            )
            txMap
        )
    ]

utxoIndexPane :: forall p i. UtxoIndex -> HTML p i
utxoIndexPane utxoIndex =
  card_
    [ cardHeader_ [ text "UtxoIndex" ]
    , cardBody_ [ div_ [ code_ [ text $ show utxoIndex ] ] ]
    ]

eventsPane :: forall p i. Array ChainEvent -> HTML p i
eventsPane events =
  card_
    [ cardHeader_
        [ text (show (Array.length events))
        , nbsp
        , text "Event(s)"
        ]
    , cardBody_ [ div_ (countedEventPane <$> countConsecutive events) ]
    ]

countedEventPane :: forall p i. Int /\ ChainEvent -> HTML p i
countedEventPane (count /\ event) = div_ [ pre_ [ text $ show count <> "x", nbsp, showEvent event ] ]

showEvent :: forall p i. ChainEvent -> HTML p i
showEvent (RecordRequest subevent) =
  span_
    [ b_
        [ text "Request:"
        , nbsp
        ]
    , showRecordRequestEvent subevent
    ]

showEvent (RecordResponse subevent) =
  span_
    [ b_
        [ text "Response:"
        , nbsp
        ]
    , showRecordResponseEvent subevent
    ]

showEvent (UserEvent subevent) =
  span_
    [ b_
        [ text "User:"
        , nbsp
        ]
    , showUserEvent subevent
    ]

showEvent (WalletEvent subevent) =
  span_
    [ b_
        [ text "Wallet:"
        , nbsp
        ]
    , showWalletEvent subevent
    ]

showEvent (NodeEvent subevent) =
  span_
    [ b_
        [ text "Node:"
        , nbsp
        ]
    , showNodeEvent subevent
    ]

showUserEvent :: forall p i. UserEvent -> HTML p i
showUserEvent (InstallContract (Contract { contractPath })) = text $ "Install " <> contractPath

showUserEvent ( ContractStateTransition
    ( ActiveContractState
      { activeContract: ActiveContract { activeContractId, activeContractPath }
    , partiallyDecodedResponse
    }
  )
) = text $ "Update " <> show activeContractId

showNodeEvent :: forall p i. NodeEvent -> HTML p i
showNodeEvent (BlockAdded []) = text $ "Empty block(s) added"

showNodeEvent event = text $ show event

showWalletEvent :: forall p i. WalletEvent -> HTML p i
showWalletEvent event = text $ show event

showRecordRequestEvent :: forall p i e. Show e => RequestEvent e -> HTML p i
showRecordRequestEvent event = text $ show event

showRecordResponseEvent :: forall p i e. Show e => ResponseEvent e -> HTML p i
showRecordResponseEvent event = text $ show event

countConsecutive :: forall a. Eq a => Array a -> Array (Tuple Int a)
countConsecutive = h <<< f
  where
  f :: Array a -> (Int /\ Maybe a /\ Array (Tuple Int a))
  f = Array.foldl g (0 /\ Nothing /\ [])

  g :: (Int /\ Maybe a /\ Array (Tuple Int a)) -> a -> (Int /\ Maybe a /\ Array (Tuple Int a))
  g (count /\ Nothing /\ accum) y = (1 /\ Just y /\ accum)

  g (count /\ Just x /\ accum) y
    | x == y = ((count + 1) /\ Just x /\ accum)
    | otherwise = (1 /\ Just y /\ Array.snoc accum (count /\ x))

  h (count /\ Nothing /\ accum) = accum

  h (count /\ Just x /\ accum) = Array.snoc accum (count /\ x)
