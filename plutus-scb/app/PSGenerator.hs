{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module PSGenerator
    ( generate
    ) where

import           Language.Plutus.Contract.Effects.UtxoAt    (UtxoAtAddress)
import           Language.Plutus.Contract.Effects.WriteTx   (WriteTxResponse)
import           Language.PureScript.Bridge.TypeParameters  (A)
import           Ledger.Index                               (UtxoIndex)
import           Plutus.SCB.Events                          (ChainEvent)
import           Plutus.SCB.Events.Contract                 (ContractRequest, ContractResponse, EventId, RequestEvent,
                                                             ResponseEvent)
import           Plutus.SCB.Events.Node                     (NodeEvent)
import           Plutus.SCB.Events.User                     (UserEvent)
import           Plutus.SCB.Events.Wallet                   (WalletEvent)
import           Plutus.SCB.Types                           (ActiveContract, ActiveContractState, Contract,
                                                             PartiallyDecodedResponse)

import           Control.Applicative                        ((<|>))
import           Control.Lens                               (set, (&))
import           Control.Monad.Reader                       (MonadReader)
import           Data.Proxy                                 (Proxy (Proxy))
import           Language.PureScript.Bridge                 (BridgePart, Language (Haskell), PSType, SumType,
                                                             TypeInfo (TypeInfo), buildBridge, doCheck, equal, functor,
                                                             genericShow, haskType, isTuple, mkSumType, order,
                                                             psTypeParameters, typeModule, typeName, writePSTypesWith,
                                                             (^==))
import           Language.PureScript.Bridge.Builder         (BridgeData)
import           Language.PureScript.Bridge.CodeGenSwitches (ForeignOptions (ForeignOptions), genForeign,
                                                             unwrapSingleConstructors)
import           Language.PureScript.Bridge.PSTypes         (psArray, psInt, psString)
import           Ledger                                     (Address, Datum, MonetaryPolicy, PubKey, PubKeyHash,
                                                             Redeemer, Signature, Tx, TxId, TxIn, TxInType, TxOut,
                                                             TxOutRef, TxOutTx, TxOutType, Validator)
import           Ledger.Ada                                 (Ada)
import           Ledger.Constraints.OffChain                (UnbalancedTx)
import           Ledger.Interval                            (Extended, Interval, LowerBound, UpperBound)
import           Ledger.Slot                                (Slot)
import           Ledger.Value                               (CurrencySymbol, TokenName, Value)
import qualified Plutus.SCB.Webserver.API                   as API
import           Plutus.SCB.Webserver.Types                 (FullReport)
import           Servant.PureScript                         (HasBridge, Settings, apiModuleName, defaultBridge,
                                                             defaultSettings, languageBridge,
                                                             writeAPIModuleWithSettings, _generateSubscriberAPI)
import           Wallet.API                                 (WalletAPIError)
import           Wallet.Rollup.Types                        (AnnotatedTx, BeneficialOwner, DereferencedInput,
                                                             SequenceId)

psJsonUUID :: PSType
psJsonUUID = TypeInfo "" "Data.Json.JsonUUID" "JsonUUID" []

psAssocMap :: MonadReader BridgeData m => m PSType
psAssocMap =
    TypeInfo "plutus-playground-client" "Language.PlutusTx.AssocMap" "Map" <$>
    psTypeParameters

psJson :: PSType
psJson = TypeInfo "" "Data.RawJson" "RawJson" []

psJsonEither :: MonadReader BridgeData m => m PSType
psJsonEither =
    TypeInfo "" "Data.Json.JsonEither" "JsonEither" <$> psTypeParameters

psJsonMap :: MonadReader BridgeData m => m PSType
psJsonMap = TypeInfo "" "Data.Json.JsonMap" "JsonMap" <$> psTypeParameters

psJsonTuple :: MonadReader BridgeData m => m PSType
psJsonTuple = TypeInfo "" "Data.Json.JsonTuple" "JsonTuple" <$> psTypeParameters

integerBridge :: BridgePart
integerBridge = do
    typeName ^== "Integer"
    pure psInt

dataBridge :: BridgePart
dataBridge = do
    typeName ^== "Data"
    typeModule ^== "Language.PlutusTx.Data"
    pure psString

assocMapBridge :: BridgePart
assocMapBridge = do
    typeName ^== "Map"
    typeModule ^== "Language.PlutusTx.AssocMap"
    psAssocMap

ledgerBytesBridge :: BridgePart
ledgerBytesBridge = do
    typeName ^== "LedgerBytes"
    typeModule ^== "LedgerBytes"
    pure psString

aesonBridge :: BridgePart
aesonBridge = do
    typeName ^== "Value"
    typeModule ^== "Data.Aeson.Types.Internal"
    pure psJson

eitherBridge :: BridgePart
eitherBridge = do
    typeName ^== "Either"
    psJsonEither

tupleBridge :: BridgePart
tupleBridge = do
    doCheck haskType isTuple
    psJsonTuple

setBridge :: BridgePart
setBridge = do
    typeName ^== "Set"
    typeModule ^== "Data.Set" <|> typeModule ^== "Data.Set.Internal"
    psArray

digestBridge :: BridgePart
digestBridge = do
    typeName ^== "Digest"
    typeModule ^== "Crypto.Hash.Types"
    pure psString

scriptBridge :: BridgePart
scriptBridge = do
    typeName ^== "Script"
    typeModule ^== "Ledger.Scripts"
    pure psString

validatorHashBridge :: BridgePart
validatorHashBridge = do
    typeName ^== "ValidatorHash"
    typeModule ^== "Ledger.Scripts"
    pure psString

mpsHashBridge :: BridgePart
mpsHashBridge = do
    typeName ^== "MonetaryPolicyHash"
    typeModule ^== "Ledger.Scripts"
    pure psString

dataHashBridge :: BridgePart
dataHashBridge = do
    typeName ^== "DatumHash"
    typeModule ^== "Ledger.Scripts"
    pure psString

uuidBridge :: BridgePart
uuidBridge = do
    typeName ^== "UUID"
    typeModule ^== "Data.UUID" <|> typeModule ^== "Data.UUID.Types.Internal"
    pure psJsonUUID

byteStringBridge :: BridgePart
byteStringBridge = do
    typeName ^== "ByteString"
    typeModule ^== "Data.ByteString.Lazy.Internal"
    pure psString

mapBridge :: BridgePart
mapBridge = do
    typeName ^== "Map"
    typeModule ^== "Data.Map.Internal"
    psJsonMap

myBridge :: BridgePart
myBridge =
    eitherBridge <|> tupleBridge <|> defaultBridge <|> integerBridge <|>
    mapBridge <|>
    assocMapBridge <|>
    aesonBridge <|>
    setBridge <|>
    uuidBridge <|>
    digestBridge <|>
    dataBridge <|>
    scriptBridge <|>
    validatorHashBridge <|>
    mpsHashBridge <|>
    dataHashBridge <|>
    byteStringBridge <|>
    ledgerBytesBridge

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
    languageBridge _ = buildBridge myBridge

myTypes :: [SumType 'Haskell]
myTypes =
    [ (equal <*> (genericShow <*> mkSumType)) (Proxy @FullReport)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ChainEvent)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Contract)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ActiveContract)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ActiveContractState)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @PartiallyDecodedResponse)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @SequenceId)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @AnnotatedTx)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @DereferencedInput)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @BeneficialOwner)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxIn)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxOut)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @TxOutTx)
    , (equal <*> (order <*> (genericShow <*> mkSumType))) (Proxy @TxOutRef)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @Datum)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @Validator)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @MonetaryPolicy)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @Redeemer)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @Signature)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @TxInType)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @TxOutType)
    , (order <*> (genericShow <*> mkSumType)) (Proxy @PubKeyHash)
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(Interval A))
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(LowerBound A))
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(UpperBound A))
    , (functor <*> (equal <*> (genericShow <*> mkSumType)))
          (Proxy @(Extended A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Ada)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Tx)
    , (equal <*> (order <*> (genericShow <*> mkSumType))) (Proxy @TxId)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Address)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Slot)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @EventId)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ContractRequest)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @ContractResponse)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(RequestEvent A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @(ResponseEvent A))
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @UnbalancedTx)
    , (equal <*> (order <*> (genericShow <*> mkSumType))) (Proxy @PubKey)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @NodeEvent)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @UserEvent)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @WalletEvent)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @Value)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @CurrencySymbol)
    , (genericShow <*> (order <*> mkSumType)) (Proxy @TokenName)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @UtxoAtAddress)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @UtxoIndex)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @WriteTxResponse)
    , (equal <*> (genericShow <*> mkSumType)) (Proxy @WalletAPIError)
    ]

mySettings :: Settings
mySettings =
    (defaultSettings & set apiModuleName "Plutus.SCB.Webserver")
        {_generateSubscriberAPI = False}

------------------------------------------------------------
generate :: FilePath -> IO ()
generate outputDir = do
    writeAPIModuleWithSettings
        mySettings
        outputDir
        myBridgeProxy
        (Proxy @API.API)
    writePSTypesWith
        (genForeign (ForeignOptions {unwrapSingleConstructors = True}))
        outputDir
        (buildBridge myBridge)
        myTypes
    putStrLn $ "Done: " <> outputDir
