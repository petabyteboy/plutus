module Halogen.Monaco where

import Data.Either (Either(..))
import Data.Lens (view)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_, traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM, RefLabel)
import Halogen as H
import Halogen.HTML (HTML, div)
import Halogen.HTML.Properties (class_, ref)
import Halogen.Query.EventSource (Emitter(..), Finalizer(..), effectEventSource)
import Monaco (CodeActionProvider, CompletionItemProvider, DocumentFormattingEditProvider, Editor, IMarker, IMarkerData, IPosition, LanguageExtensionPoint, MonarchLanguage, Theme, TokensProvider)
import Monaco as Monaco
import Prelude (Unit, bind, const, discard, pure, unit, void, ($), (>>=))

type State
  = { editor :: Maybe Editor }

data Query a
  = SetText String a
  | GetText (String -> a)
  | SetPosition IPosition a
  | Resize a
  | SetTheme String a
  | SetModelMarkers (Array IMarkerData) a

data Action
  = Init
  | HandleChange String (Array IMarker)

data Message
  = TextChanged String
  | MarkersChanged (Array IMarker)

type Settings m
  = { languageExtensionPoint :: LanguageExtensionPoint
    , theme :: Theme
    , monarchTokensProvider :: Maybe MonarchLanguage
    , tokensProvider :: Maybe TokensProvider
    , completionItemProvider :: Maybe CompletionItemProvider
    , codeActionProvider :: Maybe CodeActionProvider
    , documentFormattingEditProvider :: Maybe DocumentFormattingEditProvider
    , refLabel :: RefLabel
    , owner :: String
    , getModelMarkers :: String -> Array IMarkerData
    , setup :: Editor -> m Unit
    }

monacoComponent :: forall m. MonadAff m => MonadEffect m => Settings m -> H.Component HTML Query Unit Message m
monacoComponent settings =
  H.mkComponent
    { initialState: const { editor: Nothing }
    , render: render settings
    , eval:
      H.mkEval
        { handleAction: handleAction settings
        , handleQuery
        , initialize: Just Init
        , receive: const Nothing
        , finalize: Nothing
        }
    }

render :: forall m p i. Settings m -> State -> HTML p i
render settings state =
  div
    [ ref settings.refLabel
    , class_ $ H.ClassName "monaco-editor-container"
    ]
    []

handleAction :: forall slots m. MonadAff m => MonadEffect m => Settings m -> Action -> HalogenM State Action slots Message m Unit
handleAction settings Init = do
  monaco <- liftEffect Monaco.getMonaco
  maybeElement <- H.getHTMLElementRef settings.refLabel
  case maybeElement of
    Just element -> do
      let
        languageId = view Monaco._id settings.languageExtensionPoint
      liftEffect do
        Monaco.registerLanguage monaco settings.languageExtensionPoint
        Monaco.defineTheme monaco settings.theme
        for_ settings.monarchTokensProvider $ Monaco.setMonarchTokensProvider monaco languageId
        for_ settings.tokensProvider $ Monaco.setTokensProvider monaco languageId
        for_ settings.completionItemProvider $ Monaco.registerCompletionItemProvider monaco languageId
        for_ settings.codeActionProvider $ Monaco.registerCodeActionProvider monaco languageId
        for_ settings.documentFormattingEditProvider $ Monaco.registerDocumentFormattingEditProvider monaco languageId
      editor <- liftEffect $ Monaco.create monaco element languageId
      void $ H.modify (const { editor: Just editor })
      void $ H.subscribe $ effectEventSource (changeContentHandler monaco editor)
      H.lift $ settings.setup editor
    Nothing -> pure unit
  where
  changeContentHandler monaco editor (Emitter emitter) = do
    Monaco.onDidChangeContent editor
      ( \_ -> do
          model <- Monaco.getModel editor
          let
            v = Monaco.getValue model

            markersData = settings.getModelMarkers v
          Monaco.setModelMarkers monaco model settings.owner markersData
          markers <- Monaco.getModelMarkers monaco model
          emitter $ Left $ HandleChange (Monaco.getValue model) markers
      )
    pure $ Finalizer $ pure unit

handleAction _ (HandleChange contents markers) = do
  H.raise $ TextChanged contents
  H.raise $ MarkersChanged markers

-- If the editor has been set then we can use it when handling our Query
withEditor :: forall input m a. MonadEffect m => (Editor -> HalogenM State Action input Message m a) -> HalogenM State Action input Message m (Maybe a)
withEditor f = H.gets _.editor >>= traverse f

handleQuery :: forall a input m. MonadEffect m => Query a -> HalogenM State Action input Message m (Maybe a)
handleQuery (SetText text next) = do
  withEditor \editor -> do
    model <- liftEffect $ Monaco.getModel editor
    liftEffect $ Monaco.setValue model text
    pure next

handleQuery (GetText f) = do
  withEditor \editor -> do
    model <- liftEffect $ Monaco.getModel editor
    let
      s = Monaco.getValue model
    pure $ f s

handleQuery (SetPosition position next) = do
  withEditor \editor -> do
    liftEffect $ Monaco.setPosition editor position
    liftEffect $ Monaco.revealLine editor position.lineNumber
    pure next

handleQuery (Resize next) = do
  withEditor \editor -> do
    liftEffect $ Monaco.layout editor
    pure next

handleQuery (SetTheme themeName next) = do
  liftEffect do
    monaco <- Monaco.getMonaco
    Monaco.setTheme monaco themeName
    pure $ Just next

handleQuery (SetModelMarkers markers next) = do
  withEditor \editor ->
    liftEffect do
      let
        owner = Monaco.getEditorId editor
      monaco <- Monaco.getMonaco
      model <- Monaco.getModel editor
      Monaco.setModelMarkers monaco model owner markers
      pure next
