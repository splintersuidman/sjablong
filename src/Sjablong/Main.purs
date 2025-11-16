module Sjablong.Main
  ( Template'(..)
  , TemplateContext(..)
  , mkTemplateContext
  , Template(..)
  , mkTemplate
  , redraw
  , addEventListeners
  , mkDownloadButton
  , mkDownloadButtonClip
  , toCanvasCoordinates
  , connectInput
  , connectInputPure
  , connectCheckbox
  , connectCheckboxPure
  , connectTextArea
  , connectTextAreaPure
  , listenSelect
  , connectSelect
  , connectSelectPure
  , connectRange
  , connectRangePure
  , connectTextSizeRange
  , connectMarkupTextSizeRange
  , connectScaleRange
  , connectFileInput
  , connectFileInputPure
  , connectBlobInput
  , connectBlobInputPure
  , connectObjectUrlInput
  ) where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Maybe.Trans.Extra (hoistMaybe)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Effect (Effect)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, Dimensions, Rectangle)
import Graphics.Canvas (canvasElementToImageSource, canvasToDataURL, clearRect, drawImageFull, getCanvasDimensions, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasDimensions, withContext) as Canvas
import Partial.Unsafe (unsafePartial)
import Record as Record
import Sjablong.Layer (class Layer, class Scalable, Point, SomeLayer, mkSomeLayer, scalePreserveRatio)
import Sjablong.Layer as Layer
import Sjablong.Layer.Ref (RefLayer, mkRefLayer)
import Sjablong.Layer.Ref as RefLayer
import Sjablong.Layer.Text (TextLayer)
import Sjablong.Layer.Text as TextLayer
import Sjablong.Layer.Text.Markup (MarkupTextLayer)
import Sjablong.Layer.Text.Markup as MarkupTextLayer
import Web.DOM (Element)
import Web.DOM.Document (createElement) as Dom
import Web.DOM.Element (getBoundingClientRect, setAttribute, toEventTarget, toNode) as Dom
import Web.DOM.Element as Element
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode (getElementById) as Dom
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (EventListener)
import Web.Event.EventTarget as Event
import Web.File.Blob (Blob)
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileList as FileList
import Web.File.Url as Url
import Web.HTML (HTMLDocument, HTMLElement)
import Web.HTML (window) as Html
import Web.HTML.HTMLDocument (toNonElementParentNode) as Html
import Web.HTML.HTMLDocument as HtmlDocument
import Web.HTML.HTMLElement (click, fromElement, offsetHeight, offsetWidth, toElement, toNode) as Html
import Web.HTML.HTMLInputElement as Input
import Web.HTML.HTMLSelectElement as Select
import Web.HTML.HTMLTextAreaElement as TextArea
import Web.HTML.Window (document) as Html
import Web.TouchEvent.Touch as Touch
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.TouchEvent.TouchList as TouchList
import Web.UIEvent.MouseEvent as MouseEvent

type Template' p =
  { canvas :: CanvasElement
  , canvasElement :: Element
  , canvasHtmlElement :: HTMLElement
  , document :: HTMLDocument
  | p
  }

type TemplateContext = Template' ()

mkTemplateContext :: String -> Dimensions -> Effect (Maybe TemplateContext)
mkTemplateContext canvasId dimensions = runMaybeT do
  canvas <- hoistMaybe =<< lift (Canvas.getCanvasElementById canvasId)
  lift $ Canvas.setCanvasDimensions canvas dimensions

  document <- lift $ Html.document =<< Html.window
  canvasElement <- hoistMaybe =<< lift (Dom.getElementById canvasId $ Html.toNonElementParentNode document)
  canvasHtmlElement <- hoistMaybe $ Html.fromElement canvasElement

  pure
    { canvas
    , canvasElement
    , canvasHtmlElement
    , document
    }

type Template = Template' (layer :: RefLayer (SomeLayer Effect))

mkTemplate :: forall l. Layer Effect l => TemplateContext -> l -> Effect Template
mkTemplate templateContext layer = do
  refLayer <- mkRefLayer $ mkSomeLayer layer
  pure $ Record.merge templateContext { layer: refLayer }

redraw :: Template -> Effect Unit
redraw template = do
  ctx <- Canvas.getContext2D template.canvas
  { width, height } <- Canvas.getCanvasDimensions template.canvas
  Canvas.clearRect ctx { x: 0.0, y: 0.0, width, height }
  Canvas.withContext ctx $ Layer.draw @Effect ctx template.layer

addEventListeners :: Template -> Effect Unit
addEventListeners template = do
  mousedownListener <- mkMousedownListener template
  mousemoveListener <- mkMousemoveListener template
  mouseupListener <- mkMouseupListener template
  touchstartListener <- mkTouchstartListener template
  touchmoveListener <- mkTouchmoveListener template
  touchendListener <- mkTouchendListener template
  inputListener <- mkInputListener template
  Event.addEventListener (EventType "mousedown") mousedownListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "mousemove") mousemoveListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "mouseup") mouseupListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "touchstart") touchstartListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "touchmove") touchmoveListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "touchend") touchendListener false $ Dom.toEventTarget template.canvasElement
  Event.addEventListener (EventType "input") inputListener false $ HtmlDocument.toEventTarget template.document

mkDownloadButton :: String -> String -> Template -> Effect (Maybe Element)
mkDownloadButton buttonId filename template = runMaybeT do
  let documentNode = Html.toNonElementParentNode template.document
  downloadButtonElement <- hoistMaybe =<< lift (Dom.getElementById buttonId documentNode)
  clickListener <- lift $ Event.eventListener \_ -> unsafePartial do
    dataUrl <- Canvas.canvasToDataURL template.canvas
    link <- Dom.createElement "a" $ HtmlDocument.toDocument template.document
    Dom.setAttribute "href" dataUrl link
    Dom.setAttribute "download" filename link
    let Just linkHtmlElement = Html.fromElement link
    Html.click linkHtmlElement
  lift $ Event.addEventListener (EventType "click") clickListener false $ Dom.toEventTarget downloadButtonElement
  pure downloadButtonElement

mkDownloadButtonClip :: String -> String -> Rectangle -> Template -> Effect (Maybe Element)
mkDownloadButtonClip buttonId filename clip template = runMaybeT do
  let documentNode = Html.toNonElementParentNode template.document
  downloadButtonElement <- hoistMaybe =<< lift (Dom.getElementById buttonId documentNode)
  clickListener <- lift $ Event.eventListener \_ -> unsafePartial do
    -- Clip
    clipCanvasElement <- Dom.createElement "canvas" $ HtmlDocument.toDocument template.document
    let clipCanvasId = "hidden-clip-canvas"
    Element.setAttribute "id" clipCanvasId clipCanvasElement

    Just body <- HtmlDocument.body template.document
    Node.appendChild (Dom.toNode clipCanvasElement) (Html.toNode body)

    Just clipCanvas <- Canvas.getCanvasElementById clipCanvasId
    Canvas.setCanvasDimensions clipCanvas { width: clip.width, height: clip.height }

    clipCanvasContext <- Canvas.getContext2D clipCanvas
    let imageSource = Canvas.canvasElementToImageSource template.canvas
    Canvas.drawImageFull clipCanvasContext imageSource clip.x clip.y clip.width clip.height 0.0 0.0 clip.width clip.height

    dataUrl <- Canvas.canvasToDataURL clipCanvas
    link <- Dom.createElement "a" $ HtmlDocument.toDocument template.document
    Dom.setAttribute "href" dataUrl link
    Dom.setAttribute "download" filename link
    let Just linkHtmlElement = Html.fromElement link
    Html.click linkHtmlElement
  lift $ Event.addEventListener (EventType "click") clickListener false $ Dom.toEventTarget downloadButtonElement
  pure downloadButtonElement

mkMousedownListener :: Template -> Effect EventListener
mkMousedownListener template = Event.eventListener \event -> case MouseEvent.fromEvent event of
  Nothing -> pure unit
  Just mouseEvent -> do
    pos <- Layer.position template.layer
    { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
      { x: toNumber $ MouseEvent.clientX mouseEvent
      , y: toNumber $ MouseEvent.clientY mouseEvent
      }
    let offset = { offsetX: x - pos.x, offsetY: y - pos.y }
    _ <- Layer.dragStart offset template.layer
    redraw template

mkTouchstartListener :: Template -> Effect EventListener
mkTouchstartListener template = Event.eventListener \event -> case TouchEvent.fromEvent event of
  Nothing -> pure unit
  Just touchEvent -> case TouchList.item 0 (TouchEvent.touches touchEvent) of
    Nothing -> redraw template
    Just touch -> do
      pos <- Layer.position template.layer
      { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
        { x: toNumber $ Touch.clientX touch
        , y: toNumber $ Touch.clientY touch
        }
      let offset = { offsetX: x - pos.x, offsetY: y - pos.y }
      _ <- Layer.dragStart offset template.layer
      redraw template

mkMousemoveListener :: Template -> Effect EventListener
mkMousemoveListener template = Event.eventListener \event -> case MouseEvent.fromEvent event of
  Nothing -> pure unit
  Just mouseEvent -> do
    pos <- Layer.position template.layer
    { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
      { x: toNumber $ MouseEvent.clientX mouseEvent
      , y: toNumber $ MouseEvent.clientY mouseEvent
      }
    let translation = { translateX: x - pos.x, translateY: y - pos.y }
    _ <- Layer.drag translation template.layer
    redraw template

mkTouchmoveListener :: Template -> Effect EventListener
mkTouchmoveListener template = Event.eventListener \event -> case TouchEvent.fromEvent event of
  Nothing -> pure unit
  Just touchEvent -> case TouchList.item 0 (TouchEvent.touches touchEvent) of
    Nothing -> redraw template
    Just touch -> do
      pos <- Layer.position template.layer
      { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
        { x: toNumber $ Touch.clientX touch
        , y: toNumber $ Touch.clientY touch
        }
      let translation = { translateX: x - pos.x, translateY: y - pos.y }
      _ <- Layer.drag translation template.layer
      redraw template

mkMouseupListener :: Template -> Effect EventListener
mkMouseupListener template = Event.eventListener \event -> case MouseEvent.fromEvent event of
  Nothing -> pure unit
  Just mouseEvent -> do
    pos <- Layer.position template.layer
    { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
      { x: toNumber $ MouseEvent.clientX mouseEvent
      , y: toNumber $ MouseEvent.clientY mouseEvent
      }
    let translation = { translateX: x - pos.x, translateY: y - pos.y }
    _ <- Layer.drag translation template.layer
    _ <- Layer.dragEnd template.layer
    redraw template

mkTouchendListener :: Template -> Effect EventListener
mkTouchendListener template = Event.eventListener \event -> case TouchEvent.fromEvent event of
  Nothing -> pure unit
  Just touchEvent -> case TouchList.item 0 (TouchEvent.touches touchEvent) of
    Nothing -> redraw template
    Just touch -> do
      pos <- Layer.position template.layer
      { x, y } <- toCanvasCoordinates template.canvas template.canvasHtmlElement
        { x: toNumber $ Touch.clientX touch
        , y: toNumber $ Touch.clientY touch
        }
      let translation = { translateX: x - pos.x, translateY: y - pos.y }
      _ <- Layer.drag translation template.layer
      _ <- Layer.dragEnd template.layer
      redraw template

mkInputListener :: Template -> Effect EventListener
mkInputListener template = Event.eventListener \_ -> redraw template

toCanvasCoordinates :: CanvasElement -> HTMLElement -> Point -> Effect Point
toCanvasCoordinates canvas canvasElement { x, y } = do
  { left, top } <- Dom.getBoundingClientRect $ Html.toElement canvasElement
  offsetWidth <- Html.offsetWidth canvasElement
  offsetHeight <- Html.offsetHeight canvasElement
  canvasWidth <- Canvas.getCanvasWidth canvas
  canvasHeight <- Canvas.getCanvasHeight canvas
  pure
    { x: (x - left) * canvasWidth / offsetWidth
    , y: (y - top) * canvasHeight / offsetHeight
    }

connectInput :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> Effect l) -> Effect Unit
connectInput { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = Input.fromElement element

  initialValue <- Input.value inputElement
  RefLayer.modifyM_ (k initialValue) layer

  inputListener <- Event.eventListener \_ -> do
    value <- Input.value inputElement
    RefLayer.modifyM_ (k value) layer
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectInputPure :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> l) -> Effect Unit
connectInputPure ctx id layer k = connectInput ctx id layer ((pure <<< _) <<< k)

connectTextArea :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> Effect l) -> Effect Unit
connectTextArea { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = TextArea.fromElement element

  initialValue <- TextArea.value inputElement
  RefLayer.modifyM_ (k initialValue) layer

  inputListener <- Event.eventListener \_ -> do
    value <- TextArea.value inputElement
    RefLayer.modifyM_ (k value) layer
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectTextAreaPure :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> l) -> Effect Unit
connectTextAreaPure ctx id layer k = connectTextArea ctx id layer ((pure <<< _) <<< k)

connectCheckbox :: forall l. TemplateContext -> String -> RefLayer l -> (Boolean -> l -> Effect l) -> Effect Unit
connectCheckbox { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = Input.fromElement element

  initialValue <- Input.checked inputElement
  RefLayer.modifyM_ (k initialValue) layer

  inputListener <- Event.eventListener \_ -> do
    value <- Input.checked inputElement
    RefLayer.modifyM_ (k value) layer
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectCheckboxPure :: forall l. TemplateContext -> String -> RefLayer l -> (Boolean -> l -> l) -> Effect Unit
connectCheckboxPure ctx id layer k = connectCheckbox ctx id layer ((pure <<< _) <<< k)

-- XXX: a similar abstraction can be applied to the other connect functions
listenSelect :: TemplateContext -> String -> (String -> Effect Unit) -> Effect Unit
listenSelect { document } id k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just selectElement = Select.fromElement element

  initialValue <- Select.value selectElement
  k initialValue

  selectListener <- Event.eventListener \_ -> k =<< Select.value selectElement
  Event.addEventListener (EventType "input") selectListener false $ Dom.toEventTarget element

connectSelect :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> Effect l) -> Effect Unit
connectSelect ctx id layer k = listenSelect ctx id $ \value -> RefLayer.modifyM_ (k value) layer

connectSelectPure :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> l) -> Effect Unit
connectSelectPure ctx id layer k = connectSelect ctx id layer ((pure <<< _) <<< k)

connectRange :: forall l. TemplateContext -> String -> RefLayer l -> (Number -> l -> Effect l) -> Effect Unit
connectRange { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = Input.fromElement element

  initialValue <- Input.value inputElement
  case Number.fromString initialValue of
    Just value -> RefLayer.modifyM_ (k value) layer
    Nothing -> pure unit

  inputListener <- Event.eventListener \_ -> do
    value' <- Input.value inputElement
    case Number.fromString value' of
      Just value -> RefLayer.modifyM_ (k value) layer
      Nothing -> pure unit
  Event.addEventListener (EventType "input") inputListener false $ Dom.toEventTarget element

connectRangePure :: forall l. TemplateContext -> String -> RefLayer l -> (Number -> l -> l) -> Effect Unit
connectRangePure ctx id layer k = connectRange ctx id layer ((pure <<< _) <<< k)

connectTextSizeRange :: TemplateContext -> String -> RefLayer TextLayer -> Effect Unit
connectTextSizeRange ctx id layer = connectRangePure ctx id layer TextLayer.setFontSize

connectMarkupTextSizeRange :: TemplateContext -> String -> RefLayer MarkupTextLayer -> Effect Unit
connectMarkupTextSizeRange ctx id layer = connectRangePure ctx id layer MarkupTextLayer.setFontSize

connectScaleRange :: forall l. Scalable Effect l => TemplateContext -> String -> RefLayer l -> Effect Unit
connectScaleRange ctx id layer = do
  scaleRef <- Ref.new 1.0
  connectRange ctx id layer \s' l -> do
    s <- Ref.read scaleRef
    Ref.write s' scaleRef
    scalePreserveRatio (s' / s) l

connectFileInput :: forall l. TemplateContext -> String -> RefLayer l -> (File -> l -> Effect l) -> Effect Unit
connectFileInput { document } id layer k = unsafePartial do
  Just element <- Dom.getElementById id $ Html.toNonElementParentNode document
  let Just inputElement = Input.fromElement element

  let
    callback = \value -> case value of
      Nothing -> pure unit
      Just fileList -> case FileList.items fileList !! 0 of
        Nothing -> pure unit
        Just file -> RefLayer.modifyM_ (k file) layer

  initialValue <- Input.files inputElement
  callback initialValue

  inputListener <- Event.eventListener \_ -> callback =<< Input.files inputElement
  Event.addEventListener (EventType "change") inputListener false $ Dom.toEventTarget element

connectFileInputPure :: forall l. TemplateContext -> String -> RefLayer l -> (File -> l -> l) -> Effect Unit
connectFileInputPure ctx id layer k = connectFileInput ctx id layer ((pure <<< _) <<< k)

connectBlobInput :: forall l. TemplateContext -> String -> RefLayer l -> (Blob -> l -> Effect l) -> Effect Unit
connectBlobInput ctx id layer k = connectFileInput ctx id layer (k <<< File.toBlob)

connectBlobInputPure :: forall l. TemplateContext -> String -> RefLayer l -> (Blob -> l -> l) -> Effect Unit
connectBlobInputPure ctx id layer k = connectBlobInput ctx id layer ((pure <<< _) <<< k)

connectObjectUrlInput :: forall l. TemplateContext -> String -> RefLayer l -> (String -> l -> Effect l) -> Effect Unit
connectObjectUrlInput ctx id layer k = connectBlobInput ctx id layer \blob l -> do
  url <- Url.createObjectURL blob
  k url l
