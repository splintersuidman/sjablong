module Sjablong.Layer.Text
  ( TextLayer(..)
  , setText
  , setFontSize
  , setFillStyle
  , mapMaxWidth
  , measureTextHeight
  , measureTextWidth
  , measureMaxTextHeight
  , measureMaxTextWidth
  ) where

import Prelude

import Data.Array (any, concat, length, snoc, zip) as Array
import Data.Array.Extra (enumerate) as Array
import Data.Foldable (foldl, for_, maximum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (joinWith) as String
import Data.String.Utils (lines, words) as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (Context2D, TextAlign(..), TextBaseline(..), fillText, withContext)
import Graphics.Canvas (setFillStyle, setFont, setTextAlign, setTextBaseline) as Canvas
import Graphics.Canvas.Extra (setLetterSpacing) as Canvas
import Graphics.Canvas.TextMetrics (measureText)
import Sjablong.Layer (class Layer, DragOffset, Point, dragTranslateMaybe, translatePoint)

newtype TextLayer = TextLayer
  { text :: String
  , position :: Point
  , maxWidth :: Maybe Number
  , fillStyle :: String
  , fontName :: String
  , fontSize :: Number
  , fontStyle :: String
  , fontWeight :: String
  , align :: TextAlign
  , baseline :: TextBaseline
  , lineHeight :: Number
  , letterSpacing :: String
  , dragOffset :: Maybe DragOffset
  , context :: Context2D
  }

setText :: String -> TextLayer -> TextLayer
setText text (TextLayer l) = TextLayer l { text = text }

setFontSize :: Number -> TextLayer -> TextLayer
setFontSize fontSize (TextLayer l) = TextLayer l { fontSize = fontSize }

setFillStyle :: String -> TextLayer -> TextLayer
setFillStyle fillStyle (TextLayer l) = TextLayer l { fillStyle = fillStyle }

mapMaxWidth :: (Number -> Number) -> TextLayer -> TextLayer
mapMaxWidth f (TextLayer l) = TextLayer l { maxWidth = map f l.maxWidth }

instance MonadEffect m => Layer m TextLayer where
  position (TextLayer l) = pure l.position
  translate translation (TextLayer l) = pure $ TextLayer l { position = translatePoint translation l.position }

  -- TODO: take baseline into account
  -- TODO: take direction into account (for AlignStart and AlignEnd)
  containsPoint { x, y } (TextLayer l) = liftEffect $ withContext l.context do
    Canvas.setFillStyle l.context l.fillStyle
    Canvas.setFont l.context $ l.fontStyle <> " " <> l.fontWeight <> " " <> show l.fontSize <> "px " <> l.fontName
    Canvas.setTextBaseline l.context l.baseline
    Canvas.setTextAlign l.context l.align
    Canvas.setLetterSpacing l.context l.letterSpacing

    lines <- case l.maxWidth of
      Just maxWidth -> wrapLines l.context maxWidth l.text
      Nothing -> pure $ String.lines l.text
    widths <- traverse (measureTextWidth l.context) lines
    lineTextHeight <- measureMaxTextHeight l.context lines

    pure $ flip Array.any (Array.enumerate widths) \(Tuple i width) -> do
      -- XXX: support other baselines
      let
        lineY = case l.baseline of
          BaselineTop -> l.position.y + lineTextHeight * l.lineHeight * toNumber i
          BaselineBottom -> l.position.y - lineTextHeight - toNumber (Array.length lines - i - 1) * lineTextHeight * l.lineHeight
          _ -> l.position.y + lineTextHeight * l.lineHeight * toNumber i

      case l.align of
        AlignStart -> l.position.x <= x && x <= l.position.x + width
          && lineY <= y
          && y <= lineY + lineTextHeight
        AlignLeft -> l.position.x <= x && x <= l.position.x + width
          && lineY <= y
          && y <= lineY + lineTextHeight
        AlignEnd -> l.position.x - width <= x && x <= l.position.x
          && lineY <= y
          && y <= lineY + lineTextHeight
        AlignRight -> l.position.x - width <= x && x <= l.position.x
          && lineY <= y
          && y <= lineY + lineTextHeight
        AlignCenter -> l.position.x - width / 2.0 <= x && x <= l.position.x + width / 2.0
          && lineY <= y
          && y <= lineY + lineTextHeight

  dragStart offset (TextLayer layer) = pure $ TextLayer layer { dragOffset = Just offset }
  drag t l@(TextLayer layer) = dragTranslateMaybe layer.dragOffset t l
  dragEnd (TextLayer layer) = pure $ TextLayer layer { dragOffset = Nothing }

  draw ctx (TextLayer l) = withContext ctx do
    Canvas.setFillStyle ctx l.fillStyle
    Canvas.setFont ctx $ l.fontStyle <> " " <> l.fontWeight <> " " <> show l.fontSize <> "px " <> l.fontName
    Canvas.setTextBaseline ctx l.baseline
    Canvas.setTextAlign ctx l.align
    Canvas.setLetterSpacing ctx l.letterSpacing

    lines <- case l.maxWidth of
      Just maxWidth -> wrapLines ctx maxWidth l.text
      Nothing -> pure $ String.lines l.text
    lineTextHeight <- measureMaxTextHeight ctx lines

    for_ (Array.enumerate lines) \(Tuple i line) -> do
      -- XXX: support other baselines
      -- XXX: maybe add another property indicating whether the text
      -- should be anchored at the top or bottom, and use baseline
      -- only for what fillText uses it for (per line instead of per
      -- block)
      let
        y = case l.baseline of
          BaselineTop -> l.position.y + toNumber i * lineTextHeight * l.lineHeight
          BaselineBottom -> l.position.y - toNumber (Array.length lines - i - 1) * lineTextHeight * l.lineHeight
          _ -> l.position.y + toNumber i * lineTextHeight * l.lineHeight
      fillText ctx line l.position.x y

measureTextHeight :: Context2D -> String -> Effect Number
measureTextHeight ctx text = do
  metrics <- measureText ctx text
  -- XXX: measure based on alphabetic baseline?
  pure $ metrics.actualBoundingBoxAscent + metrics.actualBoundingBoxDescent

measureTextWidth :: Context2D -> String -> Effect Number
measureTextWidth ctx text = do
  metrics <- measureText ctx text
  -- XXX: use actual bounding box?
  -- pure $ metrics.actualBoundingBoxLeft + metrics.actualBoundingBoxRight
  pure metrics.width

measureMaxTextHeight :: Context2D -> Array String -> Effect Number
measureMaxTextHeight ctx lines = fromMaybe 0.0 <<< maximum <$> traverse (measureTextHeight ctx) lines

measureMaxTextWidth :: Context2D -> Array String -> Effect Number
measureMaxTextWidth ctx lines = fromMaybe 0.0 <<< maximum <$> traverse (measureTextWidth ctx) lines

wrapLines :: Context2D -> Number -> String -> Effect (Array String)
wrapLines ctx maxWidth text = Array.concat <$> traverse (wrapLine ctx maxWidth) (String.lines text)

wrapLine :: Context2D -> Number -> String -> Effect (Array String)
wrapLine ctx maxWidth text = do
  let words = String.words text
  wordWidths <- traverse (measureTextWidth ctx) words
  spaceWidth <- measureTextWidth ctx " "

  let
    go :: Array (Array String) /\ Array String /\ Number -> String /\ Number -> Array (Array String) /\ Array String /\ Number
    go (lines /\ [] /\ _) (word /\ wordWidth) = lines /\ [ word ] /\ wordWidth
    go (lines /\ line /\ lineWidth) (word /\ wordWidth) =
      if lineWidth + spaceWidth + wordWidth > maxWidth then Array.snoc lines line /\ [ word ] /\ wordWidth
      else lines /\ Array.snoc line word /\ (lineWidth + spaceWidth + wordWidth)
  let lines' /\ line /\ _ = foldl go ([] /\ [] /\ 0.0) (Array.zip words wordWidths)
  let lines = String.joinWith " " <$> lines'

  case line of
    [] -> pure lines
    _ -> pure $ Array.snoc lines $ String.joinWith " " line
