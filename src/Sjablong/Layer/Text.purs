module Sjablong.Layer.Text
  ( TextLayer(..)
  , setText
  , setPosition
  , setFontSize
  , setFillStyle
  , setAlign
  , mapMaxWidth
  , measureTextHeight
  , measureTextWidth
  , measureMaxTextHeight
  , measureMaxTextWidth
  , Word(..)
  , measureWordHeight
  , measureWord
  , Space(..)
  , Line(..)
  , measureMaxLineHeight
  , wrapLines
  , wrapLine
  , stringToLines
  , stringToLine
  , LayoutText(..)
  , LayoutLine(..)
  , LayoutWord(..)
  , layoutText
  , layoutLines
  , layoutLine
  ) where

import Prelude

import Data.Array (any, concat, foldl, intersperse, length, mapMaybe, snoc) as Array
import Data.Array.Extra (enumerate) as Array
import Data.Either (Either(..))
import Data.Foldable (foldl, for_, maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Utils (lines, words) as String
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (Context2D, TextAlign(..), TextBaseline(..), fillText, withContext)
import Graphics.Canvas (setFillStyle, setFont, setTextAlign, setTextBaseline) as Canvas
import Graphics.Canvas.Extra (setLetterSpacing) as Canvas
import Graphics.Canvas.TextMetrics (TextMetrics, measureText, textMetricsBoundingBoxHeight)
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

setPosition :: Point -> TextLayer -> TextLayer
setPosition position (TextLayer l) = TextLayer l { position = position }

setFontSize :: Number -> TextLayer -> TextLayer
setFontSize fontSize (TextLayer l) = TextLayer l { fontSize = fontSize }

setFillStyle :: String -> TextLayer -> TextLayer
setFillStyle fillStyle (TextLayer l) = TextLayer l { fillStyle = fillStyle }

setAlign :: TextAlign -> TextLayer -> TextLayer
setAlign align (TextLayer l) = TextLayer l { align = align }

mapMaxWidth :: (Number -> Number) -> TextLayer -> TextLayer
mapMaxWidth f (TextLayer l) = TextLayer l { maxWidth = map f l.maxWidth }

instance MonadEffect m => Layer m TextLayer where
  position (TextLayer l) = pure l.position
  translate translation (TextLayer l) = pure $ TextLayer l { position = translatePoint translation l.position }

  containsPoint { x, y } layer@(TextLayer l) = liftEffect $ withContext l.context do
    { lines, lineHeight } <- layoutText layer
    pure $ flip Array.any lines \{ position, width } ->
      position.x <= x && x <= position.x + width && position.y <= y && y <= position.y + lineHeight

  dragStart offset (TextLayer layer) = pure $ TextLayer layer { dragOffset = Just offset }
  drag t l@(TextLayer layer) = dragTranslateMaybe layer.dragOffset t l
  dragEnd (TextLayer layer) = pure $ TextLayer layer { dragOffset = Nothing }

  draw ctx layer@(TextLayer l) = withContext ctx do
    Canvas.setFillStyle ctx l.fillStyle
    Canvas.setFont ctx $ l.fontStyle <> " " <> l.fontWeight <> " " <> show l.fontSize <> "px " <> l.fontName
    -- Alignment and baseline are handled manually
    Canvas.setTextAlign ctx AlignLeft
    Canvas.setTextBaseline ctx BaselineTop
    Canvas.setLetterSpacing ctx l.letterSpacing

    layout <- layoutText layer
    for_ layout.lines \line -> for_ line.words \{ word, position: { x, y } } ->
      fillText ctx word x y

-- XXX: measure based on alphabetic baseline?
measureTextHeight :: Context2D -> String -> Effect Number
measureTextHeight ctx text = textMetricsBoundingBoxHeight <$> measureText ctx text

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

type Word = { word :: String, metrics :: TextMetrics }

measureWord :: Context2D -> String -> Effect Word
measureWord ctx word = do
  metrics <- measureText ctx word
  pure { word, metrics }

measureWordHeight :: Context2D -> Word -> Effect Number
measureWordHeight ctx { word } = measureTextHeight ctx word

type Space = { width :: Number }

type Line = { words :: Array (Either Word Space), width :: Number }

measureLineHeight :: Context2D -> Line -> Effect Number
measureLineHeight ctx line =
  let
    words = flip Array.mapMaybe line.words \w -> case w of
      Left word -> Just word
      Right _space -> Nothing
  in
    fromMaybe 0.0 <<< maximum <$> traverse (measureWordHeight ctx) words

measureMaxLineHeight :: Context2D -> Array Line -> Effect Number
measureMaxLineHeight ctx lines = fromMaybe 0.0 <<< maximum <$> traverse (measureLineHeight ctx) lines

wrapLines :: Context2D -> Number -> String -> Effect (Array Line)
wrapLines ctx maxWidth text = Array.concat <$> traverse (wrapLine ctx maxWidth) (String.lines text)

wrapLine :: Context2D -> Number -> String -> Effect (Array Line)
wrapLine ctx maxWidth text = do
  words <- traverse (measureWord ctx) $ String.words text
  spaceWidth <- measureTextWidth ctx " "

  let
    go :: Array Line /\ Line -> Word -> Array Line /\ Line
    go (lines /\ { words: [] }) word = lines /\ { words: [ Left word ], width: word.metrics.width }
    go (lines /\ line) word =
      if line.width + spaceWidth + word.metrics.width > maxWidth then Array.snoc lines line /\ { words: [ Left word ], width: word.metrics.width }
      else lines /\
        { words: line.words `Array.snoc` Right { width: spaceWidth } `Array.snoc` Left word
        , width: line.width + spaceWidth + word.metrics.width
        }

  let lines /\ line = foldl go ([] /\ { words: [], width: 0.0 }) words

  case line.words of
    [] -> pure lines
    _ -> pure $ Array.snoc lines line

stringToLines :: Context2D -> String -> Effect (Array Line)
stringToLines ctx text = traverse (stringToLine ctx) $ String.lines text

stringToLine :: Context2D -> String -> Effect Line
stringToLine ctx text = do
  spaceWidth <- measureTextWidth ctx " "
  words <- traverse (measureWord ctx) (String.words text)
  pure
    { words: Array.intersperse (Right { width: spaceWidth }) $ map Left words
    , width: sum (map (_.metrics.width) words) + spaceWidth * toNumber (Array.length words - 1)
    }

type LayoutText = { lines :: Array LayoutLine, position :: Point, lineHeight :: Number }

layoutText :: TextLayer -> Effect LayoutText
layoutText (TextLayer layer) = withContext layer.context do
  Canvas.setFillStyle layer.context layer.fillStyle
  Canvas.setFont layer.context $ layer.fontStyle <> " " <> layer.fontWeight <> " " <> show layer.fontSize <> "px " <> layer.fontName
  -- Alignment and baseline are handled manually
  Canvas.setTextAlign layer.context AlignLeft
  Canvas.setTextBaseline layer.context BaselineTop
  Canvas.setLetterSpacing layer.context layer.letterSpacing

  lines <- case layer.maxWidth of
    Just maxWidth -> wrapLines layer.context maxWidth layer.text
    Nothing -> stringToLines layer.context layer.text
  lineTextHeight <- measureMaxLineHeight layer.context lines
  pure $ layoutLines layer.position layer.fontSize layer.align layer.baseline layer.lineHeight lineTextHeight lines

-- TODO: take direction into account (for AlignStart and AlignEnd)
layoutLines :: Point -> Number -> TextAlign -> TextBaseline -> Number -> Number -> Array Line -> LayoutText
layoutLines position fontSize align baseline lineHeight lineTextHeight lines =
  { position
  , lines: Array.enumerate lines <#> \(i /\ line) ->
      let
        totalHeight = lineTextHeight * toNumber (Array.length lines) + (lineHeight - 1.0) * fontSize * toNumber (Array.length lines - 1)

        -- XXX: support other baselines
        -- XXX: maybe add another property indicating whether the text
        -- should be anchored at the top or bottom, and use baseline
        -- only for what fillText uses it for (per line instead of per
        -- block)
        y = case baseline of
          BaselineTop -> position.y + toNumber i * lineTextHeight * lineHeight
          BaselineBottom -> position.y - totalHeight + toNumber i * lineTextHeight * lineHeight
          _ -> position.y + toNumber i * lineTextHeight * lineHeight
        x = case align of
          AlignStart -> position.x
          AlignLeft -> position.x
          AlignEnd -> position.x - line.width
          AlignRight -> position.x - line.width
          AlignCenter -> position.x - line.width / 2.0
      in
        layoutLine { x, y } line
  , lineHeight: lineTextHeight
  }

type LayoutLine = { words :: Array LayoutWord, position :: Point, width :: Number }

layoutLine :: Point -> Line -> LayoutLine
layoutLine position@{ x, y } line =
  let
    folder :: { offsetX :: Number, words :: Array LayoutWord } -> Either Word Space -> { offsetX :: Number, words :: Array LayoutWord }
    folder { offsetX, words } (Left word) =
      { offsetX: offsetX + word.metrics.width
      , words: words `Array.snoc` layoutWord { x: x + offsetX, y } word
      }
    folder { offsetX, words } (Right { width }) = { offsetX: offsetX + width, words }

    { words, offsetX: width } = Array.foldl folder { offsetX: 0.0, words: [] } line.words
  in
    { words, position, width }

type LayoutWord = { word :: String, position :: Point, metrics :: TextMetrics }

layoutWord :: Point -> Word -> LayoutWord
layoutWord { x, y } { word, metrics } =
  { word
  , metrics
  , position: { x, y: y + metrics.actualBoundingBoxAscent }
  }
