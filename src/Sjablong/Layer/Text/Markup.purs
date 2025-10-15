module Sjablong.Layer.Text.Markup
  ( MarkupTextLayer(..)
  , Font(..)
  , setText
  , setText'
  , setFontSize
  , setFillStyle
  , mapMaxWidth
  , FontStyle(..)
  , FontWeight(..)
  , Markup(..)
  ) where

import Prelude

import Data.Array (all, any, concat, concatMap, cons, length, scanl, singleton, snoc, splitAt, zip) as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..))
import Data.Foldable (foldl, for_, maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null) as String
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (lines, words) as String
import Data.Traversable (for, traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (Context2D, TextAlign(..), TextBaseline(..), fillText, withContext)
import Graphics.Canvas (setFillStyle, setFont, setTextAlign, setTextBaseline) as Canvas
import Graphics.Canvas.Extra (setLetterSpacing) as Canvas
import Parsing (ParseError, Parser)
import Parsing (runParser) as Parser
import Parsing.Combinators (between, try) as Parser
import Parsing.Combinators ((<|>))
import Parsing.Combinators.Array as Parser.Array
import Parsing.String (char, eof, rest) as Parser
import Parsing.String.Basic (noneOf) as Parser
import Record as Record
import Sjablong.Layer (class Layer, DragOffset, Point, dragTranslateMaybe, translatePoint)
import Sjablong.Layer.Text as TextLayer

data FontStyle = StyleNormal | StyleItalic

derive instance Eq FontStyle

instance Show FontStyle where
  show StyleNormal = "StyleNormal"
  show StyleItalic = "StyleItalic"

data FontWeight = WeightNormal | WeightBold

derive instance Eq FontWeight

instance Show FontWeight where
  show WeightNormal = "WeightNormal"
  show WeightBold = "WeightBold"

newtype Markup = Markup
  { text :: String
  , style :: FontStyle
  , weight :: FontWeight
  }

derive instance Eq Markup

instance Show Markup where
  show (Markup m) = "Markup " <> show m

mkMarkupNormal :: String -> Markup
mkMarkupNormal text = Markup { text, style: StyleNormal, weight: WeightNormal }

textMarkup :: Markup -> String
textMarkup (Markup { text }) = text

mkMarkupSplitter :: (String -> Array String) -> Array Markup -> Array (Array Markup)
mkMarkupSplitter splitter text = case foldl go { splits: [], split: [] } text of
  { splits, split } -> Array.snoc splits split
  where
  go { splits, split } (Markup m) = case Array.splitAt 1 (splitter m.text) of
    { before: [ "" ], after: [] } -> { splits, split }
    { before: [ first ], after: [] } -> { splits, split: Array.snoc split (Markup m { text = first }) }
    { before: [ "" ], after } -> case Array.splitAt (Array.length after - 1) after of
      { before: splits', after: [ "" ] } ->
        { splits: Array.snoc splits split <> (splits' <#> \l -> [ Markup m { text = l } ])
        , split: []
        }
      { before: splits', after: [ last ] } ->
        { splits: Array.snoc splits split <> (splits' <#> \l -> [ Markup m { text = l } ])
        , split: [ Markup m { text = last } ]
        }
      -- Impossible: Array.length after is positive
      _ -> { splits, split }
    { before: [ first ], after } -> case Array.splitAt (Array.length after - 1) after of
      { before: splits', after: [ "" ] } ->
        { splits: Array.snoc splits (Array.snoc split (Markup m { text = first })) <>
            (splits' <#> \l -> [ Markup m { text = l } ])
        , split: []
        }
      { before: splits', after: [ last ] } ->
        { splits: Array.snoc splits (Array.snoc split (Markup m { text = first })) <>
            (splits' <#> \l -> [ Markup m { text = l } ])
        , split: [ Markup m { text = last } ]
        }
      -- Impossible: Array.length after is positive
      _ -> { splits, split }
    -- Impossible: Array.length >>> String.splits has positive range
    _ -> { splits, split }

wordsMarkup :: Array Markup -> Array (Array Markup)
wordsMarkup = mkMarkupSplitter String.words

linesMarkup :: Array Markup -> Array (Array Markup)
linesMarkup = mkMarkupSplitter String.lines

italic :: Markup -> Markup
italic (Markup m) = Markup m { style = StyleItalic }

bold :: Markup -> Markup
bold (Markup m) = Markup m { weight = WeightBold }

markupParser :: Parser String (Array Markup)
markupParser = map (Array.concat <<< NonEmpty.toArray) $ Parser.Array.many1 $
  italicBoldP
    <|> boldItalicP
    <|> Array.singleton <$> normal

many1' :: forall s a. Parser s a -> Parser s (Array a)
many1' = map NonEmpty.toArray <<< Parser.Array.many1

normal :: Parser String Markup
normal = mkMarkupNormal <<< fromCharArray <$> many1' (Parser.noneOf [ '_', '*' ])

italicBoldP :: Parser String (Array Markup)
italicBoldP = Parser.try (Parser.between (Parser.char '_') (Parser.char '_') $ map italic <$> many1' boldP)
  <|> Parser.char '_' *> ([] <$ Parser.eof <|> map italic <$> many1' boldP)

boldP :: Parser String Markup
boldP = Parser.try (Parser.between (Parser.char '*') (Parser.char '*') $ bold <$> normal)
  <|> normal
  <|> Parser.char '*' *> (bold <<< mkMarkupNormal <$> Parser.rest)

boldItalicP :: Parser String (Array Markup)
boldItalicP = Parser.try (Parser.between (Parser.char '*') (Parser.char '*') $ map bold <$> many1' italicP)
  <|> Parser.char '*' *> ([] <$ Parser.eof <|> map bold <$> many1' italicP)

italicP :: Parser String Markup
italicP = Parser.try (Parser.between (Parser.char '_') (Parser.char '_') $ italic <$> normal)
  <|> normal
  <|> Parser.char '_' *> (italic <<< mkMarkupNormal <$> Parser.rest)

type Font =
  { name :: String
  , size :: Number
  , style :: { normal :: String, italic :: String }
  , weight :: { normal :: String, bold :: String }
  }

newtype MarkupTextLayer = MarkupTextLayer
  { text :: Array Markup
  , position :: Point
  , maxWidth :: Maybe Number
  , fillStyle :: String
  , font :: Font
  , align :: TextAlign
  , baseline :: TextBaseline
  , lineHeight :: Number
  , emptyLineHeight :: Number
  , letterSpacing :: String
  , dragOffset :: Maybe DragOffset
  , context :: Context2D
  }

setText :: String -> MarkupTextLayer -> Either ParseError MarkupTextLayer
setText text (MarkupTextLayer l) = Parser.runParser text markupParser <#> \markup -> MarkupTextLayer l { text = markup }

-- Ignore errors and set the text with control characters
setText' :: String -> MarkupTextLayer -> MarkupTextLayer
setText' text (MarkupTextLayer l) = case Parser.runParser text markupParser of
  Right markup -> MarkupTextLayer l { text = markup }
  Left _err -> MarkupTextLayer l { text = [ Markup { text, style: StyleNormal, weight: WeightNormal } ] }

setFontSize :: Number -> MarkupTextLayer -> MarkupTextLayer
setFontSize fontSize (MarkupTextLayer l) = MarkupTextLayer l { font { size = fontSize } }

setFillStyle :: String -> MarkupTextLayer -> MarkupTextLayer
setFillStyle fillStyle (MarkupTextLayer l) = MarkupTextLayer l { fillStyle = fillStyle }

mapMaxWidth :: (Number -> Number) -> MarkupTextLayer -> MarkupTextLayer
mapMaxWidth f (MarkupTextLayer l) = MarkupTextLayer l { maxWidth = map f l.maxWidth }

instance MonadEffect m => Layer m MarkupTextLayer where
  position (MarkupTextLayer l) = pure l.position
  translate translation (MarkupTextLayer l) = pure $ MarkupTextLayer l { position = translatePoint translation l.position }

  -- TODO: take baseline into account
  -- TODO: take direction into account (for AlignStart and AlignEnd)
  containsPoint { x, y } (MarkupTextLayer l) = liftEffect $ withContext l.context do
    Canvas.setFillStyle l.context l.fillStyle
    Canvas.setTextBaseline l.context l.baseline
    Canvas.setTextAlign l.context l.align
    Canvas.setLetterSpacing l.context l.letterSpacing

    lines' <- case l.maxWidth of
      Just maxWidth -> wrapLinesMarkup l.context l.font maxWidth l.text
      Nothing -> traverse (measureMarkupLineWidth l.context l.font) $ linesMarkup l.text
    lines <- measureMarkupLineHeights l.context l.font l.emptyLineHeight lines'

    let
      totalHeight = sum (map (_.height) $ lines) + (l.lineHeight - 1.0) * l.font.size * toNumber (Array.length lines - 1)
      lineOffsets = Array.cons 0.0 $
        Array.scanl (\offsetY { height } -> offsetY + height + (l.lineHeight - 1.0) * l.font.size) 0.0 lines

    cs <- for (Array.zip lines lineOffsets) \({ width, height } /\ offsetY) -> do
      -- XXX: support other baselines, cf. draw
      let
        lineY = case l.baseline of
          BaselineTop -> l.position.y + offsetY
          BaselineBottom -> l.position.y + offsetY - totalHeight
          _ -> l.position.y + offsetY

      pure $ case l.align of
        AlignStart -> l.position.x <= x
          && x <= l.position.x + width
          && lineY <= y
          && y <= lineY + height
        AlignLeft -> l.position.x <= x
          && x <= l.position.x + width
          && lineY <= y
          && y <= lineY + height
        AlignEnd -> l.position.x - width <= x
          && x <= l.position.x
          && lineY <= y
          && y <= lineY + height
        AlignRight -> l.position.x - width <= x
          && x <= l.position.x
          && lineY <= y
          && y <= lineY + height
        AlignCenter -> l.position.x - width / 2.0 <= x
          && x <= l.position.x + width / 2.0
          && lineY <= y
          && y <= lineY + height

    pure $ Array.any identity cs

  dragStart offset (MarkupTextLayer layer) = pure $ MarkupTextLayer layer { dragOffset = Just offset }
  drag t l@(MarkupTextLayer layer) = dragTranslateMaybe layer.dragOffset t l
  dragEnd (MarkupTextLayer layer) = pure $ MarkupTextLayer layer { dragOffset = Nothing }

  draw ctx (MarkupTextLayer l) = withContext ctx do
    Canvas.setFillStyle ctx l.fillStyle
    Canvas.setTextBaseline ctx l.baseline
    -- Alignment is handled manually below
    Canvas.setTextAlign ctx AlignLeft
    Canvas.setLetterSpacing ctx l.letterSpacing

    lines' <- case l.maxWidth of
      Just maxWidth -> wrapLinesMarkup ctx l.font maxWidth l.text
      Nothing -> traverse (measureMarkupLineWidth ctx l.font) $ linesMarkup l.text
    lines <- measureMarkupLineHeights ctx l.font l.emptyLineHeight lines'

    let
      totalHeight = sum (map (_.height) $ lines) + (l.lineHeight - 1.0) * l.font.size * toNumber (Array.length lines - 1)
      lineOffsets = Array.cons 0.0 $
        Array.scanl (\offsetY { height } -> offsetY + height + (l.lineHeight - 1.0) * l.font.size) 0.0 lines

    for_ (Array.zip lines lineOffsets) \(line /\ offsetY) -> do
      let
        -- XXX: support other baselines
        -- XXX: maybe add another property indicating whether the text
        -- should be anchored at the top or bottom, and use baseline
        -- only for what fillText uses it for (per line instead of per
        -- block)
        y = case l.baseline of
          BaselineTop -> l.position.y + offsetY
          BaselineBottom -> l.position.y + offsetY - totalHeight + line.height
          _ -> l.position.y + offsetY
        xMin = case l.align of
          AlignStart -> l.position.x
          AlignLeft -> l.position.x
          AlignEnd -> l.position.x - line.width
          AlignRight -> l.position.x - line.width
          AlignCenter -> l.position.x - line.width / 2.0

      spaceWidth <- measureSpaceWidth ctx l.font
      let wordOffsets = Array.cons 0.0 $ Array.scanl (\offsetX { width } -> offsetX + spaceWidth + width) 0.0 line.line

      for_ (Array.zip line.line wordOffsets) \({ word } /\ wordOffsetX) -> do
        let fragmentOffsets = Array.cons 0.0 $ Array.scanl (\offsetX { width } -> offsetX + width) 0.0 word
        for_ (Array.zip word fragmentOffsets) \({ fragment } /\ fragmentOffsetX) -> do
          setMarkupFont ctx l.font fragment
          fillText ctx (textMarkup fragment) (xMin + wordOffsetX + fragmentOffsetX) y

setMarkupFont :: Context2D -> Font -> Markup -> Effect Unit
setMarkupFont ctx font (Markup text) = do
  let
    style = case text.style of
      StyleNormal -> font.style.normal
      StyleItalic -> font.style.italic
    weight = case text.weight of
      WeightNormal -> font.weight.normal
      WeightBold -> font.weight.bold
  Canvas.setFont ctx $ style <> " " <> weight <> " " <> show font.size <> "px " <> font.name

measureMarkupWidth :: Context2D -> Font -> Markup -> Effect Number
measureMarkupWidth ctx font m@(Markup markup) = withContext ctx do
  setMarkupFont ctx font m
  TextLayer.measureTextWidth ctx markup.text

measureSpaceWidth :: Context2D -> Font -> Effect Number
measureSpaceWidth ctx font = measureMarkupWidth ctx font $ Markup { style: StyleNormal, weight: WeightNormal, text: " " }

measureMarkupHeight :: Context2D -> Font -> Markup -> Effect Number
measureMarkupHeight ctx font m@(Markup markup) = withContext ctx do
  setMarkupFont ctx font m
  TextLayer.measureTextHeight ctx markup.text

measureMaxMarkupHeight :: Context2D -> Font -> Array Markup -> Effect Number
measureMaxMarkupHeight ctx font lines = fromMaybe 0.0 <<< maximum <$> traverse (measureMarkupHeight ctx font) lines

measureMarkupLineHeights :: Context2D -> Font -> Number -> Array MarkupLine -> Effect (Array (MarkupLineWith (height :: Number)))
measureMarkupLineHeights ctx font emptyLineHeight lines = do
  maxHeight <- measureMaxMarkupHeight ctx font $ Array.concat $ Array.concatMap forgetMarkupLine lines
  pure $ lines <#> \line -> Record.merge line
    { height: if nullMarkupLine line then emptyLineHeight * font.size else maxHeight }

-- Markup fragment of a word with width
type MarkupFragment = { fragment :: Markup, width :: Number }

mkMarkupFragment :: Markup -> Number -> MarkupFragment
mkMarkupFragment = { fragment: _, width: _ }

forgetMarkupFragment :: MarkupFragment -> Markup
forgetMarkupFragment { fragment } = fragment

nullMarkupFragment :: MarkupFragment -> Boolean
nullMarkupFragment { fragment } = String.null $ textMarkup fragment

-- Markup word, e.g. "a_bc*de*f_", with total width
type MarkupWord = { word :: Array MarkupFragment, width :: Number }

mkMarkupWord :: Array MarkupFragment -> Number -> MarkupWord
mkMarkupWord = { word: _, width: _ }

forgetMarkupWord :: MarkupWord -> Array Markup
forgetMarkupWord { word } = map forgetMarkupFragment word

nullMarkupWord :: MarkupWord -> Boolean
nullMarkupWord { word } = Array.all nullMarkupFragment word

type MarkupLineWith p = { line :: Array MarkupWord, width :: Number | p }

type MarkupLine = MarkupLineWith ()

mkMarkupLine :: Array MarkupWord -> Number -> MarkupLine
mkMarkupLine = { line: _, width: _ }

forgetMarkupLine :: forall p. MarkupLineWith p -> Array (Array Markup)
forgetMarkupLine { line } = map forgetMarkupWord line

nullMarkupLine :: forall p. MarkupLineWith p -> Boolean
nullMarkupLine { line } = Array.all nullMarkupWord line

measureMarkupFragmentWidth :: Context2D -> Font -> Markup -> Effect MarkupFragment
measureMarkupFragmentWidth ctx font fragment = mkMarkupFragment fragment <$> measureMarkupWidth ctx font fragment

measureMarkupWordWidth :: Context2D -> Font -> Array Markup -> Effect MarkupWord
measureMarkupWordWidth ctx font text = do
  word <- traverse (measureMarkupFragmentWidth ctx font) text
  let width = sum $ map (_.width) word
  pure { word, width }

measureMarkupLineWidth :: Context2D -> Font -> Array Markup -> Effect MarkupLine
measureMarkupLineWidth ctx font text = do
  let words = wordsMarkup text
  line <- traverse (measureMarkupWordWidth ctx font) words
  spaceWidth <- measureSpaceWidth ctx font
  let
    width = spaceWidth * toNumber (Array.length line - 1)
      + sum (map (_.width) line)
  pure { line, width }

snocMarkupLine :: Number -> MarkupLine -> MarkupWord -> MarkupLine
snocMarkupLine spaceWidth line word =
  { line: Array.snoc line.line word
  , width: line.width + spaceWidth + word.width
  }

wrapLinesMarkup :: Context2D -> Font -> Number -> Array Markup -> Effect (Array MarkupLine)
wrapLinesMarkup ctx font maxWidth text = Array.concat <$> traverse (wrapLineMarkup ctx font maxWidth) (linesMarkup text)

wrapLineMarkup :: Context2D -> Font -> Number -> Array Markup -> Effect (Array MarkupLine)
wrapLineMarkup ctx font maxWidth text = do
  let words = wordsMarkup text
  wordsAndWidths <- traverse (measureMarkupWordWidth ctx font) words
  spaceWidth <- measureSpaceWidth ctx font

  let
    go :: Array MarkupLine /\ MarkupLine -> MarkupWord -> Array MarkupLine /\ MarkupLine
    go (lines /\ { line: [] }) word = lines /\ { line: [ word ], width: word.width }
    go (lines /\ line) word =
      if line.width + spaceWidth + word.width > maxWidth then Array.snoc lines line /\ { line: [ word ], width: word.width }
      else lines /\ snocMarkupLine spaceWidth line word
  let lines /\ line = foldl go ([] /\ { line: [], width: 0.0 }) wordsAndWidths

  case line of
    { line: [] } -> pure lines
    _ -> pure $ Array.snoc lines line
