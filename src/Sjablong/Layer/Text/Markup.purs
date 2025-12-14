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

import Data.Array (all, any, catMaybes, concat, concatMap, foldl, intersperse, length, singleton, snoc, splitAt) as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Either (Either(..), either)
import Data.Foldable (foldl, for_, maximum, sum)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (null) as String
import Data.String.CodeUnits (fromCharArray)
import Data.String.Utils (lines, words) as String
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Graphics.Canvas (Context2D, TextAlign(..), TextBaseline(..), fillText, withContext)
import Graphics.Canvas (setFillStyle, setFont, setTextAlign, setTextBaseline) as Canvas
import Graphics.Canvas.Extra (setLetterSpacing) as Canvas
import Graphics.Canvas.TextMetrics (TextMetrics, measureText, textMetricsBoundingBoxHeight)
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

  containsPoint { x, y } layer@(MarkupTextLayer l) = liftEffect $ withContext l.context do
    { lines } <- layoutText layer
    pure $ flip Array.any lines \{ position, width, height } ->
      position.x <= x && x <= position.x + width && position.y <= y && y <= position.y + height

  dragStart offset (MarkupTextLayer layer) = pure $ MarkupTextLayer layer { dragOffset = Just offset }
  drag t l@(MarkupTextLayer layer) = dragTranslateMaybe layer.dragOffset t l
  dragEnd (MarkupTextLayer layer) = pure $ MarkupTextLayer layer { dragOffset = Nothing }

  draw ctx layer@(MarkupTextLayer l) = withContext ctx do
    Canvas.setFillStyle ctx l.fillStyle
    -- Alignment and baseline are handled manually
    Canvas.setTextAlign ctx AlignLeft
    Canvas.setTextBaseline ctx BaselineTop
    Canvas.setLetterSpacing ctx l.letterSpacing

    layout <- layoutText layer
    for_ layout.lines \line -> for_ line.words \word -> for_ word.fragments \{ fragment, position: { x, y } } -> do
      setMarkupFont ctx l.font fragment
      fillText ctx (textMarkup fragment) x y

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

measureMarkup :: Context2D -> Font -> Markup -> Effect TextMetrics
measureMarkup ctx font m@(Markup markup) = withContext ctx do
  setMarkupFont ctx font m
  measureText ctx markup.text

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
  maxHeight <- measureMaxMarkupHeight ctx font $ Array.concat $ Array.concatMap markupLineWords lines
  pure $ lines <#> \line -> Record.merge line
    { height: if nullMarkupLine line then emptyLineHeight * font.size else maxHeight }

-- Markup fragment of a word with width
type MarkupFragment = { fragment :: Markup, metrics :: TextMetrics }

mkMarkupFragment :: Markup -> TextMetrics -> MarkupFragment
mkMarkupFragment = { fragment: _, metrics: _ }

measureMarkupFragment :: Context2D -> Font -> Markup -> Effect MarkupFragment
measureMarkupFragment ctx font markup = mkMarkupFragment markup <$> measureMarkup ctx font markup

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

type Space = { width :: Number }

type MarkupLineWith p = { words :: Array (Either MarkupWord Space), width :: Number | p }

type MarkupLine = MarkupLineWith ()

markupLineWords :: forall p. MarkupLineWith p -> Array (Array Markup)
markupLineWords { words } = map forgetMarkupWord $ Array.catMaybes $ map (either Just (const Nothing)) words

nullMarkupLine :: forall p. MarkupLineWith p -> Boolean
nullMarkupLine { words } = flip Array.all words $ case _ of
  Left word -> nullMarkupWord word
  Right space -> space.width == 0.0

measureMarkupWordWidth :: Context2D -> Font -> Array Markup -> Effect MarkupWord
measureMarkupWordWidth ctx font text = do
  word <- traverse (measureMarkupFragment ctx font) text
  let width = sum $ map (_.metrics.width) word
  pure { word, width }

measureMarkupLineWidth :: Context2D -> Font -> Array Markup -> Effect MarkupLine
measureMarkupLineWidth ctx font text = do
  spaceWidth <- measureSpaceWidth ctx font
  words <- traverse (measureMarkupWordWidth ctx font) $ wordsMarkup text
  pure
    { words: Array.intersperse (Right { width: spaceWidth }) $ map Left words
    , width: sum (map (_.width) words) + spaceWidth * toNumber (Array.length words - 1)
    }

snocMarkupLine :: Number -> MarkupLine -> MarkupWord -> MarkupLine
snocMarkupLine spaceWidth line word =
  { words: Array.snoc line.words $ Left word
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
    go (lines /\ { words: [] }) word = lines /\ { words: [ Left word ], width: word.width }
    go (lines /\ line) word =
      if line.width + spaceWidth + word.width > maxWidth then Array.snoc lines line /\ { words: [ Left word ], width: word.width }
      else lines /\
        { words: line.words `Array.snoc` Right { width: spaceWidth } `Array.snoc` Left word
        , width: line.width + spaceWidth + word.width
        }
  let lines /\ line = foldl go ([] /\ { words: [], width: 0.0 }) wordsAndWidths

  case line.words of
    [] -> pure lines
    _ -> pure $ Array.snoc lines line

type LayoutMarkup = { lines :: Array LayoutMarkupLine, position :: Point }

layoutText :: MarkupTextLayer -> Effect LayoutMarkup
layoutText (MarkupTextLayer layer) = withContext layer.context do
  Canvas.setFillStyle layer.context layer.fillStyle
  -- Alignment and baseline are handled manually
  Canvas.setTextAlign layer.context AlignLeft
  Canvas.setTextBaseline layer.context BaselineTop
  Canvas.setLetterSpacing layer.context layer.letterSpacing

  lines' <- case layer.maxWidth of
    Just maxWidth -> wrapLinesMarkup layer.context layer.font maxWidth layer.text
    Nothing -> traverse (measureMarkupLineWidth layer.context layer.font) $ linesMarkup layer.text
  lines <- measureMarkupLineHeights layer.context layer.font layer.emptyLineHeight lines'
  pure $ layoutMarkupLines layer.position layer.font layer.align layer.baseline layer.lineHeight layer.emptyLineHeight lines

-- TODO: take direction into account (for AlignStart and AlignEnd)
layoutMarkupLines :: Point -> Font -> TextAlign -> TextBaseline -> Number -> Number -> Array (MarkupLineWith (height :: Number)) -> LayoutMarkup
layoutMarkupLines position font align baseline lineHeight emptyLineHeight lines =
  let
    totalHeight = sum (map (_.height) $ lines) + (lineHeight - 1.0) * font.size * toNumber (Array.length lines - 1)

    folder
      :: { offsetY :: Number, lines :: Array LayoutMarkupLine }
      -> MarkupLineWith (height :: Number)
      -> { offsetY :: Number, lines :: Array LayoutMarkupLine }
    folder { offsetY, lines } line =
      { offsetY: offsetY + line.height
      , lines:
          let
            y = case baseline of
              BaselineTop -> position.y + offsetY
              BaselineBottom -> position.y + offsetY - totalHeight
              _ -> position.y + offsetY
            xMin = case align of
              AlignStart -> position.x
              AlignLeft -> position.x
              AlignEnd -> position.x - line.width
              AlignRight -> position.x - line.width
              AlignCenter -> position.x - line.width / 2.0
          in
            lines `Array.snoc` layoutMarkupLine { x: xMin, y } font emptyLineHeight line
      }
    { lines } = Array.foldl folder { offsetY: 0.0, lines: [] } lines
  in
    { lines, position }

type LayoutMarkupLine = { words :: Array LayoutMarkupWord, position :: Point, width :: Number, height :: Number }

layoutMarkupLine :: Point -> Font -> Number -> MarkupLineWith (height :: Number) -> LayoutMarkupLine
layoutMarkupLine position@{ x, y } font emptyLineHeight line
  | nullMarkupLine line = { words: [], position, width: 0.0, height: emptyLineHeight * font.size }
  | otherwise =
      let
        folder
          :: { offsetX :: Number, words :: Array LayoutMarkupWord }
          -> Either MarkupWord Space
          -> { offsetX :: Number, words :: Array LayoutMarkupWord }
        folder { offsetX, words } (Left word) =
          { offsetX: offsetX + word.width
          , words: words `Array.snoc` layoutMarkupWord { x: x + offsetX, y } word
          }
        folder { offsetX, words } (Right { width }) = { offsetX: offsetX + width, words }

        { words, offsetX: width } = Array.foldl folder { offsetX: 0.0, words: [] } line.words
      in
        { words, position, width, height: line.height }

type LayoutMarkupWord = { fragments :: Array LayoutMarkupFragment, position :: Point, width :: Number, height :: Number }

layoutMarkupWord :: Point -> MarkupWord -> LayoutMarkupWord
layoutMarkupWord position@{ x, y } word =
  let
    folder
      :: { offsetX :: Number, fragments :: Array LayoutMarkupFragment }
      -> MarkupFragment
      -> { offsetX :: Number, fragments :: Array LayoutMarkupFragment }
    folder { offsetX, fragments } fragment =
      { offsetX: offsetX + fragment.metrics.width
      , fragments: fragments `Array.snoc` layoutMarkupFragment { x: x + offsetX, y } fragment
      }
    { fragments, offsetX: width } = Array.foldl folder { offsetX: 0.0, fragments: [] } word.word
    height = fromMaybe 0.0 $ maximum $ map (textMetricsBoundingBoxHeight <<< _.metrics) word.word
  in
    { fragments, position, width, height }

type LayoutMarkupFragment = { fragment :: Markup, position :: Point, metrics :: TextMetrics }

layoutMarkupFragment :: Point -> MarkupFragment -> LayoutMarkupFragment
layoutMarkupFragment { x, y } { fragment, metrics } =
  { fragment
  , metrics
  , position: { x, y: y + metrics.actualBoundingBoxAscent }
  }
