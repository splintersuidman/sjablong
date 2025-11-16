module Sjablong.Layer.Rectangle
  ( RectangleLayer(..)
  , mkRectangleLayer
  , setFillStyle
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Graphics.Canvas (Rectangle)
import Graphics.Canvas as Canvas
import Sjablong.Layer (class Layer, class Scalable, DragOffset, dragTranslateMaybe)

newtype RectangleLayer = RectangleLayer
  { rect :: Rectangle
  , fillStyle :: String
  , dragOffset :: Maybe DragOffset
  }

mkRectangleLayer :: Rectangle -> String -> RectangleLayer
mkRectangleLayer rect fillStyle = RectangleLayer { rect, fillStyle, dragOffset: Nothing }

setFillStyle :: String -> RectangleLayer -> RectangleLayer
setFillStyle fillStyle (RectangleLayer l) = RectangleLayer l { fillStyle = fillStyle }

instance Applicative m => Layer m RectangleLayer where
  position (RectangleLayer layer) = pure { x: layer.rect.x, y: layer.rect.y }

  translate { translateX, translateY } (RectangleLayer layer) = pure $ RectangleLayer layer
    { rect
        { x = layer.rect.x + translateX
        , y = layer.rect.y + translateY
        }
    }

  containsPoint { x, y } (RectangleLayer { rect }) = pure
    $ rect.x <= x && x <= rect.x + rect.width
        && rect.y <= y
        && y <= rect.y + rect.height

  draw ctx (RectangleLayer layer) = do
    Canvas.setFillStyle ctx layer.fillStyle
    Canvas.fillPath ctx $ Canvas.rect ctx layer.rect

  dragStart offset (RectangleLayer layer) = pure $ RectangleLayer layer { dragOffset = Just offset }
  drag translation l@(RectangleLayer layer) = dragTranslateMaybe layer.dragOffset translation l
  dragEnd (RectangleLayer layer) = pure $ RectangleLayer layer { dragOffset = Nothing }

instance Applicative m => Scalable m RectangleLayer where
  scale { scaleX, scaleY } (RectangleLayer l) = pure $ RectangleLayer l
    { rect
        { width = l.rect.width * scaleX
        , height = l.rect.height * scaleY
        }
    }
