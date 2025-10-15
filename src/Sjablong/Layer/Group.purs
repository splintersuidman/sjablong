module Sjablong.Layer.Group
  ( Group(..)
  , mkGroup
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (or, traverse_)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Graphics.Canvas as Canvas
import Sjablong.Layer (class Layer, DragOffset, Point, SomeLayer, containsPoint, dragTranslateMaybe, draw, translatePoint)

newtype Group m = Group
  { layers :: Array (SomeLayer m)
  , position :: Point
  , dragOffset :: Maybe DragOffset
  }

mkGroup :: forall @m. Array (SomeLayer m) -> Group m
mkGroup layers = Group
  { layers
  , position: { x: 0.0, y: 0.0 }
  , dragOffset: Nothing
  }

instance Monad m => Layer m (Group m) where
  position (Group l) = pure l.position
  translate t (Group l) = pure $ Group l { position = translatePoint t l.position }
  containsPoint p (Group l) = or <$> traverse (containsPoint (p - l.position)) l.layers

  draw ctx (Group l) = Canvas.withContext ctx do
    Canvas.translate ctx { translateX: l.position.x, translateY: l.position.y }
    traverse_ (Canvas.withContext ctx <<< draw @m ctx) $ Array.reverse l.layers

  dragStart offset (Group l) = pure $ Group l { dragOffset = Just offset }
  drag translation g@(Group l) = dragTranslateMaybe l.dragOffset translation g
  dragEnd (Group l) = pure $ Group l { dragOffset = Nothing }
