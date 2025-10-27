module Sjablong.Layer.Path
  ( PathLayer(..)
  , FillStyle(..)
  , setFillStyle
  ) where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (CanvasGradient, CanvasPattern, Context2D)
import Graphics.Canvas as Canvas
import Sjablong.Layer (class Layer, DragOffset, Point, dragTranslateMaybe, translatePoint)

data FillStyle
  = FillStyleString String
  | FillStylePattern CanvasPattern
  | FillStyleGradient CanvasGradient

setFillStyle :: Context2D -> FillStyle -> Effect Unit
setFillStyle ctx fillStyle = case fillStyle of
  FillStyleString s -> Canvas.setFillStyle ctx s
  FillStylePattern p -> Canvas.setPatternFillStyle ctx p
  FillStyleGradient g -> Canvas.setGradientFillStyle ctx g

newtype PathLayer = PathLayer
  { path :: Array Point
  , fillStyle :: FillStyle
  , dragOffset :: Maybe DragOffset
  }

instance Monad m => Layer m PathLayer where
  position (PathLayer layer) = case layer.path !! 0 of
    Just pos -> pure pos
    Nothing -> pure { x: 0.0, y: 0.0 }

  translate translation (PathLayer layer) = pure $ PathLayer layer { path = map (translatePoint translation) layer.path }

  -- TODO: implement
  containsPoint _ _ = pure false

  draw ctx (PathLayer layer) = Canvas.withContext ctx do
    setFillStyle ctx layer.fillStyle
    Canvas.beginPath ctx
    case Array.head layer.path of
      Just { x, y } -> Canvas.moveTo ctx x y
      Nothing -> pure unit
    case Array.tail layer.path of
      Just tail -> for_ tail \{ x, y } -> Canvas.lineTo ctx x y
      Nothing -> pure unit
    Canvas.closePath ctx
    Canvas.fill ctx

  dragStart offset (PathLayer layer) = pure $ PathLayer layer { dragOffset = Just offset }
  drag translation l@(PathLayer layer) = dragTranslateMaybe layer.dragOffset translation l
  dragEnd (PathLayer layer) = pure $ PathLayer layer { dragOffset = Nothing }
