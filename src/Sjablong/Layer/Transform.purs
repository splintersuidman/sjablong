module Sjablong.Layer.Transform
  ( TransformLayer(..)
  , mkTransformLayer
  , transformPoint
  , invertTransform
  ) where

import Prelude

import Graphics.Canvas (Transform)
import Graphics.Canvas as Canvas
import Sjablong.Layer (class Layer, class LayerWrapper, class Scalable, Point, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, scale, translate)

transformPoint :: Transform -> Point -> Point
transformPoint { a, b, c, d, e, f } { x, y } =
  { x: a * x + c * y + e
  , y: b * x + d * y + f
  }

invertTransform :: Transform -> Transform
invertTransform { a, b, c, d, e, f } =
  { a: d / (a * d - b * c)
  , b: b / (b * c - a * d)
  , c: c / (b * c - a * d)
  , d: a / (a * d - b * c)
  , e: (d * e - c * f) / (b * c - a * d)
  , f: (b * e - a * f) / (a * d - b * c)
  }

newtype TransformLayer l = TransformLayer
  { transform :: Transform
  , layer :: l
  }

derive instance Functor TransformLayer

instance LayerWrapper TransformLayer where
  mapLayerWrapper f (TransformLayer l) = f l.layer <#> \layer -> TransformLayer l { layer = layer }

mkTransformLayer :: forall l. Transform -> l -> TransformLayer l
mkTransformLayer transform layer = TransformLayer { transform, layer }

instance (Monad m, Layer m l) => Layer m (TransformLayer l) where
  position (TransformLayer l) = transformPoint l.transform <$> position l.layer
  translate = mapLayerWrapper <<< translate
  containsPoint p (TransformLayer l) = containsPoint (transformPoint (invertTransform l.transform) p) l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd

  draw ctx (TransformLayer l) = Canvas.withContext ctx do
    Canvas.transform ctx l.transform
    draw @m ctx l.layer

instance (Monad m, Scalable m l, Layer m l) => Scalable m (TransformLayer l) where
  scale = mapLayerWrapper <<< scale
