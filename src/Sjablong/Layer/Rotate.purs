module Sjablong.Layer.Rotate
  ( RotateLayer(..)
  , mkRotateLayer
  , setAngle
  ) where

import Prelude

import Data.Number (cos, sin) as Number
import Sjablong.Layer (class Layer, class LayerWrapper, class Scalable, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, scale, translate)
import Sjablong.Layer.Transform (TransformLayer(..), mkTransformLayer)

newtype RotateLayer l = RotateLayer (TransformLayer l)

derive instance Functor RotateLayer

instance LayerWrapper RotateLayer where
  mapLayerWrapper f (RotateLayer l) = RotateLayer <$> mapLayerWrapper f l

mkRotateLayer :: forall l. Number -> l -> RotateLayer l
mkRotateLayer angle layer = RotateLayer $ mkTransformLayer
  { a: Number.cos angle
  , b: Number.sin angle
  , c: -Number.sin angle
  , d: Number.cos angle
  , e: 0.0
  , f: 0.0
  }
  layer

setAngle :: forall l. Number -> RotateLayer l -> RotateLayer l
setAngle angle (RotateLayer (TransformLayer l)) = mkRotateLayer angle l.layer

instance (Monad m, Layer m l) => Layer m (RotateLayer l) where
  position (RotateLayer l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint p (RotateLayer l) = containsPoint p l
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd

  draw ctx (RotateLayer l) = draw @m ctx l

instance (Monad m, Scalable m l, Layer m l) => Scalable m (RotateLayer l) where
  scale = mapLayerWrapper <<< scale
