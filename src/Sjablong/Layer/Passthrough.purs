module Sjablong.Layer.Passthrough
  ( Passthrough(..)
  , mkPassthrough
  ) where

import Prelude

import Sjablong.Layer (class Layer, class LayerWrapper, class Scalable, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, scale, translate)

newtype Passthrough l = Passthrough l

derive instance Functor Passthrough

instance LayerWrapper Passthrough where
  mapLayerWrapper f (Passthrough l) = Passthrough <$> f l

mkPassthrough :: forall l. l -> Passthrough l
mkPassthrough = Passthrough

instance (Monad m, Layer m l) => Layer m (Passthrough l) where
  position (Passthrough l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint _ _ = pure false
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (Passthrough l) = draw @m ctx l

instance (Monad m, Scalable m l, Layer m l) => Scalable m (Passthrough l) where
  scale = mapLayerWrapper <<< scale
