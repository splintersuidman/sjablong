module Sjablong.Layer.Undraggable
  ( Undraggable(..)
  , mkUndraggable
  , UndraggableHorizontal(..)
  , mkUndraggableHorizontal
  , UndraggableVertical(..)
  , mkUndraggableVertical
  ) where

import Prelude

import Sjablong.Layer (class Layer, class LayerWrapper, class Scalable, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, scale, translate)

newtype Undraggable l = Undraggable l

derive instance Functor Undraggable

instance LayerWrapper Undraggable where
  mapLayerWrapper f (Undraggable l) = Undraggable <$> f l

mkUndraggable :: forall l. l -> Undraggable l
mkUndraggable = Undraggable

instance (Monad m, Layer m l) => Layer m (Undraggable l) where
  position (Undraggable l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint p (Undraggable l) = containsPoint p l
  dragStart = const pure
  drag = const pure
  dragEnd = pure
  draw ctx (Undraggable l) = draw @m ctx l

instance (Monad m, Scalable m l, Layer m l) => Scalable m (Undraggable l) where
  scale = mapLayerWrapper <<< scale

newtype UndraggableHorizontal l = UndraggableHorizontal l

derive instance Functor UndraggableHorizontal

instance LayerWrapper UndraggableHorizontal where
  mapLayerWrapper f (UndraggableHorizontal l) = UndraggableHorizontal <$> f l

mkUndraggableHorizontal :: forall l. l -> UndraggableHorizontal l
mkUndraggableHorizontal = UndraggableHorizontal

instance (Monad m, Layer m l) => Layer m (UndraggableHorizontal l) where
  position (UndraggableHorizontal l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint p (UndraggableHorizontal l) = containsPoint p l
  dragStart { offsetY } = mapLayerWrapper $ dragStart { offsetX: 0.0, offsetY }
  drag { translateY } = mapLayerWrapper $ drag { translateX: 0.0, translateY }
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (UndraggableHorizontal l) = draw @m ctx l

instance (Monad m, Scalable m l, Layer m l) => Scalable m (UndraggableHorizontal l) where
  scale = mapLayerWrapper <<< scale

newtype UndraggableVertical l = UndraggableVertical l

derive instance Functor UndraggableVertical

instance LayerWrapper UndraggableVertical where
  mapLayerWrapper f (UndraggableVertical l) = UndraggableVertical <$> f l

mkUndraggableVertical :: forall l. l -> UndraggableVertical l
mkUndraggableVertical = UndraggableVertical

instance (Monad m, Layer m l) => Layer m (UndraggableVertical l) where
  position (UndraggableVertical l) = position l
  translate = mapLayerWrapper <<< translate
  containsPoint p (UndraggableVertical l) = containsPoint p l
  dragStart { offsetX } = mapLayerWrapper $ dragStart { offsetX, offsetY: 0.0 }
  drag { translateX } = mapLayerWrapper $ drag { translateX, translateY: 0.0 }
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (UndraggableVertical l) = draw @m ctx l

instance (Monad m, Scalable m l, Layer m l) => Scalable m (UndraggableVertical l) where
  scale = mapLayerWrapper <<< scale
