module Sjablong.Layer.Translate
  ( TranslateLayer(..)
  , mkTranslateLayer
  ) where

import Prelude

import Graphics.Canvas (TranslateTransform)
import Graphics.Canvas as Canvas
import Sjablong.Layer (class Layer, class LayerWrapper, class Scalable, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, scale, translatePoint)

newtype TranslateLayer l = TranslateLayer
  { translation :: TranslateTransform
  , layer :: l
  }

derive instance Functor TranslateLayer

instance LayerWrapper TranslateLayer where
  mapLayerWrapper f (TranslateLayer l) = f l.layer <#> \layer -> TranslateLayer l { layer = layer }

mkTranslateLayer :: forall l. TranslateTransform -> l -> TranslateLayer l
mkTranslateLayer translation layer = TranslateLayer { translation, layer }

instance (Monad m, Layer m l) => Layer m (TranslateLayer l) where
  position (TranslateLayer l) = translatePoint l.translation <$> position l.layer
  translate translation (TranslateLayer l) = pure $ TranslateLayer l { translation = translation + l.translation }
  containsPoint p (TranslateLayer l) = containsPoint (translatePoint (-l.translation) p) l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd

  draw ctx (TranslateLayer l) = Canvas.withContext ctx do
    Canvas.translate ctx l.translation
    draw @m ctx l.layer

instance (Monad m, Scalable m l, Layer m l) => Scalable m (TranslateLayer l) where
  scale = mapLayerWrapper <<< scale
