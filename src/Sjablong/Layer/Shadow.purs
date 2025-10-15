module Sjablong.Layer.Shadow
  ( Shadow(..)
  , mkShadow
  , setColor
  ) where

import Prelude

import Graphics.Canvas (setShadowBlur, setShadowColor, setShadowOffsetX, setShadowOffsetY, withContext)
import Sjablong.Layer (class Layer, class LayerWrapper, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, translate)

newtype Shadow l = Shadow
  { layer :: l
  , offset :: { offsetX :: Number, offsetY :: Number }
  , color :: String
  , blur :: Number
  }

derive instance Functor Shadow

instance LayerWrapper Shadow where
  mapLayerWrapper f (Shadow l) = f l.layer <#> \layer -> Shadow l { layer = layer }

mkShadow :: forall l. { offsetX :: Number, offsetY :: Number } -> String -> Number -> l -> Shadow l
mkShadow offset color blur layer = Shadow { layer, offset, color, blur }

setColor :: forall l. String -> Shadow l -> Shadow l
setColor color (Shadow l) = Shadow l { color = color }

instance (Layer m l, Monad m) => Layer m (Shadow l) where
  position (Shadow l) = position l.layer
  translate = mapLayerWrapper <<< translate
  containsPoint p (Shadow l) = containsPoint p l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (Shadow l) = withContext ctx do
    setShadowBlur ctx l.blur
    setShadowOffsetX ctx l.offset.offsetX
    setShadowOffsetY ctx l.offset.offsetY
    setShadowColor ctx l.color
    draw @m ctx l.layer
