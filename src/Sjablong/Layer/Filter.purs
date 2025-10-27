module Sjablong.Layer.Filter
  ( Filter(..)
  , mkFilter
  , setFilter
  ) where

import Prelude

import Graphics.Canvas (withContext)
import Graphics.Canvas.Filter as Canvas
import Sjablong.Layer (class Layer, class LayerWrapper, containsPoint, drag, dragEnd, dragStart, draw, mapLayerWrapper, position, translate)

newtype Filter l = Filter
  { layer :: l
  , filter :: String
  }

derive instance Functor Filter

instance LayerWrapper Filter where
  mapLayerWrapper f (Filter l) = f l.layer <#> \layer -> Filter l { layer = layer }

mkFilter :: forall l. String -> l -> Filter l
mkFilter filter layer = Filter { layer, filter }

setFilter :: forall l. String -> Filter l -> Filter l
setFilter filter (Filter l) = Filter l { filter = filter }

instance (Layer m l, Monad m) => Layer m (Filter l) where
  position (Filter l) = position l.layer
  translate = mapLayerWrapper <<< translate
  containsPoint p (Filter l) = containsPoint p l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag = mapLayerWrapper <<< drag
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (Filter l) = withContext ctx do
    Canvas.setFilter ctx l.filter
    draw @m ctx l.layer
