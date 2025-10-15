module Sjablong.Layer.Snap
  ( dragSnapHorizontal
  , dragTranslateSnapHorizontal
  , dragTranslateSnapHorizontalMaybe
  , dragSnapVertical
  , dragTranslateSnapVertical
  , dragTranslateSnapVerticalMaybe
  , dragSnap
  , dragTranslateSnap
  , dragTranslateSnapMaybe
  , SnapHorizontal(..)
  , mkSnapHorizontal
  , SnapVertical(..)
  , mkSnapVertical
  , Snap(..)
  , mkSnap
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Number as Number
import Graphics.Canvas (TranslateTransform)
import Sjablong.Layer (class Layer, class LayerWrapper, DragOffset, containsPoint, drag, dragEnd, dragStart, dragTranslate, draw, mapLayerWrapper, position, translate)

dragSnapHorizontal :: forall m l. Layer m l => Monad m => Number -> Number -> TranslateTransform -> l -> m l
dragSnapHorizontal offsetX gridSizeX { translateX, translateY } layer = do
  pos <- position layer
  let x' = offsetX + gridSizeX * Number.round ((pos.x + translateX - offsetX) / gridSizeX)
  drag { translateX: x' - pos.x, translateY } layer

dragTranslateSnapHorizontal :: forall m l. Layer m l => Monad m => Number -> Number -> DragOffset -> TranslateTransform -> l -> m l
dragTranslateSnapHorizontal offsetX gridSizeX dragOffset { translateX, translateY } layer = do
  pos <- position layer
  let x' = offsetX + gridSizeX * Number.round ((pos.x + translateX - offsetX) / gridSizeX)
  dragTranslate dragOffset { translateX: x' - pos.x, translateY } layer

dragTranslateSnapHorizontalMaybe :: forall m l. Layer m l => Monad m => Number -> Number -> Maybe DragOffset -> TranslateTransform -> l -> m l
dragTranslateSnapHorizontalMaybe _ _ Nothing _ layer = pure layer
dragTranslateSnapHorizontalMaybe offsetX gridSizeX (Just dragOffset) translation layer = dragTranslateSnapHorizontal offsetX gridSizeX dragOffset translation layer

dragSnapVertical :: forall m l. Layer m l => Monad m => Number -> Number -> TranslateTransform -> l -> m l
dragSnapVertical offsetY gridSizeY { translateX, translateY } layer = do
  pos <- position layer
  let y' = offsetY + gridSizeY * Number.round ((pos.y + translateY - offsetY) / gridSizeY)
  drag { translateX, translateY: y' - pos.y } layer

dragTranslateSnapVertical :: forall m l. Layer m l => Monad m => Number -> Number -> DragOffset -> TranslateTransform -> l -> m l
dragTranslateSnapVertical offsetY gridSizeY dragOffset { translateX, translateY } layer = do
  pos <- position layer
  let y' = offsetY + gridSizeY * Number.round ((pos.y + translateY - offsetY) / gridSizeY)
  dragTranslate dragOffset { translateX, translateY: y' - pos.y } layer

dragTranslateSnapVerticalMaybe :: forall m l. Layer m l => Monad m => Number -> Number -> Maybe DragOffset -> TranslateTransform -> l -> m l
dragTranslateSnapVerticalMaybe _ _ Nothing _ layer = pure layer
dragTranslateSnapVerticalMaybe offsetY gridSizeY (Just dragOffset) translation layer = dragTranslateSnapVertical offsetY gridSizeY dragOffset translation layer

dragSnap :: forall m l. Layer m l => Monad m => { offsetX :: Number, offsetY :: Number } -> { gridSizeX :: Number, gridSizeY :: Number } -> TranslateTransform -> l -> m l
dragSnap { offsetX, offsetY } { gridSizeX, gridSizeY } { translateX, translateY } layer = do
  pos <- position layer
  let x' = offsetX + gridSizeX * Number.round ((pos.x + translateX - offsetX) / gridSizeX)
  let y' = offsetY + gridSizeY * Number.round ((pos.y + translateY - offsetY) / gridSizeY)
  drag { translateX: x' - pos.x, translateY: y' - pos.y } layer

dragTranslateSnap :: forall m l. Layer m l => Monad m => { offsetX :: Number, offsetY :: Number } -> { gridSizeX :: Number, gridSizeY :: Number } -> DragOffset -> TranslateTransform -> l -> m l
dragTranslateSnap { offsetX, offsetY } { gridSizeX, gridSizeY } dragOffset { translateX, translateY } layer = do
  pos <- position layer
  let x' = offsetX + gridSizeX * Number.round ((pos.x + translateX - offsetX) / gridSizeX)
  let y' = offsetY + gridSizeY * Number.round ((pos.y + translateY - offsetY) / gridSizeY)
  dragTranslate dragOffset { translateX: x' - pos.x, translateY: y' - pos.y } layer

dragTranslateSnapMaybe :: forall m l. Layer m l => Monad m => { offsetX :: Number, offsetY :: Number } -> { gridSizeX :: Number, gridSizeY :: Number } -> Maybe DragOffset -> TranslateTransform -> l -> m l
dragTranslateSnapMaybe _ _ Nothing _ layer = pure layer
dragTranslateSnapMaybe offset gridSize (Just dragOffset) translation layer = dragTranslateSnap offset gridSize dragOffset translation layer

newtype SnapHorizontal l = SnapHorizontal
  { layer :: l
  , offsetX :: Number
  , gridSizeX :: Number
  }

instance LayerWrapper SnapHorizontal where
  mapLayerWrapper f (SnapHorizontal l) = f l.layer <#> \layer -> SnapHorizontal l { layer = layer }

mkSnapHorizontal :: forall l. Number -> Number -> l -> SnapHorizontal l
mkSnapHorizontal offsetX gridSizeX layer = SnapHorizontal { layer, gridSizeX, offsetX }

instance (Layer m l, Monad m) => Layer m (SnapHorizontal l) where
  position (SnapHorizontal l) = position l.layer
  translate = mapLayerWrapper <<< translate
  containsPoint p (SnapHorizontal l) = containsPoint p l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag translation layer@(SnapHorizontal l) = mapLayerWrapper (dragSnapHorizontal l.offsetX l.gridSizeX translation) layer
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (SnapHorizontal l) = draw @m ctx l.layer

newtype SnapVertical l = SnapVertical
  { layer :: l
  , offsetY :: Number
  , gridSizeY :: Number
  }

instance LayerWrapper SnapVertical where
  mapLayerWrapper f (SnapVertical l) = f l.layer <#> \layer -> SnapVertical l { layer = layer }

mkSnapVertical :: forall l. Number -> Number -> l -> SnapVertical l
mkSnapVertical offsetY gridSizeY layer = SnapVertical { layer, gridSizeY, offsetY }

instance (Layer m l, Monad m) => Layer m (SnapVertical l) where
  position (SnapVertical l) = position l.layer
  translate = mapLayerWrapper <<< translate
  containsPoint p (SnapVertical l) = containsPoint p l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag translation layer@(SnapVertical l) = mapLayerWrapper (dragSnapVertical l.offsetY l.gridSizeY translation) layer
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (SnapVertical l) = draw @m ctx l.layer

newtype Snap l = Snap
  { layer :: l
  , offset :: { offsetX :: Number, offsetY :: Number }
  , gridSize :: { gridSizeX :: Number, gridSizeY :: Number }
  }

instance LayerWrapper Snap where
  mapLayerWrapper f (Snap l) = f l.layer <#> \layer -> Snap l { layer = layer }

mkSnap :: forall l. { offsetX :: Number, offsetY :: Number } -> { gridSizeX :: Number, gridSizeY :: Number } -> l -> Snap l
mkSnap offset gridSize layer = Snap { layer, gridSize, offset }

instance (Layer m l, Monad m) => Layer m (Snap l) where
  position (Snap l) = position l.layer
  translate = mapLayerWrapper <<< translate
  containsPoint p (Snap l) = containsPoint p l.layer
  dragStart = mapLayerWrapper <<< dragStart
  drag translation layer@(Snap l) = mapLayerWrapper (dragSnap l.offset l.gridSize translation) layer
  dragEnd = mapLayerWrapper dragEnd
  draw ctx (Snap l) = draw @m ctx l.layer
