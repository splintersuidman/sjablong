module Sjablong.Layer.Layers
  ( Layers(..)
  , mkLayers
  , addLayerBelow
  , addLayerAbove
  , SomeLayers(..)
  , mkSomeLayers
  , addSomeLayerBelow
  , addSomeLayerAbove
  ) where

import Prelude

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Maybe.Trans.Extra (hoistMaybe)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!))
import Data.Array as Array
import Data.Array.Extra (modifyAtM)
import Data.Foldable (or, traverse_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Graphics.Canvas as Canvas
import Sjablong.Layer (class Layer, SomeLayer, containsPoint, drag, dragEnd, dragStart, draw, mkSomeLayer, position, translate)

newtype Layers l = Layers
  { layers :: Array l
  , dragIndex :: Maybe Int
  }

type SomeLayers m = Layers (SomeLayer m)

mkLayers :: forall @l. Array l -> Layers l
mkLayers layers = Layers { layers, dragIndex: Nothing }

addLayerBelow :: forall l. l -> Layers l -> Layers l
addLayerBelow layer (Layers l) = Layers l { layers = Array.snoc l.layers layer }

addLayerAbove :: forall l. l -> Layers l -> Layers l
addLayerAbove layer (Layers l) = Layers l { layers = Array.cons layer l.layers }

mkSomeLayers :: forall @m. Array (SomeLayer m) -> SomeLayers m
mkSomeLayers = mkLayers

addSomeLayerBelow :: forall m l. Layer m l => l -> SomeLayers m -> SomeLayers m
addSomeLayerBelow layer (Layers l) = Layers l { layers = Array.snoc l.layers (mkSomeLayer layer) }

addSomeLayerAbove :: forall m l. Layer m l => l -> SomeLayers m -> SomeLayers m
addSomeLayerAbove layer (Layers l) = Layers l { layers = Array.cons (mkSomeLayer layer) l.layers }

instance (Monad m, Layer m l) => Layer m (Layers l) where
  position = const $ pure { x: 0.0, y: 0.0 }
  translate t (Layers l) = traverse (translate t) l.layers <#> \layers -> Layers l { layers = layers }
  containsPoint p (Layers l) = or <$> traverse (containsPoint p) l.layers

  draw ctx (Layers l) = traverse_ (Canvas.withContext ctx <<< draw @m ctx) $ Array.reverse l.layers

  dragStart { offsetX, offsetY } (Layers l) = fromMaybe (Layers l) <$> runMaybeT do
    cs <- lift $ traverse (containsPoint { x: offsetX, y: offsetY }) l.layers
    i <- hoistMaybe $ Array.findIndex identity cs
    layer <- hoistMaybe $ l.layers !! i
    pos <- lift $ position layer
    layers' <- modifyAtM i (dragStart { offsetX: offsetX - pos.x, offsetY: offsetY - pos.y }) l.layers
    pure $ Layers l { dragIndex = Just i, layers = layers' }

  drag { translateX, translateY } (Layers l) = fromMaybe (Layers l) <$> runMaybeT do
    i <- hoistMaybe l.dragIndex
    layer <- hoistMaybe $ l.layers !! i
    pos <- lift $ position layer
    layers' <- modifyAtM i (drag { translateX: translateX - pos.x, translateY: translateY - pos.y }) l.layers
    pure $ Layers l { layers = layers' }

  dragEnd (Layers l) = fromMaybe (Layers l { dragIndex = Nothing }) <$> runMaybeT do
    i <- hoistMaybe l.dragIndex
    layers' <- modifyAtM i dragEnd l.layers
    pure $ Layers l { dragIndex = Nothing, layers = layers' }
