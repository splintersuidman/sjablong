module Sjablong.Layer where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Graphics.Canvas (Context2D, Dimensions, ScaleTransform, TranslateTransform)

type Point = { x :: Number, y :: Number }

translatePoint :: TranslateTransform -> Point -> Point
translatePoint { translateX, translateY } { x, y } = { x: x + translateX, y: y + translateY }

type DragOffset = { offsetX :: Number, offsetY :: Number }

translateOffset :: TranslateTransform -> DragOffset -> DragOffset
translateOffset { translateX, translateY } { offsetX, offsetY } = { offsetX: offsetX + translateX, offsetY: offsetY + translateY }

scaleDimensions :: ScaleTransform -> Dimensions -> Dimensions
scaleDimensions { scaleX, scaleY } { width, height } = { width: width * scaleX, height: height * scaleY }

class Layer (m :: Type -> Type) (l :: Type) where
  position :: l -> m Point
  translate :: TranslateTransform -> l -> m l
  containsPoint :: Point -> l -> m Boolean
  dragStart :: DragOffset -> l -> m l
  drag :: TranslateTransform -> l -> m l
  dragEnd :: l -> m l
  draw :: Context2D -> l -> Effect Unit

newtype SomeLayer m = SomeLayer (forall a. (forall l. Layer m l => l -> a) -> a)

mkSomeLayer :: forall @m l. Layer m l => l -> SomeLayer m
mkSomeLayer l = SomeLayer (_ $ l)

unSomeLayer :: forall m a. (forall l. Layer m l => l -> a) -> SomeLayer m -> a
unSomeLayer k1 (SomeLayer k2) = k2 k1

modifySomeLayer :: forall m. Functor m => (forall l. Layer m l => l -> m l) -> SomeLayer m -> m (SomeLayer m)
modifySomeLayer f = unSomeLayer (map mkSomeLayer <<< f)

mapSomeLayer :: forall m l'. Layer m l' => (forall l. Layer m l => l -> l') -> SomeLayer m -> SomeLayer m
mapSomeLayer f = unSomeLayer (mkSomeLayer <<< f)

instance Functor m => Layer m (SomeLayer m) where
  position = unSomeLayer position
  translate t = modifySomeLayer (translate t)
  containsPoint p = unSomeLayer (containsPoint p)
  draw ctx = unSomeLayer (draw @m ctx)
  dragStart p = modifySomeLayer (dragStart p)
  drag t = modifySomeLayer (drag t)
  dragEnd = modifySomeLayer dragEnd

dragTranslate :: forall m l. Layer m l => DragOffset -> TranslateTransform -> l -> m l
dragTranslate { offsetX, offsetY } { translateX, translateY } layer =
  let
    translation = { translateX: translateX - offsetX, translateY: translateY - offsetY }
  in
    translate translation layer

dragTranslateMaybe :: forall m l. Applicative m => Layer m l => Maybe DragOffset -> TranslateTransform -> l -> m l
dragTranslateMaybe Nothing _ layer = pure layer
dragTranslateMaybe (Just offset) translation layer = dragTranslate offset translation layer

class Scalable m l where
  scale :: ScaleTransform -> l -> m l

scalePreserveRatio :: forall m l. Scalable m l => Number -> l -> m l
scalePreserveRatio s = scale { scaleX: s, scaleY: s }

class LayerWrapper (w :: Type -> Type) where
  mapLayerWrapper :: forall m l. Layer m l => Monad m => (l -> m l) -> w l -> m (w l)
