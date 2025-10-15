module Sjablong.Layer.Ref
  ( RefLayer(..)
  , mkRefLayer
  , read
  , modify
  , modify_
  , modifyM
  , modifyM_
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (modify, new, read) as Ref
import Effect.Ref.Extra (modifyM, modifyM_) as Ref
import Sjablong.Layer (class Layer, class Scalable, containsPoint, drag, dragEnd, dragStart, draw, position, scale, translate)

newtype RefLayer l = RefLayer (Ref l)

mkRefLayer :: forall @m l. MonadEffect m => l -> m (RefLayer l)
mkRefLayer l = RefLayer <$> liftEffect (Ref.new l)

instance (MonadEffect m, Layer m l) => Layer m (RefLayer l) where
  position (RefLayer l) = position =<< liftEffect (Ref.read l)
  translate t (RefLayer l) = RefLayer l <$ Ref.modifyM (translate t) l
  containsPoint p (RefLayer l) = containsPoint p =<< liftEffect (Ref.read l)
  dragStart o (RefLayer l) = RefLayer l <$ Ref.modifyM (dragStart o) l
  drag t (RefLayer l) = RefLayer l <$ Ref.modifyM (drag t) l
  dragEnd (RefLayer l) = RefLayer l <$ Ref.modifyM dragEnd l
  draw ctx (RefLayer l) = draw @m ctx =<< Ref.read l

instance (MonadEffect m, Scalable m l) => Scalable m (RefLayer l) where
  scale s (RefLayer l) = RefLayer l <$ Ref.modifyM (scale s) l

read :: forall m l. MonadEffect m => RefLayer l -> m l
read (RefLayer l) = liftEffect $ Ref.read l

modify :: forall m l. MonadEffect m => (l -> l) -> RefLayer l -> m l
modify f (RefLayer l) = liftEffect $ Ref.modify f l

modify_ :: forall m l. MonadEffect m => (l -> l) -> RefLayer l -> m Unit
modify_ f l = unit <$ modify f l

modifyM :: forall m l. MonadEffect m => (l -> m l) -> RefLayer l -> m l
modifyM f (RefLayer l) = Ref.modifyM f l

modifyM_ :: forall m l. MonadEffect m => (l -> m l) -> RefLayer l -> m Unit
modifyM_ f (RefLayer l) = Ref.modifyM_ f l
