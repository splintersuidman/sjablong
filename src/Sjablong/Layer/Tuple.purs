module Sjablong.Layer.Tuple
  ( TupleLayer(..)
  , mkTupleLayer
  , fstLayer
  , sndLayer
  ) where

import Prelude

import Data.Tuple (curry, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Sjablong.Layer (class Layer, containsPoint, drag, dragEnd, dragStart, draw, position, translate)

newtype TupleLayer l l' = TupleLayer (l /\ l')

mkTupleLayer :: forall l l'. l -> l' -> TupleLayer l l'
mkTupleLayer layer layer' = TupleLayer (layer /\ layer')

fstLayer :: forall l l'. TupleLayer l l' -> l
fstLayer (TupleLayer t) = fst t

sndLayer :: forall l l'. TupleLayer l l' -> l'
sndLayer (TupleLayer t) = snd t

instance (Monad m, Layer m l, Layer m l') => Layer m (TupleLayer l l') where
  position (TupleLayer (layer /\ _layer')) = position layer
  translate t (TupleLayer (layer /\ layer')) = curry TupleLayer <$> translate t layer <*> translate t layer'
  containsPoint p (TupleLayer (layer /\ layer')) = (||) <$> containsPoint p layer <*> containsPoint p layer'
  draw ctx (TupleLayer (layer /\ layer')) = draw @m ctx layer' *> draw @m ctx layer
  dragStart drag (TupleLayer (layer /\ layer')) = curry TupleLayer <$> dragStart drag layer <*> dragStart drag layer'
  drag translation (TupleLayer (layer /\ layer')) = curry TupleLayer <$> drag translation layer <*> drag translation layer'
  dragEnd (TupleLayer (layer /\ layer')) = curry TupleLayer <$> dragEnd layer <*> dragEnd layer'
