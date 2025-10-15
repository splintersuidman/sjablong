module Effect.Ref.Extra
  ( modifyM
  , modifyM_
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

modifyM :: forall @m s. MonadEffect m => (s -> m s) -> Ref s -> m s
modifyM f r = do
  x <- liftEffect $ Ref.read r
  y <- f x
  liftEffect $ Ref.write y r
  pure y

modifyM_ :: forall @m s. MonadEffect m => (s -> m s) -> Ref s -> m Unit
modifyM_ f r = unit <$ modifyM f r
