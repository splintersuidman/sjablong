module Control.Monad.Maybe.Trans.Extra (hoistMaybe) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..))
import Data.Maybe (Maybe)

hoistMaybe :: forall b m. Monad m => Maybe b -> MaybeT m b
hoistMaybe = MaybeT <<< pure
