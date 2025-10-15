module Data.Array.Extra
  ( findWithIndex
  , modifyAtM
  , enumerate
  ) where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Maybe.Trans.Extra (hoistMaybe)
import Control.Monad.Trans.Class (lift)
import Data.Array ((!!), (..))
import Data.Array as Array
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

findWithIndex :: forall a. (a -> Boolean) -> Array a -> Maybe (Tuple Int a)
findWithIndex p xs = do
  i <- Array.findIndex p xs
  x <- xs !! i
  pure $ Tuple i x

modifyAtM :: forall m a. Monad m => Int -> (a -> m a) -> Array a -> MaybeT m (Array a)
modifyAtM i f xs = do
  x <- hoistMaybe $ xs !! i
  y <- lift $ f x
  hoistMaybe $ Array.modifyAt i (const y) xs

enumerate :: forall a. Array a -> Array (Tuple Int a)
enumerate xs = Array.zip (0 .. Array.length xs) xs
