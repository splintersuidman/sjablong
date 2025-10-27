module Graphics.Canvas.Filter
  ( setFilter
  , filter
  ) where

import Prelude

import Effect (Effect)
import Graphics.Canvas (Context2D)

foreign import filter :: Context2D -> Effect String
foreign import setFilter :: Context2D -> String -> Effect Unit
