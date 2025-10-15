module Graphics.Canvas.TextMetrics where

import Effect (Effect)
import Graphics.Canvas (Context2D)

type TextMetrics =
  { width :: Number
  , actualBoundingBoxLeft :: Number
  , actualBoundingBoxRight :: Number
  , fontBoundingBoxAscent :: Number
  , fontBoundingBoxDescent :: Number
  , actualBoundingBoxAscent :: Number
  , actualBoundingBoxDescent :: Number
  , emHeightAscent :: Number
  , emHeightDescent :: Number
  , hangingBaseline :: Number
  , alphabeticBaseline :: Number
  , ideographicBaseline :: Number
  }

foreign import measureText :: Context2D -> String -> Effect TextMetrics
