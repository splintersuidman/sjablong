module Graphics.Canvas.Extra
  ( canvasImageSourceWidth
  , canvasImageSourceHeight
  , canvasImageSourceDimensions
  , drawImage'
  , drawImageDimensions
  , letterSpacing
  , setLetterSpacing
  ) where

import Prelude

import Effect (Effect)
import Graphics.Canvas (CanvasImageSource, Context2D, Dimensions, drawImage, drawImageScale)

foreign import canvasImageSourceWidth :: CanvasImageSource -> Number
foreign import canvasImageSourceHeight :: CanvasImageSource -> Number

canvasImageSourceDimensions :: CanvasImageSource -> Dimensions
canvasImageSourceDimensions src = { width: canvasImageSourceWidth src, height: canvasImageSourceHeight src }

drawImage' :: Context2D -> CanvasImageSource -> { x :: Number, y :: Number } -> Effect Unit
drawImage' ctx src { x, y } = drawImage ctx src x y

drawImageDimensions :: Context2D -> CanvasImageSource -> { x :: Number, y :: Number } -> Dimensions -> Effect Unit
drawImageDimensions ctx src { x, y } { width, height } = drawImageScale ctx src x y width height

foreign import letterSpacing :: Context2D -> Effect String
foreign import setLetterSpacing :: Context2D -> String -> Effect Unit
