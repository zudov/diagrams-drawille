module Diagrams.Backend.Drawille ( renderToCanvas
                                 , renderToString
                                 , Canvas
                                 ) where

import Diagrams.Prelude hiding (frame)
import Diagrams.Backend.Rasterific (Rasterific(..))

import Codec.Picture.Types ( Image
                           , PixelRGBA8(..)
                           , PixelRGB8(..)
                           , computeLuma
                           , pixelFold
                           )

import System.Drawille (Canvas, empty, set, frame)

-- | Render diagram into string of unicode braille characters
renderToString :: Options Rasterific R2 -> Diagram Rasterific R2 -> String
renderToString opts = frame . renderToCanvas opts

-- | Render diagram into drawille's Canvas
renderToCanvas :: Options Rasterific R2 -> Diagram Rasterific R2 -> Canvas
renderToCanvas opts dia = imgToCanvas $ renderDia Rasterific opts dia

imgToCanvas :: Image PixelRGBA8 -> Canvas
imgToCanvas = pixelFold folder empty
    where folder canvas x y (PixelRGBA8 r g b a)
              | a < 128 = canvas -- pixel is more invisible rather than visible
              | computeLuma (PixelRGB8 r g b) < 128 = set canvas (x,y) -- pixel is closer to black than to white
              | otherwise = canvas
