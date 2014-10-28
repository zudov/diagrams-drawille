{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.Drawille
import Diagrams.Backend.Rasterific

-- Taken from diagrams manual
theSq = square 1 # lwN 0.01

example =
  hcat' (with & sep .~ 0.2)
    (map (\s -> theSq # scale s) [0.5, 0.8, 1, 1.5, 2])
  # scale 20

main = putStrLn $ renderToString (RasterificOptions (Width 250)) example
