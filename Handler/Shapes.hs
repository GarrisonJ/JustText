module Handler.Shapes where

import Import
import System.Random

data Shape = Circle
           | Triangle
           | Diamond
          deriving (Enum, Show, Bounded)

instance Random Shape where
    random g = case randomR (fromEnum (minBound :: Shape), fromEnum (maxBound :: Shape)) g of
                 (r, g') -> (toEnum r, g')
    randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
                        (r, g') -> (toEnum r, g')

randomShape :: IO Shape
randomShape = randomIO

intToShape :: Int -> Shape
intToShape = toEnum

shapeToInt :: Shape -> Int
shapeToInt = fromEnum

shapeToWidget :: Shape -> Widget
shapeToWidget shape = case shape of
                        Circle   -> circleW
                        Triangle -> triangleW
                        Diamond  -> diamondW
triangleW :: Widget
triangleW = do
          toWidget [lucius|
            .triangle {
              width: 0;
              height: 0;
              border-left: 50px solid transparent;
              border-right: 50px solid transparent;
              border-bottom: 100px solid black;
            }
          |]
          toWidget [hamlet|<div .triangle>|]

diamondW :: Widget
diamondW = do
          toWidget [lucius|
          .diamond {
              width: 0;
              height: 0;
              border: 50px solid transparent;
              border-bottom-color: black;
              position: relative;
              top: -50px;
            }
          .diamond:after {
              content: '';
              position: absolute;
              left: -50px;
              top: 50px;
              width: 0;
              height: 0;
              border: 50px solid transparent;
              border-top-color: black;
            }
          |]
          toWidget [hamlet|<div .diamond>|]

circleW :: Widget
circleW = do
          toWidget [lucius|
            .circle {
              width: 100px;
              height: 100px;
              background: black;
              -moz-border-radius: 50px;
              -webkit-border-radius: 50px;
              border-radius: 50px;
            }
          |]
          toWidget [hamlet|<div .circle>|]
