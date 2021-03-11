module Test.MySolutions where

import Prelude

import Data.Int (rem)
import Math (pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r 

leftoverCents :: Int -> Int
leftoverCents cents = rem cents 100