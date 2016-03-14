module Game.Input where

import Keyboard
import Time

type alias Keys = 
  { x:Int
  , y:Int 
  }  

input : Signal (Float, Keys)
input =
  Signal.sampleOn dt (Signal.map2 (,) dt Keyboard.arrows)

dt = 
  Signal.map (\t -> t/20) (Time.fps 60)