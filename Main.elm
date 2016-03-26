module Main where

import Window
import Graphics.Element exposing (..)

import Game.Input as Input
import Time exposing (..)
import Game.Model as Model exposing (Game)
import Game.Update as Update
import Game.View as View

gameState : Signal Game
gameState =
  Signal.foldp Update.update Model.defaultGame Input.input

main : Signal Element
main =
  Signal.map2 View.view Window.dimensions gameState 
