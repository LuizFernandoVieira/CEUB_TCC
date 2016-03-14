module Game.View where

import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Game.Model as Model exposing (Game)

view : (Int, Int) -> Game -> Element
view (w',h') game =
  let
    (w,h) = (toFloat w', toFloat h')

    player = game.player

    verb =
      if player.vx > 0 then
        "walk"
      else
        "stand"

    dir =
      case player.dir of
        Model.Left -> "left"
        Model.Right -> "right"

    playerSrc =
      "img/playerstandleft.png"
      -- "/img/player/"++ verb ++ dir ++ ".png"

    groundSrc =
      "img/ground.png"

    playerImage =
      image 32 32 playerSrc

    groundImage =
      image 32 32 groundSrc

    position =
      (player.x, player.y)
  in
    collage w' h'
      [ rect w h
          |> filled (rgb 174 238 238)
      , playerImage
          |> toForm
          |> move position
      , groundImage
          |> toForm
      ]