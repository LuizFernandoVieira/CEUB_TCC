module Game.Model where

import Collision2D
import List exposing (..)
import Game.Level exposing (..)

(gameWidth,gameHeight) = (600,400)
(halfWidth,halfHeight) = (300,200)

type State = Play | Pause
type Direction = Left | Right

type alias Player =
  { x : Float
  , y : Float
  , vx : Float
  , vy : Float
  , dir : Direction
  , rect : Collision2D.Rectangle
  , grounded : Bool
  }

type alias Ground =
  { x : Float
  , y : Float
  , rect : Collision2D.Rectangle
  }

type alias Camera =
  { x : Float
  , y : Float
  }

type alias Game =
  { state: State
  , player : Player
  , grounds : List Ground
  , camera : Camera
  }

player : Float -> Float -> Float -> Float -> Direction -> Collision2D.Rectangle -> Bool -> Player
player x y vx vy dir rect grounded =
  Player x y vx vy dir rect grounded

defaultGame : Game
defaultGame =
  let
    playerRect = Collision2D.rectangle 0 250 32 32
    grounds' = loadLevel(1)
                |> buildGround
    camera' = Camera 0 0
  in
    { state = Play 
    , player = Player 0 250 0 0 Left playerRect False
    , grounds = grounds'
    , camera = camera'
    }

buildGround : List (Float,Float) -> List Ground
buildGround coordinates =
  List.map (\(x,y) -> Ground x y (Collision2D.rectangle x y 32 32)) coordinates
