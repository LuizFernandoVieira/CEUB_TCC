module Game.Update where

import Collision2D

import Game.Input as Input                  
import Time exposing (..)
import Game.Model as Model exposing (Game)
import List exposing (..)
import Debug

update : (Float, Input.Keys) -> Game -> Game
update (dt, keys) game =
  let
    newState =
      Model.Play
  in
    { game |
        state = newState,
        player = updatePlayer dt keys game.player game
    }    

updatePlayer : Time -> Input.Keys -> Model.Player -> Game -> Model.Player
updatePlayer dt keys player game =
  let
    oldX = player.x
    oldY = player.y
  in
    player
      |> gravityUpdate dt
      |> walkUpdate keys
      |> jumpUpdate keys
      |> physicsUpdate dt
      |> collisionUpdate 
      |> fullCollisions game.grounds oldY

gravityUpdate : Float -> Model.Player -> Model.Player
gravityUpdate dt player =
  { player |
      vy = 
        if player.grounded == True then 
          0
        else 
          player.vy - dt/2 
  }

walkUpdate : Input.Keys -> Model.Player -> Model.Player
walkUpdate keys player =
  { player |
    vx = 5 * toFloat keys.x,
    dir =
      if keys.x < 0 then
        Model.Left
      else if keys.x > 0 then
        Model.Right
      else
        player.dir
  }    

jumpUpdate : Input.Keys -> Model.Player -> Model.Player
jumpUpdate keys player =
  if keys.y > 0 && player.grounded == True then
    { player | 
      vy = 10.0,
      grounded = False
    }
  else
    player

physicsUpdate dt obj =
  { obj |
    x = obj.x + obj.vx * dt,
    y = obj.y + obj.vy * dt
  }

collisionUpdate player =
  { player |
    rect = Collision2D.rectangle player.x player.y 32 32
  }

fullCollisions : List Model.Ground -> Float -> Model.Player -> Model.Player
fullCollisions grounds oldY player =
  case grounds of
    [] -> player
    [ground] -> correctCollisions ground oldY player 
    h :: t -> correctCollisions h oldY player |> fullCollisions t oldY

correctCollisions : Model.Ground -> Float -> Model.Player -> Model.Player
correctCollisions ground oldY player =
  let
    rect' = ground.rect
  in
    if Collision2D.axisAlignedBoundingBox player.rect rect' then
      { player | 
          y = oldY,
          grounded = True   
      }
    else 
      { player | 
        y = player.y
      }

handleMaybe : Maybe a -> a
handleMaybe x =
  case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"
