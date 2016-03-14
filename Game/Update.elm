module Game.Update where

import Collision2D

import Game.Input as Input                  
import Time exposing (..)
import Game.Model as Model exposing (Game)

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
      |> correctCollisions game oldY

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

correctCollisions : Game -> Float -> Model.Player -> Model.Player
correctCollisions game oldY player =
  if Collision2D.axisAlignedBoundingBox player.rect game.ground.rect then
    { player | 
        y = oldY,
        grounded = True   
    }
  else 
    { player | 
      y = player.y,
      grounded = False
    }
