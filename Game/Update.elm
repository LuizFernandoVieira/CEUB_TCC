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
      |> updateCollisions game.grounds (oldX,oldY)

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

updateCollisions : List Model.Ground -> (Float,Float) -> Model.Player -> Model.Player
updateCollisions grounds (oldX,oldY) player =
  let
    grounds' = 
      filter (\g -> isColliding g.rect player.rect) grounds
    grounds'' = 
      filter ((==) (Just Collision2D.Top))  
      <| map (\g -> Collision2D.rectangleSide player.rect g.rect)
      <| grounds'
  in
    fullCollisions grounds' (oldX,oldY)
      <| { player |
            grounded = length grounds' > 0
         }

fullCollisions : List Model.Ground -> (Float,Float) -> Model.Player -> Model.Player
fullCollisions grounds (oldX,oldY) player =
  case grounds of
    [] -> player
    [ground] -> correctCollisions ground (oldX,oldY) player 
    h :: t -> correctCollisions h (oldX,oldY) player |> fullCollisions t (oldX,oldY)

correctCollisions : Model.Ground -> (Float,Float) -> Model.Player -> Model.Player
correctCollisions ground (oldX,oldY) player =
    { player | 
        y = oldY,
        x = oldX
    }

isColliding : Collision2D.Rectangle -> Collision2D.Rectangle -> Bool
isColliding ground player =
    Collision2D.axisAlignedBoundingBox player ground


handleMaybe : Maybe a -> a
handleMaybe x =
  case x of
    Just y -> y
    Nothing -> Debug.crash "error: fromJust Nothing"
