module Game.Level where
import Game.Model as Game exposing (Grounds)

loadLevel : Integer -> List Game.Grounds
loadLevel n =
  let
    groundRect1 = Collision2D.rectangle 0 0 32 32
    groundRect2 = Collision2D.rectangle 32 0 32 32
  in
    [Ground 32 0 groundRect2, Ground 0 0 groundRect1]
