module Game.Model where

import Collision2D
import List exposing (..)
import String exposing (..)
-- import Game.Level as Level exposing (..)

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

type alias Game =
  { state: State
  , player : Player
  , grounds : List Ground
  }

player : Float -> Float -> Float -> Float -> Direction -> Collision2D.Rectangle -> Bool -> Player
player x y vx vy dir rect grounded =
  Player x y vx vy dir rect grounded

defaultGame : Game
defaultGame =
  let
    playerRect = Collision2D.rectangle 0 250 32 32
    loadedGrounds = loadLevel(1)
  in
    { state = Play 
    , player = Player 0 250 0 0 Left playerRect False
    , grounds = loadedGrounds
    }

loadLevel : Int -> List Ground
loadLevel n =
  let
    level' = level
    -- groundRect1 = Collision2D.rectangle 0 0 32 32
    -- groundRect2 = Collision2D.rectangle 32 0 32 32
  in
    asciiToGround level'
    -- [Ground 32 0 groundRect2, Ground 0 0 groundRect1]


-- level = """
--   * *****         *
--   *               *
-- """
level = """
  *****************
  *                         *
  *                         *
  *                     *
  *                         *
  *     ******              *
  *                         *
  *                         *
  *                         *
  *                         *
  *              *****      *
  *                         *          
  *                         *
  *                         *
  *      ****               *
  *                         *
  ***       *****************
"""

asciiToGround : String -> List Ground
asciiToGround string =
  let 
    splitted = String.split "\n" string
    lines = splitted
      |> withIndex
  in
    List.map lineToGround lines
      |> List.concat
  --   positions = lines
  --     |> withIndex
  --     |> concatMap parseLine
  --   count = length positions
  --   makeGround (row,col) = Model.makeGround row col
  -- in map makeGround positions
lineToGround : (Int,String) -> List Ground
lineToGround line =
  let
    index = fst line
    line' = snd line
    chars' = line'
      |> toList
      |> List.map fromChar
      |> withIndex
  in
    makeGroundLine index chars'

makeGroundLine : Int -> List (Int,String) -> List Ground
makeGroundLine index line =
  let
    yPos = yFunc index
    xPos =
      parseLine line
  in
    List.map (\x -> Ground x yPos (Collision2D.rectangle x yPos 32 32)) xPos

parseLine : List (Int,String) -> List Float
parseLine line =
  List.filter (\l -> snd l == "*") line
    |> List.map (\l -> xFunc (fst l))

yFunc index =
  -400 + 32 * index
  |> Basics.toFloat

xFunc index =
  -350 + 32 * index
  |> Basics.toFloat

withIndex list = 
  list |> zip [0..List.length list]

zip xs ys = 
  List.map2 (,) xs ys
