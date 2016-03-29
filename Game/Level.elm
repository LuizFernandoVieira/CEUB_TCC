module Game.Level where

import String exposing (split,toList,fromChar)
import List exposing (..)

loadLevel : Int -> List (Float,Float)
loadLevel n =
  let
    level' = level
  in
    asciiToCoordinates level'

level = """
  *****************
  *                         *
  *                         *
  *                     *
  *                         *
  *                         *
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

asciiToCoordinates : String -> List (Float,Float)
asciiToCoordinates string =
  let 
    splitted = String.split "\n" string
      |> List.reverse
    indexedlines = splitted
      |> withIndex
  in
    map parseLineToTuples indexedlines
      |> concat
      |> calculateCoordinates

parseLineToTuples : (Int,String) -> List (Int,Int)
parseLineToTuples line =
  let
    yPos = fst line
    xPosList =
      snd line
      |> parseXCoordinates
  in
    zipPositions yPos xPosList

calculateCoordinates : List (Int,Int) -> List (Float,Float)
calculateCoordinates positions =
  map calculateTuple positions
  -- map (\tuple -> (xFunc (fst tuple), yFunc (snd tuple))) positions

calculateTuple : (Int,Int) -> (Float,Float)
calculateTuple (x,y) =
  (xFunc x, yFunc y)

parseXCoordinates : String -> List Int
parseXCoordinates line =
  line 
    |> toStringList 
    |> withIndex
    |> parseLine

parseLine : List (Int,String) -> List Int
parseLine line =
  filter (\l -> snd l == "*") line
    |> map fst

zipPositions : Int -> List Int -> List (Int,Int)
zipPositions yPos xPosList =
  map (\x -> (x,yPos)) xPosList

yFunc index =
  -400 + 32 * index
  |> toFloat

xFunc index =
  -350 + 32 * index
  |> toFloat

withIndex list = 
  list |> zip [0..length list]

toStringList : String -> List String
toStringList str =
  map fromChar <| toList str

zip xs ys = 
  map2 (,) xs ys
