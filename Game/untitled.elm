import Game.Model as Model exposing (Game)

withIndex list = 
  list |> zip [0..length list]

asciiToGround : String -> List Model.Ground
asciiToGround string =
  let 
    lines = String.split "\n" string
    positions = lines
      |> withIndex
      |> concatMap parseLine
    count = length positions
    makeGround (row,col) = Model.makeGround row col
  in map makeGround positions

level = """
  *****************
  *               *
  *               *
  *               *
  *               *
  *****************
"""

create : () -> List Model.Ground
create () =
  asciiToGround level