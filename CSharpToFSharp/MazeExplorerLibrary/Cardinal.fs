module Cardinal

open Location

type Direction = North | East | South | West

let walk (location:Location) (direction:Direction) : Location = 
    match direction with
    | North -> {location with                          Row=location.Row-1}
    | East  -> {location with Column=location.Column+1                   }
    | South -> {location with                          Row=location.Row+1}
    | West  -> {location with Column=location.Column-1                   }

let values = [North; East; South; West]

let turnClockwise = function
    | North -> East
    | East  -> South
    | South -> West
    | West  -> North

let turnAround direction =
    direction
    |> turnClockwise
    |> turnClockwise

let turnCounterClockwise direction =
    direction
    |> turnClockwise
    |> turnClockwise
    |> turnClockwise

let toString = function
    | North -> "North"
    | East  -> "East"
    | South -> "South"
    | West  -> "West"
