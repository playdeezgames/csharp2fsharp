module Maze

open Location

type Maze = Map<Location,Set<Location>>

let makeEmpty (locations: Location list) =
    locations
    |> List.map(fun item -> (item, Set.empty))
    |> Map.ofSeq

let addConnection (fromLocation: Location) (toLocation:Location) (maze: Maze) =
    let newConnections = maze.[fromLocation]
                         |> Set.add toLocation
    maze
    |> Map.add fromLocation newConnections

let addConnections (fromLocation: Location) (toLocation:Location) (maze: Maze) =
    maze
    |> addConnection fromLocation toLocation
    |> addConnection toLocation fromLocation

let pickStart (condition:Location->bool) (maze: Maze) =
    maze 
    |> Map.toSeq
    |> Seq.map (fun (k,v) -> k)
    |> Seq.filter(condition)
    |> Seq.sortBy (fun e-> System.Guid.NewGuid()) 
    |> Seq.head

let pickNeighbor (neighborFinder:Location -> Location list) (maze: Maze) (location: Location) =
    location
    |> neighborFinder
    |> Seq.filter (fun e-> maze.ContainsKey(e))
    |> Seq.sortBy (fun e-> System.Guid.NewGuid()) 
    |> Seq.head

let start (neighborFinder:Location -> Location list) (maze: Maze) =
    let start = 
        maze 
        |> pickStart (fun e-> true)
    let neighbor =
        start
        |> pickNeighbor neighborFinder maze
    maze
    |> addConnections start neighbor


let possibleConnections (neighborFinder:Location -> Location list) (maze: Maze) (location:Location) =
    location
    |> neighborFinder
    |> List.filter (fun e-> maze.ContainsKey(e))

let canConnectTo (neighborFinder:Location -> Location list) (maze: Maze) (location:Location)=
    location
    |> possibleConnections neighborFinder maze
    |> List.isEmpty
    |> not

let addRoom (neighborFinder:Location -> Location list) (maze: Maze) =
    let outside, inside = 
        maze 
        |> Map.partition (fun k v -> v.IsEmpty)
    let start = 
        outside 
        |> pickStart (fun e-> e |> canConnectTo neighborFinder inside)
    let neighbor =
        start
        |> pickNeighbor neighborFinder inside
    maze
    |> addConnections start neighbor

let (|Complete|InProgress|Empty|) (maze: Maze) =
    let outside, inside = maze |> Map.partition (fun k v -> v.IsEmpty)
    if inside.IsEmpty then
        Empty
    elif outside.IsEmpty then
        Complete
    else
        InProgress

let rec generate (neighborFinder:Location -> Location list) maze =
    match maze with
    | Complete -> maze
    | Empty    -> maze |> start neighborFinder |> generate neighborFinder
    | _        -> maze |> addRoom neighborFinder |> generate neighborFinder


