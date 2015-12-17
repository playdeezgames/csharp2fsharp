(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
open Location
open Neighbors

let MakeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let MakeEmptyMaze (locations: Location list) =
    locations
    |> List.map(fun item -> (item, Set.empty))
    |> Map.ofSeq

let AddConnection (fromLocation: Location) (toLocation:Location) (maze: Map<Location, Set<Location> >) =
    let newConnections = maze.[fromLocation]
                         |> Set.add toLocation
    maze
    |> Map.add fromLocation newConnections

let AddConnections (fromLocation: Location) (toLocation:Location) (maze: Map<Location, Set<Location> >) =
    maze
    |> AddConnection fromLocation toLocation
    |> AddConnection toLocation fromLocation

let ChooseStart (condition:Location->bool) (maze: Map<Location, Set<Location> >) =
    maze 
    |> Map.toSeq
    |> Seq.map (fun (k,v) -> k)
    |> Seq.filter(condition)
    |> Seq.sortBy (fun e-> System.Guid.NewGuid()) 
    |> Seq.head

let ChooseNeighbor (neighborFinder:Location -> Location list) (maze: Map<Location, Set<Location> >) (location: Location) =
    location
    |> neighborFinder
    |> Seq.filter (fun e-> maze.ContainsKey(e))
    |> Seq.sortBy (fun e-> System.Guid.NewGuid()) 
    |> Seq.head

let StartMaze (neighborFinder:Location -> Location list) (maze: Map<Location, Set<Location> >) =
    let start = 
        maze 
        |> ChooseStart (fun e-> true)
    let neighbor =
        start
        |> ChooseNeighbor neighborFinder maze
    maze
    |> AddConnections start neighbor


let GetPossibleConnections (neighborFinder:Location -> Location list) (maze: Map<Location, Set<Location> >) (location:Location) =
    location
    |> neighborFinder
    |> List.filter (fun e-> maze.ContainsKey(e))

let CanConnectTo (neighborFinder:Location -> Location list) (maze: Map<Location, Set<Location> >) (location:Location)=
    location
    |> GetPossibleConnections neighborFinder maze
    |> List.isEmpty
    |> not

let AddRoom (neighborFinder:Location -> Location list) (maze: Map<Location, Set<Location> >) =
    let outside, inside = 
        maze 
        |> Map.partition (fun k v -> v.IsEmpty)
    let start = 
        outside 
        |> ChooseStart (fun e-> e |> CanConnectTo neighborFinder inside)
    let neighbor =
        start
        |> ChooseNeighbor neighborFinder inside
    maze
    |> AddConnections start neighbor

let (|Complete|InProgress|Empty|) (maze: Map<Location, Set<Location> >) =
    let outside, inside = maze |> Map.partition (fun k v -> v.IsEmpty)
    if inside.IsEmpty then
        Empty
    elif outside.IsEmpty then
        Complete
    else
        InProgress

let rec MakeMaze (neighborFinder:Location -> Location list) maze =
    match maze with
    | Complete -> maze
    | Empty    -> maze |> StartMaze neighborFinder |> MakeMaze neighborFinder
    | _        -> maze |> AddRoom neighborFinder |> MakeMaze neighborFinder

[<EntryPoint>]
let main argv = 
    let maze = 
        (2,2)
        |> MakeGrid
        |> MakeEmptyMaze
        |> MakeMaze FindAllCardinal
        |> Map.toSeq
        |> printfn "%A"
    0