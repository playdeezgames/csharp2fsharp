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


type private MazeGeneratorState =
    | Initial of Maze
    | Constructing of Set<Location> * Set<Location> * Maze
    | Complete of Maze

let private InitializeMaze (picker:seq<Location>->Location)  (neighborFinder:Location -> Location list) (maze: Maze) :MazeGeneratorState=
    let start = maze |> Map.toSeq |> Seq.map fst |> picker
    let neighbors = start |> neighborFinder |> List.filter (fun i->i |> maze.ContainsKey)
    Constructing ([start]|> Set.ofList, neighbors|> Set.ofList, maze)

let private ConstructMaze (picker:seq<Location>->Location)  (neighborFinder:Location -> Location list) (inside:Set<Location>,frontier:Set<Location>,maze: Maze) :MazeGeneratorState=
    if frontier.IsEmpty then
        Complete maze
    else
        let frontierCell  = frontier |> picker
        let insideCell = frontierCell |> neighborFinder |> List.filter (fun i->i |> inside.Contains) |> picker
        let newInside = inside |> Set.add frontierCell
        let neighbors = frontierCell |> neighborFinder |> List.filter (fun i-> i |> maze.ContainsKey) |> List.filter (fun i->i |> newInside.Contains |> not) |> Set.ofList
        let newFrontier = newInside |> Set.difference frontier |> Set.union neighbors
        let newMaze = maze |> addConnections frontierCell insideCell
        Constructing (newInside, newFrontier, newMaze)

let rec private generateMaze (rng:seq<Location>->Location)  (neighborFinder:Location -> Location list) (maze: MazeGeneratorState) :Maze=
    match maze with
    | Complete finalState -> finalState
    | Initial initialState -> initialState |> InitializeMaze rng neighborFinder |> generateMaze rng neighborFinder
    | Constructing (i,f,m) -> (i,f,m) |> ConstructMaze rng neighborFinder |> generateMaze  rng neighborFinder
    

let generateNew (rng:seq<Location>->Location)  (neighborFinder:Location -> Location list) (maze:Maze) =
    Initial maze
    |> generateMaze rng neighborFinder
