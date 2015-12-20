module Maze

open Location

type Maze = Map<Location,Set<Location>>

let makeEmpty (locations: Location list) =
    locations
    |> List.map(fun item -> (item, Set.empty))
    |> Map.ofSeq

let private addConnection (fromLocation: Location) (toLocation:Location) (maze: Maze) =
    let newConnections = 
        maze.[fromLocation]
        |> Set.add toLocation
    maze
    |> Map.add fromLocation newConnections

let private addConnections (fromLocation: Location) (toLocation:Location) (maze: Maze) =
    maze
    |> addConnection fromLocation toLocation
    |> addConnection toLocation fromLocation

type private MazeGeneratorState =
    | Initial of Maze
    | Constructing of Set<Location> * Set<Location> * Maze
    | Complete of Maze

let private initializeMaze (picker:seq<Location>->Location)  (neighborFinder:Location -> Location list) (maze: Maze) :MazeGeneratorState=
    let start = maze |> Map.toSeq |> Seq.map fst |> picker
    let neighbors = start |> neighborFinder |> List.filter (fun i->i |> maze.ContainsKey)
    Constructing ([start]|> Set.ofList, neighbors|> Set.ofList, maze)

let private constructMaze (picker:seq<Location>->Location)  (neighborFinder:Location -> Location list) (inside:Set<Location>,frontier:Set<Location>,maze: Maze) :MazeGeneratorState=
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
    | Initial initialState -> initialState |> initializeMaze rng neighborFinder |> generateMaze rng neighborFinder
    | Constructing (i,f,m) -> (i,f,m) |> constructMaze rng neighborFinder |> generateMaze  rng neighborFinder
    

let generate (rng:seq<Location>->Location)  (neighborFinder:Location -> Location list) (maze:Maze) =
    Initial maze
    |> generateMaze rng neighborFinder
