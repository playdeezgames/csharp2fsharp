module GameData

open Explorer
open Location

let TileColumns = 32
let TileRows = 18
let MazeColumns = 24
let MazeRows = TileRows

type State = 
    {Visited: Set<Location>; 
    Treasures: Set<Location>; 
    Visible: Set<Location>; 
    Loot:int}

let rec visibleLocations (location:Location, direction:Cardinal.Direction, maze:Maze.Maze) =
    let nextLocation = Cardinal.walk location direction
    if maze.[location].Contains nextLocation then
        visibleLocations (nextLocation, direction, maze)
        |> Set.add location
    else
        [location]
        |> Set.ofSeq

let treasureLocations (maze:Maze.Maze) =
    maze
    |> Map.toSeq
    |> Seq.filter (fun (location, exits) -> (exits |> Set.count) = 1)
    |> Seq.map (fun (location, exits) -> location)
    |> Set.ofSeq

let createExplorer = Explorer.create (fun l->Utility.random.Next()) (fun d->Utility.random.Next())

let restart () :Explorer<Cardinal.Direction, State>= 
    let gridLocations = 
        Utility.makeGrid (MazeColumns, MazeRows)
    let newExplorer = 
        gridLocations
        |> Maze.makeEmpty
        |> Maze.generate Utility.picker Utility.findAllCardinal
        |> createExplorer (fun m l -> (m.[l] |> Set.count) > 1) Cardinal.values {Visited=Set.empty; Treasures=Set.empty;Visible=Set.empty;Loot=0}
    {newExplorer with 
        State = {newExplorer.State with 
                    Treasures = treasureLocations newExplorer.Maze;
                    Visible = visibleLocations (newExplorer.Position, newExplorer.Orientation, newExplorer.Maze); 
                    Visited = [newExplorer.Position] |> Set.ofSeq}}

let moveAction (explorer: Explorer<Cardinal.Direction, State>) = 
    let next =
        explorer.Orientation
        |> Cardinal.walk explorer.Position
    if next |> explorer.Maze.[explorer.Position].Contains then
        {explorer with 
            Position = next; 
            State = {explorer.State with 
                        Visited = next |> explorer.State.Visited.Add; 
                        Visible = visibleLocations(next, explorer.Orientation, explorer.Maze)
                        Treasures = next |> explorer.State.Treasures.Remove
                        Loot = if next |> explorer.State.Treasures.Contains then explorer.State.Loot + 1 else explorer.State.Loot}}
    else
        explorer

let turnAction direction explorer = 
    {explorer with Orientation = direction; State={explorer.State with Visible = visibleLocations(explorer.Position, direction, explorer.Maze)}}

type Command = 
     | Turn of Cardinal.Direction
     | Move
     | Restart
     | Wait

let act command explorer =
    match command with
    | Turn direction -> explorer |> turnAction direction
    | Move           -> explorer |> moveAction
    | Restart        -> restart()
    | _              -> explorer

let mutable explorer = 
    restart()

