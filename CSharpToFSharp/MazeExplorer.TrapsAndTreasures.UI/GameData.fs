module GameData

open Explorer
open Location

let TileColumns = 32
let TileRows = 18
let MazeColumns = 24
let MazeRows = TileRows
let InitialHealth = 3
let TimeLimit = 300

type ItemType = 
    | Treasure
    | Trap

let itemGenerator =
    [(Treasure,1);(Trap,1)]
    |> WeightedGenerator.ofPairs

let generateItem()= WeightedGenerator.generate (fun n->Utility.random.Next(n)) itemGenerator

type State = 
    {Visited: Set<Location>; 
    Items: Map<Location,ItemType>; 
    Visible: Set<Location>; 
    Loot:int;
    Health:int;
    StartTime: System.DateTime}

let rec visibleLocations (location:Location, direction:Cardinal.Direction, maze:Maze.Maze) =
    let nextLocation = Cardinal.walk location direction
    if maze.[location].Contains nextLocation then
        visibleLocations (nextLocation, direction, maze)
        |> Set.add location
    else
        [location]
        |> Set.ofSeq

let itemLocations (maze:Maze.Maze) =
    maze
    |> Map.toSeq
    |> Seq.filter (fun (location, exits) -> (exits |> Set.count) = 1)
    |> Seq.map (fun (location, exits) -> location, generateItem())
    |> Map.ofSeq

let createExplorer = Explorer.create (fun l->Utility.random.Next()) (fun d->Utility.random.Next())

let wonGame (explorer:Explorer.Explorer<Cardinal.Direction,State>) =
    explorer.State.Items
    |> Map.tryPick (fun k v -> if v = Treasure then Some k else None)
    |> Option.isNone

let isDead explorer = 
    explorer.State.Health <= 0

type ExplorerState = 
    | Win
    | Alive
    | Dead
    | OutOfTime

let getTimeLeft explorer =
    let elapsed = (System.DateTime.Now - explorer.State.StartTime).TotalSeconds |> int
    if elapsed >= TimeLimit then
        0
    else
        TimeLimit - elapsed

let getExplorerState explorer = 
    if explorer |> isDead then
        Dead
    elif explorer |> wonGame then
        Win
    elif (explorer |> getTimeLeft) <= 0 then
        OutOfTime
    else
        Alive


let restart () :Explorer<Cardinal.Direction, State>= 
    let gridLocations = 
        Utility.makeGrid (MazeColumns, MazeRows)
    let newExplorer = 
        gridLocations
        |> Maze.makeEmpty
        |> Maze.generate Utility.picker Utility.findAllCardinal
        |> createExplorer (fun m l -> (m.[l] |> Set.count) > 1) Cardinal.values {Visited=Set.empty; Items=Map.empty; Visible=Set.empty; Loot=0; Health=InitialHealth; StartTime=System.DateTime.Now}
    {newExplorer with 
        State = {newExplorer.State with 
                    Items = itemLocations newExplorer.Maze;
                    Visible = visibleLocations (newExplorer.Position, newExplorer.Orientation, newExplorer.Maze); 
                    Visited = [newExplorer.Position] |> Set.ofSeq}}

let updateState next explorer =
    {explorer.State with 
        Visited = next |> explorer.State.Visited.Add |> Set.union explorer.State.Visible; 
        Visible = visibleLocations(next, explorer.Orientation, explorer.Maze)
        Items = next |> explorer.State.Items.Remove
        Loot = if next |> explorer.State.Items.ContainsKey && explorer.State.Items.[next] = Treasure then explorer.State.Loot + 1 else explorer.State.Loot
        Health = if next |> explorer.State.Items.ContainsKey && explorer.State.Items.[next] = Trap then explorer.State.Health - 1 else explorer.State.Health }

let enterLocation next explorer =
    {explorer with 
        Position = next; 
        State = explorer |> updateState next}

let moveAction (explorer: Explorer<Cardinal.Direction, State>) = 
    let next =
        explorer.Orientation
        |> Cardinal.walk explorer.Position
    if next |> explorer.Maze.[explorer.Position].Contains then
        explorer
        |> enterLocation next
    else
        explorer

let turnAction direction explorer = 
    {explorer with Orientation = direction; State={explorer.State with Visited = explorer.State.Visited |> Set.union explorer.State.Visible; Visible = visibleLocations(explorer.Position, direction, explorer.Maze)}}

type Command = 
     | Turn of Cardinal.Direction
     | Move
     | Restart
     | Wait

let act command explorer =
    match command with
    | Turn direction -> if (explorer |> getExplorerState) = Alive then explorer |> turnAction direction else explorer
    | Move           -> if (explorer |> getExplorerState) = Alive then explorer |> moveAction else explorer
    | Restart        -> restart()
    | _              -> explorer

let mutable explorer = 
    restart()

