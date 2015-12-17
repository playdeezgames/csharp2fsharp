(*
    This code accompanies two blog posts.

    Part one is here https://csharptofsharp.wordpress.com/2015/12/14/immutable-maze-generation-in-f/

    Part two is here https://csharptofsharp.wordpress.com/2015/12/15/immutable-maze-generation-in-f-part-ii/
*)
type Direction = North | East | South | West

let DirectionDelta = function
    | North -> 0, -1
    | East -> 1, 0
    | South -> 0, 1
    | West -> -1, 0

let DirectionOpposite = function
    | North -> South
    | East  -> West
    | South -> North
    | West  -> East

let DirectionFlag = function
    | North -> 1
    | East  -> 2
    | South -> 4
    | West  -> 8


let Walk start direction = 
    let startX, startY = start
    let deltaX, deltaY = DirectionDelta direction
    (startX + deltaX, startY + deltaY)

type Location = {X: int; Y: int}

type Cell = {Location: Location; UnconnectedNeighbors: Set<Direction>; ConnectedNeighbors: Set<Direction> }

let FindNeighbors (cells: Cell list) (cell: Cell) = 
    [North; East; South; West]
    |> List.map (fun direction -> 
                    let nextX, nextY = Walk (cell.Location.X, cell.Location.Y) direction
                    (direction, { X = nextX; Y = nextY } ))
    |> List.map (fun location ->
                        cells
                        |> List.choose (fun c -> 
                                            let d,l = location
                                            if c.Location = l then Some d else None)
                        )
    |> List.reduce (@)
    |> Set.ofList


let MakeCells width height = 
    let cells = 
        [0..width-1]
        |> List.map (fun x-> 
                    [0..height-1]
                    |> List.map (fun y-> { Location = {X = x; Y = y}; UnconnectedNeighbors = Set.empty; ConnectedNeighbors = Set.empty}))
        |> List.reduce (@)
    cells 
    |> List.map (fun cell -> 
                { Location = cell.Location; ConnectedNeighbors=Set.empty; UnconnectedNeighbors = FindNeighbors cells cell})


let IsComplete (cells: Cell list) = 
    cells
    |> List.exists (fun c -> c.ConnectedNeighbors.IsEmpty)
    |> not

let IsBlank (cells: Cell list) = 
    cells
    |> List.filter (fun c-> not c.ConnectedNeighbors.IsEmpty)
    |> List.isEmpty

let StartMaze (cells: Cell list) = 
    let originalCell = cells |> List.sortBy(fun e-> System.Guid.NewGuid()) |> List.head
    let originalLocation = originalCell.Location
    let nextDirection = originalCell.UnconnectedNeighbors|> Set.toList |> List.sortBy(fun e-> System.Guid.NewGuid()) |> List.head
    let nextLocation = let nextX, nextY = nextDirection |> Walk (originalLocation.X, originalLocation.Y)
                       { X = nextX; Y = nextY}
    let nextCell = cells |> List.find(fun e->e.Location=nextLocation)
    let otherCells = cells |> List.filter(fun e-> e <> originalCell && e <> nextCell)
    let newOriginalCell = {
                            Location = originalCell.Location; 
                            ConnectedNeighbors = originalCell.ConnectedNeighbors |> Set.add nextDirection; 
                            UnconnectedNeighbors = originalCell.UnconnectedNeighbors |> Set.remove nextDirection}
    let newNextCell = {
                            Location = nextCell.Location; 
                            ConnectedNeighbors = nextCell.ConnectedNeighbors |> Set.add (nextDirection |> DirectionOpposite); 
                            UnconnectedNeighbors = nextCell.UnconnectedNeighbors |> Set.remove (nextDirection |> DirectionOpposite) }
    newOriginalCell :: (newNextCell :: otherCells)

let AddRoom (cells: Cell list) =
    let connectedLocations = cells
                             |> List.filter (fun e-> not e.ConnectedNeighbors.IsEmpty)
                             |> List.map (fun e->e.Location)
                             |> Set.ofList
    let frontierCells = cells
                        |> List.filter (fun unconnectedRoom->unconnectedRoom.ConnectedNeighbors.IsEmpty)
                        |> List.filter (fun unconnectedRoom->
                                            let neighbors = unconnectedRoom.UnconnectedNeighbors |> Set.map(
                                                                                                         fun elem->
                                                                                                             let neighborX, neighborY = Walk (unconnectedRoom.Location.X, unconnectedRoom.Location.Y) elem
                                                                                                             {X = neighborX; Y = neighborY})
                                            Set.intersect neighbors connectedLocations 
                                            |> Set.isEmpty |> not)
    let frontierCell = frontierCells |> List.sortBy (fun e->System.Guid.NewGuid()) |> List.head
    let frontierLocation = frontierCell.Location
    let nextDirection, nextLocation = 
                       let neighbors = frontierCell.UnconnectedNeighbors |> Set.map(
                                                                                        fun elem->
                                                                                            let neighborX, neighborY = Walk (frontierCell.Location.X, frontierCell.Location.Y) elem
                                                                                            (elem, {X = neighborX; Y = neighborY}))
                       neighbors |> Set.filter (fun elem-> 
                                                    let d, l = elem
                                                    connectedLocations.Contains(l)
                                                    )
                       |> Set.toList
                       |> List.sortBy(fun e->System.Guid.NewGuid()) 
                       |> List.head
    let nextCell = cells |> List.find(fun e->e.Location=nextLocation)
    let otherCells = cells |> List.filter(fun e-> e <> frontierCell && e <> nextCell)
    let newOriginalCell = {
                            Location = frontierCell.Location; 
                            ConnectedNeighbors = frontierCell.ConnectedNeighbors |> Set.add nextDirection; 
                            UnconnectedNeighbors = frontierCell.UnconnectedNeighbors |> Set.remove nextDirection}
    let newNextCell = {
                            Location = nextCell.Location; 
                            ConnectedNeighbors = nextCell.ConnectedNeighbors |> Set.add (nextDirection |> DirectionOpposite); 
                            UnconnectedNeighbors = nextCell.UnconnectedNeighbors |> Set.remove (nextDirection |> DirectionOpposite) }
    newOriginalCell :: (newNextCell :: otherCells)

let (|Completed|Empty|UnderConstruction|) cells = 
    if   cells |> IsComplete then Completed
    elif cells |> IsBlank    then Empty
    else                          UnderConstruction 

let rec MakeMaze (cells: Cell list) = 
    match cells with
    | Completed -> cells
    | Empty     -> cells |> StartMaze |> MakeMaze
    | _         -> cells |> AddRoom   |> MakeMaze

type MazeNode = { Location: Location; Connections: Set<Direction>}

let CellToMazeNode (cell: Cell) = 
    {Location = cell.Location; Connections= cell.ConnectedNeighbors}

let GenerateMaze columns rows = MakeCells columns rows
                                |> MakeMaze
                                |> List.map (fun elem -> elem |> CellToMazeNode)
[<EntryPoint>]
let main argv = 
    GenerateMaze 4 4
    |> printfn "%A"
    0
