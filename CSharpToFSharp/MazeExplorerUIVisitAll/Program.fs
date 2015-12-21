open System.Windows.Forms
open System.Drawing
open System
open Explorer
open Location

let makeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let findAllCardinal = Neighbor.findAll Cardinal.walk Cardinal.values

let random = new System.Random()

let picker (choices:seq<'t>) =
    let pick = choices |> Seq.length |> random.Next
    choices |> Seq.item pick

let mutable explorer = 
    let gridLocations = 
        makeGrid (32, 18)
    let explorer = 
        gridLocations
        |> Maze.makeEmpty
        |> Maze.generate picker findAllCardinal
        |> Explorer.create Cardinal.values (gridLocations |> Set.ofList)
    {explorer with State = explorer.State.Remove explorer.Position}


let flagify direction =
    match direction with
    | Cardinal.North -> 1
    | Cardinal.East -> 2
    | Cardinal.South -> 4
    | Cardinal.West -> 8

let determineCellTile exits =
    let flags = 
        exits
        |> Set.toSeq
        |> Seq.map flagify
        |> Seq.reduce (+)
    match flags with
    | 1 -> Tiles.DoorsN
    | 2 -> Tiles.DoorsE
    | 3 -> Tiles.DoorsNE
    | 4 -> Tiles.DoorsS
    | 5 -> Tiles.DoorsNS
    | 6 -> Tiles.DoorsES
    | 7 -> Tiles.DoorsNES
    | 8 -> Tiles.DoorsW
    | 9 -> Tiles.DoorsNW
    | 10 -> Tiles.DoorsEW
    | 11 -> Tiles.DoorsNEW
    | 12 -> Tiles.DoorsSW
    | 13 -> Tiles.DoorsNSW
    | 14 -> Tiles.DoorsESW
    | 15 -> Tiles.DoorsNESW
    | _ -> Tiles.Doors0

let toDirections location (exits:Set<Location>) =
    Cardinal.values
    |> List.filter (fun d->d |> Cardinal.walk location |> exits.Contains )
    |> Set.ofList

let determineExplorerTile direction =
    match direction with
    | Cardinal.North -> Tiles.ExplorerN
    | Cardinal.East  -> Tiles.ExplorerE
    | Cardinal.South -> Tiles.ExplorerS
    | Cardinal.West  -> Tiles.ExplorerW

let renderRoom (location:Location) (exits:Set<Location>) (visited:bool)=
    if visited then
        Tiles.Filled
        |> FrameBuffer.RenderTile (location.Column, location.Row)
    else
        ()
    exits
    |> toDirections location
    |> determineCellTile
    |> FrameBuffer.RenderTile (location.Column, location.Row)

let redraw graphics =
    explorer.Maze
    |> Map.iter(fun k v -> renderRoom k v (k |> explorer.State.Contains |> not))
    explorer.Orientation
    |> determineExplorerTile
    |> FrameBuffer.RenderTile (explorer.Position.Column, explorer.Position.Row)

type Command =
    | TurnOrMove of Cardinal.Direction
    | Wait

let moveAction (explorer: Explorer<Cardinal.Direction, Set<Location>>) = 
    let next =
        explorer.Orientation
        |> Cardinal.walk explorer.Position
    if next |> explorer.Maze.[explorer.Position].Contains then
        {explorer with Position=next; State= next |> explorer.State.Remove}
    else
        explorer

let turnAction direction explorer = 
    {explorer with Orientation = direction}

let act direction explorer =
    explorer
    |> turnAction direction
    |> moveAction

let keyCodeToCommand keyCode = 
    match keyCode with
    | Keys.Up -> Some Cardinal.North
    | Keys.Right -> Some Cardinal.East
    | Keys.Down -> Some Cardinal.South
    | Keys.Left -> Some Cardinal.West
    | _ -> None

let keyDown (event:KeyEventArgs) =
    let direction = 
        event.KeyCode
        |> keyCodeToCommand
    match direction with
    | Some d -> 
                explorer <- explorer |> act d
                true
    | None -> false

[<EntryPoint>]
[<STAThread>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" redraw keyDown
    |> Application.Run
    0