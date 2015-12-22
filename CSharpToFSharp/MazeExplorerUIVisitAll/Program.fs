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

let restart () = 
    let gridLocations = 
        makeGrid (32, 18)
    let newExplorer = 
        gridLocations
        |> Maze.makeEmpty
        |> Maze.generate picker findAllCardinal
        |> Explorer.create (fun m l -> true) Cardinal.values (gridLocations |> Set.ofList)
    {newExplorer with State = newExplorer.State.Remove newExplorer.Position}

let mutable explorer = 
    restart()


let determineCellTile (exits:Set<Cardinal.Direction>) =
    let flags = (exits.Contains Cardinal.North, exits.Contains Cardinal.East, exits.Contains Cardinal.South, exits.Contains Cardinal.West)
    Tiles.room.[flags]

let toDirections location (exits:Set<Location>) =
    Cardinal.values
    |> List.filter (fun d->d |> Cardinal.walk location |> exits.Contains )
    |> Set.ofList

let renderRoom (location:Location) (exits:Set<Location>) (visited:bool)=
    if visited then
        Tiles.Filled
        |> FrameBuffer.RenderTile (location.Column, location.Row)
    else
        Tiles.Empty
        |> FrameBuffer.RenderTile (location.Column, location.Row)
    exits
    |> toDirections location
    |> determineCellTile
    |> FrameBuffer.RenderTile (location.Column, location.Row)

let redraw graphics =
    explorer.Maze
    |> Map.iter(fun k v -> renderRoom k v (k |> explorer.State.Contains |> not))
    Tiles.explorer.[explorer.Orientation]
    |> FrameBuffer.RenderTile (explorer.Position.Column, explorer.Position.Row)
    Tiles.font
    |> FrameBuffer.renderString (0,0) (explorer.State |> Set.count |> string)

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

let keyCodeToCommand keyCode = 
    match keyCode with
    | Keys.Up    -> if explorer.Orientation = Cardinal.North then Move else Turn Cardinal.North
    | Keys.Right -> if explorer.Orientation = Cardinal.East  then Move else Turn Cardinal.East
    | Keys.Down  -> if explorer.Orientation = Cardinal.South then Move else Turn Cardinal.South
    | Keys.Left  -> if explorer.Orientation = Cardinal.West  then Move else Turn Cardinal.West
    | Keys.F2    -> Restart
    | _          -> Wait

let keyDown (event:KeyEventArgs) =
    let command = 
        event.KeyCode
        |> keyCodeToCommand
    match command with
    | Wait -> false
    | _ -> 
            explorer <- explorer |> act command
            true

[<EntryPoint>]
[<STAThread>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" redraw keyDown
    |> Application.Run
    0