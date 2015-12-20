open System.Windows.Forms
open System.Drawing
open System

open Location

let MakeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let FindAllCardinal = Neighbor.findAll Cardinal.walk Cardinal.values

let random = new System.Random()

let picker (choices:seq<'t>) =
    let pick = choices |> Seq.length |> random.Next
    choices |> Seq.item pick

let mutable explorer = 
    let gridLocations = MakeGrid (18, 18)
    gridLocations
    |> Maze.makeEmpty
    |> Maze.generateNew picker FindAllCardinal
    |> Explorer.create Cardinal.values (gridLocations |> Set.ofList)

let Redraw graphics =
    explorer.Maze
    |> Map.iter(fun k v-> FrameBuffer.RenderTile (k.Column, k.Row) Tiles.Doors0)

let KeyDown (event:KeyEventArgs) =
    ()

[<EntryPoint>]
[<STAThread>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" Redraw KeyDown
    |> Application.Run
    0