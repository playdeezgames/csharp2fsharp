open System.Windows.Forms
open System.Drawing
open System

let Redraw graphics =
    Tiles.DoorsNESW
    |> FrameBuffer.RenderTile (0,0)
    Tiles.ExplorerN
    |> FrameBuffer.RenderTile (0,0)

let KeyDown event =
    ()

[<EntryPoint>]
[<STAThread>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" Redraw KeyDown
    |> Application.Run
    0