open System.Windows.Forms
open System.Drawing

let Redraw graphics =
    Tiles.Tagon
    |> FrameBuffer.RenderTile (0,0)

let KeyDown args =
    ()

[<EntryPoint>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" Redraw KeyDown
    |> Application.Run 
    0