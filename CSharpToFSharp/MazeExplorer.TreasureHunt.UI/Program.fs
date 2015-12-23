open System.Windows.Forms
open System.Drawing
open System

[<EntryPoint>]
[<STAThread>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" (fun w->w) Renderer.redraw Input.keyDown
    |> Application.Run
    0