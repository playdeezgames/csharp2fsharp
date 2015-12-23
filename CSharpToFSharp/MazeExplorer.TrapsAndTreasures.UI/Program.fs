open System.Windows.Forms
open System.Drawing
open System

let createRefreshTimer (window:Form) =
    let timer = new Timer()
    timer.Interval <- 1000
    timer.Enabled <- true
    timer.Tick.AddHandler (fun _ _ -> window.Invalidate())
    window

[<EntryPoint>]
[<STAThread>]
let main argv = 
    new Size(768, 432)
    |> GameWindow.create "Maze Explorer" (createRefreshTimer) Renderer.redraw Input.keyDown
    |> Application.Run
    0