module Input

open GameData
open System.Windows.Forms

let keyCodeToCommand keyCode = 
    match keyCode with
    | Keys.Up    -> if explorer.Orientation = Cardinal.North then Move else Turn Cardinal.North
    | Keys.Right -> if explorer.Orientation = Cardinal.East  then Move else Turn Cardinal.East
    | Keys.Down  -> if explorer.Orientation = Cardinal.South then Move else Turn Cardinal.South
    | Keys.Left  -> if explorer.Orientation = Cardinal.West  then Move else Turn Cardinal.West
    | Keys.R     -> Restart
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



