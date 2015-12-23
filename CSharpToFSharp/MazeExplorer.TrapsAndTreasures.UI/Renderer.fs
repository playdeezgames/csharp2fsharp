module Renderer

open Location
open GameData

let determineCellTile (exits:Set<Cardinal.Direction>) =
    let flags = (exits.Contains Cardinal.North, exits.Contains Cardinal.East, exits.Contains Cardinal.South, exits.Contains Cardinal.West)
    Tiles.room.[flags]

let toDirections location (exits:Set<Location>) =
    Cardinal.values
    |> List.filter (fun d->d |> Cardinal.walk location |> exits.Contains )
    |> Set.ofList

let renderWalls (location:Location) (exits:Set<Location>) =
    exits
    |> toDirections location
    |> determineCellTile
    |> FrameBuffer.RenderTile (location.Column, location.Row)

let renderItem item location =
    match item with
    | Some Treasure ->
                    ExplorerTiles.Treasure
                    |> FrameBuffer.RenderTile (location.Column, location.Row)
    | Some Trap ->
                    ExplorerTiles.Trap
                    |> FrameBuffer.RenderTile (location.Column, location.Row)
    | None -> ()


let renderRoom (location:Location) (exits:Set<Location>) (visited:bool) (visible:bool) (item:ItemType option)=
    if visible then
        Tiles.Visible
        |> FrameBuffer.RenderTile (location.Column, location.Row)
        location
        |> renderItem item
        renderWalls location exits
    elif visited then
        Tiles.Empty
        |> FrameBuffer.RenderTile (location.Column, location.Row)
        renderWalls location exits
    else
        Tiles.Hidden
        |> FrameBuffer.RenderTile (location.Column, location.Row)
        
let statusTable = 
    [(GameData.Alive,     (Tiles.emeraldFont, "Alive!  "));
     (GameData.Dead,      (Tiles.garnetFont,  "Dead!   "));
     (GameData.Win,       (Tiles.goldFont,    "Win!    "));
     (GameData.OutOfTime, (Tiles.garnetFont,  "Times Up"))]
    |> Map.ofSeq

let redraw graphics =
    explorer.Maze
    |> Map.iter(fun k v -> renderRoom k v (k |> explorer.State.Visited.Contains) (k |> explorer.State.Visible.Contains) (if k |> explorer.State.Items.ContainsKey then Some explorer.State.Items.[k] else None))
    Tiles.explorer.[explorer.Orientation]
    |> FrameBuffer.RenderTile (explorer.Position.Column, explorer.Position.Row)
    Tiles.sapphireFont
    |> FrameBuffer.renderString (MazeColumns,0) (explorer.State.Visited |> Set.count |> sprintf "Room %3i")
    Tiles.goldFont
    |> FrameBuffer.renderString (MazeColumns,1) (explorer.State.Loot |> sprintf "Loot %3i" )
    Tiles.garnetFont
    |> FrameBuffer.renderString (MazeColumns,2) (explorer.State.Health |> sprintf "\u0003\u0003\u0003\u0003 %3i")
    Tiles.sapphireFont
    |> FrameBuffer.renderString (MazeColumns,16) "\u0018\u0019\u001B\u001AMove"
    Tiles.sapphireFont
    |> FrameBuffer.renderString (MazeColumns,17) "[R]eset"
    let (font, text) = statusTable.[explorer |> GameData.getExplorerState]
    font
    |> FrameBuffer.renderString (MazeColumns, 3) text
    let timeRemaining = explorer |> GameData.getTimeLeft
    if (explorer |> GameData.getExplorerState) = GameData.Alive then
        let timeFont = 
            if timeRemaining >= GameData.TimeLimit / 2 then
                Tiles.emeraldFont
            elif timeRemaining >= GameData.TimeLimit / 4 then
                Tiles.goldFont
            else
                Tiles.garnetFont
        timeFont
        |> FrameBuffer.renderString (MazeColumns,4) (timeRemaining |> sprintf "Time %3i")
    else
        Tiles.garnetFont
        |> FrameBuffer.renderString (MazeColumns,4) "        "
    


