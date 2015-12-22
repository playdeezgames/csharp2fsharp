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

let renderRoom (location:Location) (exits:Set<Location>) (visited:bool) (visible:bool) (treasure:bool)=
    if visible then
        Tiles.Visible
        |> FrameBuffer.RenderTile (location.Column, location.Row)
        if treasure then
            ExplorerTiles.Treasure
            |> FrameBuffer.RenderTile (location.Column, location.Row)
        else
            ()
        renderWalls location exits
    elif visited then
        Tiles.Empty
        |> FrameBuffer.RenderTile (location.Column, location.Row)
        renderWalls location exits
    else
        Tiles.Hidden
        |> FrameBuffer.RenderTile (location.Column, location.Row)

let redraw graphics =
    explorer.Maze
    |> Map.iter(fun k v -> renderRoom k v (k |> explorer.State.Visited.Contains) (k |> explorer.State.Visible.Contains) (k |> explorer.State.Treasures.Contains))
    Tiles.explorer.[explorer.Orientation]
    |> FrameBuffer.RenderTile (explorer.Position.Column, explorer.Position.Row)
    Tiles.font
    |> FrameBuffer.renderString (MazeColumns,0) (explorer.State.Visited |> Set.count |> string)


