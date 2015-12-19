module Tile

open System.Drawing

let rec DrawPatternRow (row: int) (column: int) (src: bool list) (foreground: Color) (background: Color) (bitmap: Bitmap)=
    match src with
    | [] -> bitmap
    | head :: tail ->
        let color = if head then foreground else background
        bitmap.SetPixel(column,row,color)
        bitmap |> DrawPatternRow row (column+1) tail foreground background

let rec DrawPattern (row: int) (src: bool list list) (foreground: Color) (background: Color) (bitmap: Bitmap)=
    match src with
    | [] -> bitmap
    | head :: tail ->
        bitmap 
        |> DrawPatternRow row 0 head foreground background
        |> DrawPattern (row+1) tail foreground background
    
type Tile(pattern: bool list list, backgroundColor: Colors.PaletteColor, foregroundColor: Colors.PaletteColor)=
    member private x.Pattern = pattern
    member private x.BackgroundColor = backgroundColor
    member private x.ForegroundColor = foregroundColor
    member x.Bitmap = 
        let foreground = Colors.GetDrawingColor x.ForegroundColor
        let background = Colors.GetDrawingColor x.BackgroundColor
        let height = x.Pattern.Length
        let width = x.Pattern |> List.maxBy (fun elem->elem.Length) |> List.length
        let bitmap = new Bitmap(width,height)
        bitmap |> DrawPattern 0 x.Pattern foreground background


