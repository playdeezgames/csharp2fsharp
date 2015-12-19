module FrameBuffer

open System.Drawing

let CellSize = new Size(8,8)
let BoardSize = new Size(32,18)
let FrameBufferSize = new Size(CellSize.Width * BoardSize.Width, CellSize.Height * BoardSize.Height)

let FrameBuffer = new Bitmap (FrameBufferSize.Width, FrameBufferSize.Height)
let private context = Graphics.FromImage(FrameBuffer)

let RenderTile (x: int, y: int) (src: Tile.Tile)  =
    context.DrawImage(src.Bitmap, new Point(x * CellSize.Width,y * CellSize.Height))


