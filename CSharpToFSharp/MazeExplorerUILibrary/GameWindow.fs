module GameWindow

open System.Windows.Forms
open System.Drawing
open System.Reflection
open System.Drawing.Drawing2D

let renderFrameBuffer (window:Form) (g: Graphics) =
    let clientSize = window.ClientSize
    let aspectRatio = [clientSize.Width / FrameBuffer.BoardSize.Width; clientSize.Height / FrameBuffer.BoardSize.Height] |> List.min 
    let xOffset = (clientSize.Width - aspectRatio * FrameBuffer.BoardSize.Width) / 2
    let yOffset = (clientSize.Height - aspectRatio * FrameBuffer.BoardSize.Height) / 2
    g.Clear(Colors.GetDrawingColor Colors.Onyx)
    g.InterpolationMode <- InterpolationMode.NearestNeighbor
    g.PixelOffsetMode <- PixelOffsetMode.Half
    g.DrawImage(FrameBuffer.FrameBuffer, new Rectangle(xOffset,yOffset,aspectRatio * FrameBuffer.BoardSize.Width,aspectRatio * FrameBuffer.BoardSize.Height))


let create caption (postCreate: Form->Form) redrawCallback keydownCallback  initialWindowSize= 
    let window = new Form(ClientSize=initialWindowSize, Text=caption)
    window.GetType().GetProperty("DoubleBuffered", BindingFlags.Instance ||| BindingFlags.NonPublic).SetValue(window,true,null)
    window.Paint.AddHandler (fun _ e -> 
                                        FrameBuffer.FrameBuffer
                                        |> Graphics.FromImage
                                        |> redrawCallback
                                        e.Graphics
                                        |> renderFrameBuffer window)
    window.Resize.AddHandler (fun _ _ -> window.Invalidate())
    window.KeyDown.AddHandler (fun _ e -> if e |> keydownCallback then window.Invalidate() else ())
    window
    |> postCreate

