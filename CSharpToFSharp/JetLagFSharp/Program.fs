(*
    This code accompanies a blog post found at https://csharptofsharp.wordpress.com/2015/12/13/and-then-i-made-a-windows-app-jetlag/

    It was my first foray into F# for a windows desktop app.
*)
open System
open System.Windows.Forms
open System.Drawing
open System.Reflection

(* 
    I like the simplicity of these declarations.
    In C#, these needed to be static readonly, but that's what 'let' means on the module scope to begin with, apparently.
*)
let screenSize = new Size(640, 480)
let boardSize = new Size(40, 30)
let cellSize = new Size(screenSize.Width / boardSize.Width, screenSize.Height / boardSize.Height)
let tailLength = 6

(*
    Yeah, so I'm using unions like enums.
    But the F# enum requires extra text
    So, these are discount enums
*)
type GameState = Title | Play
type Direction = Left | Straight | Right

(*
    Apparently this is how lookup tables are to be done.
*)
let directionDelta (direction: Direction) =
    (* 
        for some reason, I have to do it this way, with a match, instead of the function way I've seen elsewhere, because of my direction |> directionDelta 
        which means I don't understand the keyword function or I don't fully understand |>
    *)
    match direction with
    | Left  -> -1
    | Right ->  1
    | _     ->  0

(*
    I made a decision to use int list for my blocks and tail
    In C#, it made more sense in a Queue<int>
    After doing that, it makes more sense for them to be Queue<int> in F# as well
    But if I do that, what sort of F#-ness am I really using?
*)
let rec createIntList (count:int) (value:int) (current: int list)=
    match count with
    | x when x <= 0 -> current
    | _ -> value :: current |> createIntList (count-1) (value)
    
(*
    As a result of the int list, I have to draw from bottom to top so that adding a new line is just a :: operator call
    Of course, that doesn't really save me anything for when scrolling and taking the 
    Or, I could have just left the entire list of blocks and tail.
*)
let rec drawBricks (g:Graphics) (brush: Brush) (row: int) (bricks: int list) =
    match bricks with
    | [] -> ()
    | head :: tail ->
        g.FillRectangle(brush, new Rectangle(cellSize.Width * head, cellSize.Height * row, cellSize.Width, cellSize.Height))
        drawBricks g brush (row-1) tail

(*
    tail has to be drawn backwards as well.
    if I had it to do over, I'd leave the tail length check as an if
*)
let rec drawTail (g:Graphics) (tailBrush: Brush) (headBrush:Brush) (row: int) (tail:int list) =
    match tail with
    | [] -> ()
    | head :: tail ->
        let brush =
            match tail.Length with
            | x when x = (tailLength - 1) -> headBrush
            | _ -> tailBrush
        g.FillRectangle(brush, new Rectangle(cellSize.Width * head, cellSize.Height * row, cellSize.Width, cellSize.Height))
        drawTail g tailBrush headBrush (row-1) tail

(*
    This was a nice, easy way to create the form. C# doesn't quite have this equivalent.
*)
let gameWindow = new Form(ClientSize=screenSize, FormBorderStyle=FormBorderStyle.FixedSingle,  MaximizeBox=false, MinimizeBox=false)

(*
    the conversion to sequence and then back to list unsettles me
*)
let scroll (value:int) (items: int list)=
    value :: (items |> Seq.take (items.Length-1) |> List.ofSeq)

(*
    could have used recursion here, but it would have been recursion for the sake of recursion
*)
let calculateScore runSize = ( (runSize) * (runSize + 1) ) / 2

(*
    this is my first f# class
*)
type GameData()= 
    //I know that mutable is to be avoided, but these are variables
    let mutable gameState = Title
    let mutable score = 0
    let mutable blocks = createIntList boardSize.Height 0 []
    let mutable tail = createIntList tailLength (boardSize.Width / 2) []
    let mutable currentRun = 0
    let mutable direction = Straight

    //immutables
    let wallBrush = new SolidBrush(Color.Blue)
    let tailBrush = new SolidBrush(Color.Orange)
    let headBrush = new SolidBrush(Color.Red)
    let blockBrush = new SolidBrush(Color.White)
    let leftWall = new Rectangle(0,0,cellSize.Width,screenSize.Height)
    let rightWall  = new Rectangle(screenSize.Width - cellSize.Width,0,cellSize.Width,screenSize.Height)
    let font = new Font("Tahoma", float32 cellSize.Height )
    let fontBrush = new SolidBrush(Color.LightGreen)
    let random = new Random()
    let gameOverText = "Press Space"

    //I think if i had it to do over, I'd have used a tuple with keyCode, gameState, and direction in it, and matched on that
    member this.HandleKey (keyCode: Keys) = 
        match gameState with
        | Title ->
            match keyCode with
            | Keys.Space ->
                this.Reset()
                gameState <- Play
            | _ -> ()
        | _ ->
            match keyCode with
            | Keys.Left ->
                match direction with
                | Right ->
                    score <- score + (currentRun |> calculateScore)
                    currentRun <- 0
                    direction <- Left
                | Straight ->
                    currentRun <- 0
                    direction <- Left
                | _ -> ()
            | Keys.Right ->
                match direction with
                | Left ->
                    score <- score + (currentRun |> calculateScore)
                    currentRun <- 0
                    direction <- Right
                | Straight ->
                    currentRun <- 0
                    direction <- Right
                | _ -> ()
            | _ -> ()

    (*
        the match in here is gratuitous, and should be an if
        the use of |> is similarly gratuitous
        i have dubbed |> the "leapfrog operator"
    *)
    member this.HandlePaint (g:Graphics) = 
        g.Clear Color.Black
        blocks |> drawBricks g blockBrush (boardSize.Height-1)
        g.FillRectangle(wallBrush,leftWall)
        g.FillRectangle(wallBrush,rightWall)
        tail |> drawTail g tailBrush headBrush (tailLength-1)
        g.DrawString(score.ToString(),font,fontBrush,new PointF(float32 cellSize.Width , 0.0f))
        match gameState with
        | Title -> 
            let gameOverSize = g.MeasureString(gameOverText, font)
            g.DrawString(gameOverText, font, fontBrush, new PointF(float32 screenSize.Width/2.0f - gameOverSize.Width/2.0f,float32 screenSize.Height/2.0f - gameOverSize.Height/2.0f))
        | _ -> ()

    (*
        so, the <- was an early (before this project) stumbling block for me
        gratuitous use of |>
    *)
    member this.Reset () =
        blocks    <- [] |> createIntList boardSize.Height 0 
        tail      <- [] |> createIntList tailLength (boardSize.Width / 2)
        score     <- 0
        gameState <- Title
        direction <- Straight
        gameWindow.Invalidate()

    //matches should be if
    member this.HandleTick () =
        match gameState with
        | Play -> 
            let nextBlockColumn = random.Next(boardSize.Width-2)+1
            blocks <- blocks |> scroll nextBlockColumn
            let nextTailColumn = tail.Head + (direction |> directionDelta)
            tail <- tail |> scroll nextTailColumn
            currentRun <- currentRun + 1
            gameWindow.Invalidate()
            match tail.Head with
            | x when x=0 || x=boardSize.Width-1 || x = List.nth blocks (boardSize.Height - tailLength) ->
                gameState <- Title
            | _ -> ()
        | _ -> ()

let gameData = new GameData()
//its nice that I can call methods on things outside of the main function
gameData.Reset()

(*
    the following line was a big struggle.
    because of |||
    I was not expecting the |||
    its main weapons are fear and surprise
    it makes some level of sense based on how | is used in F#
    but it made me do a wtf
*)
gameWindow.GetType().GetProperty("DoubleBuffered", BindingFlags.Instance ||| BindingFlags.NonPublic).SetValue(gameWindow,true,null)
gameWindow.Text <- "F# JetLag - Why? ::shrug::"
(*
    oddly enough, adding the handlers was more verbose than the C# equivalent
*)
gameWindow.KeyDown.AddHandler( fun _ e -> gameData.HandleKey e.KeyCode )
gameWindow.Paint.AddHandler(fun _ g -> gameData.HandlePaint g.Graphics )

(*
    good ol Timer.
    not very accurate, but he does the job
*)
let timer = new Timer(Interval = 100)
timer.Tick.AddHandler(fun _ _ -> gameData.HandleTick())
timer.Start()

[<EntryPoint>]
[<STAThread>]
let main argv = 
     Application.Run(gameWindow)
     0