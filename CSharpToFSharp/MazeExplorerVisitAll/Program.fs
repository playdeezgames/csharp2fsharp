open Location
open System
open Explorer

let random = new System.Random()

let rng max = random.Next(max)

let coin = [(false,1);(true,1)] |> Map.ofSeq

let MakeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let FindAllCardinal = Neighbor.findAll Cardinal.walk Cardinal.values

let IsFinished explorer =
    explorer.State
    |> Set.isEmpty

type Command = 
     | Walk
     | Turn of Cardinal.Direction
     | Wait

let Decide explorer =
    let room = explorer.Maze.[explorer.Position]
    if coin |> WeightedGenerator.generate rng then
        Walk
    else
        Cardinal.values
        |> List.filter (fun d -> d |> Cardinal.walk explorer.Position |> room.Contains)
        |> Seq.map (fun d-> (d,1))
        |> Map.ofSeq
        |> WeightedGenerator.generate rng
        |> Turn
        

let WalkAction explorer =
    let next = Cardinal.walk explorer.Position explorer.Orientation
    let room = explorer.Maze.[explorer.Position]
    if room.Contains next then
        {explorer with Position = next}
    else
        explorer

let TurnAction direction explorer =
    {explorer with Orientation = direction}

let WaitAction explorer =
    explorer

let Act command explorer =
    let nextExplorer = {explorer with State = explorer.State |> Set.remove explorer.Position }
    match command with
    | Walk           -> nextExplorer |> WalkAction
    | Turn direction -> nextExplorer |> TurnAction direction
    | Wait           -> nextExplorer |> WaitAction


let ReportPosition explorer =
    Console.WriteLine("Explorer is at ({0}, {1}).",explorer.Position.Column,explorer.Position.Row)
    explorer

let ReportOrientation explorer =
    Console.WriteLine("Explorer is facing {0}.",explorer.Orientation |> Cardinal.toString)
    explorer

let ReportExits explorer =
    let room = explorer.Maze.[explorer.Position]
    Cardinal.values
    |> List.filter(fun d-> d |> Cardinal.walk explorer.Position |> room.Contains)
    |> List.iter (fun d-> Console.WriteLine("Explorer may travel {0}.",d|>Cardinal.toString))
    explorer

let ReportGoal explorer =
    Console.WriteLine("Explorer is yet to visit {0} rooms.",explorer.State |> Set.count)
    explorer

let PromptUser explorer =
    Console.ReadLine() |> ignore
    explorer

let ReportDecision command =
    match command with
    | Wait           -> Console.WriteLine("Explorer decides to wait.")
    | Walk           -> Console.WriteLine("Explorer decides to move.")
    | Turn direction -> Console.WriteLine("Explorer decides to turn to the {0}.", direction |> Cardinal.toString)
    command

let ReportStatusAndDecide explorer =
    explorer
    |> ReportPosition
    |> ReportOrientation
    |> ReportExits
    |> ReportGoal
    |> PromptUser
    |> Decide
    |> ReportDecision

[<EntryPoint>]
let main argv = 
    let gridLocations = MakeGrid (4,4)
    gridLocations
    |> Maze.makeEmpty
    |> Maze.generate FindAllCardinal
    |> Explorer.create Cardinal.values (gridLocations |> Set.ofList)
    |> Explorer.explore IsFinished ReportStatusAndDecide Act
    |> ignore
    0
