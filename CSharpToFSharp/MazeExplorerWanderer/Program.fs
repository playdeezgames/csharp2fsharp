open Location
open System

let MakeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let FindAllCardinal = Neighbor.findAll Cardinal.walk Cardinal.values

let CardinalWalk (explorer: Explorer.Explorer<Cardinal.Direction>) =
    {explorer with Position = explorer.Orientation |> Cardinal.walk explorer.Position}

let CanWalk (explorer: Explorer.Explorer<Cardinal.Direction>) =
    let next = (explorer |> CardinalWalk).Position
    let room = explorer.Maze.[explorer.Position]
    room.Contains next

let Tracker (explorer: Explorer.Explorer<Cardinal.Direction>) = 
    Console.WriteLine("Facing {0}", explorer.Orientation |> Cardinal.toString)
    Console.WriteLine("At location {0}, {1}", explorer.Position.Column, explorer.Position.Row)
    let room = explorer.Maze.[explorer.Position]
    Cardinal.values
    |> List.filter (fun v -> (v |> Cardinal.walk explorer.Position) |> room.Contains)
    |> List.map (fun v->v |> Cardinal.toString)
    |> List.iter(fun v-> Console.WriteLine("Can go {0}",v))
    Console.ReadLine() |> ignore
    {explorer with Orientation=Cardinal.values |> Seq.sortBy (fun e->Guid.NewGuid()) |> Seq.head}

[<EntryPoint>]
let main argv = 
    MakeGrid (4,4)
    |> Maze.makeEmpty
    |> Maze.generate FindAllCardinal
    |> Explorer.create Cardinal.values
    |> Explorer.explore Tracker CanWalk CardinalWalk
    |> ignore
    0
