﻿open Location
open System

let MakeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let FindAllCardinal = Neighbor.findAll Cardinal.walk Cardinal.values

let CardinalTurn (explorer: Explorer.Explorer<Cardinal.Direction>) =
    {explorer with Orientation=Cardinal.values |> Seq.sortBy (fun e->Guid.NewGuid()) |> Seq.head}

let CardinalWalk (explorer: Explorer.Explorer<Cardinal.Direction>) =
    {explorer with Position = explorer.Orientation |> Cardinal.walk explorer.Position}

let CanWalk (walk:Explorer.Explorer<Cardinal.Direction>->Explorer.Explorer<Cardinal.Direction>) (explorer: Explorer.Explorer<Cardinal.Direction>) =
    let next = (explorer |> walk).Position
    let room = explorer.Maze.[explorer.Position]
    room.Contains next

let Tracker (explorer: Explorer.Explorer<Cardinal.Direction>) = 
    match explorer.Orientation with
    | Cardinal.North -> Console.WriteLine("Facing North")
    | Cardinal.East -> Console.WriteLine("Facing East")
    | Cardinal.South ->  Console.WriteLine("Facing South")
    | Cardinal.West -> Console.WriteLine("Facing West")
    Console.WriteLine("At location {0}, {1}", explorer.Position.Column, explorer.Position.Row)
    Console.ReadLine() |> ignore
    explorer

[<EntryPoint>]
let main argv = 
    MakeGrid (4,4)
    |> Maze.makeEmpty
    |> Maze.generate FindAllCardinal
    |> Explorer.create Cardinal.values
    |> Explorer.explore Tracker CanWalk CardinalTurn CardinalWalk
    |> ignore
    0