module Explorer

open Location
open Maze

type Explorer<'a> =
    {Position: Location; Orientation: 'a; Maze: Maze}

let create (directions: 'a list) (maze: Maze) =
    let position = maze
                   |> Map.toSeq
                   |> Seq.map (fun (k,v)-> k)
                   |> Seq.sortBy (fun e-> System.Guid.NewGuid())
                   |> Seq.head
    let direction = directions
                    |> List.sortBy (fun e-> System.Guid.NewGuid())
                    |> List.head
    {Position=position;
    Orientation=direction;
    Maze=maze}

let rec explore (action: Explorer<'a>->Explorer<'a>) (canWalk: Explorer<'a>->bool) turn walk (explorer: Explorer<'a>) =
    let exploreNext = explore action canWalk turn walk
    let newExplorer = explorer |> action
    if newExplorer |> canWalk then
        newExplorer |> walk |> exploreNext
    else
        newExplorer |> turn |> exploreNext
