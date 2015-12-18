module Explorer

open Location
open Maze

type Explorer<'direction> =
    {Position: Location; Orientation: 'direction; Maze: Maze}

let create (directions: 'direction list) (maze: Maze) =
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

let rec explore (action: Explorer<'direction>->Explorer<'direction>) (canWalk: Explorer<'direction>->bool) walk (explorer: Explorer<'direction>) =
    let exploreNext = explore action canWalk walk
    let newExplorer = explorer |> action
    if newExplorer |> canWalk then
        newExplorer |> walk |> exploreNext
    else
        newExplorer |> exploreNext
