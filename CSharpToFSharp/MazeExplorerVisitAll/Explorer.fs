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

let rec explore (tracker: Explorer<'a>->Explorer<'a>) (canWalk: (Explorer<'a>->Explorer<'a>)->Explorer<'a>->bool) turn walk (explorer: Explorer<'a>)=
    if explorer |> tracker |> canWalk walk then
        explorer |> walk |> explore tracker canWalk turn walk
    else
        explorer |> turn |> explore tracker canWalk turn walk
