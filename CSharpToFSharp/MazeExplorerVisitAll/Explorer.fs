module Explorer

open Location
open Maze

type Explorer<'direction,'state> =
    {Position: Location; 
    Orientation: 'direction; 
    Maze: Maze; 
    State:'state}

let create (directions: 'direction list) (state:'state) (maze: Maze) =
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
    Maze=maze;
    State=state}

let rec explore isFinished decide act explorer =
    if explorer |> isFinished then
        explorer
    else
        let command = explorer |> decide
        explorer
        |> act command
        |> explore isFinished decide act
