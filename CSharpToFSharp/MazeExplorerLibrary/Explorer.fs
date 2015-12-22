module Explorer

open Location
open Maze

type Explorer<'direction,'state> =
    {Position: Location; 
    Orientation: 'direction; 
    Maze: Maze; 
    State:'state}

let create (locationSorter: Location->'random) (directionSorter: 'direction->'random) (spawnFilter: Maze -> Location -> bool) (directions: 'direction list) (state:'state) (maze: Maze) =
    let position = maze
                   |> Map.toSeq
                   |> Seq.map (fun (k,v)-> k)
                   |> Seq.filter (fun location-> location |> spawnFilter maze)
                   |> Seq.sortBy (locationSorter)
                   |> Seq.head
    let direction = directions
                    |> List.sortBy (directionSorter)
                    |> List.head
    {Position=position;
    Orientation=direction;
    Maze=maze;
    State=state}
