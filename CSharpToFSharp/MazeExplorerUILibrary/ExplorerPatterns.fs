module ExplorerPatterns

let ExplorerN =
    ["........";
     "...X....";
     "..XXX...";
     ".X.X.X..";
     "...X....";
     "...X....";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let ExplorerE =
    ["........";
     "...X....";
     "....X...";
     ".XXXXX..";
     "....X...";
     "...X....";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let ExplorerS =
    ["........";
     "...X....";
     "...X....";
     ".X.X.X..";
     "..XXX...";
     "...X....";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let ExplorerW =
    ["........";
     "...X....";
     "..X.....";
     ".XXXXX..";
     "..X.....";
     "...X....";
     "........";
     "........"]
    |> Pattern.patternFromStringListX


let Treasure =
    ["........";
     ".XXXXX..";
     "...X....";
     "...X....";
     "...X....";
     "...X....";
     "........";
     "........"]
    |> Pattern.patternFromStringListX