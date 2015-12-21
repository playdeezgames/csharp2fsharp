module Patterns

let Doors0 =
    ["XXXXXXXX";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsN = 
    ["X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsE =
    ["XXXXXXXX";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsNE = 
    ["X......X";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsS =
    ["XXXXXXXX";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsNS = 
    ["X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsES =
    ["XXXXXXXX";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsNES = 
    ["X......X";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsW =
    ["XXXXXXXX";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsNW = 
    ["X......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsEW =
    ["XXXXXXXX";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsNEW = 
    ["X......X";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let DoorsSW =
    ["XXXXXXXX";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsNSW = 
    ["X......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsESW =
    ["XXXXXXXX";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "X......X"]
    |> Pattern.patternFromStringListX

let DoorsNESW = 
    ["X......X";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "X......X"]
    |> Pattern.patternFromStringListX

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

let Filled = 
    ["XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let Empty = 
    ["........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX
