module RoomPatterns

let Rooms0 =
    ["XXXXXXXX";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsN = 
    ["X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsE =
    ["XXXXXXXX";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsNE = 
    ["X......X";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsS =
    ["XXXXXXXX";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsNS = 
    ["X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsES =
    ["XXXXXXXX";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsNES = 
    ["X......X";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X.......";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsW =
    ["XXXXXXXX";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsNW = 
    ["X......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsEW =
    ["XXXXXXXX";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsNEW = 
    ["X......X";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let RoomsSW =
    ["XXXXXXXX";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsNSW = 
    ["X......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     ".......X";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsESW =
    ["XXXXXXXX";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "X......X"]
    |> Pattern.patternFromStringListX

let RoomsNESW = 
    ["X......X";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "X......X"]
    |> Pattern.patternFromStringListX


