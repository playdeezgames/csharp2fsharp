module Patterns

let private PatterRowFromString (ch:char) (src: string) =
    [for c in src->c]
    |> List.map(fun elem -> elem = ch)

let private PatternFromStringList (ch: char) (src: string list) =
    src
    |> List.map(fun elem -> elem |> PatterRowFromString ch)

let private PatternFromStringListX = PatternFromStringList 'X'

let Tree = ["....X...";
            "....X...";
            "...XXX..";
            "...XXX..";
            "..XXXXX.";
            "..XXXXX.";
            ".XXXXXXX";
            "....X..."]
           |> PatternFromStringListX

let Bush = ["........";
            "..XXXX..";
            ".XXXX.X.";
            "XX.XXXXX";
            "XXXXX.XX";
            ".XXXXXX.";
            "...XX...";
            "...XX..."]
           |> PatternFromStringListX

let Ground = ["........";
              ".X...X..";
              "........";
              "...X...X";
              "........";
              ".X...X..";
              "........";
              "...X...X"]
            |> PatternFromStringListX

let Bricks = ["........";
              ".XXX.XXX";
              ".XXX.XXX";
              ".XXX.XXX";
              "........";
              "XX.XXX.X";
              "XX.XXX.X";
              "XX.XXX.X"]
             |> PatternFromStringListX

let Tagon = ["..XXX...";
             "..XXX...";
             "...X....";
             ".XXXXX..";
             "X.XXX.X.";
             "..XXX...";
             "..X.X...";
             "..X.X..."]
            |> PatternFromStringListX

