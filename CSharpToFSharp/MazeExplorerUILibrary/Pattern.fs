module Pattern

let patternRowFromString (ch:char) (src: string) =
    [for c in src->c]
    |> List.map(fun elem -> elem = ch)

let patternFromStringList (ch: char) (src: string list) =
    src
    |> List.map(fun elem -> elem |> patternRowFromString ch)

let patternFromStringListX = patternFromStringList 'X'
