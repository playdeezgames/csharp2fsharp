module CommonPatterns

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


