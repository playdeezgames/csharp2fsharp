module CharacterPatterns

let character0 =
    ["........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character1 =
    [".XXXXXX.";
     "X......X";
     "X.X..X.X";
     "X......X";
     "X.XXXX.X";
     "X..XX..X";
     "X......X";
     ".XXXXXX."]
    |> Pattern.patternFromStringListX

let character2 =
    [".XXXXXX.";
     "XXXXXXXX";
     "XX.XX.XX";
     "XXXXXXXX";
     "XX....XX";
     "XXX..XXX";
     "XXXXXXXX";
     ".XXXXXX."]
    |> Pattern.patternFromStringListX

let character3 =
    [".XX.XX..";
     "XXXXXXX.";
     "XXXXXXX.";
     "XXXXXXX.";
     ".XXXXX..";
     "..XXX...";
     "...X....";
     "........"]
    |> Pattern.patternFromStringListX

let character4 =
    ["...X....";
     "..XXX...";
     ".XXXXX..";
     "XXXXXXX.";
     ".XXXXX..";
     "..XXX...";
     "...X....";
     "........"]
    |> Pattern.patternFromStringListX

let character5 =
    ["..XXX...";
     ".XXXXX..";
     "..XXX...";
     "XXXXXXX.";
     "XXXXXXX.";
     "X..X..X.";
     "...X....";
     ".XXXXX.."]
    |> Pattern.patternFromStringListX

let character6 =
    ["........";
     "...X....";
     "..XXX...";
     ".XXXXX..";
     "XXXXXXX.";
     ".XXXXX..";
     "..XXX...";
     ".XXXXX.."]
    |> Pattern.patternFromStringListX

let character7 =
    ["........";
     "........";
     "...XX...";
     "..XXXX..";
     "..XXXX..";
     "...XX...";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character8 =
    ["XXXXXXXX";
     "XXXXXXXX";
     "XXX..XXX";
     "XX....XX";
     "XX....XX";
     "XXX..XXX";
     "XXXXXXXX";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let character9 =
    ["........";
     "..XXXX..";
     ".XX..XX.";
     ".X....X.";
     ".X....X.";
     ".XX..XX.";
     "..XXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character10 =
    ["XXXXXXXX";
     "XX....XX";
     "X..XX..X";
     "X.XXXX.X";
     "X.XXXX.X";
     "X..XX..X";
     "XX....XX";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let character11 =
    ["....XXXX";
     ".....XXX";
     "....XXXX";
     ".XXXXX.X";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX..."]
    |> Pattern.patternFromStringListX

let character12 =
    ["..XXXX..";
     ".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     "..XXXX..";
     "...XX...";
     ".XXXXXX.";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character13 =
    ["..XXXXXX";
     "..XX..XX";
     "..XXXXXX";
     "..XX....";
     "..XX....";
     ".XXX....";
     "XXXX....";
     "XXX....."]
    |> Pattern.patternFromStringListX

let character14 =
    [".XXXXXXX";
     ".XX...XX";
     ".XXXXXXX";
     ".XX...XX";
     ".XX...XX";
     ".XX..XXX";
     "XXX..XX.";
     "XX......"]
    |> Pattern.patternFromStringListX

let character15 =
    ["X..XX..X";
     ".X.XX.X.";
     "..XXXX..";
     "XXX..XXX";
     "XXX..XXX";
     "..XXXX..";
     ".X.XX.X.";
     "X..XX..X"]
    |> Pattern.patternFromStringListX

let character16 =
    ["X.......";
     "XXX.....";
     "XXXXX...";
     "XXXXXXX.";
     "XXXXX...";
     "XXX.....";
     "X.......";
     "........"]
    |> Pattern.patternFromStringListX

let character17 =
    ["......X.";
     "....XXX.";
     "..XXXXX.";
     "XXXXXXX.";
     "..XXXXX.";
     "....XXX.";
     "......X.";
     "........"]
    |> Pattern.patternFromStringListX

let character18 =
    ["...XX...";
     "..XXXX..";
     ".XXXXXX.";
     "...XX...";
     "...XX...";
     ".XXXXXX.";
     "..XXXX..";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character19 =
    [".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     "........";
     ".XX..XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character20 =
    [".XXXXXXX";
     "XX.XX.XX";
     "XX.XX.XX";
     ".XXXX.XX";
     "...XX.XX";
     "...XX.XX";
     "...XX.XX";
     "........"]
    |> Pattern.patternFromStringListX

let character21 =
    ["..XXXXX.";
     ".XX...XX";
     "..XXX...";
     ".XX.XX..";
     ".XX.XX..";
     "..XXX...";
     "X....XX.";
     "XXXXXX.."]
    |> Pattern.patternFromStringListX

let character22 =
    ["........";
     "........";
     "........";
     "........";
     ".XXXXXX.";
     ".XXXXXX.";
     ".XXXXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character23 =
    ["...XX...";
     "..XXXX..";
     ".XXXXXX.";
     "...XX...";
     ".XXXXXX.";
     "..XXXX..";
     "...XX...";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let character24 =
    ["...XX...";
     "..XXXX..";
     ".XXXXXX.";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character25 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     ".XXXXXX.";
     "..XXXX..";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character26 =
    ["........";
     "...XX...";
     "....XX..";
     "XXXXXXX.";
     "....XX..";
     "...XX...";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character27 =
    ["........";
     "..XX....";
     ".XX.....";
     "XXXXXXX.";
     ".XX.....";
     "..XX....";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character28 =
    ["........";
     "........";
     "XX......";
     "XX......";
     "XX......";
     "XXXXXXX.";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character29 =
    ["........";
     "..X..X..";
     ".XX..XX.";
     "XXXXXXXX";
     ".XX..XX.";
     "..X..X..";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character30 =
    ["........";
     "...XX...";
     "..XXXX..";
     ".XXXXXX.";
     "XXXXXXXX";
     "XXXXXXXX";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character31 =
    ["........";
     "XXXXXXXX";
     "XXXXXXXX";
     ".XXXXXX.";
     "..XXXX..";
     "...XX...";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character32 =
    ["........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character33 =
    ["...XX...";
     "..XXXX..";
     "..XXXX..";
     "...XX...";
     "...XX...";
     "........";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character34 =
    [".XX.XX..";
     ".XX.XX..";
     ".XX.XX..";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character35 =
    [".XX.XX..";
     ".XX.XX..";
     "XXXXXXX.";
     ".XX.XX..";
     "XXXXXXX.";
     ".XX.XX..";
     ".XX.XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character36 =
    ["...XX...";
     ".XXXXXX.";
     "XX......";
     ".XXXXX..";
     ".....XX.";
     "XXXXXX..";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character37 =
    ["........";
     "XX...XX.";
     "XX..XX..";
     "...XX...";
     "..XX....";
     ".XX..XX.";
     "XX...XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character38 =
    ["..XXX...";
     ".XX.XX..";
     "..XXX...";
     ".XXX.XX.";
     "XX.XXX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character39 =
    ["..XX....";
     "..XX....";
     ".XX.....";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character40 =
    ["...XX...";
     "..XX....";
     ".XX.....";
     ".XX.....";
     ".XX.....";
     "..XX....";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character41 =
    [".XX.....";
     "..XX....";
     "...XX...";
     "...XX...";
     "...XX...";
     "..XX....";
     ".XX.....";
     "........"]
    |> Pattern.patternFromStringListX

let character42 =
    ["........";
     ".XX..XX.";
     "..XXXX..";
     "XXXXXXXX";
     "..XXXX..";
     ".XX..XX.";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character43 =
    ["........";
     "...XX...";
     "...XX...";
     ".XXXXXX.";
     "...XX...";
     "...XX...";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character44 =
    ["........";
     "........";
     "........";
     "........";
     "........";
     "...XX...";
     "...XX...";
     "..XX...."]
    |> Pattern.patternFromStringListX

let character45 =
    ["........";
     "........";
     "........";
     ".XXXXXX.";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character46 =
    ["........";
     "........";
     "........";
     "........";
     "........";
     "...XX...";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character47 =
    [".....XX.";
     "....XX..";
     "...XX...";
     "..XX....";
     ".XX.....";
     "XX......";
     "X.......";
     "........"]
    |> Pattern.patternFromStringListX

let character48 =
    [".XXXXX..";
     "XX...XX.";
     "XX.X.XX.";
     "XX.X.XX.";
     "XX.X.XX.";
     "XX...XX.";
     ".XXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character49 =
    ["..XX....";
     ".XXX....";
     "..XX....";
     "..XX....";
     "..XX....";
     "..XX....";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character50 =
    [".XXXX...";
     "XX..XX..";
     "....XX..";
     "..XXX...";
     ".XX.....";
     "XX..XX..";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character51 =
    [".XXXX...";
     "XX..XX..";
     "....XX..";
     "..XXX...";
     "....XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character52 =
    ["...XXX..";
     "..XXXX..";
     ".XX.XX..";
     "XX..XX..";
     "XXXXXXX.";
     "....XX..";
     "...XXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character53 =
    ["XXXXXX..";
     "XX......";
     "XXXXX...";
     "....XX..";
     "....XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character54 =
    ["..XXX...";
     ".XX.....";
     "XX......";
     "XXXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character55 =
    ["XXXXXX..";
     "XX..XX..";
     "....XX..";
     "...XX...";
     "..XX....";
     "..XX....";
     "..XX....";
     "........"]
    |> Pattern.patternFromStringListX

let character56 =
    [".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character57 =
    [".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXXX..";
     "....XX..";
     "...XX...";
     ".XXX....";
     "........"]
    |> Pattern.patternFromStringListX

let character58 =
    ["........";
     "...XX...";
     "...XX...";
     "........";
     "........";
     "...XX...";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character59 =
    ["........";
     "...XX...";
     "...XX...";
     "........";
     "........";
     "...XX...";
     "...XX...";
     "..XX...."]
    |> Pattern.patternFromStringListX

let character60 =
    ["...XX...";
     "..XX....";
     ".XX.....";
     "XX......";
     ".XX.....";
     "..XX....";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character61 =
    ["........";
     "........";
     ".XXXXXX.";
     "........";
     ".XXXXXX.";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character62 =
    [".XX.....";
     "..XX....";
     "...XX...";
     "....XX..";
     "...XX...";
     "..XX....";
     ".XX.....";
     "........"]
    |> Pattern.patternFromStringListX

let character63 =
    ["..XXXX..";
     ".XX..XX.";
     "....XX..";
     "...XX...";
     "...XX...";
     "........";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character64 =
    [".XXXXX..";
     "XX...XX.";
     "XX.XXXX.";
     "XX.XXXX.";
     "XX.XXX..";
     "XX......";
     ".XXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character65 =
    ["..XX....";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     "XXXXXX..";
     "XX..XX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character66 =
    ["XXXXXX..";
     ".XX..XX.";
     ".XX..XX.";
     ".XXXXX..";
     ".XX..XX.";
     ".XX..XX.";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character67 =
    ["..XXXX..";
     ".XX..XX.";
     "XX......";
     "XX......";
     "XX......";
     ".XX..XX.";
     "..XXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character68 =
    ["XXXXX...";
     ".XX.XX..";
     ".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     ".XX.XX..";
     "XXXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character69 =
    ["XXXXXXX.";
     ".XX...X.";
     ".XX.X...";
     ".XXXX...";
     ".XX.X...";
     ".XX...X.";
     "XXXXXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character70 =
    ["XXXXXXX.";
     ".XX...X.";
     ".XX.X...";
     ".XXXX...";
     ".XX.X...";
     ".XX.....";
     "XXXX....";
     "........"]
    |> Pattern.patternFromStringListX

let character71 =
    ["..XXXX..";
     ".XX..XX.";
     "XX......";
     "XX......";
     "XX..XXX.";
     ".XX..XX.";
     "..XXX.X.";
     "........"]
    |> Pattern.patternFromStringListX

let character72 =
    ["XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XXXXXX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character73 =
    [".XXXX...";
     "..XX....";
     "..XX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character74 =
    ["...XXXX.";
     "....XX..";
     "....XX..";
     "....XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character75 =
    ["XXX..XX.";
     ".XX..XX.";
     ".XX.XX..";
     ".XXXX...";
     ".XX.XX..";
     ".XX..XX.";
     "XXX..XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character76 =
    ["XXXX....";
     ".XX.....";
     ".XX.....";
     ".XX.....";
     ".XX...X.";
     ".XX..XX.";
     "XXXXXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character77 =
    ["XX...XX.";
     "XXX.XXX.";
     "XXXXXXX.";
     "XXXXXXX.";
     "XX.X.XX.";
     "XX...XX.";
     "XX...XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character78 =
    ["XX...XX.";
     "XXX..XX.";
     "XXXX.XX.";
     "XX.XXXX.";
     "XX..XXX.";
     "XX...XX.";
     "XX...XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character79 =
    ["..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "XX...XX.";
     "XX...XX.";
     ".XX.XX..";
     "..XXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character80 =
    ["XXXXXX..";
     ".XX..XX.";
     ".XX..XX.";
     ".XXXXX..";
     ".XX.....";
     ".XX.....";
     "XXXX....";
     "........"]
    |> Pattern.patternFromStringListX

let character81 =
    [".XXXXX..";
     "XX...XX.";
     "XX...XX.";
     "XX...XX.";
     "XX.X.XX.";
     ".XXXXX..";
     "....XXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character82 =
    ["XXXXXX..";
     ".XX..XX.";
     ".XX..XX.";
     ".XXXXX..";
     ".XX.XX..";
     ".XX..XX.";
     "XXX..XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character83 =
    [".XXXXX..";
     "XX...XX.";
     "XXX.....";
     ".XXXX...";
     "....XXX.";
     "XX...XX.";
     ".XXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character84 =
    ["XXXXXX..";
     "X.XX.X..";
     "..XX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character85 =
    ["XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character86 =
    ["XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "..XX....";
     "........"]
    |> Pattern.patternFromStringListX

let character87 =
    ["XX...XX.";
     "XX...XX.";
     "XX...XX.";
     "XX...XX.";
     "XX.X.XX.";
     "XXXXXXX.";
     ".XX.XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character88 =
    ["XX...XX.";
     "XX...XX.";
     ".XX.XX..";
     "..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "XX...XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character89 =
    ["XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character90 =
    ["XXXXXXX.";
     "XX...XX.";
     "X...XX..";
     "...XX...";
     "..XX..X.";
     ".XX..XX.";
     "XXXXXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character91 =
    [".XXXX...";
     ".XX.....";
     ".XX.....";
     ".XX.....";
     ".XX.....";
     ".XX.....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character92 =
    ["XX......";
     ".XX.....";
     "..XX....";
     "...XX...";
     "....XX..";
     ".....XX.";
     "......X.";
     "........"]
    |> Pattern.patternFromStringListX

let character93 =
    [".XXXX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character94 =
    ["...X....";
     "..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character95 =
    ["........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let character96 =
    ["..XX....";
     "..XX....";
     "...XX...";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character97 =
    ["........";
     "........";
     ".XXXX...";
     "....XX..";
     ".XXXXX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character98 =
    ["XXX.....";
     ".XX.....";
     ".XX.....";
     ".XXXXX..";
     ".XX..XX.";
     ".XX..XX.";
     "XX.XXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character99 =
    ["........";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XX......";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character100 =
    ["...XXX..";
     "....XX..";
     "....XX..";
     ".XXXXX..";
     "XX..XX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character101 =
    ["........";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XXXXXX..";
     "XX......";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character102 =
    ["..XXX...";
     ".XX.XX..";
     ".XX..X..";
     "XXXX....";
     ".XX.....";
     ".XX.....";
     "XXXX....";
     "........"]
    |> Pattern.patternFromStringListX

let character103 =
    ["........";
     "........";
     ".XXX.XX.";
     "XX..XX..";
     "XX..XX..";
     ".XXXXX..";
     "....XX..";
     "XXXXX..."]
    |> Pattern.patternFromStringListX

let character104 =
    ["XXX.....";
     ".XX.....";
     ".XX.XX..";
     ".XXX.XX.";
     ".XX..XX.";
     ".XX..XX.";
     "XXX..XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character105 =
    ["..XX....";
     "........";
     ".XXX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character106 =
    ["....XX..";
     "........";
     "...XXX..";
     "....XX..";
     "....XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX..."]
    |> Pattern.patternFromStringListX

let character107 =
    ["XXX.....";
     ".XX.....";
     ".XX..XX.";
     ".XX.XX..";
     ".XXXX...";
     ".XX.XX..";
     "XXX..XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character108 =
    [".XXX....";
     "..XX....";
     "..XX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character109 =
    ["........";
     "........";
     "XX..XX..";
     "XXXXXXX.";
     "XXXXXXX.";
     "XX.X.XX.";
     "XX.X.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character110 =
    ["........";
     "........";
     "X.XXX...";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character111 =
    ["........";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character112 =
    ["........";
     "........";
     "XX.XXX..";
     ".XX..XX.";
     ".XX..XX.";
     ".XXXXX..";
     ".XX.....";
     "XXXX...."]
    |> Pattern.patternFromStringListX

let character113 =
    ["........";
     "........";
     ".XXX.XX.";
     "XX..XX..";
     "XX..XX..";
     ".XXXXX..";
     "....XX..";
     "...XXXX."]
    |> Pattern.patternFromStringListX

let character114 =
    ["........";
     "........";
     "XX.XXX..";
     ".XXX.XX.";
     ".XX...X.";
     ".XX.....";
     "XXXX....";
     "........"]
    |> Pattern.patternFromStringListX

let character115 =
    ["........";
     "........";
     ".XXXXX..";
     "XX......";
     ".XXX....";
     "...XXX..";
     "XXXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character116 =
    ["...X....";
     "..XX....";
     "XXXXXX..";
     "..XX....";
     "..XX....";
     "..XX.X..";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character117 =
    ["........";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character118 =
    ["........";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "..XX....";
     "........"]
    |> Pattern.patternFromStringListX

let character119 =
    ["........";
     "........";
     "XX...XX.";
     "XX...XX.";
     "XX.X.XX.";
     "XXXXXXX.";
     ".XX.XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character120 =
    ["........";
     "........";
     "XX...XX.";
     ".XX.XX..";
     "..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character121 =
    ["........";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXXX..";
     "....XX..";
     "XXXXX..."]
    |> Pattern.patternFromStringListX

let character122 =
    ["........";
     "........";
     "XXXXXX..";
     "X..XX...";
     "..XX....";
     ".XX..X..";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character123 =
    ["...XXX..";
     "..XX....";
     "..XX....";
     "XXX.....";
     "..XX....";
     "..XX....";
     "...XXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character124 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "........";
     "...XX...";
     "...XX...";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character125 =
    ["XXX.....";
     "..XX....";
     "..XX....";
     "...XXX..";
     "..XX....";
     "..XX....";
     "XXX.....";
     "........"]
    |> Pattern.patternFromStringListX

let character126 =
    [".XXX.XX.";
     "XX.XXX..";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character127 =
    ["........";
     "...X....";
     "..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "XX...XX.";
     "XXXXXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character128 =
    [".XXXXX..";
     "XX...XX.";
     "XX......";
     "XX...XX.";
     ".XXXXX..";
     "....XX..";
     ".....XX.";
     ".XXXXX.."]
    |> Pattern.patternFromStringListX

let character129 =
    ["........";
     "XX..XX..";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character130 =
    ["...XXX..";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XXXXXX..";
     "XX......";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character131 =
    [".XXXXXX.";
     "X......X";
     "..XXXX..";
     ".....XX.";
     "..XXXXX.";
     ".XX..XX.";
     "..XXX.XX";
     "........"]
    |> Pattern.patternFromStringListX

let character132 =
    ["XX..XX..";
     "........";
     ".XXXX...";
     "....XX..";
     ".XXXXX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character133 =
    ["XXX.....";
     "........";
     ".XXXX...";
     "....XX..";
     ".XXXXX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character134 =
    ["..XX....";
     "..XX....";
     ".XXXX...";
     "....XX..";
     ".XXXXX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character135 =
    ["........";
     "........";
     ".XXXXX..";
     "XX...XX.";
     "XX......";
     ".XXXX...";
     "....XX..";
     "..XXX..."]
    |> Pattern.patternFromStringListX

let character136 =
    [".XXXXXX.";
     "X......X";
     "..XXXX..";
     ".XX..XX.";
     ".XXXXXX.";
     ".XX.....";
     "..XXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character137 =
    ["XX..XX..";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XXXXXX..";
     "XX......";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character138 =
    ["XXX.....";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XXXXXX..";
     "XX......";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character139 =
    ["XX..XX..";
     "........";
     ".XXX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character140 =
    [".XXXXX..";
     "X.....X.";
     "..XXX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "..XXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character141 =
    ["XXX.....";
     "........";
     ".XXX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character142 =
    ["XX...XX.";
     "...X....";
     ".XXXXX..";
     "XX...XX.";
     "XXXXXXX.";
     "XX...XX.";
     "XX...XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character143 =
    ["..XX....";
     "..XX....";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XXXXXX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character144 =
    ["...XXX..";
     "........";
     "XXXXXX..";
     ".XX.....";
     ".XXXX...";
     ".XX.....";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character145 =
    ["........";
     "........";
     ".XXXXXXX";
     "....XX..";
     ".XXXXXXX";
     "XX..XX..";
     ".XXXXXXX";
     "........"]
    |> Pattern.patternFromStringListX

let character146 =
    ["..XXXXX.";
     ".XX.XX..";
     "XX..XX..";
     "XXXXXXX.";
     "XX..XX..";
     "XX..XX..";
     "XX..XXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character147 =
    [".XXXX...";
     "X....X..";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character148 =
    ["........";
     "XX..XX..";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character149 =
    ["........";
     "XXX.....";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character150 =
    [".XXXX...";
     "X....X..";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character151 =
    ["........";
     "XXX.....";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character152 =
    ["........";
     "XX..XX..";
     "........";
     "XX..XX..";
     "XX..XX..";
     ".XXXXX..";
     "....XX..";
     "XXXXX..."]
    |> Pattern.patternFromStringListX

let character153 =
    ["XX....XX";
     "...XX...";
     "..XXXX..";
     ".XX..XX.";
     ".XX..XX.";
     "..XXXX..";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character154 =
    ["XX..XX..";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character155 =
    ["...XX...";
     "...XX...";
     ".XXXXXX.";
     "XX......";
     "XX......";
     ".XXXXXX.";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character156 =
    ["..XXX...";
     ".XX.XX..";
     ".XX..X..";
     "XXXX....";
     ".XX.....";
     "XXX..XX.";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character157 =
    ["XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "..XX....";
     "XXXXXX..";
     "..XX....";
     "XXXXXX..";
     "..XX...."]
    |> Pattern.patternFromStringListX

let character158 =
    ["XXXXX...";
     "XX..XX..";
     "XX..XX..";
     "XXXXX.X.";
     "XX...XX.";
     "XX..XXXX";
     "XX...XX.";
     "XX....XX"]
    |> Pattern.patternFromStringListX

let character159 =
    ["....XXX.";
     "...XX.XX";
     "...XX...";
     "..XXXX..";
     "...XX...";
     "...XX...";
     "XX.XX...";
     ".XXX...."]
    |> Pattern.patternFromStringListX

let character160 =
    ["...XXX..";
     "........";
     ".XXXX...";
     "....XX..";
     ".XXXXX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character161 =
    ["..XXX...";
     "........";
     ".XXX....";
     "..XX....";
     "..XX....";
     "..XX....";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character162 =
    ["........";
     "...XXX..";
     "........";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character163 =
    ["........";
     "...XXX..";
     "........";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character164 =
    ["........";
     "XXXXX...";
     "........";
     "X.XXX...";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character165 =
    ["XXXXXX..";
     "........";
     "XX..XX..";
     "XXX.XX..";
     "XXXXXX..";
     "XX.XXX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character166 =
    ["..XXXX..";
     ".XX.XX..";
     ".XX.XX..";
     "..XXXXX.";
     "........";
     ".XXXXXX.";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character167 =
    ["..XXX...";
     ".XX.XX..";
     ".XX.XX..";
     "..XXX...";
     "........";
     ".XXXXX..";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character168 =
    ["...XX...";
     "........";
     "...XX...";
     "...XX...";
     "..XX....";
     ".XX..XX.";
     "..XXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character169 =
    ["........";
     "........";
     "........";
     "XXXXXX..";
     "XX......";
     "XX......";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character170 =
    ["........";
     "........";
     "........";
     "XXXXXX..";
     "....XX..";
     "....XX..";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character171 =
    ["XX...XX.";
     "XX..XX..";
     "XX.XX...";
     "..XX.XX.";
     ".XX.X.XX";
     "XX....X.";
     "X....X..";
     "....XXXX"]
    |> Pattern.patternFromStringListX

let character172 =
    ["XX....XX";
     "XX...XX.";
     "XX..XX..";
     "XX.XX.XX";
     "..XX.XXX";
     ".XX.XX.X";
     "XX..XXXX";
     "......XX"]
    |> Pattern.patternFromStringListX

let character173 =
    ["...XX...";
     "........";
     "...XX...";
     "...XX...";
     "..XXXX..";
     "..XXXX..";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character174 =
    ["........";
     "..XX..XX";
     ".XX..XX.";
     "XX..XX..";
     ".XX..XX.";
     "..XX..XX";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character175 =
    ["........";
     "XX..XX..";
     ".XX..XX.";
     "..XX..XX";
     ".XX..XX.";
     "XX..XX..";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character176 =
    ["..X...X.";
     "X...X...";
     "..X...X.";
     "X...X...";
     "..X...X.";
     "X...X...";
     "..X...X.";
     "X...X..."]
    |> Pattern.patternFromStringListX

let character177 =
    [".X.X.X.X";
     "X.X.X.X.";
     ".X.X.X.X";
     "X.X.X.X.";
     ".X.X.X.X";
     "X.X.X.X.";
     ".X.X.X.X";
     "X.X.X.X."]
    |> Pattern.patternFromStringListX

let character178 =
    ["XX.XXX.X";
     ".XXX.XXX";
     "XX.XXX.X";
     ".XXX.XXX";
     "XX.XXX.X";
     ".XXX.XXX";
     "XX.XXX.X";
     ".XXX.XXX"]
    |> Pattern.patternFromStringListX

let character179 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character180 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "XXXXX...";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character181 =
    ["...XX...";
     "...XX...";
     "XXXXX...";
     "...XX...";
     "XXXXX...";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character182 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "XXXX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character183 =
    ["........";
     "........";
     "........";
     "........";
     "XXXXXXX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character184 =
    ["........";
     "........";
     "XXXXX...";
     "...XX...";
     "XXXXX...";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character185 =
    ["..XX.XX.";
     "..XX.XX.";
     "XXXX.XX.";
     ".....XX.";
     "XXXX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character186 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character187 =
    ["........";
     "........";
     "XXXXXXX.";
     ".....XX.";
     "XXXX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character188 =
    ["..XX.XX.";
     "..XX.XX.";
     "XXXX.XX.";
     ".....XX.";
     "XXXXXXX.";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character189 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "XXXXXXX.";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character190 =
    ["...XX...";
     "...XX...";
     "XXXXX...";
     "...XX...";
     "XXXXX...";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character191 =
    ["........";
     "........";
     "........";
     "........";
     "XXXXX...";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character192 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character193 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "XXXXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character194 =
    ["........";
     "........";
     "........";
     "........";
     "XXXXXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character195 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character196 =
    ["........";
     "........";
     "........";
     "........";
     "XXXXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character197 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "XXXXXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character198 =
    ["...XX...";
     "...XX...";
     "...XXXXX";
     "...XX...";
     "...XXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character199 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character200 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XXX";
     "..XX....";
     "..XXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character201 =
    ["........";
     "........";
     "..XXXXXX";
     "..XX....";
     "..XX.XXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character202 =
    ["..XX.XX.";
     "..XX.XX.";
     "XXXX.XXX";
     "........";
     "XXXXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character203 =
    ["........";
     "........";
     "XXXXXXXX";
     "........";
     "XXXX.XXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character204 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XXX";
     "..XX....";
     "..XX.XXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character205 =
    ["........";
     "........";
     "XXXXXXXX";
     "........";
     "XXXXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character206 =
    ["..XX.XX.";
     "..XX.XX.";
     "XXXX.XXX";
     "........";
     "XXXX.XXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character207 =
    ["...XX...";
     "...XX...";
     "XXXXXXXX";
     "........";
     "XXXXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character208 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "XXXXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character209 =
    ["........";
     "........";
     "XXXXXXXX";
     "........";
     "XXXXXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character210 =
    ["........";
     "........";
     "........";
     "........";
     "XXXXXXXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character211 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XXXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character212 =
    ["...XX...";
     "...XX...";
     "...XXXXX";
     "...XX...";
     "...XXXXX";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character213 =
    ["........";
     "........";
     "...XXXXX";
     "...XX...";
     "...XXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character214 =
    ["........";
     "........";
     "........";
     "........";
     "..XXXXXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character215 =
    ["..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX.";
     "XXXXXXXX";
     "..XX.XX.";
     "..XX.XX.";
     "..XX.XX."]
    |> Pattern.patternFromStringListX

let character216 =
    ["...XX...";
     "...XX...";
     "XXXXXXXX";
     "...XX...";
     "XXXXXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character217 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "XXXXX...";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character218 =
    ["........";
     "........";
     "........";
     "........";
     "...XXXXX";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character219 =
    ["XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let character220 =
    ["........";
     "........";
     "........";
     "........";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX"]
    |> Pattern.patternFromStringListX

let character221 =
    ["XXXX....";
     "XXXX....";
     "XXXX....";
     "XXXX....";
     "XXXX....";
     "XXXX....";
     "XXXX....";
     "XXXX...."]
    |> Pattern.patternFromStringListX

let character222 =
    ["....XXXX";
     "....XXXX";
     "....XXXX";
     "....XXXX";
     "....XXXX";
     "....XXXX";
     "....XXXX";
     "....XXXX"]
    |> Pattern.patternFromStringListX

let character223 =
    ["XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "XXXXXXXX";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character224 =
    ["........";
     "........";
     ".XXX.XX.";
     "XX.XXX..";
     "XX..X...";
     "XX.XXX..";
     ".XXX.XX.";
     "........"]
    |> Pattern.patternFromStringListX

let character225 =
    [".XXXXX..";
     "XX...XX.";
     "XX...XX.";
     "XX..XX..";
     "XX...XX.";
     "XX....XX";
     "XX..XXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character226 =
    ["........";
     "XXXXXX..";
     "XX..XX..";
     "XX......";
     "XX......";
     "XX......";
     "XX......";
     "........"]
    |> Pattern.patternFromStringListX

let character227 =
    ["........";
     "........";
     "XXXXXXX.";
     ".XX.XX..";
     ".XX.XX..";
     ".XX.XX..";
     ".XX.XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character228 =
    ["XXXXXX..";
     "XX..XX..";
     ".XX.....";
     "..XX....";
     ".XX.....";
     "XX..XX..";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character229 =
    ["........";
     "........";
     ".XXXXXX.";
     "XX.XX...";
     "XX.XX...";
     "XX.XX...";
     ".XXX....";
     "........"]
    |> Pattern.patternFromStringListX

let character230 =
    ["........";
     ".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     ".XX..XX.";
     ".XXXXX..";
     ".XX.....";
     "XX......"]
    |> Pattern.patternFromStringListX

let character231 =
    ["........";
     ".XXX.XX.";
     "XX.XXX..";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character232 =
    ["XXXXXX..";
     "..XX....";
     ".XXXX...";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "..XX....";
     "XXXXXX.."]
    |> Pattern.patternFromStringListX

let character233 =
    ["..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "XXXXXXX.";
     "XX...XX.";
     ".XX.XX..";
     "..XXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character234 =
    ["..XXX...";
     ".XX.XX..";
     "XX...XX.";
     "XX...XX.";
     ".XX.XX..";
     ".XX.XX..";
     "XXX.XXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character235 =
    ["...XXX..";
     "..XX....";
     "...XX...";
     ".XXXXX..";
     "XX..XX..";
     "XX..XX..";
     ".XXXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character236 =
    ["........";
     "........";
     ".XXXXXX.";
     "XX.XX.XX";
     "XX.XX.XX";
     ".XXXXXX.";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character237 =
    [".....XX.";
     "....XX..";
     ".XXXXXX.";
     "XX.XX.XX";
     "XX.XX.XX";
     ".XXXXXX.";
     ".XX.....";
     "XX......"]
    |> Pattern.patternFromStringListX

let character238 =
    ["..XXX...";
     ".XX.....";
     "XX......";
     "XXXXX...";
     "XX......";
     ".XX.....";
     "..XXX...";
     "........"]
    |> Pattern.patternFromStringListX

let character239 =
    [".XXXX...";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "XX..XX..";
     "........"]
    |> Pattern.patternFromStringListX

let character240 =
    ["........";
     ".XXXXXX.";
     "........";
     ".XXXXXX.";
     "........";
     ".XXXXXX.";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character241 =
    ["...XX...";
     "...XX...";
     ".XXXXXX.";
     "...XX...";
     "...XX...";
     "........";
     ".XXXXXX.";
     "........"]
    |> Pattern.patternFromStringListX

let character242 =
    [".XX.....";
     "..XX....";
     "...XX...";
     "..XX....";
     ".XX.....";
     "........";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character243 =
    ["...XX...";
     "..XX....";
     ".XX.....";
     "..XX....";
     "...XX...";
     "........";
     "XXXXXX..";
     "........"]
    |> Pattern.patternFromStringListX

let character244 =
    ["....XXX.";
     "...XX.XX";
     "...XX.XX";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX..."]
    |> Pattern.patternFromStringListX

let character245 =
    ["...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "...XX...";
     "XX.XX...";
     "XX.XX...";
     ".XXX...."]
    |> Pattern.patternFromStringListX

let character246 =
    ["...XX...";
     "...XX...";
     "........";
     ".XXXXXX.";
     "........";
     "...XX...";
     "...XX...";
     "........"]
    |> Pattern.patternFromStringListX

let character247 =
    ["........";
     ".XXX.XX.";
     "XX.XXX..";
     "........";
     ".XXX.XX.";
     "XX.XXX..";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character248 =
    ["..XXX...";
     ".XX.XX..";
     ".XX.XX..";
     "..XXX...";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character249 =
    ["........";
     "........";
     "........";
     "...XX...";
     "...XX...";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character250 =
    ["........";
     "........";
     "........";
     "........";
     "...XX...";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character251 =
    ["....XXXX";
     "....XX..";
     "....XX..";
     "....XX..";
     "XXX.XX..";
     ".XX.XX..";
     "..XXXX..";
     "...XXX.."]
    |> Pattern.patternFromStringListX

let character252 =
    [".X.XX...";
     ".XX.XX..";
     ".XX.XX..";
     ".XX.XX..";
     ".XX.XX..";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character253 =
    [".XXX....";
     "X..XX...";
     "..XX....";
     ".XX.....";
     "XXXXX...";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character254 =
    ["........";
     "........";
     "..XXXX..";
     "..XXXX..";
     "..XXXX..";
     "..XXXX..";
     "........";
     "........"]
    |> Pattern.patternFromStringListX

let character255 =
    ["........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........";
     "........"]
    |> Pattern.patternFromStringListX