module RoomTiles

let createCardinal background foreground = 
    [((false,false,false,false), RoomPatterns.Rooms0);
    ((true ,false,false,false), RoomPatterns.RoomsN);
    ((false,true ,false,false), RoomPatterns.RoomsE);
    ((true ,true ,false,false), RoomPatterns.RoomsNE);
    ((false,false,true ,false), RoomPatterns.RoomsS);
    ((true ,false,true ,false), RoomPatterns.RoomsNS);
    ((false,true ,true ,false), RoomPatterns.RoomsES);
    ((true ,true ,true ,false), RoomPatterns.RoomsNES);
    ((false,false,false,true ), RoomPatterns.RoomsW);
    ((true ,false,false,true ), RoomPatterns.RoomsNW);
    ((false,true ,false,true ), RoomPatterns.RoomsEW);
    ((true ,true ,false,true ), RoomPatterns.RoomsNEW);
    ((false,false,true ,true ), RoomPatterns.RoomsSW);
    ((true ,false,true ,true ), RoomPatterns.RoomsNSW);
    ((false,true ,true ,true ), RoomPatterns.RoomsESW);
    ((true ,true ,true ,true ), RoomPatterns.RoomsNESW)]
    |> Seq.map (fun (k,p) -> (k, new Tile.Tile(p,background,foreground)))
    |> Map.ofSeq


