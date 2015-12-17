(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
module Neighbors

open Location

let FindAll (walk: Location->'direction->Location) (directions:'direction list) (location:Location) : Location list =
    directions
    |> List.map(walk location)
    

let FindAllCardinal = FindAll Cardinal.Walk Cardinal.Values
let FindAllIntercardinal = FindAll Intercardinal.Walk Intercardinal.Values
let FindAllRowHex = FindAll RowHex.Walk RowHex.Values
let FindAllColumnHex = FindAll ColumnHex.Walk ColumnHex.Values