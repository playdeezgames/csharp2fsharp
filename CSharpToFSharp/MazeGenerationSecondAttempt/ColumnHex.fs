(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
module ColumnHex

open Location

type Direction = North | Northeast | Southeast | South | Southwest | Northwest

let Walk (location:Location) (direction:Direction) : Location = 
    match direction with
    | North     -> {location with                               Row = location.Row - 1}
    | Northeast -> {location with Column = location.Column + 1                        }
    | Southeast -> {              Column = location.Column + 1; Row = location.Row + 1}
    | South     -> {location with                               Row = location.Row + 1}
    | Southwest -> {location with Column = location.Column - 1                        }
    | Northwest -> {              Column = location.Column - 1; Row = location.Row - 1}

let Values = [North; Northeast; Southeast; South; Southwest; Northwest]