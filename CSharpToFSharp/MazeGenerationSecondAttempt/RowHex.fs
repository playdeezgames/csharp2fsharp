(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
module RowHex

open Location

type Direction = Northeast | East | Southeast | Southwest | West | Northwest

let Walk (location:Location) (direction:Direction) : Location = 
    match direction with
    | Northeast -> {location with                               Row = location.Row - 1}
    | East      -> {location with Column = location.Column + 1                        }
    | Southeast -> {              Column = location.Column + 1; Row = location.Row + 1}
    | Southwest -> {location with                               Row = location.Row + 1}
    | West      -> {location with Column = location.Column - 1                        }
    | Northwest -> {              Column = location.Column - 1; Row = location.Row - 1}

let Values = [Northeast; East; Southeast; Southwest; West; Northwest]