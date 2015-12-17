(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
module Intercardinal

open Location

type Direction = North | Northeast | East | Southeast | South | Southwest | West | Northwest

let Walk (location:Location) (direction:Direction) : Location = 
    match direction with
    | North     -> {location with                               Row = location.Row - 1}
    | Northeast -> {              Column = location.Column + 1; Row = location.Row - 1}
    | East      -> {location with Column = location.Column + 1                        }
    | Southeast -> {              Column = location.Column + 1; Row = location.Row + 1}
    | South     -> {location with                               Row = location.Row + 1}
    | Southwest -> {              Column = location.Column - 1; Row = location.Row + 1}
    | West      -> {location with Column = location.Column - 1                        }
    | Northwest -> {              Column = location.Column - 1; Row = location.Row - 1}

let Values = [North; Northeast; East; Southeast; South; Southwest; West; Northwest]