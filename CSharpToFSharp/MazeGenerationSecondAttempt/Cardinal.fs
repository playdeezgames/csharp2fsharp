﻿(*
    This code accompanies two blog posts.

    The first can be found at https://csharptofsharp.wordpress.com/2015/12/16/immutable-maze-generation-in-f-part-iii/

    The second can be found at https://csharptofsharp.wordpress.com/2015/12/17/immutable-maze-generation-in-f-part-iv/
*)
module Cardinal

open Location

type Direction = North | East | South | West

let Walk (location:Location) (direction:Direction) : Location = 
    match direction with
    | North -> {location with                          Row=location.Row-1}
    | East  -> {location with Column=location.Column+1                   }
    | South -> {location with                          Row=location.Row+1}
    | West  -> {location with Column=location.Column-1                   }

let Values = [North; East; South; West]