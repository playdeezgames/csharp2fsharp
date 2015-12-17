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