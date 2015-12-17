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