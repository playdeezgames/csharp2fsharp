module Visitor

let rec explore isFinished decide act explorer =
    if explorer |> isFinished then
        explorer
    else
        let command = explorer |> decide
        explorer
        |> act command
        |> explore isFinished decide act
