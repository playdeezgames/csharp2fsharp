module Utility

open Location

let makeGrid (columns, rows) = 
    [for c in [0..columns-1] do
        for r in [0..rows-1] do
            yield {Column=c; Row=r}]

let findAllCardinal = Neighbor.findAll Cardinal.walk Cardinal.values

let random = new System.Random()

let picker (choices:seq<'t>) =
    let pick = choices |> Seq.length |> random.Next
    choices |> Seq.item pick


