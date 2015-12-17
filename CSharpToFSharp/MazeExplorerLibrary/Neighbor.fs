module Neighbor

open Location

let findAll (walk: Location->'direction->Location) (directions:'direction list) (location:Location) : Location list =
    directions
    |> List.map(walk location)