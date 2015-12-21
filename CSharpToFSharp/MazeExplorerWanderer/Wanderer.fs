module Wanderer

open Explorer

let rec explore (action: Explorer<'direction, unit>->Explorer<'direction, unit>) (canWalk: Explorer<'direction,unit>->bool) walk (explorer: Explorer<'direction,unit>) =
    let exploreNext = explore action canWalk walk
    let newExplorer = explorer |> action
    if newExplorer |> canWalk then
        newExplorer |> walk |> exploreNext
    else
        newExplorer |> exploreNext
