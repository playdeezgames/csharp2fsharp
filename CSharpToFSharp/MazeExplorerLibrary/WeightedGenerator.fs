module WeightedGenerator

let totalWeight generator =
    generator
    |> Map.fold (fun acc k v -> acc + v) 0

let generationFold (current: 'a option, accumulator:int) (key:'a) (value:int) =
    (if accumulator>=0 && accumulator < value then
        Some key
    else
        current
    , accumulator - value)

let generate (rng: int->int) (generator: Map<'a,int>) =
    let value =
        generator
        |> totalWeight
        |> rng
    generator
    |> Map.fold generationFold (None, value)
    |> fst
    |> Option.get

let combine (keyCombiner: 'a->'a->'b) (first: Map<'a, int>) (second:Map<'a,int>) : Map<'b,int>=
    first
    |> Map.toList
    |> List.map (fun (k1, v1) -> second
                                 |> Map.toList
                                 |> List.map (fun (k2, v2) -> (keyCombiner k1 k2, v1 * v2)))
    |> Seq.reduce (@)
    |> Seq.groupBy (fun (k,_)->k)
    |> Seq.map(fun (k,v)-> (k, v |> Seq.fold (fun acc (_,v2)-> acc + v2) 0))
    |> Map.ofSeq

let ofPairs (items:('a * int) list) =
    items
    |> Map.ofList

let ofList (items: 'a list) =
    items
    |> List.map (fun i->(i,1))
    |> ofPairs

let ofSeq (items: seq<'a>) =
    items
    |> Seq.map (fun i->(i,1))
    |> Map.ofSeq