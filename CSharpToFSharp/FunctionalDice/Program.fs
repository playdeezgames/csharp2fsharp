let d6 =
    [(1, 1);
    (2, 1);
    (3, 1);
    (4, 1);
    (5, 1);
    (6, 1);]
    |> Map.ofSeq

let coin =
    [(true,1);
    (false,1)]
    |> Map.ofSeq

let vowelGenerator = 
    [("a",1);
    ("e",1);
    ("i",1);
    ("o",1);
    ("u",1)]
    |> Map.ofSeq

let twoD6 =
    [(2, 1);
    (3, 2);
    (4, 3);
    (5, 4);
    (6, 5);
    (7, 6);
    (8, 5);
    (9, 4);
    (10, 3);
    (11, 2);
    (12, 1);]
    |> Map.ofSeq

let random = new System.Random()

let rangeGenerator minimum maximum =
    random.Next() % (maximum - minimum + 1) + minimum

let rng = rangeGenerator 0

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

let test = combine (+) d6 d6

[<EntryPoint>]
let main argv = 
    test
    |> Map.toSeq
    |> printfn "%A"
    twoD6
    |> generate rng
    |> printfn "%A"
    0
