open System.Collections.Generic

type IterOps =
    static member private permutations(list, length, taken) =
        seq { if Set.count taken = length then yield [] else
                for l in list do
                    if not (Set.contains l taken) then
                        for perm in IterOps.permutations(list, length, (Set.add l taken)) do
                            yield l::perm }

    static member permutations(list: 'a list) =
        IterOps.permutations(list, List.length list, Set.empty)

    static member permutations(list: seq<'a>) =
        IterOps.permutations(list, Seq.length list, Set.empty)

    static member permutations(list, length) =
        IterOps.permutations(list, length, Set.empty)


let rec digits n =
    let n' = n/10
    match n with
    | _ when n' <> 0 -> n%10 :: digits n'
    | _ -> [n]

let fromDigits' ns =
    Seq.map2 ( * ) ns (Seq.initInfinite (pown 10)) |> Seq.sum

let fromDigits ns = fromDigits' (List.rev (Seq.toList ns))


// naïve solution, takes about 5 minutes to complete
// possible optimizations: permute over an array, and only over [2..6] or smaller
let pandigital =
    let results = new HashSet<int>()
    for n in IterOps.permutations [1..9] do
        for product::[equals] in (IterOps.permutations([1..8], 2)) do
            if equals > product then
                let f1 = fromDigits (Seq.take product n)
                let f2 = fromDigits (Seq.skip product (Seq.take equals n))
                let result = fromDigits (Seq.skip equals n)
                if f1*f2 = result then do
                        results.Add result |> ignore
    results

let main = printfn "%A" (Seq.sum pandigital)



