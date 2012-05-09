#light
 
let divides y x =
    x % y = 0
 
let triangles =
    Seq.unfold (fun (acc, state) -> Some (acc, (state + acc, state + 1))) (0, 1)
    |> Seq.skip 1 // SKIP THE INITIAL 0
 
let rec all_factors_quick_rec n i factors =
    if divides i n then
        let y = n / i
        if (i < y) then
            all_factors_quick_rec n (i + 1) (i::y::factors)
        elif (i = y) then
            // WE HAVE REACHED THE SQUARE ROOT VALUE
            i::factors
        else
            factors
    elif i > int (sqrt (float n)) then
        // WE ARE BEYOND THE SQUARE ROOT VALUE
        factors
    else
        // TRY WITH THE NEXT NUMBER
        all_factors_quick_rec n (i + 1) factors
 
let all_factors_quick n =
    all_factors_quick_rec n 1 []
 
let find_index m =
    triangles
    |> Seq.map all_factors_quick
    |> Seq.tryFindIndex (fun x -> List.length x >= m)
 
let resolve_problem_12 =
    let i = find_index 500
    match i with
    | None -> failwith "Cannot resolve problem"
    | Some(i) ->
        let list = triangles |> Seq.take (i+1) |> List.ofSeq
        list.[i]

let main = printf "%A" resolve_problem_12