
let rec factors' n x =
    let divides = (x % n) = 0
    let quotient = x/n
    match n,x with
    //| _ when (>) n << sqrt <| float x -> [x] // Ugly as sin!
    | _ when n > int (sqrt (float x)) -> [x]
    | _ when divides && quotient <> n -> n:: quotient:: factors' (n+1) x
    | _ when divides -> n::factors' (n+1) x
    | _ -> factors' (n+1) x

let factors x = 1::factors' 2 x

let rec factorsSeq' n x =
    let divides = (x % n) = 0
    let quotient = x/n
    seq{
        match n,x with
        | _ when n > int (sqrt (float x)) -> yield x
        | _ when divides && quotient <> n ->
            yield n
            yield quotient
            yield! factorsSeq' (n+1) x
        | _ when divides ->
            yield n
            yield! factorsSeq' (n+1) x
        | _ -> yield! factorsSeq' (n+1) x
    }

let factorsSeq x = seq{
        yield 1
        yield! factorsSeq' 2 x
    }

let rec smallest_triangle' x ni n =
    let fs = factors x
    let ni' = ni+1
    match n with
    | _ when fs.Length >= n -> x
    | _ -> smallest_triangle' (x+ni') ni' n

let smallest_triangle = smallest_triangle' 3 2

let main = printfn "%A" (smallest_triangle 501)