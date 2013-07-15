
let rec factors' n x =
    let divides = (x mod n) = 0 in
    let quotient = x/n in
    match n,x with
    (*//| _ when (>) n << sqrt <| float x -> [x] // Ugly as sin!*)
    | _ when n > int_of_float (sqrt (float x)) -> [x]
    | _ when divides && quotient <> n -> n:: quotient:: factors' (n+1) x
    | _ when divides -> n::factors' (n+1) x
    | _ -> factors' (n+1) x

let factors x = 1::factors' 2 x

let rec smallest_triangle' x ni n =
    let fs = factors x in
    let ni' = ni+1 in
    match n with
    | _ when List.length fs >= n -> x
    | _ -> smallest_triangle' (x+ni') ni' n

let smallest_triangle = smallest_triangle' 3 2

let main = Printf.printf "%d" (smallest_triangle 501)
