open Batteries

module IntSet = Set.Make(Int)
  
let comp f g x =
  f (g x)

let flip f a b =
  f b a

let permutations ?length list =
  let len = match length with
	| Some n -> n
	| None -> List.length list in
  let rec inner list length taken =
	match length with
	| _ when Set.cardinal taken = length -> Enum.singleton []
	| _ -> Enum.concat 
	  (Enum.map 
		 (fun l -> Enum.map 
		   (fun x->l::x) 
		   (inner list length (Set.add l taken)))
		 (Enum.filter (comp not (flip Set.mem taken)) (List.enum list))) in
  inner list len Set.empty


let rec range i j =
  let rec inner i j accum =
	if j >= i then
	  inner (i+1) j (List.append accum [i])
	else
	  accum in 
  inner i j []

let (|>) a f =
    f a

let fromDigits' ns =
    Enum.mapi (fun i e -> Int.pow 10 i * e) (List.enum ns) |> Enum.sum

let fromDigits = comp fromDigits' List.rev

let pandigital =
    let results = IntSet.empty in
	let valid_position numbers position =
	  match position with
	  | product::[equals]->
        if equals > product then
          let f1 = fromDigits (List.take product numbers) in
          let f2 = fromDigits (List.drop product (List.take equals numbers)) in
          let result = fromDigits (List.drop equals numbers) in
          if f1*f2 = result then
			Some result
		  else None
		else None
	  | _ -> failwith "not a 2-list" in
	Enum.map (fun numbers -> 
	  Enum.filter_map (valid_position numbers) (permutations ~length:2 (range 1 8))
	) (permutations (range 1 9)) |> Enum.concat |>
		Enum.fold (flip IntSet.add) results 


let main = print_int (IntSet.fold (+) pandigital 0); print_newline ()



