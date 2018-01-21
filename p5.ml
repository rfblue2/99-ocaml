let rev (xs : 'a list) : 'a list =
  let rec aux (xs: 'a list) (acc: 'a list) =
    match xs with
    | [] -> acc 
    | x::xs' -> aux xs' (x::acc) 
  in aux xs []

let _ =
  assert(rev ["a"; "b"; "c"] = ["c"; "b"; "a"]);
  ()

