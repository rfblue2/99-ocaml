let compress (ls: 'a list): 'a list =
  let rec aux (ls: 'a list) (prev: 'a option): 'a list =
    match ls, prev with
    | [],_ -> []
    | hd::tl, Some x when hd = x -> aux tl (Some x)
    | hd::tl, _ -> hd :: (aux tl (Some hd))
  in
  aux ls None

let _ = 
  assert(compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = ["a"; "b"; "c"; "a"; "d"; "e"]);
  ()
