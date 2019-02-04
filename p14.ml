let rec duplicate (ls: string list): string list =
  match ls with
  | [] -> []
  | hd::tl -> hd::hd::(duplicate tl)

let _ = assert(duplicate ["a";"b";"c";"c";"d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]) 
