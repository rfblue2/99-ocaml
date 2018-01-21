(* Tail recursive solution using continuous style passing *)
let length(xs: 'a list) =
  let rec length (xs:'a list) (cps: int -> int) : int =
    match xs with
    | [] -> cps 0
    | x::xs' -> length xs' (fun (len:int) -> cps (len + 1))
  in
  length xs (fun x -> x)

let _ = 
  assert(length [ "a" ; "b" ; "c"] = 3);
  assert(length [] = 0);
  ()
