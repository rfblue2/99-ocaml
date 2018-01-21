let rec at (n:int) (xs: 'a list) : 'a option =
  match xs with
  | [] -> None
  | x::xs' when n = 1 -> Some x
  | _::xs' -> at (n-1) xs'
  
let _ = 
  assert(at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c");
  assert(at 3 [ "a" ] = None);
  ()
