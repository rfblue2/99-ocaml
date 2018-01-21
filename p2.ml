let rec last_two (xs: 'a list) : ('a * 'a) option =
  match xs with
  | [] -> None
  | [x] -> None
  | x :: x' ::[] -> Some (x, x')
  | _::xs' -> last_two xs'

let _ =
  assert(last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"));
  assert(last_two ["a"] = None);
  ()
