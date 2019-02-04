type 'a rle =
    | One of 'a
    | Many of int * 'a

let decode (encoded: string rle list): string list =
  let rec aux (current: string rle) (base: string list): string list =
    (match current with
    | One a | Many(1,a) -> a::base
    | Many(n,a) -> aux (Many(n-1, a)) (a::base))
  in
  List.fold_right aux encoded [""]

let _ =
  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")]
