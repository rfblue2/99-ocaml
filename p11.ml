type 'a rle =
    | One of 'a
    | Many of int * 'a

let encode (l: string list): string rle list =
  let build (e: string) (base: string rle list): string rle list =
    match base with
    | [] ->                             [One e]
    | (One e')::tl when e'=e ->         Many(2,e)::tl
    | (Many(count, e'))::tl when e'=e -> Many(count + 1, e)::tl
    | _ ->                              (One e)::base
  in
  List.fold_right build l []

let _ =
  assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")])
