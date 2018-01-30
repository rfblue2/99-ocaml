let encode (l: string list): (int * string) list =
  let build (e: string) (base: (int*string) list):  (int*string) list =
    match base with
    | [] -> [(1, e)]
    | (count,e')::tl -> 
        if e'=e then (count+1,e')::tl
        else (1,e)::base
  in
  List.fold_right build l []

let _ =
  assert(encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"] = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
)
