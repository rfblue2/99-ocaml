let pack (xs: 'a list) : 'a list list =
  let rec aux (xs: 'a list) (acc: 'a list list option): 'a list list =
    match xs, acc with
    | [], None -> []
    | [], Some acc' -> acc'
    | x::xs', None -> aux xs' (Some [[x]])
    | x::xs', Some acc' -> 
        (match acc' with
        | [] | []::_-> aux xs' (Some [[x]])
        | (xx::accx)::accxs -> 
            if x=xx then aux xs' (Some ((x::xx::accx)::accxs))
            else aux xs' (Some ([x]::(xx::accx)::accxs)))
  in
  List.rev (aux xs None)

let _ = 
  assert(pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]=[["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 ["e"; "e"; "e"; "e"]]);
 ()
