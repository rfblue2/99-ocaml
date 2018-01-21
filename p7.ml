type 'a node =
    | One of 'a 
    | Many of 'a node list

let flatten (n: 'a node list) : 'a list =
  let rec aux (n: 'a node list) (acc: 'a list): 'a list =
    match n with
    | [] -> acc
    | (One x)::n' -> aux n' (x::acc) 
    | (Many n'')::n' -> 
       aux n' (aux n'' acc)
  in
  List.rev (aux n [] )

let _ =
  assert(flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]);
  ()
