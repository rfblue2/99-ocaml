let is_palindrome (xs: 'a list): bool =
  List.rev xs = xs

let _ = 
  assert(is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]);
  assert(not(is_palindrome [ "a" ; "b" ]));
  ()
