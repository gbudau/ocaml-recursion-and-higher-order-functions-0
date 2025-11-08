let repeat_string ?(str = "x") (n: int) : string =
  let rec repeat accumulator count =
    if count < 0 then "Error"
    else if count = 0 then accumulator
    else repeat (accumulator ^ str) (count - 1)
  in
  repeat "" n

let () = 
  print_endline (repeat_string (-1)); (* Should output 'Error' *)
  print_endline (repeat_string 0); (* Should output '' (empty string) *)
  print_endline (repeat_string ~str:"Toto" 1); (* Should output 'Toto' *)
  print_endline (repeat_string 2); (* Should output 'xx' *)
  print_endline (repeat_string ~str:"a" 5); (* Should output 'aaaaa' *)
  print_endline (repeat_string ~str:"what" 3); (* Should output 'whatwhatwhat' *)
  ()
