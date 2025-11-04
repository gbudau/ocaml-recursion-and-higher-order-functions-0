let repeat_x (n: int) : string =
  let rec repeat accumulator count =
    if count < 0 then "Error"
    else if count = 0 then accumulator
    else repeat (accumulator ^ "x") (count - 1)
  in
  repeat "" n

let () =
  print_endline (repeat_x (-1));  (* Should output 'Error' *)
  print_endline (repeat_x 0);  (* Should output '' (empty string) *)
  print_endline (repeat_x 1);  (* Should output 'x' *)
  print_endline (repeat_x 2);  (* Should output 'xx' *)
  print_endline (repeat_x 5);  (* Should output 'xxxxx' *)
  ()
