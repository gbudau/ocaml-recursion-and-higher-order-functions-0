let fibonacci (n: int) : int = 
  let rec fibonacci' (steps_left: int) (curr_fib: int) (next_fib: int) : int =
    if steps_left = 0 then curr_fib
    else fibonacci' (steps_left - 1) next_fib (curr_fib + next_fib)
  in
  if n < 0 then -1 else fibonacci' n 0 1

let () =
  fibonacci (-42) |> string_of_int |> print_endline; (* Should output '-1' *)
  fibonacci 0 |> string_of_int |> print_endline; (* Should output '0' *)
  fibonacci 1 |> string_of_int |> print_endline; (* Should output '1' *)
  fibonacci 3 |> string_of_int |> print_endline; (* Should output '2' *)
  fibonacci 6 |> string_of_int |> print_endline; (* Should output '8' *)
  fibonacci 42 |> string_of_int |> print_endline; (* Should output '267914296' *)
