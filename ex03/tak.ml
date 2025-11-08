let tak (x: int) (y: int) (z: int) : int =
  let rec tak' (x': int) (y': int) (z': int) : int =
    if y' >= x' then z'
    else tak' (tak' (x' - 1) y' z') (tak' (y' - 1) z' x') (tak' (z' - 1) x' y')
  in
  tak' x y z

let () =
  tak 1 2 3 |> string_of_int |> print_endline; (* Should output '3' *)
  tak 5 23 7 |> string_of_int |> print_endline; (* Should output '7' *)
  tak 9 1 0 |> string_of_int |> print_endline; (* Should output '1' *)
  tak 1 1 1 |> string_of_int |> print_endline; (* Should output '1' *)
  tak 0 42 0 |> string_of_int |> print_endline; (* Should output '0' *)
  tak 23498 98734 98776 |> string_of_int |> print_endline; (* Should output 98776 *)
  ()
