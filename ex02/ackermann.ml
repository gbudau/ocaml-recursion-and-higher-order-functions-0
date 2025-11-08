let ackermann (m: int) (n: int) : int =
  let rec ack (m': int) (n': int) : int =
    if (m' = 0) then n' + 1
    else if (m' > 0) && (n' = 0) then ack (m' - 1) 1
    else ack (m' - 1) (ack m' (n' - 1))
  in
  if (m < 0) || (n < 0) then -1 else ack m n

let () =
  ackermann (-1) 7 |> string_of_int |> print_endline; (* Should output '-1' *)
  ackermann 0 0 |> string_of_int |> print_endline; (* Should output '1' *)
  ackermann 2 3 |> string_of_int |> print_endline; (* Should output '9' *)
  ackermann 4 1 |> string_of_int |> print_endline; (* Should output '65533' *)
