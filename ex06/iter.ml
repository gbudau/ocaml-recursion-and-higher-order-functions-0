let iter (f: int -> int) (start: int) (n: int) : int =
  let rec aux (current: int) (count: int) : int =
    if count < 0 then -1
    else if count = 0 then current
    else aux (f current) (count - 1)
  in
  aux start n

let () =
  iter (fun x -> x + 1) 2 (-10) |> print_int |> print_newline; (* Should output -1 *)
  iter (fun x -> x * x) 2 4 |> print_int |> print_newline; (* Should output 65536 *)
  iter (fun x -> x * 2) 2 4 |> print_int |> print_newline; (* Should output 32 *)
    iter (fun x -> x + 1) 0 5 |> print_int |> print_newline; (* Should output 5 *)
  ()
