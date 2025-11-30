let leibniz_pi (delta: float): int = 
  if delta < 0.0 then -1
  else
  let pi = (4. *. atan 1.) in
  let rec aux (accumulator: float) (current: int): int =
    let sign = if current mod 2 = 0 then 1. else -1. in
    let leibniz_term = 4. *. (sign /. (2. *. float_of_int current +. 1.)) in
    if abs_float (accumulator -. pi) < delta then current
    else aux (accumulator +. leibniz_term) (current + 1)
  in
  aux 0.0 0

let () =
 leibniz_pi 1. |> print_int |> print_newline; (* Should out put 1 *)
 leibniz_pi 0.1 |> print_int |> print_newline; (* Should output 10 *)
 leibniz_pi 0.01 |> print_int |> print_newline; (* Should output 100 *)
 leibniz_pi 0.001 |> print_int |> print_newline; (* Should output 1000 *)
 leibniz_pi (-0.5) |> print_int |> print_newline; (* Should output -1 *)
 ()
