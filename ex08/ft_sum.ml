let ft_sum (f: int -> float) (lower_bound: int) (upper_bound: int): float =
  if upper_bound < lower_bound then nan
  else
  let rec aux (accumulator: float) (l: int) (u: int): float =
    if u < l then accumulator
    else aux (accumulator +. f (l)) (l + 1) u
  in
  aux 0.0 lower_bound upper_bound

let () =
  ft_sum (fun i -> float_of_int (i * i)) 1 10 |> print_float |> print_newline; (* Should output 385. *)
 ()
