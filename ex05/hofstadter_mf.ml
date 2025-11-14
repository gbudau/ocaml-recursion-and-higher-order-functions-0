let rec hfs_f (n: int) : int = 
  if n < 0 then -1
  else if n = 0 then 1
  else n - hfs_m (hfs_f (n - 1))
and hfs_m (n: int) : int =
  if n < 0 then -1
  else if n = 0 then 0
  else n - hfs_f (hfs_m (n - 1))

let () =
  (* https://en.wikipedia.org/wiki/Hofstadter_sequence#Hofstadter_Female_and_Male_sequences *)
  hfs_m 0 |> string_of_int |> print_endline;  (* Should output '0' *)
  hfs_f 0 |> string_of_int |> print_endline;  (* Should output '1' *)
  hfs_m 4 |> string_of_int |> print_endline;  (* Should output '2' *)
  hfs_f 4 |> string_of_int |> print_endline;  (* Should output '3' *)
  hfs_m 8 |> string_of_int |> print_endline; (* Should output '5' *)
  hfs_f 8 |> string_of_int |> print_endline; (* Should output '5' *)
  hfs_m 12 |> string_of_int |> print_endline; (* Should output '6' *)
  hfs_f 12 |> string_of_int |> print_endline; (* Should output '7' *)
