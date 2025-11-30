let converges (f: 'a -> 'a) (start: 'a) (n: int) : bool =
  let rec aux (current: 'a) (count: int) : bool =
    if count < 0 then false
    else if current = (f current) then true
    else aux (f current) (count - 1)
  in
  aux start n

let () =
  print_endline (if converges (( * ) 2) 2 5 then "true" else "false"); (* Should output false *)
  print_endline (if converges (fun x -> x / 2) 2 3 then "true" else "false"); (* Should output true *)
  print_endline (if converges (fun x -> x / 2) 2 2 then "true" else "false"); (* Should output true *)
  ()
