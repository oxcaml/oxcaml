type t = int * int

let all =
  [
    (4, 08);
    (4, 09);
    (4, 10);
    (4, 11);
    (4, 12);
    (4, 13);
    (4, 14);
    (5, 0);
    (5, 1);
    (5, 2);
    (5, 3);
    (5, 4);
    (5, 5);
  ]

let to_string (a, b) =
  if a < 5 then Printf.sprintf "%d.%02d" a b else Printf.sprintf "%d.%d" a b

let to_int (a, b) = (a * 100) + b

let of_string s =
  let t = Scanf.sscanf s "%u.%u" (fun a b -> (a, b)) in
  if List.mem t all then Some t else None
