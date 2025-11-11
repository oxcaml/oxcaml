(* Test exception handling *)

exception My_error of string

exception Not_found_custom

exception With_value of int

(* Basic try/with *)
let safe_div x y = try x / y with Division_by_zero -> 0

(* Raising exceptions *)
let check_positive n = if n < 0 then raise (My_error "negative value") else n

(* Multiple exception handlers *)
let handle_many f x =
  try f x with
  | Division_by_zero -> -1
  | Invalid_argument _ -> -2
  | My_error _ -> -3
  | _ -> -999

(* Re-raising exceptions *)
let reraise_example f x =
  try f x
  with e ->
    (* Do some cleanup *)
    let _ = 42 in
    raise e

(* Nested try/with *)
let nested_try x =
  try
    try
      if x < 0
      then raise Not_found_custom
      else if x = 0
      then raise (With_value 0)
      else x * 2
    with Not_found_custom -> -1
  with With_value v -> v

(* Exception in tail position *)
let rec find_first p = function
  | [] -> raise Not_found
  | x :: xs -> if p x then x else find_first p xs

(* Finally-like pattern using match *)
let with_resource acquire release f =
  let r = acquire () in
  match f r with
  | result ->
    release r;
    result
  | exception e ->
    release r;
    raise e

(* Entry point *)
let () =
  let _ = safe_div 10 2 in
  let _ = safe_div 10 0 in
  let _ = check_positive 5 in
  let _ = try check_positive (-1) with My_error _ -> 0 in
  let _ = handle_many (fun x -> x / 0) 1 in
  let _ = nested_try 5 in
  let _ = nested_try (-1) in
  let _ = nested_try 0 in
  let _ =
    try find_first (fun x -> x > 3) [1; 2; 3; 4; 5] with Not_found -> 0
  in
  let _ = try find_first (fun x -> x > 10) [1; 2; 3] with Not_found -> 0 in
  ()
