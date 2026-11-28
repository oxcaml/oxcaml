(* TEST
 flags += "-extension mode_polymorphism_alpha -extension mode_polymorphism_printing";
 expect;
*)

module type S = sig
  val binary_search
    : ('elt : value_or_null) 'key 't.
    't
    -> length:('t -> int) @ local
    -> get:('t -> int -> 'elt) @ local
    -> compare:('elt -> 'key -> int) @ local
    -> 'key
    -> int option
end
[%%expect{|
module type S =
  sig
    val binary_search :
      't ->
      length:('t -> int) @ local ->
      get:('t -> int -> 'elt) @ local ->
      compare:('elt -> 'key -> int) @ local -> 'key -> int option
  end
|}]

module M : S = struct
  let linear_search_first_satisfying t ~get ~lo ~hi ~pred = exclave_
    if lo > hi
    then None
    else if pred (get t lo)
    then Some lo
    else None

  let find_first_satisfying t ~get ~length ~pred = exclave_
    linear_search_first_satisfying t ~get ~lo:0 ~hi:(length t - 1) ~pred

  let find_last_satisfying t ~pred ~get ~length = exclave_
    let len = length t in
    if len = 0
    then None
    else (
      match
        find_first_satisfying t ~get ~length ~pred:(fun x -> not (pred x))
      with
      | None -> Some (len - 1)
      | Some i when i = 0 -> None
      | Some i -> Some (i - 1))

  let binary_search t ~(local_ length) ~(local_ get) ~(local_ compare) v
    = exclave_
    if compare (get t 0) v > 0
    then find_first_satisfying t ~get ~length ~pred:(fun x -> compare x v >= 0)
    else find_last_satisfying t ~get ~length ~pred:(fun x -> compare x v <= 0)
end
[%%expect{|
Lines 1-29, characters 15-3:
 1 | ...............struct
 2 |   let linear_search_first_satisfying t ~get ~lo ~hi ~pred = exclave_
 3 |     if lo > hi
 4 |     then None
 5 |     else if pred (get t lo)
...
26 |     if compare (get t 0) v > 0
27 |     then find_first_satisfying t ~get ~length ~pred:(fun x -> compare x v >= 0)
28 |     else find_last_satisfying t ~get ~length ~pred:(fun x -> compare x v <= 0)
29 | end
Error: Signature mismatch:
       The value "binary_search" does not match its expected declaration
       The type
         "... ->
         length:... ->
         get:... ->
         compare:... -> 'a -> int option @ [> local aliased dynamic]"
       is not compatible with the type
         "... -> length:... -> get:... -> compare:... -> 'a -> int option"
       Type "'a -> int option @ [> local aliased dynamic]"
       is not compatible with type "'a -> int option"
       The return mode was expected to be "global" but is "local"
|}]
