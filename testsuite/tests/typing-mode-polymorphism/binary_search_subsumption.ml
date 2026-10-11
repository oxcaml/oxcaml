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
       Modules do not match:
         sig
           val linear_search_first_satisfying :
             'a @ [< 'o] ->
             get:('a @ [> 'o] -> 'b @ [> 'n | aliased] -> 'c @ [< 'm]) @ 'mm1 ->
             lo:'b @ [< 'q & 'n & many read_write] ->
             hi:'b @ [< many read_write] ->
             pred:('c @ [> 'm | dynamic] -> bool @ 'p) @ 'mm0 ->
             'b option @ [> 'q | local aliased dynamic]
           val find_first_satisfying :
             'a @ [< 'p & 'n & many] ->
             get:('a @ [> 'n | aliased] -> int @ [> aliased] -> 'b @ [< 'm]) @ 'mm2 ->
             length:('a @ [> 'p | aliased] -> int @ 'o) @ 'mm1 ->
             pred:('b @ [> 'm | dynamic] -> bool @ 'q) @ 'mm0 ->
             int option @ [> local aliased dynamic]
           val find_last_satisfying :
             'a @ [< 'p & 'o & many] ->
             pred:('b @ [> 'n | dynamic] -> bool @ 'm) @ 'mm0 ->
             get:('a @ [> 'o | aliased] -> int @ [> aliased] -> 'b @ [< 'n]) @ 'q ->
             length:('a @ [> 'p | aliased] -> int @ [< many read_write]) @ [< many] ->
             int option @ [> local dynamic]
           val binary_search :
             'a @ [< past('p) & 'mm0 & 'm & many] ->
             length:('a @ [> 'm | aliased] -> int @ [< many read_write]) @ [< many > local] ->
             get:('a @ [< past('n) > 'mm0 | aliased] ->
                  (int @ [> aliased] -> 'b @ [< 'q]) @ [> past('n) | past('o) | past('p) | local]) @ [< past('o) & many > local] ->
             compare:('b @ [> 'q | dynamic] ->
                      'c @ [> 'mm1 | aliased] -> int @ [< many read_write]) @ [< many > local] ->
             'c @ [< 'mm1 & many] -> int option @ [> local aliased dynamic]
         end
       is not included in
         S
       Values do not match:
         val binary_search :
           'a @ [< past('p) & 'mm0 & 'm & many] ->
           length:('a @ [> 'm | aliased] -> int @ [< many read_write]) @ [< many > local] ->
           get:('a @ [< past('n) > 'mm0 | aliased] ->
                (int @ [> aliased] -> 'b @ [< 'q]) @ [> past('n) | past('o) | past('p) | local]) @ [< past('o) & many > local] ->
           compare:('b @ [> 'q | dynamic] ->
                    'c @ [> 'mm1 | aliased] -> int @ [< many read_write]) @ [< many > local] ->
           'c @ [< 'mm1 & many] -> int option @ [> local aliased dynamic]
       is not included in
         val binary_search :
           't ->
           length:('t -> int) @ local ->
           get:('t -> int -> 'elt) @ local ->
           compare:('elt -> 'key -> int) @ local -> 'key -> int option
       The type
         "'a @ [< 'n & 'm & global many read_write] ->
         length:('a @ [< global many read_write > 'm | aliased] ->
                 int @ [< many read_write > dynamic]) @ [< many > local] ->
         get:('a @ [< global many read_write > 'n | aliased] ->
              int @ [> aliased] ->
              'b @ [< global many read_write > aliased stateful dynamic]) @ [< many > local] ->
         compare:('b @ [< global many read_write > aliased stateful dynamic] ->
                  'c @ [< global many read_write > aliased stateful dynamic] ->
                  int @ [< many read_write > dynamic]) @ [< many > local] ->
         'c @ [< global many read_write > aliased stateful dynamic] ->
         int option @ [> local aliased dynamic]"
       is not compatible with the type
         "'a ->
         length:('a -> int) @ local ->
         get:('a -> int -> 'b) @ local ->
         compare:('b -> 'c -> int) @ local -> 'c -> int option"
       Type
         "'c @ [< global many read_write > aliased stateful dynamic] ->
         int option @ [> local aliased dynamic]"
       is not compatible with type "'c -> int option"
       The return mode was expected to be "global" but is "local"
|}]
