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
         "'a @ [< past('mm44) & past('mm25) & 'm & 'n & global many read_write] ->
         (length:('a @ [< past('mm5) & past('mm6) & past('mm7) & past('mm8) & past('mm9) & past('mm2) & global many read_write > 'n | aliased] ->
                  int @ [< past('mm3) & past('mm4) & many read_write > dynamic]) @ [< past('mm43) & past('mm50) & past('mm24) & past('mm36) & past('mm15) & many > local] ->
          (get:('a @ [< past('mm29) & past('mm30) & past('mm22) & past('mm31) & past('mm14) & past('mm1) & global many read_write > 'm | aliased] ->
                (int @ [> aliased] ->
                 'b @ [< past('mm26) & past('mm27) & past('mm21) & past('mm28) & past('mm13) & past('mm0) & global many read_write > aliased stateful dynamic]) @ [> past('mm16) | past('mm17) | past('mm18) | past('mm19) | past('mm20) | past('mm21) | past('mm22) | past('mm23) | past('mm4) | past('mm7) | past('mm24) | past('mm25) | local stateful dynamic]) @ [< past('mm42) & past('mm49) & past('mm23) & past('mm35) & many > local] ->
           (compare:('b @ [< past('mm40) & past('mm46) & past('mm19) & past('mm34) & past('mm12) & past('q) & global many read_write > aliased stateful dynamic] ->
                     ('c @ [< past('mm39) & past('mm45) & past('mm18) & past('mm33) & past('mm11) & past('p) & global many read_write > aliased stateful dynamic] ->
                      int @ [< past('mm38) & past('mm17) & many read_write > dynamic]) @ [> past('mm37) | past('mm38) | past('mm39) | past('mm40) | past('mm41) | past('mm26) | past('mm29) | past('mm42) | past('mm3) | past('mm5) | past('mm43) | past('mm44) | local stateful dynamic]) @ [< past('mm41) & past('mm48) & past('mm20) & many > local] ->
            ('c @ [< past('mm37) & past('mm47) & past('mm16) & past('mm32) & past('mm10) & past('o) & global many read_write > aliased stateful dynamic] ->
             int option @ [> local aliased dynamic]) @ [> close('m) | close('n) | past('mm47) | past('mm45) | past('mm46) | past('mm48) | past('mm27) | past('mm30) | past('mm49) | past('mm6) | past('mm50) | local stateful]) @ [> close('m) | close('n) | past('mm32) | past('mm33) | past('mm34) | past('mm28) | past('mm31) | past('mm35) | past('mm8) | past('mm36) | local stateful]) @ [> close('m) | close('n) | past('mm10) | past('mm11) | past('mm12) | past('mm13) | past('mm14) | past('mm9) | past('mm15) | local stateful]) @ [> close('m) | close('n) | past('o) | past('p) | past('q) | past('mm0) | past('mm1) | past('mm2) | stateful]"
       is not compatible with the type
         "'a ->
         length:('a -> int) @ local ->
         get:('a -> int -> 'b) @ local ->
         compare:('b -> 'c -> int) @ local -> 'c -> int option"
       Type
         "'c @ [< past('mm37) & past('mm47) & past('mm16) & past('mm32) & past('mm10) & past('o) & global many read_write > aliased stateful dynamic] ->
         int option @ [> local aliased dynamic]"
       is not compatible with type "'c -> int option"
       The return mode was expected to be "global" but is "local"
|}]
