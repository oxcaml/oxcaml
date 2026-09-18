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
             'a @ [< 'm] ->
             (get:('a @ [< past('n) > 'm] ->
                   ('b @ [> 'q | aliased] -> 'c @ [< 'p]) @ [> past('n) | past('o)]) @ [< past('mm4) & past('mm2) & past('o) & past('mm0)] ->
              (lo:'b @ [< 'mm1 & 'q & many uncontended read_write] ->
               (hi:'b @ [< past('mm3) & many uncontended read_write] ->
                (pred:('c @ [> 'p | dynamic] -> bool @ 'mm5) @ 'mm6 ->
                 'b option @ [> 'mm1 | local aliased dynamic]) @ [> close('mm1) | close('q) | close('m) | past('mm3) | past('mm4) | local nonportable stateful]) @ [> close('mm1) | close('q) | close('m) | past('mm2) | local nonportable stateful]) @ [> close('m) | past('mm0) | local stateful]) @ [> close('m) | stateful]
           val find_first_satisfying :
             'a @ [< 'm & 'n & many] ->
             (get:('a @ [> 'n | aliased] ->
                   (int @ [> aliased] -> 'b @ [< 'p]) @ 'o) @ [< past('mm2) & past('q)] ->
              (length:('a @ [> 'm | aliased] -> int @ 'mm0) @ [< past('mm1)] ->
               (pred:('b @ [> 'p | dynamic] -> bool @ 'mm3) @ 'mm4 ->
                int option @ [> local aliased dynamic]) @ [> close('m) | close('n) | past('mm1) | past('mm2) | local stateful]) @ [> close('m) | close('n) | past('q) | local stateful]) @ [> close('m) | close('n) | stateful]
           val find_last_satisfying :
             'a @ [< 'm & 'n & many] ->
             (pred:('b @ [> 'p | dynamic] -> bool @ 'o) @ [< past('mm2) & past('q)] ->
              (get:('a @ [> 'n | aliased] ->
                    (int @ [> aliased] -> 'b @ [< 'p]) @ 'mm0) @ [< past('mm1)] ->
               (length:('a @ [> 'm | aliased] ->
                        int @ [< many uncontended read_write]) @ [< many] ->
                int option @ [> local dynamic]) @ [> close('m) | close('n) | past('mm1) | past('mm2) | local stateful]) @ [> close('m) | close('n) | past('q) | local stateful]) @ [> close('m) | close('n) | stateful]
           val binary_search :
             'a @ [< past('mm0) & 'm & 'n & many] ->
             (length:('a @ [> 'n | aliased] ->
                      int @ [< many uncontended read_write]) @ [< past('mm7) & past('mm3) & past('o) & many > local] ->
              (get:('a @ [< past('p) > 'm | aliased] ->
                    (int @ [> aliased] -> 'b @ [< 'mm1]) @ [> past('p) | past('q) | past('mm0) | local]) @ [< past('mm6) & past('q) & past('mm2) & many > local] ->
               (compare:('b @ [> 'mm1 | dynamic] ->
                         'c @ [> 'mm4 | aliased] ->
                         int @ [< many uncontended read_write]) @ [< past('mm5) & many > local] ->
                ('c @ [< 'mm4 & many] ->
                 int option @ [> local aliased dynamic]) @ [> close('m) | close('n) | past('mm5) | past('mm6) | past('mm7) | local stateful]) @ [> close('m) | close('n) | past('mm2) | past('mm3) | local stateful]) @ [> close('m) | close('n) | past('o) | local stateful]) @ [> close('m) | close('n) | stateful]
         end
       is not included in
         S
       Values do not match:
         val binary_search :
           'a @ [< past('mm0) & 'm & 'n & many] ->
           (length:('a @ [> 'n | aliased] ->
                    int @ [< many uncontended read_write]) @ [< past('mm7) & past('mm3) & past('o) & many > local] ->
            (get:('a @ [< past('p) > 'm | aliased] ->
                  (int @ [> aliased] -> 'b @ [< 'mm1]) @ [> past('p) | past('q) | past('mm0) | local]) @ [< past('mm6) & past('q) & past('mm2) & many > local] ->
             (compare:('b @ [> 'mm1 | dynamic] ->
                       'c @ [> 'mm4 | aliased] ->
                       int @ [< many uncontended read_write]) @ [< past('mm5) & many > local] ->
              ('c @ [< 'mm4 & many] -> int option @ [> local aliased dynamic]) @ [> close('m) | close('n) | past('mm5) | past('mm6) | past('mm7) | local stateful]) @ [> close('m) | close('n) | past('mm2) | past('mm3) | local stateful]) @ [> close('m) | close('n) | past('o) | local stateful]) @ [> close('m) | close('n) | stateful]
       is not included in
         val binary_search :
           't ->
           length:('t -> int) @ local ->
           get:('t -> int -> 'elt) @ local ->
           compare:('elt -> 'key -> int) @ local -> 'key -> int option
       The type
         "'a @ [< 'n & 'm & global many uncontended forkable unyielding read_write] ->
         length:('a @ [< global many uncontended forkable unyielding read_write > 'm | aliased] ->
                 int @ [< many uncontended read_write > dynamic]) @ [< many > local] ->
         get:('a @ [< global many uncontended forkable unyielding read_write > 'n | aliased] ->
              (int @ [> aliased] ->
               'b @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic]) @ [> local nonportable unforkable yielding stateful dynamic]) @ [< many > local] ->
         compare:('b @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic] ->
                  ('c @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic] ->
                   int @ [< many uncontended read_write > dynamic]) @ [> local nonportable unforkable yielding stateful dynamic]) @ [< many > local] ->
         'c @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic] ->
         int option @ [> local aliased dynamic]"
       is not compatible with the type
         "'a ->
         length:('a -> int) @ local ->
         get:('a -> int -> 'b) @ local ->
         compare:('b -> 'c -> int) @ local -> 'c -> int option"
       Type
         "'c @ [< global many uncontended forkable unyielding read_write > aliased nonportable stateful dynamic] ->
         int option @ [> local aliased dynamic]"
       is not compatible with type "'c -> int option"
       The return mode was expected to be "global" but is "local"
|}]
