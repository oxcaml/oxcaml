(* TEST
 expect;
*)

let r = ref None

module M : sig
  val write : ([< `A of string | `B of int ] -> unit)
end = struct
  let write x =
    match x with `A _ | `B _ -> r := Some x
end
[%%expect{|
val r : '_weak1 option ref = {contents = None}
Lines 5-8, characters 6-3:
5 | ......struct
6 |   let write x =
7 |     match x with `A _ | `B _ -> r := Some x
8 | end
Error: Signature mismatch:
       Values do not match:
         val write : ([< `A of '_weak3 | `B of '_weak4 ] as '_weak2) -> unit
       is not included in
         val write : [< `A of string | `B of int ] -> unit
       The type "([< `A of '_weak3 | `B of '_weak4 ] as '_weak2) -> unit"
       is not compatible with the type "[< `A of string | `B of int ] -> unit"
       Type "[< `A of '_weak3 | `B of '_weak4 ] as '_weak2"
       is not compatible with type "[< `A of string | `B of int ]"
|}]
