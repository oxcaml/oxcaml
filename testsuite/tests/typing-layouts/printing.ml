(* TEST
 flags = "-verbose-types";
 expect;
*)

module M : sig
  val f : int -> bool -> char
end = struct
  let f () _ = ()
end

[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f () _ = ()
5 | end
Error: Signature mismatch:
       The value "f" does not match its expected declaration
       The type "unit -> ..." is not compatible with the type "int -> ..."
       Type "unit" is not compatible with type "int"
|}]
