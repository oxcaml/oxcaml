(* TEST
 expect;
*)

module M : sig
  val x : bool * int
end = struct
  let x = false , "not an int"
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let x = false , "not an int"
5 | end
Error: Signature mismatch:
       The value "x" does not match its expected declaration
       The type "... * string" is not compatible with the type "... * int"
       Type "string" is not compatible with type "int"
|}]

module T : sig
  val f : int -> (float * string option) list
end = struct
  let f x = x + List.length [0.0, Some true]
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f x = x + List.length [0.0, Some true]
5 | end
Error: Signature mismatch:
       The value "f" does not match its expected declaration
       The type "... -> int" is not compatible with the type
         "... -> (float * string option) list"
       Type "int" is not compatible with type "(float * string option) list"
|}]

(* Alpha-equivalence *)
module T : sig
  val f : ('a list * 'b list -> int)
end = struct
  let f : ('c list * 'd option  -> int) = assert false
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   let f : ('c list * 'd option  -> int) = assert false
5 | end
Error: Signature mismatch:
       The value "f" does not match its expected declaration
       The type "... * 'a option -> ..." is not compatible with the type
         "... * 'b list -> ..."
       Type "'a option" is not compatible with type "'b list"
|}]

module T : sig
  type t = int * float
end = struct
  type t = bool * float
end
[%%expect{|
Lines 3-5, characters 6-3:
3 | ......struct
4 |   type t = bool * float
5 | end
Error: Signature mismatch:
       Type declarations do not match:
         type t = bool * float
       is not included in
         type t = int * float
       The type "bool * float" is not equal to the type "int * float"
       Type "bool" is not equal to type "int"
|}]
