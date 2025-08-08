(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

(* binary type constructors *)
type ('a, 'b) t =
  | A
  | B of 'a
[@@option_like]

let f (?(x = 4) : (int, float) t) () = x
let v = f()
[%%expect {|
type ('a, 'b) t = A | B of 'a
Line 6, characters 6-33:
6 | let f (?(x = 4) : (int, float) t) () = x
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of int t is any
         because the .cmi file for t is missing.
       But the layout of int t must be a sublayout of value
         because it has to be value for the V1 safety check.
       No .cmi file found containing t.
|}]

(* ternary type constructors *)
type ('a, 'b, 'c) t =
  | A
  | B of 'a
[@@option_like]

let f (?(x = 4) : (int, float, string) t) () = x
let v = f()
[%%expect {|
type ('a, 'b, 'c) t = A | B of 'a
Line 6, characters 6-41:
6 | let f (?(x = 4) : (int, float, string) t) () = x
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of int t is any
         because the .cmi file for t is missing.
       But the layout of int t must be a sublayout of value
         because it has to be value for the V1 safety check.
       No .cmi file found containing t.
|}]

(* with constant *)

type t =
  | A
  | B of int
[@@option_like]
let f (?(x = 4) : t) () = x
let v = f()
[%%expect {|
type t = A | B of int
val f : (?x):t -> unit -> int = <fun>
val v : int = 4
|}]

(* binary type constructors *)
type ('a, 'b) t =
  | A
  | B of int
[@@option_like]

let f (?(x = 4) : (int, float) t) () = x
let v = f()
[%%expect {|
type ('a, 'b) t = A | B of int
val f : (?x):('a, 'b) t -> unit -> int = <fun>
val v : int = 4
|}]

(* ternary type constructors *)
type ('a, 'b, 'c) t =
  | A
  | B of int
[@@option_like]

let f (?(x = 4) : (int, float, string) t) () = x
let v = f()
[%%expect {|
type ('a, 'b, 'c) t = A | B of int
val f : (?x):('a, 'b, 'c) t -> unit -> int = <fun>
val v : int = 4
|}]

(* With tuples *)

(* binary type constructors *)
type ('a, 'b) t =
  | A
  | B of ('a * 'b)
[@@option_like]

let f (?(x = 4, 4.2) : (int, float) t) () = x
let v = f()
[%%expect {|
type ('a, 'b) t = A | B of ('a * 'b)
Line 6, characters 6-38:
6 | let f (?(x = 4, 4.2) : (int, float) t) () = x
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of (int * float) t is any
         because the .cmi file for t is missing.
       But the layout of (int * float) t must be a sublayout of value
         because it has to be value for the V1 safety check.
       No .cmi file found containing t.
|}]

(* ternary type constructors *)
type ('a, 'b, 'c) t =
  | A
  | B of ('a * 'b * 'c)
[@@option_like]

let f (?(x = 4, 4.2, "4.2") : (int, float, string) t) () = x
let v = f()
[%%expect {|
type ('a, 'b, 'c) t = A | B of ('a * 'b * 'c)
Line 6, characters 6-53:
6 | let f (?(x = 4, 4.2, "4.2") : (int, float, string) t) () = x
          ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Non-value detected in [value_kind].
       Please report this error to the Jane Street compilers team.
       The layout of (int * float * string) t is any
         because the .cmi file for t is missing.
       But the layout of (int * float * string) t must be a sublayout of
           value
         because it has to be value for the V1 safety check.
       No .cmi file found containing t.
|}]

(* with constant *)

type t =
  | A
  | B of (int * string)
[@@option_like]
let f (?(x = 4, "4") : t) () = x
let v = f()
[%%expect {|
type t = A | B of (int * string)
val f : (?x):t -> unit -> int * string = <fun>
val v : int * string = (4, "4")
|}]

(* binary type constructors *)
type ('a, 'b) t =
  | A
  | B of (int * float)
[@@option_like]

let f (?(x = 4, 4.2) : (int, float) t) () = x
let v = f()
[%%expect {|
type ('a, 'b) t = A | B of (int * float)
val f : (?x):('a, 'b) t -> unit -> int * float = <fun>
val v : int * float = (4, 4.2)
|}]

(* ternary type constructors *)
type ('a, 'b, 'c) t =
  | A
  | B of (unit * string)
[@@option_like]

let f (?(x = (), "x") : (int, float, string) t) () = x
let v = f()
[%%expect {|
type ('a, 'b, 'c) t = A | B of (unit * string)
val f : (?x):('a, 'b, 'c) t -> unit -> unit * string = <fun>
val v : unit * string = ((), "x")
|}]
