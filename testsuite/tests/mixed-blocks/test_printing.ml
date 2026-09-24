(* TEST
 { expect; }
 { expect.opt; }
*)

(* Test printing of values in the bytecode and native toplevels *)

(* All-float *)
type t = { flt : float; uflt : float# } [@@flatten_floats]

let t = { flt = 4.0; uflt = #5.0 }

[%%expect {|
type t = { flt : float; uflt : float#; }
val t : t = {flt = 4.; uflt = <abstr>}
|}];;

(* Non-empty value prefix *)
type t = { uflt : float#; str : string }

let t = { uflt = #5.0; str = "str" }

[%%expect {|
type t = { uflt : float#; str : string; }
val t : t = {uflt = <abstr>; str = "str"}
|}];;

(* Flat suffix mixes float# and imm *)
type t = { str : string; uflt : float#; imm : int }

let t = { str = "str"; uflt = #5.0; imm = 5 }

[%%expect {|
type t = { str : string; uflt : float#; imm : int; }
val t : t = {str = "str"; uflt = <abstr>; imm = 5}
|}];;

(* Boxed float after a flat field *)
type t = { uflt : float#; flt : float; str : string }

let t = { uflt = #5.0; flt = 4.0; str = "str" }

[%%expect {|
type t = { uflt : float#; flt : float; str : string; }
val t : t = {uflt = <abstr>; flt = 4.; str = "str"}
|}];;

(* Several flat fields, some before values *)
type t =
  { i64 : int64_u; str : string; imm : int; f32 : float32_u; str2 : string }

let t = { i64 = #5L; str = "str"; imm = 5; f32 = #4.0s; str2 = "str2" }

[%%expect {|
type t = {
  i64 : int64_u;
  str : string;
  imm : int;
  f32 : float32_u;
  str2 : string;
}
val t : t =
  {i64 = <abstr>; str = "str"; imm = 5; f32 = <abstr>; str2 = "str2"}
|}];;

(* Void field *)
type t = { uflt : float#; void : unit#; str : string }

let t = { uflt = #5.0; void = #(); str = "str" }

[%%expect {|
type t = { uflt : float#; void : unit#; str : string; }
val t : t = {uflt = <abstr>; void = <void>; str = "str"}
|}];;

(* Inline record *)
type t = A of { uflt : float#; str : string } | B

let t = A { uflt = #5.0; str = "str" }

[%%expect {|
type t = A of { uflt : float#; str : string; } | B
val t : t = A {uflt = <abstr>; str = "str"}
|}];;

(* Constructor arguments *)
type t = A of float# * string * int64_u * string | B

let t = A (#5.0, "str", #5L, "str2")

[%%expect {|
type t = A of float# * string * int64_u * string | B
val t : t = A (<abstr>, "str", <abstr>, "str2")
|}];;

(* Void constructor argument *)
type t = A of float# * unit# * string | B

let t = A (#5.0, #(), "str")

[%%expect {|
type t = A of float# * unit# * string | B
val t : t = A (<abstr>, <void>, "str")
|}];;
