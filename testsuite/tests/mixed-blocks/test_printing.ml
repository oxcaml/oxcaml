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

#1.5;;
let float64_bad = -#2.5;;
float64_bad;;
-#0.0;;
[%%expect {|
- : float# = <abstr>
val float64_bad : float# = <abstr>
- : float# = <abstr>
- : float# = <abstr>
|}];;

#1.5s;;
let float32_bad = -#2.5s;;
float32_bad;;
-#0.0s;;
[%%expect {|
- : float32_u = <abstr>
val float32_bad : float32_u = <abstr>
- : float32_u = <abstr>
- : float32_u = <abstr>
|}];;

#127s;;
let int8_bad = -#128s;;
int8_bad;;
[%%expect {|
- : int8# = <abstr>
val int8_bad : int8# = <abstr>
- : int8# = <abstr>
|}];;

#32767S;;
let int16_bad = -#32768S;;
int16_bad;;
[%%expect {|
- : int16# = <abstr>
val int16_bad : int16# = <abstr>
- : int16# = <abstr>
|}];;

#2147483647l;;
let int32_bad = -#2147483648l;;
int32_bad;;
[%%expect {|
- : int32_u = <abstr>
val int32_bad : int32_u = <abstr>
- : int32_u = <abstr>
|}];;

#9223372036854775807L;;
let int64_bad = -#9223372036854775808L;;
int64_bad;;
[%%expect {|
- : int64_u = <abstr>
val int64_bad : int64_u = <abstr>
- : int64_u = <abstr>
|}];;

#1048577n;;
let nativeint_bad = -#1048577n;;
nativeint_bad;;
[%%expect {|
- : nativeint_u = <abstr>
val nativeint_bad : nativeint_u = <abstr>
- : nativeint_u = <abstr>
|}];;

#1048577m;;
let int_bad = -#1048577m;;
int_bad;;
[%%expect {|
- : int# = <abstr>
val int_bad : int# = <abstr>
- : int# = <abstr>
|}];;

#();;
let unit_bad = #();;
unit_bad;;
[%%expect {|
- : unit# = <abstr>
val unit_bad : unit# = <abstr>
- : unit# = <abstr>
|}];;

type float_alias = float#
type int_alias = int64_u
type unit_alias = unit#

let float_alias_bad : float_alias = #3.5
let int_alias_bad : int_alias = #9007199254740993L
let unit_alias_bad : unit_alias = #();;

float_alias_bad;;
int_alias_bad;;
unit_alias_bad;;
[%%expect {|
type float_alias = float#
type int_alias = int64_u
type unit_alias = unit#
val float_alias_bad : float_alias = <abstr>
val int_alias_bad : int_alias = <abstr>
val unit_alias_bad : unit_alias = <abstr>
- : float_alias = <abstr>
- : int_alias = <abstr>
- : unit_alias = <abstr>
|}];;

type scalars = {
  f64 : float#;
  before : string;
  u : unit#;
  i8 : int8#;
  i16 : int16#;
  i32 : int32_u;
  f32 : float32_u;
  i64 : int64_u;
  word : nativeint_u;
  i : int#;
  after : string;
}

let scalars_bad = {
  f64 = -#1.5;
  before = "before";
  u = #();
  i8 = -#128s;
  i16 = -#32768S;
  i32 = -#2147483648l;
  f32 = -#2.5s;
  i64 = #9007199254740993L;
  word = -#1048577n;
  i = -#1048577m;
  after = "after";
}
[%%expect {|
type scalars = {
  f64 : float#;
  before : string;
  u : unit#;
  i8 : int8#;
  i16 : int16#;
  i32 : int32_u;
  f32 : float32_u;
  i64 : int64_u;
  word : nativeint_u;
  i : int#;
  after : string;
}
val scalars_bad : scalars =
  {f64 = <abstr>; before = "before"; u = <void>; i8 = <abstr>; i16 = <abstr>;
   i32 = <abstr>; f32 = <abstr>; i64 = <abstr>; word = <abstr>; i = <abstr>;
   after = "after"}
|}];;

type scalar_args =
  | Scalars of int8# * string * int16# * int32_u * float32_u * int# * unit#

let scalar_args_bad =
  Scalars (-#128s, "middle", -#32768S, -#2147483648l, -#2.5s, -#1048577m, #())
[%%expect {|
type scalar_args =
    Scalars of int8# * string * int16# * int32_u * float32_u * int# * unit#
val scalar_args_bad : scalar_args =
  Scalars (<abstr>, "middle", <abstr>, <abstr>, <abstr>, <abstr>, <void>)
|}];;

type unit_args =
  | Unit of unit# [@immediate_all_void_constructor]

let unit_arg_bad = Unit #()
[%%expect {|
type unit_args = Unit of unit# [@immediate_all_void_constructor]
val unit_arg_bad : unit_args = Unit <void>
|}];;

type inherited_float = { inherit f64 : float# }
let inherited_float_bad = { f64 = -#3.5 }
[%%expect {||}];;

type inherited_int32 = { inherit i32 : int32_u }
let inherited_int32_bad = { i32 = -#2147483648l }
[%%expect {||}];;

type inherited_float32 = { inherit f32 : float32_u }
let inherited_float32_bad = { f32 = -#2.5s }
[%%expect {||}];;

type inherited_unit = { inherit u : unit# }
let inherited_unit_bad = { u = #() }
[%%expect {||}];;

type inherited_string = { inherit s : string }
let inherited_string_bad = { s = "inherited" }
[%%expect {||}];;

type addressable_float = #{ af : float# }
type inherited_addressable = { inherit addressed : addressable_float }
let inherited_addressable_bad = { addressed = #{ af = #5.25 } }
[%%expect {||}];;

type inherited_last = { before : string; inherit last : float# }
let inherited_last_bad = { before = "before"; last = -#6.5 }
[%%expect {||}];;

type addressable_unit = #{ au : unit# }
type addressable_unit_arg =
  | Addressable_unit of addressable_unit [@immediate_all_void_constructor]
let addressable_unit_arg_bad = Addressable_unit #{ au = #() }
[%%expect {||}];;
