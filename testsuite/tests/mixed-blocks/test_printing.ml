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
val t : t = {flt = 4.; uflt = #5.}
|}];;

(* Non-empty value prefix *)
type t = { uflt : float#; str : string }

let t = { uflt = #5.0; str = "str" }

[%%expect {|
type t = { uflt : float#; str : string; }
val t : t = {uflt = #5.; str = "str"}
|}];;

(* Flat suffix mixes float# and imm *)
type t = { str : string; uflt : float#; imm : int }

let t = { str = "str"; uflt = #5.0; imm = 5 }

[%%expect {|
type t = { str : string; uflt : float#; imm : int; }
val t : t = {str = "str"; uflt = #5.; imm = 5}
|}];;

(* Boxed float after a flat field *)
type t = { uflt : float#; flt : float; str : string }

let t = { uflt = #5.0; flt = 4.0; str = "str" }

[%%expect {|
type t = { uflt : float#; flt : float; str : string; }
val t : t = {uflt = #5.; flt = 4.; str = "str"}
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
val t : t = {i64 = #5L; str = "str"; imm = 5; f32 = #4.s; str2 = "str2"}
|}];;

(* Void field *)
type t = { uflt : float#; void : unit#; str : string }

let t = { uflt = #5.0; void = #(); str = "str" }

[%%expect {|
type t = { uflt : float#; void : unit#; str : string; }
val t : t = {uflt = #5.; void = #(); str = "str"}
|}];;

(* Inline record *)
type t = A of { uflt : float#; str : string } | B

let t = A { uflt = #5.0; str = "str" }

[%%expect {|
type t = A of { uflt : float#; str : string; } | B
val t : t = A {uflt = #5.; str = "str"}
|}];;

(* Constructor arguments *)
type t = A of float# * string * int64_u * string | B

let t = A (#5.0, "str", #5L, "str2")

[%%expect {|
type t = A of float# * string * int64_u * string | B
val t : t = A (#5., "str", #5L, "str2")
|}];;

(* Void constructor argument *)
type t = A of float# * unit# * string | B

let t = A (#5.0, #(), "str")

[%%expect {|
type t = A of float# * unit# * string | B
val t : t = A (#5., #(), "str")
|}];;

#1.5;;
let float64 = -#2.5;;
float64;;
-#0.0;;
[%%expect {|
- : float# = #1.5
val float64 : float# = -#2.5
- : float# = -#2.5
- : float# = -#0.
|}];;

#1.5s;;
let float32 = -#2.5s;;
float32;;
-#0.0s;;
[%%expect {|
- : float32_u = #1.5s
val float32 : float32_u = -#2.5s
- : float32_u = -#2.5s
- : float32_u = -#0.s
|}];;

#127s;;
let int8 = -#128s;;
int8;;
[%%expect {|
- : int8# = #127s
val int8 : int8# = -#128s
- : int8# = -#128s
|}];;

#32767S;;
let int16 = -#32768S;;
int16;;
[%%expect {|
- : int16# = #32767S
val int16 : int16# = -#32768S
- : int16# = -#32768S
|}];;

#2147483647l;;
let int32 = -#2147483648l;;
int32;;
[%%expect {|
- : int32_u = #2147483647l
val int32 : int32_u = -#2147483648l
- : int32_u = -#2147483648l
|}];;

#9223372036854775807L;;
let int64 = -#9223372036854775808L;;
int64;;
[%%expect {|
- : int64_u = #9223372036854775807L
val int64 : int64_u = -#9223372036854775808L
- : int64_u = -#9223372036854775808L
|}];;

#1048577n;;
let nativeint = -#1048577n;;
nativeint;;
[%%expect {|
- : nativeint_u = #1048577n
val nativeint : nativeint_u = -#1048577n
- : nativeint_u = -#1048577n
|}];;

#1048577m;;
let int = -#1048577m;;
int;;
[%%expect {|
- : int# = #1048577m
val int : int# = -#1048577m
- : int# = -#1048577m
|}];;

#();;
let unit = #();;
unit;;
[%%expect {|
- : unit# = #()
val unit : unit# = #()
- : unit# = #()
|}];;

type float_alias = float#
type int_alias = int64_u
type unit_alias = unit#

let float_alias : float_alias = #3.5
let int_alias : int_alias = #9007199254740993L
let unit_alias : unit_alias = #();;

float_alias;;
int_alias;;
unit_alias;;
[%%expect {|
type float_alias = float#
type int_alias = int64_u
type unit_alias = unit#
val float_alias : float_alias = #3.5
val int_alias : int_alias = #9007199254740993L
val unit_alias : unit_alias = #()
- : float_alias = #3.5
- : int_alias = #9007199254740993L
- : unit_alias = #()
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

let scalars = {
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
val scalars : scalars =
  {f64 = -#1.5; before = "before"; u = #(); i8 = -#128s; i16 = -#32768S;
   i32 = -#2147483648l; f32 = -#2.5s; i64 = #9007199254740993L;
   word = -#1048577n; i = -#1048577m; after = "after"}
|}];;

type scalar_args =
  | Scalars of int8# * string * int16# * int32_u * float32_u * int# * unit#

let scalar_args =
  Scalars (-#128s, "middle", -#32768S, -#2147483648l, -#2.5s, -#1048577m, #())
[%%expect {|
type scalar_args =
    Scalars of int8# * string * int16# * int32_u * float32_u * int# * unit#
val scalar_args : scalar_args =
  Scalars (-#128s, "middle", -#32768S, -#2147483648l, -#2.5s, -#1048577m,
   #())
|}];;

type unit_args =
  | Unit of unit# [@immediate_all_void_constructor]

let unit_arg = Unit #()
[%%expect {|
type unit_args = Unit of unit# [@immediate_all_void_constructor]
val unit_arg : unit_args = Unit #()
|}];;

type inherited_float = { inherit f64 : float# }
let inherited_float = { f64 = -#3.5 }
[%%expect {|
type inherited_float = { inherit f64 : float#; }
val inherited_float : inherited_float = {f64 = -#3.5}
|}];;

type inherited_int32 = { inherit i32 : int32_u }
let inherited_int32 = { i32 = -#2147483648l }
[%%expect {|
type inherited_int32 = { inherit i32 : int32_u; }
val inherited_int32 : inherited_int32 = {i32 = -#2147483648l}
|}];;

type inherited_float32 = { inherit f32 : float32_u }
let inherited_float32 = { f32 = -#2.5s }
[%%expect {|
type inherited_float32 = { inherit f32 : float32_u; }
val inherited_float32 : inherited_float32 = {f32 = -#2.5s}
|}];;

type inherited_unit = { inherit u : unit# }
let inherited_unit = { u = #() }
[%%expect {|
type inherited_unit = { inherit u : unit#; }
val inherited_unit : inherited_unit = {u = #()}
|}];;

type inherited_string = { inherit s : string }
let inherited_string = { s = "inherited" }
[%%expect {|
type inherited_string = { inherit s : string; }
val inherited_string : inherited_string = {s = "inherited"}
|}];;

type addressable_float = #{ af : float# }
type inherited_addressable = { inherit addressed : addressable_float }
let inherited_addressable = { addressed = #{ af = #5.25 } }
[%%expect {|
type addressable_float = #{ af : float#; }
type inherited_addressable = { inherit addressed : addressable_float; }
val inherited_addressable : inherited_addressable =
  {addressed = #{af = #5.25}}
|}];;

type inherited_last = { before : string; inherit last : float# }
let inherited_last = { before = "before"; last = -#6.5 }
[%%expect {|
type inherited_last = { before : string; inherit last : float#; }
val inherited_last : inherited_last = {before = "before"; last = -#6.5}
|}];;

type addressable_unit = #{ au : unit# }
type addressable_unit_arg =
  | Addressable_unit of addressable_unit [@immediate_all_void_constructor]
let addressable_unit_arg = Addressable_unit #{ au = #() }
[%%expect {|
type addressable_unit = #{ au : unit#; }
type addressable_unit_arg =
    Addressable_unit of addressable_unit [@immediate_all_void_constructor]
val addressable_unit_arg : addressable_unit_arg =
  Addressable_unit #{au = #()}
|}];;

type wrapped_float = Wrapped_float of float# [@@unboxed]
let wrapped_float = Wrapped_float (-#2.5)
[%%expect {|
type wrapped_float = Wrapped_float of float# [@@unboxed]
val wrapped_float : wrapped_float = Wrapped_float (-#2.5)
|}];;

type wrapped_unit = Wrapped_unit of unit# [@@unboxed]
let wrapped_unit = Wrapped_unit #()
[%%expect {|
type wrapped_unit = Wrapped_unit of unit# [@@unboxed]
val wrapped_unit : wrapped_unit = Wrapped_unit #()
|}];;
