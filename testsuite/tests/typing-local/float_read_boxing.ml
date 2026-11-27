(* TEST
   flags = "-dlambda -dcanonical-ids";
   expect;
*)

(* Reading a float from a flat float array or record allocates a box. The box
   must not be on the local stack if the read value can be used beyond its
   region. *)

type 'a g = { g : 'a @@ global } [@@unboxed]

external get :
  ('a local_array[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
external iget :
  ('a iarray[@local_opt]) -> int -> ('a[@local_opt]) = "%array_safe_get"
[%%expect {|
0
type 'a g = { g : 'a @@ global; } [@@unboxed]
0
external get : ('a local_array [@local_opt]) -> int -> ('a [@local_opt])
  = "%array_safe_get"
0
external iget : ('a iarray [@local_opt]) -> int -> ('a [@local_opt])
  = "%array_safe_get"
|}]

(* Plain floats are boxed at the mode of the read. *)
let local_float (a : float local_array @ local) = exclave_ get a 0
[%%expect {|
(let
  (local_float/0 =
     (function {nlocal = 1} a/0[L][value<floatarray>] : stackfloat
       (array.get[float(local) indexed by int] a/0 0)))
  (apply (field_imm 1 (global Toploop!)) "local_float" local_float/0))
val local_float : float local_array @ local -> float @ local = <fun>
|}]

let local_float_iarray (a : float iarray @ local) = exclave_ iget a 0
[%%expect {|
(let
  (local_float_iarray/0 =
     (function {nlocal = 1} a/1[L][value<floatarray>] : stackfloat
       (iarray.get[float(local) indexed by int] a/1 0)))
  (apply (field_imm 1 (global Toploop!)) "local_float_iarray"
    local_float_iarray/0))
val local_float_iarray : float iarray @ local -> float @ local = <fun>
|}]

(* Element types that cross locality are boxed on the heap. *)
let crossing_float (a : float g local_array @ local) = (get a 0).g
[%%expect {|
(let
  (crossing_float/0 =
     (function {nlocal = 1} a/2[L][value<floatarray>] : float
       (array.get[float indexed by int] a/2 0)))
  (apply (field_imm 1 (global Toploop!)) "crossing_float" crossing_float/0))
val crossing_float : float g local_array @ local -> float = <fun>
|}]

let crossing_float_iarray (a : float g iarray @ local) = (iget a 0).g
[%%expect {|
(let
  (crossing_float_iarray/0 =
     (function {nlocal = 1} a/3[L][value<floatarray>] : float
       (iarray.get[float indexed by int] a/3 0)))
  (apply (field_imm 1 (global Toploop!)) "crossing_float_iarray"
    crossing_float_iarray/0))
val crossing_float_iarray : float g iarray @ local -> float = <fun>
|}]

(* Generic local reads are boxed on the heap, since the element type might
   cross locality. *)
let generic (a : 'a local_array @ local) = exclave_ get a 0
[%%expect {|
(let
  (generic/0 =
     (function {nlocal = 1} a/4[L][value<genarray>] : stack
       (array.get[gen indexed by int] a/4 0)))
  (apply (field_imm 1 (global Toploop!)) "generic" generic/0))
val generic : 'a local_array @ local -> 'a @ local = <fun>
|}]

let generic_iarray (a : 'a iarray @ local) = exclave_ iget a 0
[%%expect {|
(let
  (generic_iarray/0 =
     (function {nlocal = 1} a/5[L][value<genarray>] : stack
       (iarray.get[gen indexed by int] a/5 0)))
  (apply (field_imm 1 (global Toploop!)) "generic_iarray" generic_iarray/0))
val generic_iarray : 'a iarray @ local -> 'a @ local = <fun>
|}]

(* The same applies to flat float records. *)
type r1 = { x : float; y : float }
type r2 = { a : float g; b : float }
[%%expect {|
0
type r1 = { x : float; y : float; }
0
type r2 = { a : float g; b : float; }
|}]

let local_field (r : r1 @ local) = exclave_ r.x
[%%expect {|
(let
  (local_field/0 =
     (function {nlocal = 1}
       r/0[L][value<
               (consts ()) (non_consts ([254: value<float>, value<float>]))>]
       : stackfloat (floatfieldlocal 0 r/0)))
  (apply (field_imm 1 (global Toploop!)) "local_field" local_field/0))
val local_field : r1 @ local -> float @ local = <fun>
|}]

let crossing_field (r : r2 @ local) = r.a.g
[%%expect {|
(let
  (crossing_field/0 =
     (function {nlocal = 1}
       r/1[L][value<
               (consts ()) (non_consts ([254: value<float>, value<float>]))>]
       : float (floatfield 0 r/1)))
  (apply (field_imm 1 (global Toploop!)) "crossing_field" crossing_field/0))
val crossing_field : r2 @ local -> float = <fun>
|}]
