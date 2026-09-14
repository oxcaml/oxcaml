(* TEST
 { expect; }{ expect.opt; }
*)

type non_unit_void : void
external non_unit_void : unit -> non_unit_void = "%unbox_unit"
[%%expect{|
type non_unit_void : void
external non_unit_void : unit -> non_unit_void = "%unbox_unit"
|}]

(* Warning non-unit-statement does not occur with unit# *)
let x = #(); 42
[%%expect{|
val x : int = 42
|}]

(* Warning non-unit-statement does occur with arbitrary void types *)
let x = non_unit_void (); 42
[%%expect{|
Line 1, characters 8-24:
1 | let x = non_unit_void (); 42
            ^^^^^^^^^^^^^^^^
Warning 10 [non-unit-statement]: this expression should have type unit.

val x : int = 42
|}]
