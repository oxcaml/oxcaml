(* TEST
   flags = "-extension layouts_alpha";
   expect;
*)

(* Stage-4a regression (rev_ik1's probe): exercises the ikind_carrier
   Param-RELABEL rename path into carrier validation. Parametric with-bound
   decls instantiated at concrete types (int/string/bool) via functor/alias use
   produce non-identity Param renames that reach [Ikind.validate_carrier] under
   OXCAML_IKINDS_VALIDATE=1 -- coverage the pre-existing ikinds corpus lacked
   (its renames are identity or never reach validation). The flag-on carrier
   self-test (unseeded 0 HARD; wrong-rename seed -> HARD) is documented in
   STAGE4A-DESIGN.md. This expect test only pins the shape in the suite. *)

type ('a : immutable_data) box : immutable_data with 'a = { v : 'a }
type 'a pair : immutable_data with 'a = { l : 'a; r : 'a }
let f (x : int box) = x
let g (y : string pair) = y
module M = struct type 'a t : immutable_data with 'a = A of 'a | B end
type mi = int M.t
type ms = string M.t
let h (z : bool M.t) = z
[%%expect{|
type ('a : immutable_data) box = { v : 'a; }
type 'a pair = { l : 'a; r : 'a; }
val f : int box -> int box = <fun>
val g : string pair -> string pair = <fun>
module M : sig type 'a t = A of 'a | B end
type mi = int M.t
type ms = string M.t
val h : bool M.t -> bool M.t = <fun>
|}]
