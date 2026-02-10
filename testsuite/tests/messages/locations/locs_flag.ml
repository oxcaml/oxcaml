(* TEST
 flags = "";
 {
   compiler_reference = "${test_source_directory}/no_locs.reference";
   flags = "-no-locs";
   toplevel;
 }{
   compiler_reference = "${test_source_directory}/locs.reference";
   flags = "-locs";
   toplevel;
 }
*)

(* Environment error *)
let _ = x;;

(* Type error *)
let _ = 1 + "abc";;

(* Multi-line error *)
let _ = (
  "abc"
  ^ (
    1
    +
    2
  )
);;

(* Missing location information -- from [typing-misc/pr6416.ml] *)
(* This test may break in the future if we track the location better *)
module rec M: sig type t type a = M.t end  =
struct type t module M = struct type t end type a = M.t end;;
