(* TEST
 flambda2;
 no-flat-float-array;
 {
   bytecode;
 }
*)

(* Regression test for [%makearray_dynamic] on float# arrays under
   -no-flat-float-array. float# arrays are flat [Double_array_tag] blocks whose
   accessors read raw doubles, so the dynamic allocation must build the same
   flat block; a non-flat block is misread on element access. *)

external[@layout_poly] makearray_dynamic :
  ('a : any mod separable). int -> 'a -> 'a array = "%makearray_dynamic"

external get : ('a : float64) array -> int -> ('a : float64) = "%array_safe_get"
external set : ('a : float64) array -> int -> ('a : float64) -> unit = "%array_safe_set"
external to_float : ('a : float64) -> (float[@local_opt]) = "%box_float"

let () =
  let a = makearray_dynamic 4 #3.5 in
  Printf.printf "tag %d\n" (Obj.tag (Obj.repr a));
  Printf.printf "init %.2f %.2f\n" (to_float (get a 0)) (to_float (get a 3));
  set a 1 #1.25;
  set a 2 #2.5;
  Printf.printf "set  %.2f %.2f\n" (to_float (get a 1)) (to_float (get a 2))
