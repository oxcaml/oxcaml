(* TEST
 flags = "-djsir -nopervasives";
 jsir;
*)

(* Checks the JSIR produced by Flambda 2 for accesses to arrays whose kind is
   not known statically. Flambda 2 tests whether such an array is a flat float
   array, and then accesses it as an array of naked floats or of values.

   Under js_of_ocaml both accesses are the same, as a flat float array is an
   ordinary array of numbers, so the test is the constant [false], which
   js_of_ocaml removes together with the access to the float array. Under
   wasm_of_ocaml the test is [%is_float_array], a type test that is inlined
   (instead of a call to [caml_obj_tag]). *)

external ( = ) : 'a -> 'a -> bool = "%equal"
external opaque : 'a -> 'a = "%opaque"
external make : int -> 'a -> 'a array = "caml_make_vect"
external unsafe_get : 'a array -> int -> 'a = "%array_unsafe_get"
external unsafe_set : 'a array -> int -> 'a -> unit = "%array_unsafe_set"

let[@inline never] get (a : 'a array) i = unsafe_get a i

let[@inline never] set (a : 'a array) i (x : 'a) = unsafe_set a i x

let[@inline never] check (a : 'a array) (x : 'a) (y : 'a) =
  assert (get a 0 = x);
  assert (get a 1 = x);
  set a 1 y;
  assert (get a 0 = x);
  assert (get a 1 = y)

let () =
  let literal : float array = opaque [| 1.5; 1.5 |] in
  check literal 1.5 2.5;
  assert (unsafe_get literal 1 = 2.5);
  let made : float array = opaque (make 2 1.5) in
  check made 1.5 2.5;
  assert (unsafe_get made 1 = 2.5);
  check (opaque [| "a"; "a" |]) "a" "b";
  check (opaque [| 1; 1 |]) 1 2
