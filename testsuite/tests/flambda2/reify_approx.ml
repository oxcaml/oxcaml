(* TEST
 reference = "${test_source_directory}/reify_approx.reference";
 flambda2;
 {
   flags = "-Oclassic";
   native;
 }
*)

(* Test of the [%reify_approx] primitive: [reify_approx foo] compiles to a
   constant string literal giving the marshalled
   [Value_approximation.Standalone.t] form of the approximation of [foo]. *)

external reify_approx : 'a = "%reify_approx"

(* Applications of [reify_approx] trigger warning 20 (ignored extra
   argument) since its type is only a variable. *)
[@@@ocaml.warning "-20"]

(* Must be kept in sync with the constructor order of
   [Value_approximation.Standalone.t]. *)
let describe name (s : string) =
  let v : Obj.t = Marshal.from_string s 0 in
  assert (Obj.is_block v);
  let constructor =
    match Obj.tag v with
    | 0 -> "Unknown"
    | 1 -> "Value_symbol"
    | 2 -> "Value_const"
    | 3 -> "Closure_approximation"
    | 4 -> "Block_approximation"
    | _ -> assert false
  in
  Printf.printf "%s: %s\n" name constructor

let pair = (1, 2)

let f x = x + 1

let () =
  describe "constant" (reify_approx 42);
  describe "toplevel pair" (reify_approx pair);
  describe "toplevel function" (reify_approx f);
  let dynamic = Sys.opaque_identity 5 in
  describe "dynamic value" (reify_approx dynamic)
