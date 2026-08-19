(* TEST
 flags = "-extension layouts_beta";
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* A never-returning local function with a layout-any result that survives
   to Simplif.simplify_local_functions with several uses becomes a static
   catch.  The catch kind must come from the apply sites (the scope's
   layout); taking the function's Pbottom result instead would die in
   closure conversion with "Cannot convert bottom to
   Flambda_arity.Component_for_creation". *)

module F = Stdlib_upstream_compatible.Float_u

let value_context x =
  let fail : type (a : any). string -> a = fun _ -> assert false in
  begin match x with
  | 0 -> fail "a"
  | 1 -> ()
  | _ -> fail "b"
  end;
  x + 1

let float64_context x =
  let fail : type (a : any). string -> a = fun _ -> assert false in
  let r : float# =
    match x with
    | 0 -> fail "a"
    | 1 -> #1.5
    | _ -> fail "b"
  in
  F.add r #1.0

let () =
  assert (value_context 1 = 2);
  (match value_context 0 with
   | exception Assert_failure _ -> ()
   | _ -> assert false);
  assert (F.to_float (float64_context 1) = 2.5);
  (match float64_context 3 with
   | exception Assert_failure _ -> ()
   | _ -> assert false);
  ()
