(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Regression test: a symbol-valued applied argument of a direct partial
   application is baked into the stub's full application (deliberately
   not stored in a value slot of the wrapper closure), so nothing but
   the stub's machine code references it. In an unloadable compilation
   unit the argument's static block must therefore be kept alive through
   the stub's [Code_block] dependencies, which come from the code's
   recorded free names ([Code.free_names_of_params_and_body], recomputed
   when the manufactured stub is re-simplified; see
   [Simplify_apply_expr.make_partial_application]).

   Unlike [unload_partial_app_static_callee] — where the callee closure
   would be passed to a function with an empty environment and never
   read, so its reclamation would go unnoticed — this test's completion
   *reads through* the applied argument: [r] below is a constant pair,
   so it is lifted to a static block and applied by symbol, and
   [Sys.opaque_identity] stops Simplify from constant-folding the
   projection. If the dependency chain is ever broken, the sweeps below
   free [r]'s block in place (its first field becomes the free-block
   size word) and the completion returns garbage instead of
   [fst r + b]. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let partial : (int -> int) ref = ref (fun _ -> 0)

let[@inline never] setup () =
  partial
    := Eval.eval
         <[ let r = (42, 58) in
            let f p b = fst (Sys.opaque_identity p) + b in
            f r ]>

let () =
  report "start";
  setup ();
  (* Only the partial application survives; the static pair [r] is
     referenced solely from the stub's code. *)
  Gc.compact ();
  Gc.compact ();
  report "after compact (partial held; unit must NOT unload)";
  (* Completing the application runs the stub's full application, which
     reads field 0 of [r]'s static block. *)
  Printf.printf "completed: %d\n" (!partial 3);
  (* Release and confirm the unit unloads. *)
  partial := (fun _ -> 0);
  Gc.compact ();
  report "after release + compact"
