(* TEST
  include eval;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Regression test: sets of closures that To_cmm itself lifts to static
   allocation (because their environment turns out to be empty at Cmm
   translation time, even though Simplify did not lift them) must be
   recorded as [Code_block] dependencies of the enclosing function.
   The fresh symbol is invented during Cmm translation, so it appears in
   no [free_names_of_params_and_body]; without the extra dependency edge
   the GC would reclaim the static closure block individually while the
   enclosing function's code — which returns its address — is still
   callable.

   The inner closure [g] below follows the shape documented in
   [To_cmm_set_of_closures]: it is not lifted by Simplify (its environment
   mentions [x] and [y]) but To_cmm's extra knowledge about unused closure
   variables enables the lifting ([y] is a known constant, [x] is dead in
   [g]'s body once [y] is known to be [true]).

   We call the enclosing function, drop the result, force major GCs (which
   would sweep the lifted block if it were unreferenced), then call the
   enclosing function again and use the fresh result. Without the
   dependency edge this dereferences a freed block. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let make : (unit -> unit -> int) ref = ref (fun () -> fun () -> 0)

let[@inline never] setup () =
  make
    := Eval.eval
         <[ fun () ->
              let x = Sys.opaque_identity 0 in
              let y = true in
              let g () = if y then 1 else x in
              g ]>

let () =
  report "start";
  setup ();
  (* First use: obtain the (statically-lifted) inner closure and call it. *)
  let g1 = !make () in
  Printf.printf "first: %d\n" (g1 ());
  (* [g1] is dead from here on; only [make] (and hence the enclosing
     function's code) still references the lifted static block. *)
  Gc.compact ();
  Gc.compact ();
  report "after drop + compact (unit held; must NOT unload)";
  (* Second use: the enclosing function returns the same static closure
     block; if it was swept, this crashes or returns garbage. *)
  let g2 = !make () in
  Printf.printf "second: %d\n" (g2 ());
  (* Release everything and confirm the unit unloads. *)
  make := (fun () -> fun () -> 0);
  Gc.compact ();
  report "after release + compact"
