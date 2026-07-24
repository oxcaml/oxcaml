(* TEST
  include eval;
  include stdlib_upstream_compatible;
  flags = "-extension runtime_metaprogramming";
  no-address-sanitizer;
  { native; }
*)

#syntax quotations on

(* Unboxed numerics ([float#], [int64#], [int32#]) captured by an
   eval'd closure are stored INLINE in the closure's non-scannable
   prefix (between the function slot and the scannable env). The
   slot-size check in [caml_darken_unloadable_code_blocks_in_closure]
   must use the closinfo arity to determine the function slot size,
   not "look for an infix header at slot+2/slot+3" — the non-scannable
   words sitting at those offsets are not infix headers and could even
   happen to alias the [Infix_tag] byte. *)

let report label =
  let r = Eval.unloadable_units_registered_total () in
  let u = Eval.unloadable_units_unloaded_total () in
  Printf.printf "%s: registered=%d unloaded=%d live=%d\n"
    label r u (r - u)

let f_float = ref (fun n -> float_of_int n)
let f_i64 = ref (fun n -> Int64.of_int n)
let f_mixed = ref (fun n -> float_of_int n)

let[@inline never] populate () =
  (* Single [float#] capture: 1 non-scannable word in closure prefix. *)
  f_float := Eval.eval <[
    let scale = Stdlib_upstream_compatible.Float_u.of_float 2.5 in
    fun n ->
      Stdlib_upstream_compatible.Float_u.to_float
        (Stdlib_upstream_compatible.Float_u.mul
           (Stdlib_upstream_compatible.Float_u.of_float (float_of_int n))
           scale)
  ]>;
  (* [int64#] capture. *)
  f_i64 := Eval.eval <[
    let bias = Stdlib_upstream_compatible.Int64_u.of_int64 1000L in
    fun n ->
      Stdlib_upstream_compatible.Int64_u.to_int64
        (Stdlib_upstream_compatible.Int64_u.add bias
           (Stdlib_upstream_compatible.Int64_u.of_int64
              (Int64.of_int n)))
  ]>;
  (* Mix of unboxed-float + boxed-int + scannable string: closure env
     has non-scannable words AND scannable words. *)
  f_mixed := Eval.eval <[
    let scale = Stdlib_upstream_compatible.Float_u.of_float 1.5 in
    let offset = 17 in
    let label = "mixed" in
    fun n ->
      let _ = label in
      Stdlib_upstream_compatible.Float_u.to_float
        (Stdlib_upstream_compatible.Float_u.add scale
           (Stdlib_upstream_compatible.Float_u.of_float
              (float_of_int (n + offset))))
  ]>

let () =
  report "start";
  populate ();
  Printf.printf "f_float 4 = %.2f\n" (!f_float 4);
  Printf.printf "f_i64 5 = %Ld\n" (!f_i64 5);
  Printf.printf "f_mixed 3 = %.2f\n" (!f_mixed 3);
  report "after populate";

  Gc.compact ();
  Gc.compact ();
  Printf.printf "stable: f_float 8 = %.2f, f_i64 9 = %Ld, f_mixed 11 = %.2f\n"
    (!f_float 8) (!f_i64 9) (!f_mixed 11);
  report "after 2x Gc.compact (held)";

  f_float := (fun n -> float_of_int n);
  f_i64 := (fun n -> Int64.of_int n);
  f_mixed := (fun n -> float_of_int n);
  Gc.compact ();
  report "after release"
