module SL = Slambda

(* Check if a layout contains splices *)
let rec layout_contains_splices : Lambda.layout -> bool = function
  | Psplicevar _ -> true
  | Ptop | Pbottom | Pvalue _ | Punboxed_float _
  | Punboxed_or_untagged_integer _ | Punboxed_vector _ -> false
  | Punboxed_product layouts -> List.exists layout_contains_splices layouts

(* Check if lambda contains any splices *)
let rec assert_no_splices lam =
  (match lam with
  | Lambda.Lsplice _ ->
      Misc.fatal_error
        "Slambda contains splices but layout_poly extension is disabled"
  | _ -> ());
  Lambda.iter_head_constructor assert_no_splices lam

(* Check that slambda is trivial (contains no splices) *)
let assert_slambda_is_trivial (SL.Quote lam) =
  assert_no_splices lam

let do_eval ({ Lambda.code = slam } as p) =
  if not Language_extension.(is_enabled Layout_poly) then
    assert_slambda_is_trivial slam;
  let SL.Quote lam = slam in
  { p with code = lam }

let eval p = Profile.(record static_eval) do_eval p
