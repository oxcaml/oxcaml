(* The counters of the cases of switches compiled by
   [Cmm_helpers.transl_switch_clambda]. Each switch has a counter per case:
   those of the cases that keep an edge must be on it, and those of the cases
   that lose it (because the switch, or a cluster of it, became a table lookup)
   must be on the edge into the lookup, or returned to be put on the edges into
   the switch. No counter may be lost. *)

let () =
  Env.set_current_unit
    (Unit_info.make ~source_file:"cmm_switch_counters.ml"
       ~for_pack_prefix:Compilation_unit.Prefix.empty Impl "cmm_switch_counters")

let function_ =
  Fdo_counter.Function.create_original
    (Fdo_counter.Name.create ~name_str:"Test.f" ~discriminator:0)

let counter arm =
  Fdo_counter.of_position
    (Fdo_counter.Position.create_branch ~function_
       ~body_hash:(Fdo_counter.Body_hash.of_int32 1l)
       ~index:0 ~arm)

let annots arm =
  Branch_annotations.create ~likelihood:Likelihood.default
    ~fdo_annotation:(Fdo_annotation.singleton (counter arm))

let dbg = Debuginfo.none

let constant n = Cmm.Cconst_int ((2 * n) + 1, dbg)

let exit label args = Cmm.Cexit (Lbl label, args, [])

(* The counters on the edges of [expr]. *)
let rec edge_counters (expr : Cmm.expression) =
  let of_annots annots =
    Fdo_annotation.to_list (Branch_annotations.fdo_annotation annots)
  in
  match expr with
  | Cifthenelse (cond, _, ifso_annots, ifso, _, ifnot_annots, ifnot, _) ->
    List.concat
      [ edge_counters cond;
        of_annots ifso_annots;
        edge_counters ifso;
        of_annots ifnot_annots;
        edge_counters ifnot ]
  | Cswitch (scrutinee, _, cases, _) ->
    edge_counters scrutinee
    @ List.concat_map
        (fun (case, _, annots) -> of_annots annots @ edge_counters case)
        (Array.to_list cases)
  | _ ->
    let counters = ref [] in
    Cmm.iter_shallow (fun e -> counters := !counters @ edge_counters e) expr;
    !counters

let print_counters title counters =
  Format.printf "  @[<hov 2>%s:%a@]@." title
    (Format.pp_print_list
       ~pp_sep:(fun _ () -> ())
       (fun ppf counter ->
         Format.fprintf ppf "@ %s" (Fdo_counter.to_string counter)))
    counters

(* [index.(v)] is the case of the value [v]; case [i] has the counter of arm
   [i]. *)
let test name ~index cases =
  let scrutinee = Cmm.Cvar (Backend_var.create_local "x") in
  let cases = Array.mapi (fun arm case -> case, annots arm) cases in
  let expr, residual =
    Cmm_helpers.transl_switch_clambda dbg scrutinee index cases
  in
  let on_edges = edge_counters expr in
  let residual = Fdo_annotation.to_list residual in
  let lost =
    List.filter
      (fun arm ->
        not
          (List.exists (Fdo_counter.equal (counter arm)) (on_edges @ residual)))
      (List.init (Array.length cases) Fun.id)
  in
  Format.printf "%s:@.%a@." name Printcmm.expression expr;
  print_counters "on edges" on_edges;
  print_counters "returned" residual;
  print_counters "lost" (List.map counter lost);
  Format.printf "@."

let () =
  (* Each case keeps its edge, and its counter. *)
  let exits = Array.init 6 (fun _ -> exit (Lambda.next_raise_count ()) []) in
  test "jump table" ~index:(Array.init 6 Fun.id) exits;
  test "shared cases" ~index:[| 0; 1; 0; 1; 2 |] (Array.sub exits 0 3);
  test "tests"
    ~index:
      (Array.init 20 (fun v -> if v < 10 then 0 else if v < 19 then 1 else 2))
    (Array.sub exits 0 3);
  (* Constants: a table lookup, whose counters are all returned. *)
  test "lookup" ~index:[| 0; 1; 2; 3 |]
    [| constant 1; constant 5; constant 2; constant 8 |];
  (* Constant arguments to the same handler: a table lookup too. *)
  let l = Lambda.next_raise_count () in
  test "lookup of arguments" ~index:[| 0; 1; 2; 3 |]
    [| exit l [constant 1];
       exit l [constant 5];
       exit l [constant 2];
       exit l [constant 8]
    |];
  (* A cluster of constants in a sparse switch: a table lookup, with its
     counters on the edge into it. *)
  let l = Lambda.next_raise_count () in
  test "lookup cluster"
    ~index:
      (Array.init 25 (fun v -> if v < 4 then v else if v < 24 then 4 else 5))
    [| constant 1; constant 5; constant 2; constant 8; exit l []; constant 9 |];
  (* All the values go to the same handler: no edges are left, all the counters
     are returned. *)
  let l = Lambda.next_raise_count () in
  test "single case" ~index:[| 0; 1; 2 |] [| exit l []; exit l []; exit l [] |]
