(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Stage-5m: the ikind engine is the sole kind checker.  The [enable_*] seam
   toggles (stage-5a) and the [-no-ikinds] legacy fallback (stage-5d/5m) are
   gone; the seams are unconditional. *)

(* Stage-1 validation harness (see STAGE1-DESIGN.md). When
   [Clflags.ikinds_validate] (env var OXCAML_IKINDS_VALIDATE) is set, each kind
   subcheck / crossing query re-derives the ikind twice -- once using the
   stored decl ikinds ([type_declaration.type_ikind]) and once forcing a full
   recompute from each declaration -- and asserts the two agree. This is the
   representation-level "stored ikind agrees with the derived-on-the-fly one".
   It is inert (default builds byte-identical) unless the flag is set. *)
let () =
  match Sys.getenv_opt "OXCAML_IKINDS_VALIDATE" with
  | Some ("1" | "true" | "yes" | "on") -> Clflags.ikinds_validate := true
  | Some _ | None -> ()

(* Env alias for [-print-from-ikinds], so the full expect-test suite can be run
   flag-on through the real [make test-one] runner (which fixes the compiler's
   command-line flags) without a rebuild.  Same idiom as OXCAML_IKINDS_VALIDATE. *)
let () =
  match Sys.getenv_opt "OXCAML_PRINT_FROM_IKINDS" with
  | Some ("1" | "true" | "yes" | "on") -> Clflags.print_from_ikinds := true
  | Some _ | None -> ()

(* When set, [lookup_of_env] ignores any stored [Constructor_ikind] and takes
   the recompute-from-declaration fallback. Used only by the validation harness
   to obtain the "derived on the fly" reference. *)
let force_recompute_ikinds = ref false

(* When set (together with [force_recompute_ikinds]), [lookup_of_env] keeps the
   stored ikind for a recursive-module fixpoint residue (a [Type_abstract
   Definition] decl whose stored ikind carries a foreign [Param]).  Only the
   validation harness sets it, to build the CLASS-B residue-trusting
   reference. *)
let trust_residue_stored = ref false

(* Seeded-fault hook (test/debug only; env [OXCAML_IKIND_RESIDUE_FAULT]).  When
   set, the residue-trusting reference derivation returns a deliberately-wrong
   (top-joined) value for a residue instead of its stored ikind.  This leaves
   the compile path and the stored/coarse derivations untouched -- so the
   SAME divergences are detected -- but makes [recomputed_residue] disagree with
   [stored], demonstrating that (a) every residue-trust decision is still
   COUNTED under [residue_trusted] (the tag fires; the trust boundary is not
   a blind spot), and (b) a residue-trust value that does not match the
   stored fixpoint value is NOT silently classified CLASS-B -- it escalates
   to a HARD mismatch.
   Gated so it can only ever perturb the validation harness -- default off, so
   ordinary builds (flag-on and flag-off) are byte-identical.  See
   STAGE4B-DESIGN.md. *)
let residue_fault = ref false

let () =
  match Sys.getenv_opt "OXCAML_IKIND_RESIDUE_FAULT" with
  | Some ("1" | "true" | "yes" | "on") -> residue_fault := true
  | Some _ | None -> ()

(* Counts how many times the print-from-ikind floor deriver actually fired
   (a with-bounds-free jkind printed under [-print-from-ikinds]).  Evidence the
   seam is exercised corpus-wide. *)
let print_floor_derivations = ref 0

(* Coverage counters for the full-rendering path (with-bounds jkinds under the
   flag): how many were rendered normalized from the ikind vs fell back to the
   legacy renderer because the derivation raised (the genuinely-underivable
   class). *)
let print_withbounds_rendered = ref 0

let print_render_fallbacks = ref 0

(* Seeded-fault hook (test/debug only; env [OXCAML_PRINT_FLOOR_FAULT]).  When
   set, the print-from-ikind floor deriver returns a deliberately-wrong value
   ([top] = crosses everything) instead of the true floor, so the printed
   mod-bounds diverge from the legacy value.  This proves the flag-on
   differential (expect corpus, flag on) can FIRE: with the fault on, at least
   one printed kind must change.  Default off, so flag-on output is otherwise
   byte-identical. *)
let print_floor_fault = ref false

let () =
  match Sys.getenv_opt "OXCAML_PRINT_FLOOR_FAULT" with
  | Some ("1" | "true" | "yes" | "on") -> print_floor_fault := true
  | Some _ | None -> ()

(* Stage-5a re-entrancy probe: global Solver-cache entries a print-path context
   creation EVICTS.  With the pre-fix clearing [create_ctx] this counts the warm
   cache a mid-print [create_ctx] wipes (hazard H1); after the scratch-ctx fix it
   stays 0 (the scratch ctx allocates fresh tables and never touches the
   globals).  Reported in the [-ikinds-debug] at_exit summary. *)
let print_ctx_evicted_entries = ref 0

(* Stage-5a re-entrancy REGRESSION probe (env [OXCAML_IK5A_REENTRANCY_PROBE],
   test/debug only, default off).  On the first [crossing_of_jkind] of a compile
   it fabricates an outer mid-check state (a pending gfp; the seam has already
   warmed the Solver caches), then PRINTS the jkind through the real print funnel
   under [-print-from-ikinds] -- a genuinely nested, mid-check jkind print -- and
   reports whether the outer Solver-cache size and pending-gfp count SURVIVED.
   With the fix (scratch ctx + [with_isolated_pending]) both are untouched
   (evicted=0, drained=0 => OK); a regression to the clearing ctx or an
   un-isolated [solve_pending] would evict/drain (=> CORRUPTION).  The whole
   probe is sandboxed in [with_isolated_pending] so it cannot perturb the live
   check it runs inside, and default-off so ordinary builds are unaffected. *)
let reentrancy_probe = ref false

let () =
  match Sys.getenv_opt "OXCAML_IK5A_REENTRANCY_PROBE" with
  | Some ("1" | "true" | "yes" | "on") -> reentrancy_probe := true
  | Some _ | None -> ()

let reentrancy_probe_done = ref false

(* Stage-4d cross-unit seeded-fault hook (test/debug only; env
   [OXCAML_IKIND_SAVE_FAULT]).  When set, a decl ikind is deliberately corrupted
   (base -> bottom, the tighter/genuine-finding direction) ON THE cmi SAVE PATH
   only.  The defining unit compiles unaffected (the corruption happens while
   preparing the signature for saving, after its own checks); the corruption
   rides into the .cmi, so an importing unit loads a wrong stored ikind while the
   defining unit's legacy jkind fields (from which the importer recomputes the
   reference) stay intact.  Under [OXCAML_IKINDS_VALIDATE] the importer's harness
   then sees stored < recompute and escalates to a HARD mismatch -- demonstrating
   that the cmi boundary is not a validation blind spot.  Default off, so
   ordinary builds (flag-on and flag-off) are byte-identical. *)
let save_fault = ref false

let () =
  match Sys.getenv_opt "OXCAML_IKIND_SAVE_FAULT" with
  | Some ("1" | "true" | "yes" | "on") -> save_fault := true
  | Some _ | None -> ()

(* Stage-4d cross-unit [Param]-id collision DETECTOR (validate-only, read-side
   bookkeeping; no persistence-format or CLASS-B change).  A foreign [Param] in
   a persisted (imported) decl ikind keys off a stale live-[type_expr] id from
   the defining unit; if it numerically collides with a [Param] id the importer
   mints for one of its OWN live type variables (rigids intern by [stable_hash
   name]), [decompose_into_linear_terms] can conflate them and mis-attribute the
   residue's contribution -- a narrow, pre-existing potential unsound accept
   (STAGE4D-DESIGN.md, stage-5 MUST-FIX).  Until the stage-5 residue
   representation removes the stale id, this converts the hazard from silent to
   OBSERVABLE: [imported_foreign_param_ids] records every foreign [Param] id seen
   in an IMPORTED stored decl ikind (recorded at the load/lookup site, BEFORE
   interning, so no post-intern marker is needed to tell persisted-origin ids
   apart), and each live [Param] mint checks membership, counting overlaps.
   Over-approximate (flags numeric overlap even absent a shared [decompose]) --
   acceptable for a detector.  Gated on [ikinds_validate]; ordinary builds pay
   nothing and are byte-identical. *)
let imported_foreign_param_ids : (int, unit) Hashtbl.t = Hashtbl.create 16

let param_id_collisions = ref 0

(* Seeded negative control (env [OXCAML_IKIND_COLLISION_FAULT]): inject a
   synthetic collision by recording each live-minted [Param] id into the
   imported set just before the membership check, so the very next check on that
   id fires.  Proves the counter can increment (a real numeric collision is not
   source-constructible -- [type_expr] id allocation is not controllable).
   Default off. *)
let collision_fault = ref false

let () =
  match Sys.getenv_opt "OXCAML_IKIND_COLLISION_FAULT" with
  | Some ("1" | "true" | "yes" | "on") -> collision_fault := true
  | Some _ | None -> ()

(* Detector check at a live [Param] mint: [id] is a [type_expr] id the current
   unit is turning into a [Param] rigid.  If it numerically matches an id
   recorded from an imported (persisted) decl ikind, count the overlap.  Called
   at EVERY live-param mint, including the [decompose_into_linear_terms] universe
   sites (the actual collision-harm site -- see STAGE4D-DESIGN.md), not just
   [Solver.rigid].  Validate-gated. *)
let check_live_param_id (id : int) : unit =
  if !Clflags.ikinds_validate
  then (
    (* Seeded control ([OXCAML_IKIND_COLLISION_FAULT]) injects a collision by
       recording the id first, so the check then fires. *)
    if !collision_fault then Hashtbl.replace imported_foreign_param_ids id ();
    if Hashtbl.mem imported_foreign_param_ids id then incr param_id_collisions)

let validate_checks = ref 0

let validate_mismatches = ref 0

(* Benign class (stage 3): a divergence where the stored derivation is a
   conservative OVER-approximation of the recompute ([recompute <= stored]). The
   L-jkind carrying with-bounds is always the SUB of a [sub <= super] check
   ([Ldd.leq_with_reason sub super]; see [compute_subcheck_polys] below), so a
   larger stored value can only make the check HARDER -- conservative
   over-rejection, never an unsound acceptance. Divergences in the OTHER
   direction ([stored <= recompute], stored strictly tighter) are NOT
   whitelisted: they stay hard mismatches and are a genuine-soundness-finding
   trigger. Counted and reported separately. *)
let validate_benign = ref 0

(* CLASS-B (stage 4b): a divergence fully explained by recursive-module
   fixpoint-residue trust -- the stored ikind of a [Type_abstract Definition]
   decl carrying a foreign [Param] is TRUSTED rather than independently
   validated, because a from-scratch reference cannot reconstruct the
   declaration-time recursive-module fixpoint (the same trust boundary as
   CLASS-A's fresh-[Tvar] temp decls).  We do NOT hard-fail these, but we COUNT
   and LOG every one so the trust decision stays auditable -- a deliberately
   wrong residue stored ikind still lands here (the tag fires), never silently
   absorbed. *)
let validate_class_b = ref 0

(* Lookup-level audit tag: counts every residue-trust DECISION -- each time the
   residue-trusting reference derivation keeps a residue's stored ikind instead
   of recomputing it.  Fires regardless of whether that stored value is correct,
   so it is the visibility guarantee the trust boundary needs: a wrong residue
   stored ikind (e.g. under [OXCAML_IKIND_RESIDUE_FAULT]) is still counted here,
   never a silent blind spot.  Only ever incremented in the harness (guarded by
   [force_recompute_ikinds] && [trust_residue_stored]). *)
let residue_trusted = ref 0

(* Counts, in the normal (non-forced) path, how many constructor lookups used a
   stored decl ikind vs fell back to recompute. *)
let stored_decl_ikind_hits = ref 0

let recomputed_decl_ikind = ref 0

(* Stage-5b telemetry (validate/debug only): [residue_neutralized] counts foreign
   [Param] residues rewritten to unit-qualified [Residue] on the cmi save path;
   [imported_residues] counts [Residue] atoms seen in imported stored decl ikinds.
   Corroboration for the coexistence-window: residues cross the cmi as [Residue]
   (imported-residues > 0) and [param-id-collisions] stays 0 (no live-[Param] can
   alias a [Residue] -- collision-free by construction). *)
let residue_neutralized = ref 0

let imported_residues = ref 0

let () =
  at_exit (fun () ->
      if !Clflags.ikinds_validate
      then
        Format.eprintf
          "[ikind-validate] summary: checks=%d mismatches=%d benign=%d \
           class_b=%d residue_trusted=%d; decl-ikind stored=%d recomputed=%d; \
           imported-foreign-params=%d param-id-collisions=%d; \
           residue-neutralized=%d imported-residues=%d@."
          !validate_checks !validate_mismatches !validate_benign
          !validate_class_b !residue_trusted !stored_decl_ikind_hits
          !recomputed_decl_ikind
          (Hashtbl.length imported_foreign_param_ids)
          !param_id_collisions !residue_neutralized !imported_residues)

let () =
  at_exit (fun () ->
      (* Gated on [-ikinds-debug] (not the flag itself) so [-print-from-ikinds]
         does not spew to stderr and pollute captured compiler output. *)
      if !Clflags.print_from_ikinds && !Clflags.ikinds_debug
      then
        Format.eprintf
          "[ikind-print] floor derivations=%d with-bounds rendered=%d render \
           fallbacks=%d fault=%b ctx-evicted=%d@."
          !print_floor_derivations !print_withbounds_rendered
          !print_render_fallbacks !print_floor_fault !print_ctx_evicted_entries)

module Ldd = Types.Ldd

let instance_poly_for_jkind' =
  ref (fun _univars _ty -> Misc.fatal_error "instance_poly_for_jkind")

let fresh_unknown_uid () : Types.Uid.t =
  let current_unit =
    Some
      (Unit_info.make_dummy ~input_name:"<ikind>"
         (Compilation_unit.get_current_or_dummy ()))
  in
  Types.Uid.mk ~current_unit

(** A kind solver specialized to [Types.Ldd] and [Types.type_expr].

    The solver computes LDD polynomials of the form base ⊔ Σ_i (arg_i ⊓ coeff_i)
    where [base] is the intrinsic kind of a constructor and each [coeff_i]
    describes the contribution coming from the i-th type argument. *)
module Solver = struct
  type mode =
    | Normal
    | Round_up

  (* Hash tables avoiding polymorphic structural comparison on deep values.
     [Btype.TypeHash] keys by the representative of a [type_expr], so
     union-find aliases map to a single entry. This table is used to cache
     repeated kind computations, as well as to make circular types work. *)
  module TyTbl = Btype.TypeHash

  let constr_to_string (path : Path.t) : string =
    Format_doc.asprintf "%a" Path.print path

  (* Hash table for caching constructor kinds. *)
  module ConstrTbl = Path.Tbl

  (** Kind function for constructors: computes a kind from a context. This is
      used because many kinds don't make sense outside of a context, e.g., the
      kind of a type containing a constructor depends on the context telling us
      what its kind is. *)
  type ckind = ctx -> Ldd.node

  (** Result of constructor lookup. [Ty] describes a constructor declaration
      with arguments and a kind function; [Poly] provides a cached polynomial
      form. *)
  and constr_decl =
    | Ty of
        { args : Types.type_expr list;
          kind : ckind;
          abstract : bool
        }
    | Poly of Ldd.node * Ldd.node array

  and ctx =
    { env : Env.t option;
      lookup_of_env : Env.t -> Path.t -> constr_decl;
      mode : mode;
      ty_to_kind : Ldd.node TyTbl.t;
      constr_to_coeffs : (Ldd.node * Ldd.node array) ConstrTbl.t
    }

  let global_ty_to_kind : Ldd.node TyTbl.t = TyTbl.create 1

  let global_constr_to_coeffs : (Ldd.node * Ldd.node array) ConstrTbl.t =
    ConstrTbl.create 1

  let create_ctx ~(mode : mode) ~(env : Env.t option)
      ~(lookup_of_env : Env.t -> Path.t -> constr_decl) =
    TyTbl.clear global_ty_to_kind;
    ConstrTbl.clear global_constr_to_coeffs;
    { env;
      lookup_of_env;
      mode;
      ty_to_kind = global_ty_to_kind;
      constr_to_coeffs = global_constr_to_coeffs
    }

  (* Stage-5a probe: total live entries in the two GLOBAL caches.  A print-path
     [create_ctx] clears both, evicting exactly this many; measured to quantify
     the re-entrancy hazard H1 (STAGE5A-NOTES.md). *)
  let global_cache_size () : int =
    TyTbl.length global_ty_to_kind + ConstrTbl.length global_constr_to_coeffs

  (* Stage-5a re-entrancy fix: a per-print SCRATCH context.  Unlike [create_ctx]
     it allocates FRESH cache tables and neither clears nor reuses the globals,
     so a derivation run for PRINTING cannot evict or corrupt the cache an outer
     solve is using mid-flight.  Behaviourally identical to the cleared-global
     ctx for the derivation itself: [create_ctx] already cleared the globals on
     every call, so the cache was always a per-derivation memo; this just makes
     that memo genuinely private instead of a shared table it wipes. *)
  let create_scratch_ctx ~(mode : mode) ~(env : Env.t option)
      ~(lookup_of_env : Env.t -> Path.t -> constr_decl) =
    { env;
      lookup_of_env;
      mode;
      ty_to_kind = TyTbl.create 1;
      constr_to_coeffs = ConstrTbl.create 1
    }

  let reset_for_mode (ctx : ctx) ~(mode : mode) : ctx = { ctx with mode }

  let rigid_name (ctx : ctx) (name : Ldd.Name.t) : Ldd.node =
    match ctx.mode with
    | Normal -> Ldd.node_of_var (Ldd.rigid name)
    | Round_up -> Ldd.const Axis_lattice.top

  (** A rigid variable corresponding to a type parameter [t]. *)
  let rigid (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    let param_id = Types.get_id ty in
    check_live_param_id param_id;
    rigid_name ctx (Ldd.Name.param param_id)

  let type_may_be_circular (ty : Types.type_expr) : bool =
    match Types.get_desc ty with
    | Types.Tvariant _ -> true
    | Types.Tconstr _ -> true
    | Types.Tobject _ -> true
    | _ -> !Clflags.recursive_types

  let is_principal_type (ty : Types.type_expr) : bool =
    (not !Clflags.principal) || Types.get_level ty = Btype.generic_level

  (* CR jujacobs: we could optimize the join with masks you see below
     using a combined [Ldd.join_with_mask left mask right] operation. *)

  let identity_constr_decl ~(arity : int) (path : Path.t) : constr_decl =
    let open Ldd in
    let base = node_of_var (rigid (Name.atomic path 0)) in
    let coeffs =
      Array.init arity (fun i -> node_of_var (rigid (Name.atomic path (i + 1))))
    in
    Poly (base, coeffs)

  let lookup_constr (ctx : ctx) ~(min_arity : int) (path : Path.t) : constr_decl
      =
    match ctx.env with
    | Some env -> ctx.lookup_of_env env path
    | None -> identity_constr_decl ~arity:min_arity path

  (** Fetch or compute the polynomial for constructor [c]. *)
  let rec constr_kind (ctx : ctx) ~(min_arity : int) (path : Path.t) :
      Ldd.node * Ldd.node array =
    (* Return placeholder nodes stored in [constr_to_coeffs] for recursion. *)
    match ConstrTbl.find_opt ctx.constr_to_coeffs path with
    | Some base_and_coeffs -> base_and_coeffs
    | None -> (
      match lookup_constr ctx ~min_arity path with
      | Poly (base, coeffs) ->
        (* Install placeholder nodes before rehydrating cached
           polynomials.  This breaks recursion cycles between
           mutually-recursive types. *)
        let base_var = Ldd.new_var () in
        let coeff_vars =
          Array.init (Array.length coeffs) (fun _ -> Ldd.new_var ())
        in
        let base_poly = Ldd.node_of_var base_var in
        let coeffs_poly = Array.map Ldd.node_of_var coeff_vars in
        ConstrTbl.add ctx.constr_to_coeffs path (base_poly, coeffs_poly);
        (* Replace rigid atoms that refer to other constructors with the
           corresponding cached placeholders.  Atoms that refer back to
           [c] are kept rigid to avoid infinite expansion. *)
        let instantiate (name : Ldd.Name.t) : Ldd.node =
          match name with
          | Param _ | Unknown _ | Residue _ -> rigid_name ctx name
          | KAtom kpath -> (
            match ctx.env with
            | None -> rigid_name ctx name
            | Some env -> (
              match Env.find_jkind kpath env with
              | exception Not_found -> rigid_name ctx name
              | { jkind_manifest = None; _ } -> rigid_name ctx name
              | { jkind_manifest = Some jkind_const; _ } ->
                ckind_of_jkind_desc ctx jkind_const))
          | Atom { constr = other_path; arg_index } ->
            if Path.same other_path path
            then rigid_name ctx name
            else
              let base_poly, coeffs_poly =
                constr_kind ctx ~min_arity:arg_index other_path
              in
              if arg_index = 0
              then base_poly
              else if arg_index - 1 < Array.length coeffs_poly
              then coeffs_poly.(arg_index - 1)
              else rigid_name ctx name
        in
        let rehydrate poly = Ldd.map_rigid instantiate poly in
        let base_rhs = rehydrate base in
        let coeffs_rhs = Array.map rehydrate coeffs in
        Ldd.solve_lfp base_var base_rhs;
        Array.iter2 (fun v rhs -> Ldd.solve_lfp v rhs) coeff_vars coeffs_rhs;
        let res =
          ( Ldd.inline_solved_vars base_poly,
            Array.map Ldd.inline_solved_vars coeffs_poly )
        in
        ConstrTbl.replace ctx.constr_to_coeffs path res;
        res
      | Ty { args = params; kind = body; abstract } ->
        let base_var = Ldd.new_var () in
        let coeff_vars =
          Array.init (List.length params) (fun _ -> Ldd.new_var ())
        in
        let base_poly = Ldd.node_of_var base_var in
        let coeffs_poly = Array.map Ldd.node_of_var coeff_vars in
        ConstrTbl.add ctx.constr_to_coeffs path (base_poly, coeffs_poly);
        let rigid_vars =
          List.map
            (fun ty ->
              let id = Types.get_id ty in
              check_live_param_id id;
              Ldd.rigid (Ldd.Name.param id))
            params
        in
        (* We add the parameters to the TyTbl so that they will refer to
           rigid variables that represent them in the solver. *)
        List.iter2
          (fun ty var ->
            (* Parameters written as plain type variables may have explicit
               bounds, as in [('a : bound)]. Cap their rigid variables by those
               bounds (needed for recursive payload ikinds). Other parameter
               expressions do not have a written parameter bound to apply here,
               so keep their rigid atoms bare. *)
            let param_kind =
              match Types.get_desc ty with
              | Types.Tvar { jkind; _ } ->
                Ldd.meet (Ldd.node_of_var var) (ckind_of_jkind ctx jkind)
              | Types.Tunivar _ ->
                Misc.fatal_error
                  ("Ikind.type_declaration_ikind_of_jkind: "
                 ^ "unexpected Tunivar in parameter list")
              | _ -> Ldd.node_of_var var
            in
            TyTbl.add ctx.ty_to_kind ty param_kind)
          params rigid_vars;
        (* Compute body kind *)
        (* CR jujacobs: potential efficiency win:
           we could still compute the kind in Right mode to keep
           the cache consistent, but we don't need to. *)
        let body_kind = body ctx in
        (* Decompose [body_kind] into a base and one coefficient
           per parameter. *)
        let base_rhs, coeffs_rhs_list =
          Ldd.decompose_into_linear_terms ~universe:rigid_vars body_kind
        in
        let coeffs_rhs = Array.of_list coeffs_rhs_list in
        if Array.length coeff_vars <> Array.length coeffs_rhs
        then
          failwith
            (Printf.sprintf
               "jkind_solver: coeffs mismatch for constr %s (length %d vs %d)"
               (constr_to_string path) (Array.length coeff_vars)
               (Array.length coeffs_rhs));
        if abstract
        then (
          (* For abstract types we solve the solver variables using
             greatest fixpoints. This ensures that abstract types'
             bounds are incorporated into all kind polynomials that
             mention the abstract type. This way, we can check kind
             subsumption without having to consider hypotheses for the
             bounds of abstract types. *)
          Ldd.enqueue_gfp base_var
            (Ldd.meet base_rhs (rigid_name ctx (Ldd.Name.atomic path 0)));
          Array.iteri
            (fun idx coeff_var ->
              let coeff_rhs = coeffs_rhs.(idx) in
              let rhs = Ldd.join coeff_rhs base_rhs in
              let bound =
                Ldd.meet rhs (rigid_name ctx (Ldd.Name.atomic path (idx + 1)))
              in
              Ldd.enqueue_gfp coeff_var bound)
            coeff_vars)
        else (
          Ldd.solve_lfp base_var base_rhs;
          Array.iter2
            (fun coeff_var coeff_rhs -> Ldd.solve_lfp coeff_var coeff_rhs)
            coeff_vars coeffs_rhs);
        base_poly, coeffs_poly)

  (* Apply a constructor polynomial to argument types. *)
  and constr (ctx : ctx) (path : Path.t) (args : Types.type_expr list) :
      Ldd.node =
    let base, coeffs = constr_kind ctx ~min_arity:(List.length args) path in
    let rec loop acc remaining i =
      if i = Array.length coeffs
      then acc
      else
        match remaining with
        | arg :: rest ->
          let arg_kind = kind ~use_tables:true ctx arg in
          loop (Ldd.join acc (Ldd.meet arg_kind coeffs.(i))) rest (i + 1)
        | [] -> failwith "Missing arg"
    in
    loop base args 0

  (* Converting surface jkinds to solver ckinds. *)
  and ckind_of_jkind_desc : type a l r.
      ctx -> (a, l * r) Types.base_and_axes -> Ldd.node =
   fun ctx jkind_desc ->
    let expand =
      match ctx.env with
      | None ->
        let expand : type b.
            (b, l * r) Types.base_and_axes ->
            Types.mod_bounds * (l * r) Types.with_bounds * Path.t option =
         fun jkind_desc ->
          let unresolved_base =
            match jkind_desc.base with
            | Types.Layout _ -> None
            | Types.Kconstr (path, _) -> Some path
          in
          jkind_desc.mod_bounds, jkind_desc.with_bounds, unresolved_base
        in
        expand
      | Some env ->
        let rec expand : type b.
            (b, l * r) Types.base_and_axes ->
            Types.mod_bounds * (l * r) Types.with_bounds * Path.t option =
         fun jkind_desc ->
          match Jkind.Const.expand_once env jkind_desc with
          | Some jkind_const -> expand jkind_const
          | None ->
            let unresolved_base =
              match jkind_desc.base with
              | Types.Layout _ -> None
              | Types.Kconstr (path, _) -> Some path
            in
            jkind_desc.mod_bounds, jkind_desc.with_bounds, unresolved_base
        in
        expand
    in
    let mod_bounds, with_bounds, unresolved_base = expand jkind_desc in
    let base_mod_bounds =
      Ldd.const (Jkind.Mod_bounds.to_axis_lattice mod_bounds)
    in
    let base =
      match unresolved_base with
      | None -> base_mod_bounds
      | Some path ->
        let atom = rigid_name ctx (Ldd.Name.katom path) in
        Ldd.meet base_mod_bounds atom
    in
    (* For each with-bound (ty, axes), contribute
       modality(axes_mask, kind ty). *)
    Jkind.With_bounds.to_seq with_bounds
    |> Seq.fold_left
         (fun acc (ty, bound_info) ->
           let axes = bound_info.Types.With_bounds_type_info.relevant_axes in
           let mask = Axis_lattice.of_axis_set axes in
           let ty_kind = kind ~use_tables:true ctx ty in
           Ldd.join acc (Ldd.meet (Ldd.const mask) ty_kind))
         base

  and ckind_of_jkind : type l r. ctx -> (l * r) Types.jkind -> Ldd.node =
   fun ctx jkind -> ckind_of_jkind_desc ctx jkind.jkind

  (** Compute the kind for [t]. *)
  and kind ?(check_principality = true) ~use_tables (ctx : ctx)
      (ty : Types.type_expr) : Ldd.node =
    if check_principality && not (is_principal_type ty)
    then Ldd.const Axis_lattice.top
    else
      match TyTbl.find_opt ctx.ty_to_kind ty with
      | Some kind_poly -> kind_poly
      | None ->
        if not use_tables
        then kind_uncached ctx ty
        else if type_may_be_circular ty
        then (
          let var = Ldd.new_var () in
          let placeholder = Ldd.node_of_var var in
          TyTbl.add ctx.ty_to_kind ty placeholder;
          let kind_rhs = kind_uncached ctx ty in
          Ldd.solve_lfp var kind_rhs;
          let kind_inlined = Ldd.inline_solved_vars placeholder in
          TyTbl.replace ctx.ty_to_kind ty kind_inlined;
          kind_inlined)
        else
          let kind_rhs = kind_uncached ctx ty in
          TyTbl.add ctx.ty_to_kind ty kind_rhs;
          kind_rhs

  (* Worker for [kind]; does not memoize.
     Only call from [kind] so caching and LFP handling apply. *)
  and kind_uncached (ctx : ctx) (ty : Types.type_expr) : Ldd.node =
    (* Compute the ikind polynomial for an arbitrary [type_expr]. This is the
       semantic counterpart of [Jkind.jkind_of_type], but expressed in LDD
       form. *)
    let kind_poly =
      (* [ty] is expected to be representative: no links/substs/fields/nil. *)
      match Types.get_desc ty with
      | Types.Tvar { name = _name; jkind }
      | Types.Tunivar { name = _name; jkind } ->
        (* Keep a rigid param, but cap it by its annotated jkind. *)
        Ldd.meet (rigid ctx ty) (ckind_of_jkind ctx jkind)
      | Types.Tconstr (path, args, _abbrev_memo) -> constr ctx path args
      | Types.Ttuple elts ->
        (* Boxed tuples: immutable_data base + per-element contributions
           under id modality. *)
        let base = Ldd.const Axis_lattice.immutable_data in
        Ldd.sum elts ~base ~f:(fun (_lbl, t) -> kind ~use_tables:true ctx t)
      | Types.Tunboxed_tuple elts ->
        (* Unboxed tuples: per-element contributions; shallow axes relevant
           only for arity = 1. *)
        Ldd.sum elts ~base:Ldd.bot ~f:(fun (_lbl, t) ->
            kind ~use_tables:true ctx t)
      | Types.Tarrow (_lbl, _t1, _t2, _commu) ->
        (* Arrows use the dedicated per-axis bounds (no with-bounds). *)
        Ldd.const Axis_lattice.arrow
      | Types.Tlink _ -> failwith "Tlink shouldn't appear in kind"
      | Types.Tsubst _ -> failwith "Tsubst shouldn't appear in kind"
      | Types.Trepr (ty, _sort_vars) -> kind ~use_tables:true ctx ty
      | Types.Tpoly (ty, univars) ->
        (* CR ikinds: this is sound but not fully precise.
          Internal ticket 5746. *)
        let ty = !instance_poly_for_jkind' univars ty in
        (* We intentionally skip the principality check here. Enforcing it
           breaks the stdlib build, and the old env-var escape hatch never had
           a viable setting in practice. Track removing this workaround as part
           of internal ticket 5746. *)
        kind ~check_principality:false ~use_tables:true ctx ty
      | Types.Tof_kind jkind -> ckind_of_jkind ctx jkind
      | Types.Tobject _ -> Ldd.const Axis_lattice.object_legacy
      | Types.Tbox t ->
        let base = Ldd.const Axis_lattice.mutable_data in
        Ldd.join base (kind ~use_tables:true ctx t)
      | Types.Tfield _ -> failwith "Tfield shouldn't appear in kind"
      | Types.Tnil -> failwith "Tnil shouldn't appear in kind"
      | Types.Tquote _ | Types.Tsplice _ | Types.Tquote_eval _ ->
        (* Treat quoted/spliced/evaluated quoted types conservatively as
           boxed values. *)
        Ldd.const Axis_lattice.value
      | Types.Tvariant row ->
        if Btype.tvariant_not_immediate row
        then
          if Btype.static_row row
          then
            (* Closed, boxed polymorphic variant: immutable_data base plus
               per-constructor args. *)
            let base = Ldd.const Axis_lattice.immutable_data in
            Btype.fold_row
              (fun acc ty ->
                let ty_kind = kind ~use_tables:true ctx ty in
                Ldd.join acc ty_kind)
              base row
          else
            (* CR ikinds: open rows get conservative non-float value (boxed)
               intersected with an unknown rigid so the solver treats it as an
               unknown element. This can be improved. Internal ticket 6205. *)
            let unknown =
              rigid_name ctx (Ldd.Name.unknown (fresh_unknown_uid ()))
            in
            Ldd.meet (Ldd.const Axis_lattice.nonfloat_value) unknown
        else
          (* All-constant (immediate) polymorphic variant. *)
          Ldd.const Axis_lattice.immediate
      | Types.Tpackage _ ->
        (* Like open polymorphic variants, model first-class modules as boxed
           values intersected with an unknown so they behave as not-best. *)
        let unknown =
          rigid_name ctx (Ldd.Name.unknown (fresh_unknown_uid ()))
        in
        Ldd.meet (Ldd.const Axis_lattice.nonfloat_value) unknown
    in
    kind_poly

  (* Evaluate a ckind in [ctx] and flush pending GFP constraints. *)
  let normalize (kind_poly : Ldd.node) : Ldd.node =
    Ldd.solve_pending ();
    kind_poly

  let node_of_name (ctx : ctx) (name : Ldd.Name.t) : Ldd.node =
    rigid_name ctx name

  (* Materialize a solved polynomial for storing in
     [Types.constructor_ikind]. *)
  let constr_kind_poly (ctx : ctx) (c : Path.t) : Ldd.node * Ldd.node array =
    let base, coeffs = constr_kind ctx ~min_arity:0 c in
    Ldd.solve_pending ();
    base, coeffs

  let round_up (k : Ldd.node) : Axis_lattice.t = Ldd.round_up k
end

let constructor_ikind ~base ~coeffs : Types.constructor_ikind =
  (* Keep coefficients disjoint from the base (subtract-normal form). *)
  for i = 0 to Array.length coeffs - 1 do
    let coeff = coeffs.(i) in
    let coeff' = Ldd.sub_subsets coeff base in
    if coeff != coeff' then coeffs.(i) <- coeff'
  done;
  ({ Types.base; coeffs } : Types.constructor_ikind)

let pp_coeffs (coeffs : Ldd.node array) : string =
  coeffs |> Array.map Ldd.pp |> Array.to_list |> String.concat "; "

let with_ikinds_enabled (f : unit -> Types.constructor_ikind) : Types.type_ikind
    =
  Types.Constructor_ikind (f ())

let origin_suffix_of = function None -> "" | Some o -> " origin=" ^ o

let pp_axes (axes : Jkind_axis.Axis.packed list) : string =
  axes
  |> List.map (fun (Jkind_axis.Axis.Pack ax) -> Jkind_axis.Axis.name ax)
  |> String.concat ", "

let axis_disagreement_reasons (axes : Jkind_axis.Axis.packed list) :
    Jkind.Sub_failure_reason.t list =
  List.map (fun axis -> Jkind.Sub_failure_reason.Axis_disagreement axis) axes

let label_mutability_contribution (lbl : Types.label_declaration) =
  Ldd.const
    (match lbl.ld_mutable with
    | Immutable -> Axis_lattice.immediate
    | Mutable { atomic = Atomic; _ } -> Axis_lattice.sync_data
    | Mutable { atomic = Nonatomic; _ } -> Axis_lattice.mutable_data)

let sum_record_label_contributions ~(base : Ldd.node)
    ~(payload_kind : Types.type_expr -> Ldd.node)
    ~(mutability_contribution : Types.label_declaration -> Ldd.node)
    (lbls : Types.label_declaration list) : Ldd.node =
  Ldd.sum lbls ~base ~f:(fun (lbl : Types.label_declaration) ->
      let mask = Axis_lattice.mask_of_modality lbl.ld_modalities in
      Ldd.join
        (mutability_contribution lbl)
        (Ldd.meet (Ldd.const mask) (payload_kind lbl.ld_type)))

(* Unboxed records ignore field mutability: with no heap identity to mutate
   through, mutability adds nothing to the kind. This matches the legacy
   [Jkind_desc.product], which never consults [ld_mutable]. *)
let no_mutability_contribution (_ : Types.label_declaration) = Ldd.bot

(* Gather constructor-local vars from [tys]. *)
let collect_type_vars (tys : Types.type_expr list) :
    (int, Types.type_expr) Hashtbl.t =
  let vars = Hashtbl.create 16 in
  Types.with_type_mark (fun mark ->
      let super = Btype.type_iterators mark in
      let it =
        { super with
          it_type_expr =
            (fun self ty ->
              match Types.get_desc ty with
              | Types.Tvar _ | Types.Tunivar _ ->
                let id = Types.get_id ty in
                Hashtbl.replace vars id ty
              | _ -> super.it_type_expr self ty)
        }
      in
      List.iter (it.it_type_expr it) tys);
  vars

(* Use each local variable's declared jkind as its fallback bound. *)
let local_var_bounds (ctx : Solver.ctx)
    (local_vars : (int, Types.type_expr) Hashtbl.t) =
  let bounds = Hashtbl.create (Hashtbl.length local_vars) in
  Hashtbl.iter
    (fun id ty ->
      let bound =
        match Types.get_desc ty with
        | Types.Tvar { jkind; _ } | Types.Tunivar { jkind; _ } ->
          Solver.ckind_of_jkind ctx jkind
        | _ -> Ldd.const Axis_lattice.top
      in
      Hashtbl.replace bounds id bound)
    local_vars;
  bounds

(* For a plain-variable result argument, map it directly to [lhs_kind]. *)
let add_plain_var_projection ~(local_vars : ('a, Types.type_expr) Hashtbl.t)
    ~(local_subst : ('a, Ldd.node) Hashtbl.t) ~(lhs_kind : Ldd.node)
    (res_arg : Types.type_expr) : unit =
  match Types.get_desc res_arg with
  | Types.Tvar _ | Types.Tunivar _ ->
    let id = Types.get_id res_arg in
    if Hashtbl.mem local_vars id && not (Hashtbl.mem local_subst id)
    then Hashtbl.add local_subst id lhs_kind
  | _ -> ()

let make_gadt_payload_projector ~(decl_params : Types.type_expr list)
    (ctx : Solver.ctx) :
    Types.constructor_declaration -> Types.type_expr -> Ldd.node =
  (* For a GADT constructor, compute payload kinds under a projection from
     constructor-local vars (existentials/equated vars) to declaration
     parameters. This keeps payload ikinds comparable to the type's declared
     parameters while remaining conservative for unmapped locals.

     Examples:
     - type 'a t = C : 'b -> 'b t
       Here result arg is plain var ['b], aligned with param ['a], so we map
       ['b -> kind('a)] when computing payload kind of ['b].

     - type ('a, 'b) u = C : 'x * 'y -> ('x, int) u
       We map ['x -> kind('a)] from the first result arg. The second result
       arg is [int], so ['y] gets no projection and falls back to its declared
       local bound.

     - type ('a, 'b) same = C : 'x -> ('x, 'x) same
       First hit wins: ['x] is mapped from the first result arg to kind('a);
       the second occurrence does not overwrite it. *)
  let fallback ty = Solver.kind ~use_tables:true ctx ty in
  fun (c : Types.constructor_declaration) ->
    match c.cd_res with
    | None -> fallback
    | Some res -> (
      match Types.get_desc res with
      | Types.Tconstr (_, res_args, _) ->
        let payload_tys = Types.tys_of_constr_args c.cd_args in
        (* Step 1: collect constructor-local vars seen in payload/result.
           GADT constructor vars are in their own scope, distinct from the
           type declaration parameters. *)
        let local_vars = collect_type_vars (payload_tys @ res_args) in
        if Hashtbl.length local_vars = 0
        then fallback
        else
          let local_var_bounds = local_var_bounds ctx local_vars in
          (* Step 2: build a partial substitution local_var -> projected kind
             from the constructor result arguments. Earlier mappings win. *)
          let local_subst = Hashtbl.create (Hashtbl.length local_vars) in
          List.iter2
            (fun decl_param res_arg ->
              add_plain_var_projection ~local_vars ~local_subst
                ~lhs_kind:(Solver.kind ~use_tables:true ctx decl_param)
                res_arg)
            decl_params res_args;
          (* Step 3: apply the substitution to payload kinds:
             - mapped locals use their projected kinds
             - unmapped locals fall back to their declared bounds
             - non-local names are left unchanged. *)
          (* Rewrite projected local vars in payload kinds; unmapped locals
             fall back to their declared bounds. *)
          let map_name (name : Ldd.Name.t) =
            match name with
            | Ldd.Name.Param id -> (
              match Hashtbl.find_opt local_subst id with
              | Some projected -> projected
              | None ->
                if Hashtbl.mem local_vars id
                then
                  match Hashtbl.find_opt local_var_bounds id with
                  | Some bound -> bound
                  | None -> Ldd.const Axis_lattice.top
                else Solver.node_of_name ctx name)
            | Ldd.Name.Unknown _ | Ldd.Name.Atom _ | Ldd.Name.KAtom _
            | Ldd.Name.Residue _ ->
              Solver.node_of_name ctx name
          in
          fun ty ->
            let raw_kind = Solver.kind ~use_tables:true ctx ty in
            Ldd.map_rigid map_name raw_kind
      | _ ->
        failwith
          "ikind: expected GADT constructor result to be a type constructor")

(* A stored constructor ikind for a [Type_abstract Definition] decl that carries
   a [Param] atom foreign to the decl's own type parameters is a
   recursive-module
   fixpoint residue: the declaration-time fixpoint (in the module-type-body
   scope) gated a recursive sibling's contribution on a symbolic atom that a
   from-scratch recompute cannot reproduce -- recompute resolves the closed
   manifest coarsely (e.g. an object to [object_legacy]) instead. Stored decl
   ikinds are otherwise free of foreign [Param] atoms (parameter dependence is
   captured positionally in the coeff array), so this is a precise signature of
   the residue. See STAGE4B-DESIGN.md. *)
let stored_ikind_has_foreign_param ~(own_params : Types.type_expr list)
    (base : Ldd.node) (coeffs : Ldd.node array) : bool =
  let own_ids = List.map Types.get_id own_params in
  let found = ref false in
  let visit (name : Ldd.Name.t) : Ldd.node =
    (match name with
    | Ldd.Name.Param id when not (List.mem id own_ids) -> found := true
    (* An imported residue is a [Residue] atom (neutralized on save, stage 5b);
       the distinct constructor IS the CLASS-B marker -- recognize it here so
       residue recognition survives the save-path neutralization. *)
    | Ldd.Name.Residue _ -> found := true
    | _ -> ());
    Ldd.node_of_var (Ldd.rigid name)
  in
  ignore (Ldd.map_rigid visit base : Ldd.node);
  Array.iter (fun c -> ignore (Ldd.map_rigid visit c : Ldd.node)) coeffs;
  !found

(* Stage-4d detector: record every foreign [Param] id carried by a persisted
   (imported) decl ikind into [imported_foreign_param_ids].  Called at the
   load/lookup site, so the id's persisted origin is known WITHOUT a post-intern
   marker.  Validate-only. *)
let record_imported_foreign_params ~(own_params : Types.type_expr list)
    (base : Ldd.node) (coeffs : Ldd.node array) : unit =
  let own_ids = List.map Types.get_id own_params in
  let visit (name : Ldd.Name.t) : Ldd.node =
    (match name with
    | Ldd.Name.Param id when not (List.mem id own_ids) ->
      Hashtbl.replace imported_foreign_param_ids id ()
    (* A neutralized residue arrives as a [Residue] atom, not a foreign [Param];
       count it separately (telemetry) -- it can never alias a live [Param]. *)
    | Ldd.Name.Residue _ -> incr imported_residues
    | _ -> ());
    Ldd.node_of_var (Ldd.rigid name)
  in
  ignore (Ldd.map_rigid visit base : Ldd.node);
  Array.iter (fun c -> ignore (Ldd.map_rigid visit c : Ldd.node)) coeffs

(* Stage-4d detector: is [uid] a decl minted in a DIFFERENT compilation unit
   (i.e. imported from a cmi)?  Keys on the decl's OWN uid origin, not the
   syntactic access path, so a module ALIAS / functor-param / local bind
   ([module L = Foo; L.t]) is still recognized as imported -- the syntactic
   [Path.head]/[Ident.is_global] gate missed those (reviewer blind-spot repro).
   [comp_unit] is [Compilation_unit.full_path_as_string] of the defining unit
   (see [Shape.Uid.mk]); compare against the current unit's, computed the same
   way. *)
let decl_is_imported (uid : Types.Uid.t) : bool =
  let current =
    match Env.get_current_unit () with
    | Some ui ->
      Some (Compilation_unit.full_path_as_string (Unit_info.modname ui))
    | None -> None
  in
  match uid with
  | Types.Uid.Item { comp_unit; _ } | Types.Uid.Compilation_unit comp_unit -> (
    match current with
    | Some cur -> not (String.equal comp_unit cur)
    (* No current unit (toplevel / special tools): there is no cmi-import
       boundary to police, so treat as NOT imported.  [None -> true] here would
       flag every same-session decl as imported and produce massive false-
       positive self-collisions in the toplevel (the detector is over-approximate
       but must not fire on a single in-memory session). Batch compilation -- the
       real cross-cmi scenario -- always has [Some] current unit. *)
    | None -> false)
  | Types.Uid.Predef _ | Types.Uid.Internal | Types.Uid.Unboxed_version _ ->
    false

(* Lookup function supplied to the solver.
   We prefer a stored ikind (when present) and otherwise recompute from the
   type declaration in [env]. *)
let lookup_of_env ~(env : Env.t) (path : Path.t) : Solver.constr_decl =
  match Env.find_type path env with
  | exception Not_found ->
    (* Format.eprintf "ERROR: unknown constructor %a@." Path.print path; *)
    (* WE CANNOT ACTUALLY GIVE AN ERROR HERE! *)
    (* Explanation: build systems sometimes heuristically do not
       include all cmis for performance reasons. Because of that,
       we could encounter types that appear not to exist. We must
       treat those as abstract unknowns. *)
    (* Fallback for unknown constructors: treat them as abstract,
       non-recursive values. *)
    let unknown = Ldd.Name.unknown (fresh_unknown_uid ()) in
    let kind : Solver.ckind = fun _ctx -> Ldd.node_of_var (Ldd.rigid unknown) in
    Solver.Ty { args = []; kind; abstract = true }
  | type_decl ->
    (* Here we can switch to using the cached ikind or not. *)
    let fallback () =
      (* When we have no stored ikind, we go to this fallback and compute. *)
      match type_decl.type_manifest with
      | Some body_ty ->
        (* Concrete: compute kind of body. *)
        let args = type_decl.type_params in
        let kind : Solver.ckind =
         fun ctx -> Solver.kind ~use_tables:true ctx body_ty
        in
        Solver.Ty { args; kind; abstract = false }
      | None -> (
        (* No manifest: may still be "concrete" (record/variant/...).
           Build ckind. *)
        let allow_any_crossing =
          match type_decl.type_kind with
          | Types.Type_record (_, _, umc_opt)
          | Types.Type_record_unboxed_product (_, _, umc_opt)
          | Types.Type_variant (_, _, umc_opt) ->
            Option.is_some umc_opt
          | Types.Type_abstract _ | Types.Type_open -> false
        in
        let use_decl_jkind ~treat_as_abstract =
          let kind : Solver.ckind =
           fun ctx -> Solver.ckind_of_jkind ctx type_decl.type_jkind
          in
          Solver.Ty
            { args = type_decl.type_params; kind; abstract = treat_as_abstract }
        in
        match type_decl.type_kind with
        (* For abstract types and allow_any_crossing types, we derive the
           ikind from the jkind annotation, instead of computing it from
           the type declaration's body: *)
        | _ when allow_any_crossing -> use_decl_jkind ~treat_as_abstract:false
        | Types.Type_abstract _ ->
          use_decl_jkind
            ~treat_as_abstract:(not (Jkind.is_best type_decl.type_jkind))
        (* For other cases, we compute the ikind from the type definition{} *)
        | Types.Type_record (lbls, rep, _umc_opt) ->
          (* Build from components: base (non-float value) + per-label
             contributions. *)
          let immutable_base =
            Ldd.const
              (match rep with
              | Types.Record_unboxed -> Axis_lattice.immediate
              (* CR box: This will no longer be [non_float] once we update the
                 representation of singleton float64 records *)
              | _ -> Axis_lattice.immutable_data)
          in
          let kind : Solver.ckind =
           fun (ctx : Solver.ctx) ->
            sum_record_label_contributions ~base:immutable_base
              ~payload_kind:(fun ty -> Solver.kind ~use_tables:true ctx ty)
              ~mutability_contribution:label_mutability_contribution lbls
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_record_unboxed_product (lbls, _rep, _umc_opt) ->
          let kind : Solver.ckind =
           fun (ctx : Solver.ctx) ->
            let base = Ldd.const Axis_lattice.immediate in
            sum_record_label_contributions ~base
              ~payload_kind:(fun ty -> Solver.kind ~use_tables:true ctx ty)
              ~mutability_contribution:no_mutability_contribution lbls
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_variant (_cstrs, Types.Variant_with_null, _umc_opt) ->
          (* [Variant_with_null] (i.e. [or_null]) has semantics that are not
             captured by its constructors: nullability/separability and
             mode-crossing are baked into its representation. We defer to
             jkinds because ikinds cannot express this today. This deferral
             can be removed once separability and nullability become layout
             properties rather than modal axes. *)
          use_decl_jkind ~treat_as_abstract:false
        | Types.Type_variant (cstrs, rep, _umc_opt) ->
          (* Choose base: immediate for void-only variants; sync if any record
             field is atomic; mutable if any non-atomic mutable field appears;
             otherwise immutable. *)
          let all_args_void =
            List.for_all
              (fun (c : Types.constructor_declaration) ->
                match c.cd_args with
                | Types.Cstr_tuple args ->
                  List.for_all
                    (fun (arg : Types.constructor_argument) ->
                      match arg.ca_sort with
                      | Some sort -> Jkind_types.Sort.Const.all_void sort
                      | None -> false)
                    args
                | Types.Cstr_record lbls ->
                  List.for_all
                    (fun (lbl : Types.label_declaration) ->
                      match lbl.ld_sort with
                      | Some sort -> Jkind_types.Sort.Const.all_void sort
                      | None -> false)
                    lbls)
              cstrs
          in
          let kind : Solver.ckind =
           fun (ctx : Solver.ctx) ->
            let base_lat0 =
              match rep with
              | Types.Variant_unboxed -> Axis_lattice.immediate
              | _ ->
                if all_args_void
                then Axis_lattice.immediate
                else Axis_lattice.immutable_data
            in
            let payload_kind_of_constructor =
              make_gadt_payload_projector ~decl_params:type_decl.type_params ctx
            in
            let constructor_contrib (c : Types.constructor_declaration) =
              let payload_kind = payload_kind_of_constructor c in
              match c.cd_args with
              | Types.Cstr_tuple args ->
                Ldd.sum args ~base:Ldd.bot
                  ~f:(fun (arg : Types.constructor_argument) ->
                    let mask =
                      Axis_lattice.mask_of_modality arg.ca_modalities
                    in
                    Ldd.meet (Ldd.const mask) (payload_kind arg.ca_type))
              | Types.Cstr_record lbls ->
                sum_record_label_contributions ~base:Ldd.bot ~payload_kind
                  ~mutability_contribution:label_mutability_contribution lbls
            in
            Ldd.sum cstrs ~base:(Ldd.const base_lat0) ~f:constructor_contrib
          in
          Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        | Types.Type_open ->
          (* Use the stored jkind here in case it is `exn`,
             which is special. *)
          use_decl_jkind ~treat_as_abstract:false
        (*
           (* This is the code we'd use otherwise *)
           let kind : Solver.ckind =
            fun _ctx ->
             Ldd.const Axis_lattice.nonfloat_value
           in
           Solver.Ty { args = type_decl.type_params; kind; abstract = false }
        *)
        )
    in
    (* CLASS-A (stage 3, validate-only): a [Type_abstract Definition] decl with a
       fresh [Tvar] manifest is the temporary declaration [Typedecl.enter_type]
       enters for a (possibly recursive) type before its body is analyzed. Its
       stored ikind is the user's DECLARED jkind; its manifest is a placeholder
       type variable collecting usage constraints, NOT the real body. So a
       from-scratch recompute -- which follows that fresh Tvar -- is not a valid
       reference for this decl, and the validation harness keeps the stored
       (declared) ikind even in the recompute reference.

       Soundness (exhaustive case split; the stored value IS the declared jkind):
       - enclosing type ACCEPTED => the decl kind-check proved [body <= declared],
         so [stored(declared) >= true body kind] = over-approximation = sound
         (over-reject only);
       - [declared] too tight ([stored <= true]) => the decl kind-check FAILS and
         the type is REJECTED => the tight stored is discarded, never
         authoritatively accepts anything.
       A too-tight declared jkind rejects the DEFINITION; it can never accept a
       USE. *)
    let is_def_tvar_temp_decl =
      match type_decl.type_kind, type_decl.type_manifest with
      | Types.Type_abstract Types.Definition, Some body_ty -> (
        match Types.get_desc body_ty with Types.Tvar _ -> true | _ -> false)
      | _ -> false
    in
    (* Stage 4b: a recursive-module fixpoint residue gets the same
       reference-exclusion as CLASS-A, detected by a foreign [Param] in the
       stored ikind rather than a fresh-[Tvar] manifest. See
       STAGE4B-DESIGN.md. *)
    let is_def_abstract =
      match type_decl.type_kind with
      | Types.Type_abstract Types.Definition -> true
      | _ -> false
    in
    (* Prefer a stored constructor ikind if one is present and enabled. *)
    let ikind =
      match type_decl.type_ikind with
      | Types.Constructor_ikind { base; coeffs }
        when not !force_recompute_ikinds ->
        if !Clflags.ikinds_validate then incr stored_decl_ikind_hits;
        (* Detector: an IMPORTED decl's stored ikind (persistent path head) may
           carry a foreign [Param] with a stale live id; record it. *)
        if !Clflags.ikinds_validate && decl_is_imported type_decl.type_uid
        then
          record_imported_foreign_params ~own_params:type_decl.type_params base
            coeffs;
        Solver.Poly (base, coeffs)
      | Types.Constructor_ikind { base; coeffs }
        when !force_recompute_ikinds && is_def_tvar_temp_decl ->
        (* CLASS-A: keep the stored (declared) ikind in the recompute reference;
           the fresh Tvar manifest is a placeholder, not the body. *)
        Solver.Poly (base, coeffs)
      | Types.Constructor_ikind { base; coeffs }
        when !force_recompute_ikinds && !trust_residue_stored && is_def_abstract
             && stored_ikind_has_foreign_param ~own_params:type_decl.type_params
                  base coeffs ->
        (* CLASS-B (stage 4b, residue-trusting reference only): a
           recursive-module
           fixpoint residue -- a [Type_abstract Definition] whose stored ikind
           carries a foreign [Param].  A from-scratch recompute cannot reproduce
           the declaration-time recursive-module fixpoint, so it is not a valid
           independent reference; the validation harness re-derives with this
           branch enabled and COUNTS the resulting agreement under CLASS-B
           rather
           than hard-failing.  Never fires at compile time (guarded by
           [force_recompute_ikinds], set only by the harness). *)
        incr residue_trusted;
        if !residue_fault
        then Solver.Poly (Ldd.join base (Ldd.const Axis_lattice.top), coeffs)
        else Solver.Poly (base, coeffs)
      | Types.No_constructor_ikind reason ->
        if !Clflags.ikinds_debug then Format.eprintf "[ikind-miss] %s@." reason;
        if !Clflags.ikinds_validate && not !force_recompute_ikinds
        then incr recomputed_decl_ikind;
        fallback ()
      | Types.Constructor_ikind _ ->
        if !Clflags.ikinds_validate && not !force_recompute_ikinds
        then incr recomputed_decl_ikind;
        fallback ()
      | Types.Saved_ikind _ ->
        (* Invariant: the cmi deserialize boundary rehydrates every persisted
           Saved_ikind to a Constructor_ikind before any consumer runs, so a
           Saved_ikind here is a bug (stage-5 format lock-in). *)
        Misc.fatal_error
          "ikind: unrehydrated Saved_ikind reached the decl lookup"
    in
    (if !Clflags.ikinds_debug
     then
       let ikind_msg =
         match ikind with
         | Solver.Ty _ -> "Ty"
         | Solver.Poly (base, coeffs) ->
           let coeffs =
             coeffs |> Array.map Ldd.pp |> Array.to_list |> String.concat "; "
           in
           Format.asprintf "Poly(base=%s; coeffs=[%s])" (Ldd.pp base) coeffs
       in
       Format.eprintf "[ikind] %a: %s@."
         (Format_doc.compat Path.print)
         path ikind_msg);
    ikind

(* Package the above into a full evaluation context. *)
let create_ctx ~(mode : Solver.mode) ~(env : Env.t option) =
  Solver.create_ctx ~mode ~env ~lookup_of_env:(fun env path ->
      lookup_of_env ~env path)

let create_scratch_ctx ~(mode : Solver.mode) ~(env : Env.t option) =
  Solver.create_scratch_ctx ~mode ~env ~lookup_of_env:(fun env path ->
      lookup_of_env ~env path)

(* Stage-5a: context-creation helper for the two print-from-ikind derivations.
   The print path builds a SCRATCH context (fresh tables, globals untouched) so
   it is re-entrant by construction -- a print mid-check cannot wipe the outer
   solve's cache.  Eviction is still measured FAITHFULLY (global cache size
   immediately before vs after the context creation): the probe counter summed
   >0 with the old clearing [create_ctx] and must now stay 0, so the counter is
   a live regression guard, not a value hard-coded to 0. *)
let create_print_ctx ~(mode : Solver.mode) ~(env : Env.t option) =
  let before = Solver.global_cache_size () in
  let ctx = create_scratch_ctx ~mode ~env in
  let after = Solver.global_cache_size () in
  print_ctx_evicted_entries
    := !print_ctx_evicted_entries + max 0 (before - after);
  ctx

let normalize ~(env : Env.t option) (jkind : Types.jkind_l) : Ldd.node =
  let ctx = create_ctx ~mode:Solver.Normal ~env in
  Solver.normalize (Solver.ckind_of_jkind ctx jkind)

let type_declaration_ikind ~(env : Env.t option) ~(path : Path.t) :
    Types.constructor_ikind =
  let ctx = create_ctx ~mode:Solver.Normal ~env in
  let base, coeffs = Solver.constr_kind_poly ctx path in
  constructor_ikind ~base ~coeffs

let type_declaration_ikind_gated ~(env : Env.t option) ~(path : Path.t) :
    Types.type_ikind =
  (* This function gets called separately for each
    type definition of a mutually recursive group. This is
    safe but computationally wasteful. In the future we might
    want to give this function a list of paths and compute the
    ikind for all of them at once. Alternatively, keep the cache
    between calls to this function from the same mutually recursive
    group. *)
  with_ikinds_enabled (fun () ->
      let ikind = type_declaration_ikind ~env ~path in
      (if !Clflags.ikinds_debug
       then
         let stored_jkind =
           match env with
           | None -> "?"
           | Some env -> (
             match Env.find_type path env with
             | exception Not_found -> "?"
             | _decl -> "<stored-jkind>")
         in
         Format.eprintf "[ikind] %a: stored=%s, base=%s, coeffs=[%s]@."
           (Format_doc.compat Path.print)
           path stored_jkind (Ldd.pp ikind.base) (pp_coeffs ikind.coeffs));
      ikind)

let type_declaration_ikind_of_jkind ~(env : Env.t option)
    ~(params : Types.type_expr list) (type_jkind : Types.jkind_l) :
    Types.type_ikind =
  with_ikinds_enabled (fun () ->
      let poly = normalize ~env type_jkind in
      let rigid_vars =
        List.map
          (fun ty ->
            let id = Types.get_id ty in
            check_live_param_id id;
            Ldd.rigid (Ldd.Name.param id))
          params
      in
      let base, coeffs =
        Ldd.decompose_into_linear_terms ~universe:rigid_vars poly
      in
      let coeffs = Array.of_list coeffs in
      let payload = constructor_ikind ~base ~coeffs in
      if !Clflags.ikinds_debug
      then
        Format.eprintf "[ikind] from jkind: base=%s; coeffs=[%s]@."
          (Ldd.pp payload.base) (pp_coeffs payload.coeffs);
      payload)

(* Compute a declaration's ikind from its manifest body over [params]. This is
   the eager form of the recompute [lookup_of_env] performs for a manifest decl
   carrying [No_constructor_ikind]; it mirrors [Solver.constr_kind]'s [Ty]
   branch (parameters bound to rigid vars, plain type variables capped by their
   written jkind) so the stored ikind equals that recompute. Used to fill in
   the ikind at manifest-installing sites (e.g. with-constraints) that today
   fall back to env-recompute -- see STAGE1-DESIGN.md and STAGE0C-CENSUS.md. *)
let type_declaration_ikind_of_manifest ~(env : Env.t option)
    ~(params : Types.type_expr list) (manifest : Types.type_expr) :
    Types.type_ikind =
  with_ikinds_enabled (fun () ->
      let ctx = create_ctx ~mode:Solver.Normal ~env in
      let rigid_vars =
        List.map
          (fun ty ->
            let id = Types.get_id ty in
            check_live_param_id id;
            Ldd.rigid (Ldd.Name.param id))
          params
      in
      List.iter2
        (fun ty var ->
          let param_kind =
            match Types.get_desc ty with
            | Types.Tvar { jkind; _ } ->
              Ldd.meet (Ldd.node_of_var var) (Solver.ckind_of_jkind ctx jkind)
            | Types.Tunivar _ ->
              Misc.fatal_error
                "Ikind.type_declaration_ikind_of_manifest: unexpected Tunivar"
            | _ -> Ldd.node_of_var var
          in
          Solver.TyTbl.add ctx.Solver.ty_to_kind ty param_kind)
        params rigid_vars;
      let body_kind = Solver.kind ~use_tables:true ctx manifest in
      Ldd.solve_pending ();
      let base, coeffs =
        Ldd.decompose_into_linear_terms ~universe:rigid_vars body_kind
      in
      let coeffs = Array.of_list coeffs in
      let payload = constructor_ikind ~base ~coeffs in
      if !Clflags.ikinds_debug
      then
        Format.eprintf "[ikind] from manifest: base=%s; coeffs=[%s]@."
          (Ldd.pp payload.base) (pp_coeffs payload.coeffs);
      payload)

(* Stage-4c print-from-ikind: derive a WITH-BOUNDS-FREE const jkind's mod-bounds
   floor from its ikind (round_up of the derived LDD), installed into
   [Jkind.Const.floor_from_ikind].  Stage-5c: this fires on the DEFAULT print
   path (ikinds on), not just under [-print-from-ikinds] -- the floor is now
   read from the ikind rather than the legacy [mod_bounds] field.  Re-entrancy-
   safe by construction: 5a re-homed it onto a scratch ctx + isolated pending.
   Returns [None] -- printing then falls back to the legacy [mod_bounds] field
   -- when ikinds are disabled, the jkind carries with-bounds (whose surface
   [with]-clause syntax the LDD cannot reconstruct, see STAGE4C-DESIGN.md P2, so
   the floor keeps reading [mod_bounds] there), or the derivation raises.  For
   the with-bounds-free case the ikind is a pure floor (a const, or a const meet
   a KAtom that rounds up to top), so [round_up] returns exactly
   [to_axis_lattice mod_bounds] and [of_axis_lattice] round-trips it to the
   floor -- i.e. byte-identical to the legacy read (STAGE4C P1, corpus-proven). *)
let mod_bounds_floor_for_printing : type l r.
    Env.t -> (l * r) Jkind.Const.t -> Jkind.Mod_bounds.t option =
 fun env jkind ->
  match jkind.Types.with_bounds with
  | Types.With_bounds _ -> None
  | Types.No_with_bounds -> (
    match
      (* Scratch ctx (fresh caches) + isolated pending: a print mid-check
           perturbs neither the outer solve's Solver caches nor its pending
           gfps. *)
      Ldd.with_isolated_pending (fun () ->
          let ctx = create_print_ctx ~mode:Solver.Normal ~env:(Some env) in
          Solver.round_up (Solver.ckind_of_jkind_desc ctx jkind))
    with
    | exception _ -> None
    | lat ->
      incr print_floor_derivations;
      let lat = if !print_floor_fault then Axis_lattice.top else lat in
      Some (Jkind.Mod_bounds.of_axis_lattice lat))

let () =
  Jkind.Const.set_floor_from_ikind
    { Jkind.Const.derive = mod_bounds_floor_for_printing }

(* Stage-4c full print-from-ikind: render an entire WITH-BOUNDS jkind from its
   ikind, with [with]-clauses NORMALIZED from the LDD terms.  Installed into
   [Jkind.Const.render_from_ikind].  Returns [None] (=> legacy renderer, which
   for the with-bounds-free case applies the byte-identical floor seam) when the
   flag is off, ikinds are disabled, the jkind is with-bounds-free, or the
   derivation raises.  For with-bounds jkinds the normalized rendering
   intentionally diverges from legacy surface syntax (opt-in under the flag):
   the base (names=[]) term is the unconditional floor, rendered via the normal
   path on a synthetic with-bounds-free jkind (so the layout/abbreviation choice
   matches legacy), and each non-base term [(coeff, names)] is rendered as a
   [with] clause [with (name1 & name2 ... @ coeff)] mirroring the LDD algebra. *)
let render_jkind_from_ikind : type l r.
    Env.t -> (l * r) Jkind.Const.t -> Outcometree.out_jkind_const option =
 fun env jkind ->
  if not !Clflags.print_from_ikinds
  then None
  else
    match jkind.Types.with_bounds with
    | Types.No_with_bounds -> None
    | Types.With_bounds _ -> (
      match
        (* Scratch ctx (fresh caches) + isolated pending: this derivation's
           solve_pending drains only its OWN gfps, never an outer mid-check
           solve's pending. *)
        Ldd.with_isolated_pending (fun () ->
            let ctx = create_print_ctx ~mode:Solver.Normal ~env:(Some env) in
            let node = Solver.ckind_of_jkind_desc ctx jkind in
            Ldd.solve_pending ();
            Ldd.to_terms (Ldd.inline_solved_vars node))
      with
      | exception _ ->
        incr print_render_fallbacks;
        None
      | terms ->
        incr print_withbounds_rendered;
        let floor =
          List.fold_left
            (fun acc (c, names) ->
              match names with [] -> Axis_lattice.join acc c | _ -> acc)
            Axis_lattice.bot terms
        in
        let base_jkind : (l * r) Jkind.Const.t =
          { jkind with
            Types.mod_bounds = Jkind.Mod_bounds.of_axis_lattice floor;
            Types.with_bounds = Types.No_with_bounds
          }
        in
        let base_out = Jkind.Const.to_out_jkind_const env base_jkind in
        let with_clauses =
          List.filter_map
            (fun (c, names) ->
              match names with
              | [] -> None
              | _ ->
                let names_str =
                  String.concat " & "
                    (List.map Types.Rigid_name.to_string names)
                in
                let stuff =
                  if Axis_lattice.equal c Axis_lattice.top
                  then names_str
                  else
                    Printf.sprintf "%s @ %s" names_str
                      (Axis_lattice.to_string c)
                in
                Some (Outcometree.Otyp_stuff stuff))
            terms
        in
        Some
          (List.fold_left
             (fun acc oty -> Outcometree.Ojkind_const_with (acc, oty, []))
             base_out with_clauses))

let () =
  Jkind.Const.set_render_from_ikind
    { Jkind.Const.render = render_jkind_from_ikind }

let predef_ikind_of_jkind ~params type_jkind =
  type_declaration_ikind_of_jkind ~env:None ~params type_jkind

let () = Predef.set_ikind_of_jkind predef_ikind_of_jkind

type subcheck_fast_path =
  | No_fast_path
  | Rhs_top_fast_path

type subcheck_polys =
  { lhs_for_leq : Ldd.node;
    rhs_for_leq : Ldd.node;
    fast_path : subcheck_fast_path
  }

(* Compute polynomials for a subcheck:
   - compute [super] in Normal mode
   - fast path: if [super] is constant top, no need to compute [sub]
   - otherwise, if [super] is constant, try the lhs mod-bounds floor fast path
   - otherwise, only round up [sub] if [super] is constant *)
(* Stage-1 validation harness helpers (see STAGE1-DESIGN.md). *)

(* Unknown atoms ([fresh_unknown_uid]: open rows, first-class modules, some
   existential projections) get a fresh, non-deterministic uid on every
   derivation, so two independent derivations of the same jkind disagree on
   their identity. Canonicalize all unknowns to a single atom before comparing,
   so the harness compares the determinate structure rather than reporting these
   as spurious mismatches. *)
let canonical_unknown_node : Ldd.node =
  Ldd.node_of_var (Ldd.rigid (Ldd.Name.unknown (fresh_unknown_uid ())))

let canonicalize_unknowns (n : Ldd.node) : Ldd.node =
  Ldd.map_rigid
    (fun (name : Ldd.Name.t) ->
      match name with
      | Ldd.Name.Unknown _ -> canonical_unknown_node
      | Ldd.Name.Param _ | Ldd.Name.Atom _ | Ldd.Name.KAtom _
      | Ldd.Name.Residue _ ->
        Ldd.node_of_var (Ldd.rigid name))
    n

(* Semantic (not structural) equality of two ikinds: mutual subsumption, modulo
   unknown-atom identity. Two derivations of the same jkind may differ
   structurally (fresh var ids) while denoting the same kind, and mutual [leq]
   is exactly the "they agree" property stage 2 relies on. *)
let ldd_semantically_equal (a : Ldd.node) (b : Ldd.node) : bool =
  Ldd.solve_pending ();
  let a = canonicalize_unknowns a in
  let b = canonicalize_unknowns b in
  (match Ldd.leq_with_reason a b with [] -> true | _ -> false)
  && match Ldd.leq_with_reason b a with [] -> true | _ -> false

(* One-directional [a <= b], modulo unknown-atom identity (same canonicalization
   as [ldd_semantically_equal]). *)
let ldd_leq (a : Ldd.node) (b : Ldd.node) : bool =
  Ldd.solve_pending ();
  let a = canonicalize_unknowns a in
  let b = canonicalize_unknowns b in
  match Ldd.leq_with_reason a b with [] -> true | _ -> false

(* Derive the ikind of [jkind] in [mode], either using stored decl ikinds
   ([use_stored:true]) or forcing a full recompute from each declaration
   ([use_stored:false]). Allocates its own solver context, so it must only be
   called at a top-level seam entry, never inside another derivation. *)
let derive_ikind ~(use_stored : bool) ?(trust_residue = false)
    ~(mode : Solver.mode) env (jkind : ('l * 'r) Types.jkind) : Ldd.node =
  let saved = !force_recompute_ikinds in
  let saved_tr = !trust_residue_stored in
  force_recompute_ikinds := not use_stored;
  trust_residue_stored := trust_residue;
  Fun.protect
    ~finally:(fun () ->
      force_recompute_ikinds := saved;
      trust_residue_stored := saved_tr)
    (fun () ->
      let ctx = create_ctx ~mode ~env:(Some env) in
      let node = Solver.ckind_of_jkind ctx jkind in
      Ldd.solve_pending ();
      node)

(* Assert that the stored-ikind derivation of [jkind] agrees with a full
   recompute, in both solver modes. Inert unless [ikinds_validate] is set. *)
let validate_ikind ~(in_sub_position : bool) ~(origin : string option) env
    (jkind : ('l * 'r) Types.jkind) : unit =
  if !Clflags.ikinds_validate
  then
    List.iter
      (fun mode ->
        let stored = derive_ikind ~use_stored:true ~mode env jkind in
        let recomputed = derive_ikind ~use_stored:false ~mode env jkind in
        incr validate_checks;
        if not (ldd_semantically_equal stored recomputed)
        then
          let mode_s =
            match mode with
            | Solver.Normal -> "normal"
            | Solver.Round_up -> "round_up"
          in
          (* Classify (see [validate_benign]): [recomputed <= stored] is the
             conservative over-approximation direction, but it is sound to
             whitelist ONLY in the SUB (LHS) position of [leq_with_reason sub
             super]. A larger stored SUB makes the check harder (over-reject,
             never unsound accept). In the SUPER (RHS) position a larger stored
             instead EASES the check -- and [sub_jkind_l] takes [super] as a
             [jkind_l], which can carry a with-bound and therefore diverge -- and
             [crossing_of_jkind] is not a subsumption check at all. So over-
             approximation at a super/crossing site is NOT provably benign;
             restrict the whitelist to [in_sub_position] and keep every
             super/crossing divergence a hard mismatch regardless of direction. *)
          if in_sub_position && ldd_leq recomputed stored
          then incr validate_benign
          else
            (* Stage 4b CLASS-B: is the divergence fully explained by
               recursive-module fixpoint-residue trust?  Re-derive the reference
               trusting the stored ikind ONLY for [Type_abstract Definition]
               decls whose stored ikind carries a foreign [Param].  If that
               residue-trusting reference matches the stored derivation, the
               whole
               divergence is residue-caused -- a from-scratch reference cannot
               reconstruct the declaration-time recursive-module fixpoint, so it
               is not a valid independent reference (the same trust boundary as
               CLASS-A).  Count and log it (auditable) rather than hard-fail; a
               deliberately-wrong residue stored ikind still lands here. *)
            let recomputed_residue =
              derive_ikind ~use_stored:false ~trust_residue:true ~mode env jkind
            in
            if ldd_semantically_equal stored recomputed_residue
            then (
              incr validate_class_b;
              Format.eprintf
                "[ikind-validate] CLASS-B%s mode=%s (recursive-module \
                 fixpoint-residue: stored trusted, not independently \
                 validated)@;\
                 @;\
                 stored=%s@;\
                 recompute=%s@."
                (origin_suffix_of origin) mode_s (Ldd.pp stored)
                (Ldd.pp recomputed))
            else (
              incr validate_mismatches;
              Format.eprintf
                "[ikind-validate] MISMATCH%s mode=%s@;\
                 @;\
                 stored=%s@;\
                 recompute=%s@."
                (origin_suffix_of origin) mode_s (Ldd.pp stored)
                (Ldd.pp recomputed)))
      [Solver.Normal; Solver.Round_up]

let compute_subcheck_polys ~context:_ env (sub : ('l1 * 'r1) Types.jkind)
    (super : ('l2 * 'r2) Types.jkind) : subcheck_polys =
  let ctx = create_ctx ~mode:Solver.Normal ~env:(Some env) in
  let super_poly = Solver.ckind_of_jkind ctx super in
  let super_is_constant =
    Ldd.solve_pending ();
    Ldd.is_const super_poly
  in
  if
    super_is_constant
    && Axis_lattice.equal (Ldd.round_up super_poly) Axis_lattice.top
  then
    { lhs_for_leq = Ldd.bot;
      rhs_for_leq = super_poly;
      fast_path = Rhs_top_fast_path
    }
  else
    (* Stage-5c: the legacy [mod_bounds]-floor fast path is gone (§C.2
       differential proved it verdict-equivalent to this full derivation
       corpus-wide).  When [super] is constant we derive the sub polynomial in
       [Round_up] mode, exactly as before; the LDD is now the sole answering
       engine for the floor. *)
    let sub_ctx =
      if super_is_constant
      then Solver.reset_for_mode ctx ~mode:Solver.Round_up
      else ctx
    in
    let sub_poly = Solver.ckind_of_jkind sub_ctx sub in
    { lhs_for_leq = sub_poly;
      rhs_for_leq = super_poly;
      fast_path = No_fast_path
    }

(* Stage-5d S4: ikind sub verdict for [combine_histories]' history-ordering
   differential, installed into [Jkind.set_sub_verdict_from_ikind].  Returns the
   ikind verdict [Less]/[Equal]/[Not_le] for [a] vs [b] by deriving both
   polynomials and comparing them with [Ldd.leq_with_reason] in both directions
   (M4's intended replacement for the legacy [Jkind_desc.sub] there).
   [combine_histories] runs mid-check, so the derivation uses a SCRATCH ctx +
   [with_isolated_pending] (the 5a re-entrancy helpers) and cannot perturb the
   outer solve.  Returns [None] (=> the differential skips this combine) when
   ikinds are disabled or the derivation raises. *)
let sub_verdict_for_history : type la ra lb rb.
    context:Jkind.jkind_context ->
    Env.t ->
    (la * ra) Types.jkind ->
    (lb * rb) Types.jkind ->
    Misc.Le_result.t option =
 fun ~context:_ env a b ->
  match
    Ldd.with_isolated_pending (fun () ->
        (* Derive both polynomials in ONE scratch ctx (Normal mode) so shared
             type params get identical rigid names, then raw [leq_with_reason]
             both directions -- the ordering comparison M4 would use, without the
             one-directional sub-check fast paths (Rhs_top/round-up) that would
             coarsen an order into spurious Equals. *)
        let ctx = create_scratch_ctx ~mode:Solver.Normal ~env:(Some env) in
        let a_poly = Solver.ckind_of_jkind ctx a in
        let b_poly = Solver.ckind_of_jkind ctx b in
        Ldd.solve_pending ();
        let leq x y =
          match Ldd.leq_with_reason x y with [] -> true | _ -> false
        in
        leq a_poly b_poly, leq b_poly a_poly)
  with
  | exception _ -> None
  | true, true -> Some Misc.Le_result.Equal
  | true, false -> Some Misc.Le_result.Less
  | false, _ -> Some Misc.Le_result.Not_le

let () =
  Jkind.set_sub_verdict_from_ikind { Jkind.verdict = sub_verdict_for_history }

let sub_jkind_l ?allow_any_crossing ?origin
    ~type_equal:(_ : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) env (sub : Types.jkind_l)
    (super : Types.jkind_l) : (unit, Jkind.Violation.t) result =
  let open Misc.Stdlib.Monad.Result.Syntax in
  let () =
    if !Clflags.ikinds_validate
    then (
      validate_ikind ~in_sub_position:true ~origin env sub;
      validate_ikind ~in_sub_position:false ~origin env super)
  in
  (* Check layouts first; if that fails, print both sides with full
       info and return the error. *)
  let* () =
    match Jkind.sub_layout_or_error ~context env sub super with
    | Ok () -> Ok ()
    | Error v -> Error v
  in
  let allow_any =
    match allow_any_crossing with Some true -> true | _ -> false
  in
  if allow_any
  then (
    (if !Clflags.ikinds_debug
     then
       let origin_suffix = origin_suffix_of origin in
       Format.eprintf "[ikind-subjkind] call%s allow_any=true@." origin_suffix);
    Ok ())
  else
    let { lhs_for_leq = sub_poly; rhs_for_leq = super_poly; fast_path } =
      compute_subcheck_polys ~context env sub super
    in
    let violating_axes = Ldd.leq_with_reason sub_poly super_poly in
    (if !Clflags.ikinds_debug
     then
       let origin_suffix = origin_suffix_of origin in
       let fast_path =
         match fast_path with
         | No_fast_path -> "none"
         | Rhs_top_fast_path -> "rhs_top"
       in
       Format.eprintf
         "[ikind-subjkind] call%s allow_any=false fast_path=%s@;\
          @;\
          sub_poly=%s@;\
          super_poly=%s@."
         origin_suffix fast_path (Ldd.pp sub_poly) (Ldd.pp super_poly));
    match violating_axes with
    | [] -> Ok ()
    | _ ->
      let () =
        if !Clflags.ikinds_debug
        then
          let axes = pp_axes violating_axes in
          Format.eprintf "[ikind-subjkind] failure on axes: %s@." axes
      in
      (* Do not try to adjust allowances; Violation.Not_a_subjkind
           accepts an r-jkind. *)
      let axis_reasons = axis_disagreement_reasons violating_axes in
      Error
        (Jkind.Violation.of_ ~context env
           (Jkind.Violation.Not_a_subjkind (sub, super, axis_reasons)))

let crossing_of_jkind ~context:(_ : Jkind.jkind_context) env
    (jkind : ('l * 'r) Types.jkind) : Mode.Crossing.t =
  let () =
    if !reentrancy_probe && not !reentrancy_probe_done
    then begin
      reentrancy_probe_done := true;
      (* Sandbox the whole probe so it cannot perturb the live check it runs
         inside: [with_isolated_pending] restores the check's pending list. *)
      Ldd.with_isolated_pending (fun () ->
          (* Run the print-path derivation core (scratch ctx + solve) as the
             derivers do, ONCE without and ONCE with [with_isolated_pending],
             each against a fresh outer pending gfp [v].  The scratch ctx keeps
             the Solver caches safe in BOTH cases (cache eviction is measured
             corpus-wide by the [ctx-evicted] counter); the gfp DRAIN is the H2
             signal isolated here: un-isolated, the print's [solve_pending]
             drains the outer [v]; isolated, [v] survives. *)
          let drain ~isolate =
            let v = Ldd.new_var () in
            Ldd.enqueue_gfp v (Ldd.const Axis_lattice.bot);
            let p0 = Ldd.pending_count () in
            let body () =
              let pctx =
                create_print_ctx ~mode:Solver.Round_up ~env:(Some env)
              in
              try
                ignore
                  (Solver.round_up (Solver.ckind_of_jkind pctx jkind)
                    : Axis_lattice.t)
              with _ -> ()
            in
            if isolate then Ldd.with_isolated_pending body else body ();
            let p1 = Ldd.pending_count () in
            p0, p1
          in
          let a0, a1 = drain ~isolate:false in
          let b0, b1 = drain ~isolate:true in
          Format.eprintf
            "[ik5a-reentrancy] gfp drain by a mid-check print: un-isolated \
             %d->%d (drained=%d, the H2 hazard); isolated %d->%d (drained=%d, \
             fixed) -- %s@."
            a0 a1
            (max 0 (a0 - a1))
            b0 b1
            (max 0 (b0 - b1))
            (if a1 < a0 && b1 = b0
             then "OK (with_isolated_pending prevents the drain)"
             else "UNEXPECTED"))
    end
  in
  let () =
    if !Clflags.ikinds_validate
    then
      validate_ikind ~in_sub_position:false ~origin:(Some "crossing") env jkind
  in
  let with_bounds_is_empty : type l r. (l * r) Types.with_bounds -> bool =
    function
    | No_with_bounds -> true
    | With_bounds _ -> false
  in
  match jkind.jkind.base with
  | Types.Layout _ when with_bounds_is_empty jkind.jkind.with_bounds ->
    (* Stage-5d S1: the mode crossing of a with-bounds-free jkind is the
         crossing of its lattice floor -- the ikind const base, which STAGE5C
         proved equals [to_axis_lattice mod_bounds] for this class.  Read it
         directly (no legacy [normalize], no LDD build). *)
    let floor = Jkind.Mod_bounds.to_axis_lattice jkind.jkind.mod_bounds in
    Axis_lattice.to_mode_crossing floor
  | _ ->
    let ctx = create_ctx ~mode:Solver.Round_up ~env:(Some env) in
    let lat = Solver.round_up (Solver.ckind_of_jkind ctx jkind) in
    Axis_lattice.to_mode_crossing lat

let round_up_type env (ty : Types.type_expr) : Axis_lattice.t =
  let ctx = create_ctx ~mode:Solver.Round_up ~env:(Some env) in
  Solver.round_up (Solver.kind ~use_tables:false ctx ty)

let crossing_of_type env (ty : Types.type_expr) : Mode.Crossing.t =
  let lat = round_up_type env ty in
  Axis_lattice.to_mode_crossing lat

type sub_or_intersect = Jkind.sub_or_intersect

let with_bounds_is_empty : type l r. (l * r) Types.with_bounds -> bool =
  function
  | Types.No_with_bounds -> true
  | Types.With_bounds _ -> false

let fast_sub_of_value_sub : type r.
    Axis_lattice.t -> (Allowance.allowed * r) Types.jkind -> bool =
 fun super_lat (sub : (Allowance.allowed * r) Types.jkind) ->
  if Axis_lattice.equal super_lat Axis_lattice.top
  then true
  else if not (with_bounds_is_empty sub.jkind.with_bounds)
  then false
  else
    let sub_lat = Jkind.Mod_bounds.to_axis_lattice sub.jkind.mod_bounds in
    Axis_lattice.leq sub_lat super_lat

let fast_sub_of_any_super : type r.
    Types.mod_bounds -> (Allowance.allowed * r) Types.jkind -> bool =
 fun mod_bounds sub ->
  match sub.jkind.base with
  | Types.Layout
      (Jkind_types.Layout.Sort (_sub_sort, { nullability = _; separability = _ }))
    ->
    fast_sub_of_value_sub (Jkind.Mod_bounds.to_axis_lattice mod_bounds) sub
  | Types.Layout _ | Types.Kconstr _ -> false

let fast_sub_of_sort_super : type r.
    Jkind_types.Sort.t ->
    Types.mod_bounds ->
    (Allowance.allowed * r) Types.jkind ->
    bool =
 fun super_sort mod_bounds sub ->
  match sub.jkind.base with
  | Types.Layout
      (Jkind_types.Layout.Sort (sub_sort, { nullability = _; separability = _ }))
    ->
    if not (Jkind_types.Sort.equate sub_sort super_sort)
    then false
    else fast_sub_of_value_sub (Jkind.Mod_bounds.to_axis_lattice mod_bounds) sub
  | Types.Layout _ | Types.Kconstr _ -> false

let fast_sub : type r1 l2.
    context:Jkind.jkind_context ->
    Env.t ->
    (Allowance.allowed * r1) Types.jkind ->
    (l2 * Allowance.allowed) Types.jkind ->
    bool =
 fun ~context:_ _env (sub : (Allowance.allowed * r1) Types.jkind)
     (super : (l2 * Allowance.allowed) Types.jkind) ->
  match super.jkind with
  | { base =
        (* CR rtjoa for jujacobs: I guessed you want [max] here? *)
        Types.Layout
          (Jkind_types.Layout.Sort
             ( super_sort,
               { separability = Jkind_axis.Separability.Maybe_separable;
                 nullability = Jkind_axis.Nullability.Maybe_null
               } ));
      mod_bounds;
      with_bounds = Types.No_with_bounds;
      _
    } ->
    fast_sub_of_sort_super super_sort mod_bounds sub
  | { base =
        Types.Layout
          (Jkind_types.Layout.Any
             { separability = Jkind_axis.Separability.Maybe_separable;
               nullability = Jkind_axis.Nullability.Maybe_null
             });
      mod_bounds;
      with_bounds = Types.No_with_bounds;
      _
    } ->
    fast_sub_of_any_super mod_bounds sub
  | _ -> false

let sub_or_intersect ?origin
    ~type_equal:(_ : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) env
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) : sub_or_intersect =
  let debug_polys ?polys ~outcome () =
    if !Clflags.ikinds_debug
    then
      let sub_poly, super_poly =
        match polys with
        | Some polys -> polys
        | None ->
          let subcheck = compute_subcheck_polys ~context env t1 t2 in
          subcheck.lhs_for_leq, subcheck.rhs_for_leq
      in
      let origin_suffix = origin_suffix_of origin in
      Format.eprintf
        "[ikind-sub-or-intersect] outcome=%s%s@;@;sub_poly=%s@;super_poly=%s@."
        outcome origin_suffix (Ldd.pp sub_poly) (Ldd.pp super_poly)
  in
  let generic_sub_or_intersect () =
    (* Old behavior adapted to abstract kinds:
       1) gate on env-aware layout subchecking
       2) if layouts are compatible, decide based on ikind polynomials *)
    match Jkind.sub_layout_or_error ~context env t1 t2 with
    | Error _ ->
      (* Layouts are incompatible, so the sub necessarily fails.  Classify
         Disjoint vs May_have_intersection via [may_have_intersection] (a [Base]
         intersection question) and build the failure reason ikind-natively: a
         layout disagreement is what a layout-incompatible pair fails on. *)
      let reasons : Jkind.Sub_failure_reason.t Misc.Nonempty_list.t =
        [Jkind.Sub_failure_reason.Layout_disagreement]
      in
      if Jkind.may_have_intersection env t1 t2
      then (
        debug_polys ~outcome:"May_have_intersection" ();
        Jkind.May_have_intersection reasons)
      else (
        debug_polys ~outcome:"Disjoint" ();
        Jkind.Disjoint reasons)
    | Ok () -> (
      let subcheck = compute_subcheck_polys ~context env t1 t2 in
      let sub_poly = subcheck.lhs_for_leq in
      let super_poly = subcheck.rhs_for_leq in
      match Ldd.leq_with_reason sub_poly super_poly with
      | [] ->
        debug_polys ~polys:(sub_poly, super_poly) ~outcome:"Sub" ();
        Jkind.Sub
      | violating_axes ->
        (if !Clflags.ikinds_debug
         then
           let axes = pp_axes violating_axes in
           Format.eprintf
             "[ikind-sub-or-intersect] outcome=May_have_intersection \
              axes=[%s]@."
             axes);
        debug_polys ~polys:(sub_poly, super_poly)
          ~outcome:"May_have_intersection" ();
        let reasons : Jkind.Sub_failure_reason.t Misc.Nonempty_list.t =
          match axis_disagreement_reasons violating_axes with
          | [] -> [Jkind.Sub_failure_reason.Layout_disagreement]
          | hd :: tl -> hd :: tl
        in
        Jkind.May_have_intersection reasons)
  in
  if !Clflags.ikinds_validate
  then (
    validate_ikind ~in_sub_position:true ~origin env t1;
    validate_ikind ~in_sub_position:false ~origin env t2);
  if fast_sub ~context env t1 t2
  then (
    (if !Clflags.ikinds_debug
     then
       let origin_suffix = origin_suffix_of origin in
       Format.eprintf "[ikind-sub-or-intersect] outcome=Sub%s fast_sub=true@."
         origin_suffix);
    Jkind.Sub)
  else generic_sub_or_intersect ()

let sub_or_error ?origin
    ~type_equal:(_ : Types.type_expr -> Types.type_expr -> bool)
    ~(context : Jkind.jkind_context) env
    (t1 : (Allowance.allowed * 'r1) Types.jkind)
    (t2 : ('l2 * Allowance.allowed) Types.jkind) :
    (unit, Jkind.Violation.t) result =
  let () =
    if !Clflags.ikinds_validate
    then (
      validate_ikind ~in_sub_position:true ~origin env t1;
      validate_ikind ~in_sub_position:false ~origin env t2)
  in
  (* The ikind engine owns the verdict AND the error.  Layout check first: on
       failure return that layout violation (as [sub_jkind_l] does).  Otherwise
       the modal-axis subcheck decides; a non-empty violating-axes list is the
       reject, synthesized ikind-natively into [Not_a_subjkind]. *)
  match Jkind.sub_layout_or_error ~context env t1 t2 with
  | Error _ as layout_err -> layout_err
  | Ok () -> (
    let { lhs_for_leq = sub_poly; rhs_for_leq = super_poly; _ } =
      compute_subcheck_polys ~context env t1 t2
    in
    match Ldd.leq_with_reason sub_poly super_poly with
    | [] -> Ok ()
    | violating_axes ->
      let reasons = axis_disagreement_reasons violating_axes in
      Error
        (Jkind.Violation.of_ ~context env
           (Jkind.Violation.Not_a_subjkind (t1, t2, reasons))))

(** Substitute constructor ikinds according to [lookup] without requiring Env.
*)

let poly_of_type_function_in_identity_env ~(params : Types.type_expr list)
    ~(body : Types.type_expr) : Ldd.node * Ldd.node array =
  (* Approximate type-function substitution by evaluating in an identity
     environment, i.e. every constructor contributes an independent rigid
     atom. *)
  let ctx = create_ctx ~mode:Solver.Normal ~env:None in
  let poly = Solver.normalize (Solver.kind ~use_tables:true ctx body) in
  let rigid_vars =
    List.map (fun ty -> Ldd.rigid (Ldd.Name.param (Types.get_id ty))) params
  in
  let base, coeffs =
    Ldd.decompose_into_linear_terms ~universe:rigid_vars poly
  in
  base, Array.of_list coeffs

(* Full path of the unit currently being compiled/saved, used to unit-qualify
   [Residue] atoms on the cmi save path (stage 5b).  Same identity the stage-4d
   import detector uses ([full_path_as_string]). *)
let current_unit_full_path () : string =
  Compilation_unit.full_path_as_string
    (Compilation_unit.get_current_or_dummy ())

let substitute_decl_ikind_with_lookup
    ~(lookup_type : Path.t -> Subst.Ikind_substitution.type_lookup_result)
    ~(lookup_jkind : Path.t -> Subst.Ikind_substitution.jkind_lookup_result)
    ~(for_saving : bool) (ikind_entry : Types.type_ikind) : Types.type_ikind =
  (* Inline type functions in an identity environment (no Env). *)
  match ikind_entry with
  | No_constructor_ikind _ -> ikind_entry
  | Saved_ikind _ ->
    (* Live code only ever holds Constructor_ikind (deserialize rehydrates);
       a Saved_ikind reaching subst is a stage-5 format-lock-in bug. *)
    Misc.fatal_error
      "ikind: Saved_ikind reached substitute_decl_ikind_with_lookup"
  | Constructor_ikind packed ->
    let payload = packed in
    let memo : (Path.t, Ldd.node * Ldd.node array) Hashtbl.t =
      Hashtbl.create 17
    in
    (* Rewrite a polynomial by mapping each rigid atom through [lookup]. *)
    let rec map_poly (expanding : Path.Set.t) (poly : Ldd.node) : Ldd.node =
      Ldd.map_rigid (map_name expanding) poly
    and map_name (expanding : Path.Set.t) (name : Ldd.Name.t) : Ldd.node =
      match name with
      | Param id when for_saving ->
        (* Stage 5b soundness gate: neutralize a foreign [Param] residue to a
           unit-qualified [Residue] on the cmi save path ONLY.  Stored decl
           ikinds are otherwise Param-free (own params are factored positionally
           by [decompose_into_linear_terms]), so any [Param] reaching a save is a
           recursive-module fixpoint residue carrying a stale live-[type_expr] id.
           Tagging it with the defining unit makes it collision-free BY
           CONSTRUCTION (an importer's live [Param n] can never alias
           [Residue {unit; n}]) and keeps it recognizable for CLASS-B (a distinct
           constructor).  Within-unit ([for_saving]=false) the [Param] is kept --
           the fixpoint value is meaningful there (stage 4b). *)
        if !Clflags.ikinds_validate || !Clflags.ikinds_debug
        then incr residue_neutralized;
        Ldd.node_of_var
          (Ldd.rigid (Ldd.Name.residue (current_unit_full_path ()) id))
      | Param _ -> Ldd.node_of_var (Ldd.rigid name)
      | Unknown _ -> Ldd.node_of_var (Ldd.rigid name)
      | Residue _ -> Ldd.node_of_var (Ldd.rigid name)
      | KAtom path -> (
        match lookup_jkind path with
        | Subst.Ikind_substitution.Lookup_jkind_identity ->
          Ldd.node_of_var (Ldd.rigid name)
        | Subst.Ikind_substitution.Lookup_jkind_path alias_path ->
          Ldd.node_of_var (Ldd.rigid (Ldd.Name.katom alias_path))
        | Subst.Ikind_substitution.Lookup_jkind_const jkind_const ->
          let raw =
            let ctx = create_ctx ~mode:Solver.Normal ~env:None in
            Solver.normalize (Solver.ckind_of_jkind_desc ctx jkind_const)
          in
          map_poly expanding raw)
      | Atom { constr = path; arg_index } -> (
        match lookup_type path with
        | Subst.Ikind_substitution.Lookup_identity ->
          Ldd.node_of_var (Ldd.rigid name)
        | Subst.Ikind_substitution.Lookup_path alias_path ->
          Ldd.node_of_var (Ldd.rigid (Ldd.Name.atomic alias_path arg_index))
        | Subst.Ikind_substitution.Lookup_type_fun (params, body) ->
          (* Inline a type function by evaluating it in an identity
             environment.  The [expanding] set prevents infinite
             unfolding of recursive type functions. *)
          if Path.Set.mem path expanding
          then Ldd.node_of_var (Ldd.rigid name)
          else
            let base_raw, coeffs_raw =
              (* Memoized by [path] to avoid recomputation *)
              match Hashtbl.find_opt memo path with
              | Some v -> v
              | None ->
                let v = poly_of_type_function_in_identity_env ~params ~body in
                Hashtbl.add memo path v;
                v
            in
            let expanding = Path.Set.add path expanding in
            let base = map_poly expanding base_raw in
            let coeffs = Array.map (map_poly expanding) coeffs_raw in
            if arg_index = 0
            then base
            else if arg_index - 1 < Array.length coeffs
            then coeffs.(arg_index - 1)
            else
              (* Fallback: if coefficient missing, keep original
                 atom. *)
              Ldd.node_of_var (Ldd.rigid name))
    in
    let base_poly = map_poly Path.Set.empty payload.base in
    let coeffs_poly = Array.map (map_poly Path.Set.empty) payload.coeffs in
    (* Stage-4d cross-unit seeded fault: corrupt the persisted ikind (base ->
       bottom) so an importer loads a wrong stored value while the defining
       unit's legacy fields stay intact.  See [save_fault]. *)
    let base_poly = if for_saving && !save_fault then Ldd.bot else base_poly in
    let payload = constructor_ikind ~base:base_poly ~coeffs:coeffs_poly in
    Types.Constructor_ikind payload

let () =
  Subst.Ikind_substitution.substitute_decl_ikind_with_lookup
    := substitute_decl_ikind_with_lookup
