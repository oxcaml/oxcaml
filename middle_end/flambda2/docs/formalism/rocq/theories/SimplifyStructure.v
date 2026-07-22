(* SimplifyStructure.v -- ch. 09 (09-simplify-structure.md): the
   structure of the Simplify pass.  All 37 S.Struct.* rules.
   Owner: Girard (wave 4).
   Imports: Base, Syntax, TypeGrammar, MeetJoin (for nway_join).

   Contents:
   - Section 1: syntactic helpers (static-const placement predicates,
     per-parameter column extraction for joins).
   - Sections 2-7 follow the doc's sections 1-6; each rule appears in
     doc order under its RULE/STATUS/CODE comment.
   - Real artifacts: S.Struct.Run.ClosedResult,
     S.Struct.Run.NoPendingConstants, S.Struct.Lift.EmptyAtEnd (the
     unit contracts, consumed by Simplify.v's SU_intro), plus
     S.Struct.JoinParams and S.Struct.Rec.NoFixpoint (provided for
     chs. 10/12, currently unconsumed -- those chapters quantify
     entry types directly).  Everything else is a documented
     anchor. *)

(* ENCODING NOTE (file-wide; coordinator-approved status-mapping
   deviation): ch. 09 documents the Simplify ALGORITHM -- the
   downwards/upwards traversal, its accumulators (dacc/uacc), the
   dataflow analysis at the turn, and the lifting machinery.  The
   mechanization deliberately does not model that algorithm: the
   model's simplifier is a rewrite relation (chs. 10-12, unioned and
   closed in Simplify.v), and dacc/uacc/flow_acc are compiler state
   with no term-model counterpart.  Under the approved decision rule
   ("a ch. 09 rule gets a real Prop iff another chapter leans on it"),
   exactly five rules get real artifacts here (list in the header);
   the remaining 32 -- INCLUDING their normative and conjectured
   members, which the standard status mapping would otherwise make
   Theorems or constructors -- are carried as documented anchors.
   Each anchor's rule header keeps its true STATUS, so the coverage
   audit still sees the doc's statuses. *)

From Stdlib Require Import ZArith List Bool.
From Flambda2 Require Import Base Syntax TypeGrammar MeetJoin.
Import ListNotations.

(* ================================================================== *)
(* 1. Helpers                                                         *)
(* ================================================================== *)

(* Does an expression contain a static-const binding (a Let of
   N_static_consts) anywhere, including inside code bodies?
   named_has_statics does not recurse (the binding itself is the
   hit), so it is a plain Definition rather than a member of the
   mutual block below. *)
Definition named_has_statics (n : named) : bool :=
  match n with
  | N_static_consts _ => true
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ =>
      false
  end.

Fixpoint expr_has_statics (e : expr) : bool :=
  match e with
  | E_let _ d body => named_has_statics d || expr_has_statics body
  | E_let_cont_nonrec _ h body =>
      ch_has_statics h || expr_has_statics body
  | E_let_cont_rec _ hs body =>
      (fix go (l : list (continuation * cont_handler)) : bool :=
         match l with
         | [] => false
         | (_, h) :: r => ch_has_statics h || go r
         end) hs || expr_has_statics body
  | E_apply _ | E_apply_cont _ | E_switch _ | E_invalid _ => false
  end
with ch_has_statics (h : cont_handler) : bool :=
  match h with Mk_cont_handler _ e _ _ => expr_has_statics e end.

(* "No static-const binding under a lambda": every code body
   (Mk_code0's body, reachable only through SCC_code) contains no
   static-const binding at all.  This is the term-model residue of
   the lifted-constant placement discipline; see the rule headers on
   S.Struct.Run.NoPendingConstants / S.Struct.Lift.EmptyAtEnd and the
   S.Struct.Lift.PlaceAtToplevel anchor. *)
Definition scc_body_no_statics (s : static_const_or_code) : bool :=
  match s with
  | SCC_code c => negb (expr_has_statics (c0_body c))
  | SCC_deleted_code | SCC_static_const _ => true
  end.

Definition named_statics_toplevel (n : named) : bool :=
  match n with
  | N_static_consts scg => forallb scc_body_no_statics scg
  | N_simple _ | N_prim _ | N_set_of_closures _ _ | N_rec_info _ =>
      true
  end.

Fixpoint expr_statics_toplevel (e : expr) : bool :=
  match e with
  | E_let _ d body =>
      named_statics_toplevel d && expr_statics_toplevel body
  | E_let_cont_nonrec _ h body =>
      ch_statics_toplevel h && expr_statics_toplevel body
  | E_let_cont_rec _ hs body =>
      (fix go (l : list (continuation * cont_handler)) : bool :=
         match l with
         | [] => true
         | (_, h) :: r => ch_statics_toplevel h && go r
         end) hs && expr_statics_toplevel body
  | E_apply _ | E_apply_cont _ | E_switch _ | E_invalid _ => true
  end
with ch_statics_toplevel (h : cont_handler) : bool :=
  match h with Mk_cont_handler _ e _ _ => expr_statics_toplevel e end.

Definition all_statics_placed (u : flambda_unit) : Prop :=
  expr_statics_toplevel (fu_body u) = true.

(* Column j of a list of rows; None if some row is too short.  Used
   to state per-parameter joins over per-use argument-type rows. *)
Fixpoint column {A : Type} (j : nat) (rows : list (list A))
  : option (list A) :=
  match rows with
  | [] => Some []
  | row :: rest =>
      match nth_error row j, column j rest with
      | Some x, Some xs => Some (x :: xs)
      | _, _ => None
      end
  end.

(* ================================================================== *)
(* 2. Top-level contract (doc section 1)                              *)
(* ================================================================== *)

(** RULE S.Struct.Run (STATUS normative) -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify.ml#run
    CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0

    Simplify.run maps a flambda_unit to an optimized flambda_unit
    plus side results { free_names; final_typing_env; all_code;
    slot_offsets; unit }: it builds an initial downwards environment
    and accumulator, simplifies the unit body with simplify_toplevel
    against the unit's return/exception continuations, and returns
    the rebuilt body; final_typing_env is the env recorded at the
    single use of the return continuation.  Normative, but it fixes
    the INTERFACE AND ORCHESTRATION of the out-of-model algorithm
    (see the file-wide ENCODING NOTE); the model-side counterpart of
    "what run does" is the simplifies relation (Simplify.v), and the
    meaning-preservation claim is ch. 13's. *)
Definition S_Struct_Run_documented : Prop := True.

(** RULE S.Struct.Run.ClosedResult (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify.ml#run

    After simplification the only variables permitted to occur free
    in the unit's body are the unit's own region parameters; every
    other free name must be a symbol (run checks this and
    fatal-errors otherwise).  Stated as a predicate on the output
    unit; wave-6 files (Simplify.v / Soundness.v) quantify over it,
    and it matches OS.Unit.Init: an initial config references only
    names rho_0 binds, and rho_0 binds BOTH toplevel_my_region and
    toplevel_my_ghost_region.  ENCODING NOTE: the code's check is
    strictly TIGHTER than this predicate -- run's fatal-error path
    exempts ONLY toplevel_my_region (a free ghost region trips it
    too).  Permitting the ghost region here is a deliberate
    conclusion-side widening to Init's binding set: every unit the
    code accepts satisfies this predicate, and the widening only
    weakens what SU_intro's consumers may assume.  Free symbols
    are unconstrained (imports and the unit's own definitions are
    allowed).  The continuation conjunct bounds free continuations
    by the unit's return/exn parameters (what Init's K_0 binds),
    via the sanctioned free_conts Parameter (Syntax.v, the
    continuation-namespace sibling of free_vars). *)
Definition S_Struct_Run_ClosedResult (u : flambda_unit) : Prop :=
  (forall x, free_vars (fu_body u) x ->
     x = fu_toplevel_my_region u \/
     x = fu_toplevel_my_ghost_region u) /\
  (forall k, free_conts (fu_body u) k ->
     k = fu_return_continuation u \/
     k = fu_exn_continuation u).

(** RULE S.Struct.Run.NoPendingConstants (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify.ml#run
    CODE middle_end/flambda2/simplify/lifting/lifted_constant_state.mli

    LCS.is_empty (UA.lifted_constants uacc) at the end of run: every
    lifted constant discovered during simplification has been placed
    as a "let symbol" binding by the time the upwards pass reaches
    the top of the unit.
    ENCODING NOTE: the lifted-constant accumulator is algorithm state
    with no term-model counterpart -- in a term, a constant exists
    only where it is placed.  The model-visible content of this rule
    (together with S.Struct.Lift.EmptyAtEnd and the placement policy
    S.Struct.Lift.PlaceAtToplevel) is that the OUTPUT unit carries no
    static-const binding under a lambda: all_statics_placed.  Stated
    as a predicate on the output unit for wave-6 use. *)
Definition S_Struct_Run_NoPendingConstants (u : flambda_unit)
  : Prop :=
  all_statics_placed u.

(** RULE S.Struct.SingleRound (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/flambda2.ml#flambda_to_flambda0
    CODE middle_end/flambda2/simplify/simplify.ml#run

    flambda_to_flambda0 sets round = 0 and calls Simplify.run exactly
    once per unit; there is no outer loop re-running Simplify to a
    whole-unit fixpoint.  Bounded LOCAL re-simplification exists
    (S.Struct.Resimplify, S.Struct.InlineResimplify) and recursive
    continuations are handled in a single traversal
    (S.Struct.Rec.NoFixpoint), but none is a whole-unit fixpoint.
    Anchor: a claim about the driver, out of model scope. *)
Definition S_Struct_SingleRound_documented : Prop := True.

(* ================================================================== *)
(* 3. Two-phase structure (doc section 2)                             *)
(* ================================================================== *)

(** RULE S.Struct.TwoPhase (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_expr
    CODE middle_end/flambda2/simplify/simplify_common.mli

    Every expression is simplified in a downwards phase (scope-order
    traversal accumulating a downwards accumulator dacc) followed by
    an upwards phase (bottom-up rebuild against an upwards
    accumulator uacc), woven by two continuation layers: the
    term-level down_to_up and the compiler-level rebuild /
    after_rebuild.  Shape: down : dacc -> (uacc -> (e', uacc'));
    up : uacc -> (e', uacc').  The downwards ENVIRONMENT (denv) dies
    at a terminator; the downwards ACCUMULATOR threads through the
    whole traversal. *)
Definition S_Struct_TwoPhase_documented : Prop := True.

(** RULE S.Struct.Dacc (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#t
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#t

    dacc = { denv; continuation_uses_env; shareable_constants;
    used_value_slots; lifted_constants; flow_acc;
    demoted_exn_handlers; code_ids_to_remember/_to_never_delete/
    _never_simplified; slot_offsets; debuginfo_rewrites;
    are_lifting_conts; lifted_continuations;
    continuation_lifting_budget; continuations_to_specialize;
    specialization_map }.  denv carries typing_env (the abstract
    domain, chs. 07-08), round, machine_width, inlining_state,
    disable_inlining, at_unit_toplevel, cse/comparison_results,
    are_rebuilding_terms, closure_info, all_code, loopify_state,
    replay_history, specialization_cost, join_analysis, and the
    continuation-lifting fields.  denv behaves like a conventional
    typing environment (discarded at terminators); the other dacc
    fields persist. *)
Definition S_Struct_Dacc_documented : Prop := True.

(** RULE S.Struct.Uacc (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/env/upwards_acc.ml#t
    CODE middle_end/flambda2/simplify/env/upwards_acc.ml#create

    uacc = { uenv; creation_dacc; lifted_constants; all_code;
    name_occurrences; cost_metrics; slot_offsets; flow_result;
    resimplify }.  Created at the turn by UA.create ~flow_result uenv
    dacc; it embeds the creating dacc (creation_dacc) so upwards code
    can still read downwards information.  name_occurrences and
    cost_metrics are cleared/restored across binders so each rebuilt
    subterm is measured in isolation; used_value_slots (in the dacc)
    is accumulated and never save/restored. *)
Definition S_Struct_Uacc_documented : Prop := True.

(** RULE S.Struct.TypesMonotoneDown (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_variable
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_equation_on_name

    Umbrella intuition: during a downwards descent the typing env
    only gains equations, and the type of an in-scope name only moves
    down the lattice.  The doc records this rule as SPLIT: the
    algorithmic half holds and is S.Struct.EnvRefineOnly; the
    semantic half (gamma of a name only shrinks) holds for the alias
    sub-domain (INV.Simplify.AliasesMonotoneDown, ch. 13) but is
    REFUTED for stored concrete types at the non-GLB meet corner
    (T.Meet.MutableBlockMissedBottom).  Soundness is unaffected
    (types may soundly move up); the casualty is only the
    "prover queries stay successful" argument for non-alias
    witnesses.  Anchor: a claim about the algorithm's env-update
    discipline (see file-wide ENCODING NOTE). *)
Definition S_Struct_TypesMonotoneDown_documented : Prop := True.

(** RULE S.Struct.EnvRefineOnly (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#with_typing_env
    CODE middle_end/flambda2/types/env/meet_env.ml#add_concrete_equation_on_canonical
    CODE middle_end/flambda2/types/meet_and_join.ml#meet_head_of_kind_value_non_null
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env

    (a) Algorithmic monotonicity: every env installed downwards is
    obtained from the one it replaces exclusively by definitions,
    add_equation / add_env_extension (meets onto canonicals), or
    code-age extension -- except the two sites installing the env of
    a DIFFERENT program point (handler-entry env from a join; the
    recorded use-env lineage, incl. replay), both themselves derived
    monotonically from the fork/recorded env.  (b) Negative half: the
    semantic reading gamma_{E'}(x) subset gamma_E(x) does NOT follow,
    because add_concrete_equation_on_canonical stores the raw meet
    result and meet is not a lower bound in the Variant/Mutable_block
    corner (T.Meet.MutableBlockMissedBottom); no reachable Simplify
    trigger is known, so it survives as an empirical per-run
    invariant.  Anchor: quantifies over the algorithm's env
    installation sites (see file-wide ENCODING NOTE). *)
Definition S_Struct_EnvRefineOnly_documented : Prop := True.

(** RULE S.Struct.SetOfClosuresEager (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_expr.mli
    CODE middle_end/flambda2/simplify/simplify_named.ml#simplify_named0
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml#simplify_non_lifted_set_of_closures
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common

    Exception to "all downwards, then all upwards": when a Let binds
    a Set_of_closures, each function body is simplified FULLY (down
    and up, via simplify_function_body = simplify_toplevel_common)
    during the downwards pass of the enclosing expression, before the
    Let's body is traversed -- so the Flambda type of the closure
    records the already-simplified body, and later inlining inlines
    optimized code.  Cost: function-body simplification cannot use
    whole-program usage information that only becomes available
    later.  Anchor: normative about traversal ORDER, an
    algorithm-only notion (file-wide ENCODING NOTE); the model-side
    rewrite relations are order-free. *)
Definition S_Struct_SetOfClosuresEager_documented : Prop := True.

(* ================================================================== *)
(* 4. Continuation handling (doc section 3)                           *)
(* ================================================================== *)

(** RULE S.Struct.LetCont.BodyFirst (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_let_cont0
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body

    To simplify (let_cont k params = handler in body):
    save-and-clear the lifted constants; traverse body downwards,
    during which every (apply_cont k args) records a use of k
    carrying (env_at_use, arg_types); form the handler's entry
    environment from the recorded uses (the join,
    S_Struct_JoinParams below); traverse the handler downwards; on
    the way up, rebuild handler then body, deciding inlining /
    removal.  If k has no uses, its handler is not traversed at all
    and the Let_cont is dropped.  The continuation_uses_env is NOT
    cleared before the body (uses of enclosing in-scope continuations
    recorded in the body propagate upward); the only clear-to-empty
    is per-handler, at the start of the handler traversal.  Anchor:
    traversal staging, algorithm-only (file-wide ENCODING NOTE). *)
Definition S_Struct_LetCont_BodyFirst_documented : Prop := True.

(** RULE S.Struct.ContUse (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use
    CODE middle_end/flambda2/simplify/env/continuation_uses_env.mli
    CODE middle_end/flambda2/simplify/env/one_continuation_use.ml

    Each apply_cont / switch arm targeting an in-scope continuation k
    records a One_continuation_use.t = { env_at_use; id; use_kind;
    arg_types } into the continuation_uses_env, under a fresh rewrite
    id naming the use site for a later Apply_cont_rewrite
    (S.Struct.ApplyContRewrite); use_kind is Inlinable or
    Non_inlinable { escaping }, derived from the syntactic context.
    Anchor: accumulator bookkeeping.  Model-side residue: the rows of
    S_Struct_JoinParams' use_arg_types are exactly these recorded
    uses. *)
Definition S_Struct_ContUse_documented : Prop := True.

(** RULE S.Struct.Switch.ArmIsolation (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#record_continuation_use
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env

    Switch on a scrutinee with arms {i_1 |-> action_1, ...}: (i) the
    meet of the scrutinee type with arm i's discriminant yields
    env_i, recorded as arm i's continuation-use env but NEVER
    installed in the threaded dacc; (ii) hence a fact conditional on
    arm i reaches other program points ONLY through the join of the
    destination continuation's recorded uses; (iii) an arm whose meet
    is bottom is dropped WITHOUT recording a use, so statically-dead
    switch edges contribute nothing to any parameter join.
    Arm-conditional facts DO reach the arm's own action (sound: the
    arm runs only under its condition).  Anchor: quantifies over the
    algorithm's env threading (file-wide ENCODING NOTE); the
    model-visible residue is that dropped-arm rows are absent from
    S_Struct_JoinParams' use_arg_types.  Composes T.Meet.Bottom,
    T.Join.Levels, S.Struct.EnvRefineOnly. *)
Definition S_Struct_Switch_ArmIsolation_documented : Prop := True.

(** RULE S.Struct.JoinParams (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
    CODE middle_end/flambda2/simplify/join_points.ml#join
    CODE middle_end/flambda2/types/flambda2_types.mli#cut_and_n_way_join

    For a continuation k with recorded uses u_1 ... u_m (m >= 2, or
    m = 1 non-inlinable), each supplying argument types T^i_1 ...
    T^i_n for k's n parameters, the entry type of parameter j in the
    handler environment is the n-way join of the T^i_j over the use
    environments (T.cut_and_n_way_join), provided joining is enabled
    (Flambda_features.join_points (), or at most one use).  Stated
    over the data the model can see: use_arg_types has one row per
    recorded use, in the doc's u_1 ... u_m order; entry_types are the
    handler's parameter entry types; each column of use_arg_types
    joins to the corresponding entry type via MeetJoin's nway_join.
    The enabling flag and the fork-scope cutting are algorithm-side
    (the disabled case is S.Struct.NoJoinUnknown below). *)
Definition S_Struct_JoinParams
  (E : tenv) (use_arg_types : list (list ftype))
  (entry_types : list ftype) : Prop :=
  use_arg_types <> [] /\
  (forall row, In row use_arg_types ->
     length row = length entry_types) /\
  (forall j T_j, nth_error entry_types j = Some T_j ->
     exists col,
       column j use_arg_types = Some col /\
       nway_join E col T_j).

(* Short name provided for chs. 10/12; currently unconsumed (those
   chapters quantify entry types directly). *)
Abbreviation join_params := S_Struct_JoinParams.

(** RULE S.Struct.JoinParams.AnalysisExtraParams (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
    CODE middle_end/flambda2/simplify/join_points.ml#add_extra_params_from_join_analysis
    CODE middle_end/flambda2/types/join_levels.ml#cut_and_n_way_join
    CODE driver/oxcaml_flags.ml#o2

    The n-way join-analysis extra-param path is EXERCISED for k iff
    should_do_join (join_points () or #uses <= 1) holds AND
    join_algorithm () is N_way or Checked (Binary yields no
    analysis).  Under every standard profile join_algorithm stays
    Binary (only join_points flips at O2+), so the machinery is inert
    without an explicit -flambda2-join-algorithm n-way; with n-way
    selected, even a single non-inlinable use runs the analysis.
    -flambda2-match-in-match forces N_way but defaults false in every
    profile.  Anchor: flag plumbing over the out-of-model
    algorithm. *)
Definition S_Struct_JoinParams_AnalysisExtraParams_documented
  : Prop := True.

(** RULE S.Struct.NoJoinUnknown (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#add_parameters_with_unknown_types

    When joining is disabled (Flambda_features.join_points () false
    and k has >= 2 uses), each parameter of k is given the type
    unknown_with_subkind of its declared subkind, discarding
    argument-type information from the use sites
    (add_parameters_with_unknown_types, for both ordinary and extra
    params).  Anchor: the disabled-join branch of the algorithm; the
    model's rewrite relations never depend on it (an Unknown entry
    type licenses strictly fewer rewrites). *)
Definition S_Struct_NoJoinUnknown_documented : Prop := True.

(** RULE S.Struct.SingleInlinableUse (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env

    If k is non-recursive with exactly one use and that use is
    Inlinable, no join is performed: the handler is simplified
    directly in the use's own environment (augmented with the
    parameter equations and any lifted constants from the body) and
    the result is marked is_single_inlinable_use, a candidate for
    splicing at its single call site (decision and mechanism in
    ch. 10).  Exception handlers are never marked single-inlinable.
    Anchor: environment-management detail of the algorithm. *)
Definition S_Struct_SingleInlinableUse_documented : Prop := True.

(** RULE S.Struct.CSE.JoinPhi (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#join_one_cse_equation
    CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#cse_with_eligible_lhs
    CODE middle_end/flambda2/simplify/common_subexpression_elimination.ml#cut_cse_environment
    CODE middle_end/flambda2/simplify/join_points.ml#join

    With joining enabled, if a CSE-eligible primitive p maps -- after
    canonicalizing p's arguments per-use and rewriting them into fork
    scope -- to a value b_i on EVERY path into k, then the handler
    env's CSE table maps p to: b when all b_i = b (a fork-scope
    simple); else a FRESH continuation parameter v appended to k with
    b_i passed as an extra argument at each use -- a
    compiler-synthesized phi node.  An occurrence of p in k's handler
    is then replaced by v; if there is none, v is unused and
    dead-param elimination removes it (the introduction is
    self-cleaning).  One path lacking p kills the equation; equations
    already present at the fork never re-join.  Fires only when
    join_points is ON (-O2/-O3 set it).  Anchor: CSE-table state of
    the algorithm; the term-level effects it licenses are ch. 10's
    S.Rewrite.CSE.Eligible / S.Rewrite.CSE.Replace /
    S.Rewrite.LetCont.UnusedParam. *)
Definition S_Struct_CSE_JoinPhi_documented : Prop := True.

(** RULE S.Struct.Rec.NoFixpoint (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#prepare_dacc_for_handlers

    A recursive Let_cont is simplified in a single downwards
    traversal of each handler; there is no iteration to a typing
    fixpoint over the loop.  prepare_dacc_for_handlers computes the
    invariant-parameter entry types ONCE, from the uses seen while
    traversing the BODY, and each handler is then traversed exactly
    once; recursive calls discovered inside a handler only extend the
    set of reachable handlers still to traverse, never the parameter
    types already fixed.  Model-side content: the entry types are a
    function of the body-recorded uses ONLY -- the recursive uses are
    structurally ABSENT from the join's rows.  Stated by applying
    S_Struct_JoinParams to the body uses alone.  Scope: the rows
    carry the INVARIANT-parameter columns only; applied to a
    handler's variant-parameter columns the predicate would be wrong
    (those join over that handler's own uses, recursive ones
    included). *)
Definition S_Struct_Rec_NoFixpoint
  (E : tenv) (body_use_arg_types : list (list ftype))
  (entry_types : list ftype) : Prop :=
  S_Struct_JoinParams E body_use_arg_types entry_types.

(* Short name provided for the wave-5/6 files; currently
   unconsumed. *)
Abbreviation rec_invariant_entry := S_Struct_Rec_NoFixpoint.

(** RULE S.Struct.Rec.InvariantVsVariant (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_single_recursive_handler
    CODE middle_end/flambda2/simplify/join_points.ml#compute_handler_env

    For a recursive group with invariant params p and per-handler
    variant params q_i: the invariant params get the n-way join of
    the invariant arguments over the body-recorded uses, under the
    SAME should_do_join gate as S.Struct.JoinParams /
    S.Struct.NoJoinUnknown (so under default flags, a group entered
    from >= 2 body sites gets unknown_with_subkind instead); each
    handler's variant params always get unknown_with_subkind of their
    declared subkinds.  Soundness intuition: an invariant param is by
    construction passed the same value at every (including recursive)
    call, so a type valid at the body's entry uses remains valid; a
    variant param changes around the loop and is conservatively
    Unknown.  Anchor; the joined half is S_Struct_Rec_NoFixpoint
    above. *)
Definition S_Struct_Rec_InvariantVsVariant_documented : Prop := True.

(** RULE S.Struct.ApplyContRewrite (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/apply_cont_rewrite.mli
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_single_non_recursive_handler
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#make_rewrite_for_recursive_continuation
    CODE middle_end/flambda2/simplify/expr_builder.mli#rewrite_apply_cont

    When a handler is rebuilt, an Apply_cont_rewrite built from
    (original_params, extra_params_and_args, decide_param_usage) is
    registered in the upwards env under k; on the way up, each
    (apply_cont k args) at a recorded use id is rewritten: unused
    parameters' arguments are dropped and extra arguments (looked up
    by the use's rewrite id) are appended.  decide_param_usage
    classifies each param Used / Unused / Used_as_invariant from the
    rebuilt handler's free names (non-recursive) or the dataflow
    required-names set (recursive); for non-recursive handlers the
    code asserts the two agree.  Anchor: arity-change plumbing; the
    ch. 10 parameter rewrites state the use-site adjustment directly
    on the term. *)
Definition S_Struct_ApplyContRewrite_documented : Prop := True.

(** RULE S.Struct.LetCont.UnreachableClosure (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_handlers
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#simplify_recursive_handlers
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_arm
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#rebuild_let_cont

    Define the residual jump graph J over a body's continuations,
    with an edge for every apply_cont / switch-arm use actually
    RECORDED downwards (pruned switch arms and contexts truncated by
    an Invalid contribute no edges).  Then: (1) a handler is
    downwards-traversed iff its continuation is reachable from entry
    in J (recursive groups drop whole unreachable sub-SCCs); (2)
    every continuation unreachable in J is deleted from the output
    this same pass, its handler never simplified; (3) deadness is
    transitively closed within the pass -- an untraversed handler
    records no uses, so continuations used only from it are
    themselves unreachable -- a lazy reachability fixpoint computed
    by the traversal itself, with a known one-round lag for
    required_names-based decisions when switch arms merge on the
    upwards pass.  Anchor: quantifies over traversal state; the
    term-level deletions are ch. 10's S.Rewrite.LetCont.DeadHandler /
    S.Rewrite.Let.DeadBinding, which the model applies without a
    reachability oracle. *)
Definition S_Struct_LetCont_UnreachableClosure_documented
  : Prop := True.

(** RULE S.Struct.LiftCont.Gate (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/are_lifting_conts.mli#t
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#get_continuation_lifting_budget
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#after_downwards_traversal_of_body_and_handlers
    CODE middle_end/flambda2/simplify/simplify_switch_expr.ml#simplify_switch
    CODE driver/oxcaml_flags.ml#o2

    A let_cont k' nested inside the handler of let_cont k can be
    lifted out of k only if ALL of: (a) budget > 0 and lifting_cost
    <= budget (budget 0 at default/-O1/-Oclassic, so the mechanism is
    inert there; 100 at -O2, 1000 at -O3, 3000 at -O4, decremented
    per lift); (b) the enclosing context carries none of the five
    no_lifting reasons (At_toplevel, In_speculative_inlining,
    In_continuation_specialization, In_recursive_continuation,
    In_inlinable_continuation); (c) the status moves Analyzing{k} ->
    Lifting_out_of{k} at a LEAF of k's handler, exclusively in
    simplify_switch -- never eagerly at the let_cont itself.
    Corollary: a continuation nested in a loopified self-loop handler
    is never lifted out of the loop.  Anchor: budget/flag gating of
    the out-of-model lifting machinery. *)
Definition S_Struct_LiftCont_Gate_documented : Prop := True.

(* ================================================================== *)
(* 5. The turn: dataflow analysis (doc section 4)                     *)
(* ================================================================== *)

(** RULE S.Struct.Turn (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common
    CODE middle_end/flambda2/simplify/flow/flow_analysis.mli#analyze
    CODE middle_end/flambda2/simplify/flow/flow_types.ml

    At the transition from downwards to upwards for a
    toplevel/function body, Flow.Analysis.analyze runs on the
    accumulated Flow.Acc.t, producing Flow_result =
    { data_flow_result; aliases_result; mutable_unboxing_result };
    the upwards accumulator is created carrying this result and
    rebuilding begins.  The analysis is per-function-body: each
    Set_of_closures function body runs its own (as does the unit
    toplevel), the graph being local to a body.  Anchor: the pivot of
    the out-of-model traversal (file-wide ENCODING NOTE). *)
Definition S_Struct_Turn_documented : Prop := True.

(** RULE S.Struct.Flow.RequiredNames (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/flow/flow_types.ml#Data_flow_result
    CODE middle_end/flambda2/simplify/env/upwards_acc.ml#required_names
    CODE middle_end/flambda2/simplify/env/upwards_acc.ml#reachable_code_ids

    data_flow_result = { required_names; reachable_code_ids }.
    required_names over-approximates the names whose bindings must be
    kept; a binding whose bound name is not required is dead and may
    be removed on the way up.  It drives keep/drop of recursive
    continuation params and of lifted constants; the free-name count
    of the rebuilt term is the ground truth, and the code checks
    required_names is a superset of actual uses
    (S.Struct.ApplyContRewrite).  reachable_code_ids identifies live
    Code for cmx export (Unknown inside closures).  Anchor: analysis
    state; the term-level licence it feeds is ch. 10's dead-binding /
    unused-param rewrites, stated there via occurrence predicates on
    the term. *)
Definition S_Struct_Flow_RequiredNames_documented : Prop := True.

(** RULE S.Struct.Flow.UnusedParams (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/flow/flow_analysis.mli
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive

    For recursive continuations, parameters not in required_names
    (and not aliased away) are removed -- the primary purpose of the
    dataflow analysis, since a recursive handler cannot be
    re-inspected after rebuilding to discover its own dead
    parameters.  Non-recursive handlers instead use the exact free
    names of the rebuilt handler, the dataflow result serving only as
    a consistency check.  Anchor. *)
Definition S_Struct_Flow_UnusedParams_documented : Prop := True.

(** RULE S.Struct.Flow.DeadLoopParam (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#required_names
    CODE middle_end/flambda2/simplify/flow/data_flow_graph.ml#add_continuation_info
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#decide_param_usage_recursive
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let

    With G the flow dependency graph (bindings to the free names of
    their definitions for at-most-generative bindings and aliases;
    param p_j to the names of the argument supplied for p_j at each
    apply_cont to a non-return/exn continuation) and U the
    unconditionally-used set, any X closed under "no element
    reachable from U in G" -- X may contain cycles: params fed only
    by themselves/each other through pure bindings -- is removed
    wholesale in the single upwards pass: params in X dropped, their
    argument positions dropped at every use site, at-most-generative
    bindings in X becoming occurrence-free and deleted.
    required_names = reachability from U; its complement dies all at
    once, cycles included, with no rewrite iteration.  Local
    use-counting can never remove such a cycle (each member has
    syntactic occurrences; each deletion is licensed only by the
    others).  Six documented blocking conditions keep X alive
    (arbitrary-effects consumers, return/exn escapes, exn bucket
    params, Unknown value-slot capture, mutable-unboxing candidates,
    phantom lets).  Anchor: states which removals the ANALYSIS
    licenses; the removals themselves are ch. 10's rewrites, and the
    model's rewrite closure can delete a dead cycle by applying them
    simultaneously without a reachability oracle. *)
Definition S_Struct_Flow_DeadLoopParam_documented : Prop := True.

(** RULE S.Struct.Flow.Aliases (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/flow/flow_types.ml#Alias_result
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#extra_params_for_continuation_param_aliases
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler

    aliases_result identifies continuation parameters that alias one
    another or an outer name; aliased params are removed and replaced
    by extra args threaded from call sites or by let-bindings around
    the handler.  Recovers loop-carried aliases the straight-line
    downwards tracking cannot; a useful one found in a loop can
    trigger resimplification (S.Struct.Resimplify).  Anchor. *)
Definition S_Struct_Flow_Aliases_documented : Prop := True.

(** RULE S.Struct.Flow.ExnFirstParam (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/flow/control_flow_graph.ml#minimize_extra_args_for_one_continuation
    CODE middle_end/flambda2/terms/exn_continuation.mli#t

    The bucket (first) parameter x_b of a handler that REMAINS an
    exception handler is never removed by alias-based param removal,
    even when proved aliased to a simple s: if s is a symbol or
    poison the fact is discarded and alias-based removal is disabled
    for EVERY param of that handler; if s is a variable a,
    rematerialization is INVERTED (let a = x_b at the handler top,
    other params aliased to a rerouted to x_b).  Reason: the bucket
    arrives via the raise machinery (OS.ApplyCont.ExnBoundary; the
    Cmm trywith binds it in a fixed location), not via a rewritable
    apply_cont argument list, so no use-site rewrite can drop or
    re-source it.  Anchor: a constraint on the algorithm's alias
    machinery; the term-level content is carried by the premise
    [exn = true -> j <> 0] on RewritesControl.v's
    S_Rewrite_LetCont_UnusedParam and
    S_Rewrite_LetCont_AliasedParam (the bucket param is never
    removed from a kept exception handler). *)
Definition S_Struct_Flow_ExnFirstParam_documented : Prop := True.

(** RULE S.Struct.Flow.MutableUnboxing (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/flow/mutable_unboxing.mli
    CODE middle_end/flambda2/simplify/flow/flow_types.ml#Mutable_unboxing_result
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let

    mutable_unboxing_result turns a non-escaping mutable block into
    mutable continuation parameters threaded through the control-flow
    graph, eliminating the allocation: { did_unbox_a_mutable_block;
    additional_epa; let_rewrites }, where additional_epa adds
    params/args to continuations and let_rewrites rewrites or deletes
    the block's load/store/alloc primitives on the way up.  Doing it
    forces a resimplification pass so the new parameters get proper
    types.  Anchor: the transform's term-level content belongs to
    ch. 12's unboxing treatment; here only the analysis plumbing is
    documented. *)
Definition S_Struct_Flow_MutableUnboxing_documented : Prop := True.

(* ================================================================== *)
(* 6. Lifting constants to symbols (doc section 5)                    *)
(* ================================================================== *)

(** RULE S.Struct.Lift.Accumulate (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/lifting/reification.ml#try_to_reify
    CODE middle_end/flambda2/simplify/lifting/lifted_constant_state.mli
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#add_to_lifted_constant_accumulator

    When a binding's type shows it denotes a constant (or a
    block/closure of constants), try_to_reify produces a fresh
    symbol, records a Lifted_constant describing its static
    definition into dacc.lifted_constants, and rewrites the binding
    to reference the symbol.  Lifted constants may be mutually
    recursive (sorted innermost-first for placement) and flow up
    through uacc until placed; sets of closures are lifted through a
    dedicated path.  Anchor: accumulator state (see the file-wide
    ENCODING NOTE and the note on S.Struct.Run.NoPendingConstants:
    in a term, a constant exists only where it is placed). *)
Definition S_Struct_Lift_Accumulate_documented : Prop := True.

(** RULE S.Struct.Lift.PlaceAtToplevel (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_let_expr.ml#rebuild_let
    CODE middle_end/flambda2/simplify/expr_builder.mli#place_lifted_constants
    CODE middle_end/flambda2/simplify/simplify_let_cont_expr.ml#add_lets_around_handler
    CODE middle_end/flambda2/simplify/env/downwards_env.ml#at_unit_toplevel

    Lifted constants are placed (as "let symbol" bindings) only where
    DE.at_unit_toplevel holds; constants discovered under a lambda
    float up in the accumulator until a toplevel binding is reached
    -- no constant is ever placed under a lambda.  A constant is kept
    only if its symbol is in required_names or its code is reachable.
    at_unit_toplevel stays true through a non-recursive Let_cont
    whose handler postdominates its body, and goes false on entering
    a closure or a recursive continuation.  Anchor DESPITE normative
    status: the rule constrains WHERE the algorithm's placement
    machinery may emit, quantifying over at_unit_toplevel -- pure
    traversal state.  Its model-visible residue ("never under a
    lambda") is exactly [all_statics_placed], carried by the real
    Props S_Struct_Run_NoPendingConstants and
    S_Struct_Lift_EmptyAtEnd. *)
Definition S_Struct_Lift_PlaceAtToplevel_documented : Prop := True.

(** RULE S.Struct.Lift.EmptyAtEnd (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify.ml#run

    When the upwards pass reaches the top of the unit, the
    lifted-constant accumulator is empty: every discovered constant
    has been placed.  Guaranteed because the unit's top is
    at_unit_toplevel; run asserts it (S.Struct.Run.NoPendingConstants
    is the assertion's site).  Model-visible content: together with
    the placement policy above, the OUTPUT unit carries no static
    binding under a lambda (statics inside continuation handlers
    pass, per the PlaceAtToplevel anchor) -- the same predicate as
    S_Struct_Run_NoPendingConstants, restated here as this rule's own
    artifact so both ids carry their content independently. *)
Definition S_Struct_Lift_EmptyAtEnd (u : flambda_unit) : Prop :=
  all_statics_placed u.

(* ================================================================== *)
(* 7. Resimplification, inlining re-traversal, loopify               *)
(*    (doc section 6)                                                 *)
(* ================================================================== *)

(** RULE S.Struct.InlineResimplify (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application
    CODE middle_end/flambda2/simplify/inlining/inlining_transforms.ml

    When an Apply is inlined, the inlined body (the call's arguments
    substituted) is handed straight back to simplify_expr in the
    current downwards pass, so the inlined code is simplified in the
    calling context with the caller's types in scope --
    re-simplification within the single pass, not a new round (cf.
    S.Struct.SingleRound).  Anchor DESPITE normative status: it fixes
    the algorithm's re-traversal wiring.  The model-side counterpart
    is free: the rewrite closure in Simplify.v may apply further
    rewrites inside an inlined body simply because it is closed under
    congruence -- no artifact beyond ch. 11's inlining rewrite is
    needed. *)
Definition S_Struct_InlineResimplify_documented : Prop := True.

(** RULE S.Struct.SpeculativeSandbox (STATUS conjectured)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/inlining/call_site_inlining_decision.ml#speculative_inlining
    CODE middle_end/flambda2/simplify/env/downwards_acc.ml#prepare_for_speculative_inlining
    CODE middle_end/flambda2/simplify/flow/flow_analysis.ml#analyze
    CODE middle_end/flambda2/simplify/simplify_apply_expr.ml#simplify_direct_full_application

    The inlining oracle may run the full Simplify machinery on a
    candidate body to measure its cost; that speculative traversal is
    hermetic: (1) it runs on a COPY of dacc with a fresh flow_acc and
    is discarded after cost-metrics extraction -- no continuation
    use, flow edge, value-slot use, CSE equation or lifted constant
    reaches the real accumulators; (2) flow-dependent rewrites are
    disabled inside the sandbox (loopification, continuation lifting,
    nested inlining, term rebuild itself); (3) no speculative
    artifact is emitted -- an Inline decision re-simplifies the body
    from scratch against the real dacc (S.Struct.InlineResimplify).
    The sole escape is Cost_metrics.  Anchor: hermeticity of
    algorithm state.  Model-side, the inlining decision is the
    Inline? oracle Parameter (ch. 11), so the sandbox is invisible by
    construction. *)
Definition S_Struct_SpeculativeSandbox_documented : Prop := True.

(** RULE S.Struct.Resimplify (STATUS descriptive)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_toplevel_common
    CODE middle_end/flambda2/simplify/env/upwards_acc.ml#set_resimplify
    CODE middle_end/flambda2/simplify/simplify_set_of_closures.ml

    At the turn, uacc is marked resimplify = true if the flow
    analysis performed mutable unboxing or added a useful alias in a
    loop; a function body so marked is simplified again, up to a
    bounded count (max_function_simplify_run), so the
    newly-introduced parameters/aliases get proper types.  Local to a
    single function body and bounded -- not a whole-unit fixpoint
    (cf. S.Struct.SingleRound).  Anchor. *)
Definition S_Struct_Resimplify_documented : Prop := True.

(** RULE S.Struct.Loopify (STATUS normative)
    -- 09-simplify-structure.md
    CODE middle_end/flambda2/simplify/simplify_expr.ml#simplify_function_body
    CODE middle_end/flambda2/simplify/loopify_state.mli
    VERIFIED 14-validation/loopify-01-escaping-tailrec.md

    A function whose loopify attribute satisfies should_loopify -- an
    attribute fixed deterministically at closure conversion, not
    chosen by a cost oracle -- is unconditionally loopified: its body
    is wrapped in a recursive continuation and the self-tail-calls
    become continuation calls; loopify_state in denv records
    Do_not_loopify or Loopify cont, and when Loopify cont the body is
    simplified as a recursive Let_cont.  Anchor DESPITE normative
    status: this rule is the DRIVER wiring (when the machinery runs);
    the term-level transformation is entirely ch. 10's
    S.Rewrite.Loopify.Attribute / .Body / .SelfTailCall, which are
    real artifacts there.  The loopified body then follows the
    recursive-continuation treatment of section 4 above
    (S_Struct_Rec_NoFixpoint). *)
Definition S_Struct_Loopify_documented : Prop := True.
