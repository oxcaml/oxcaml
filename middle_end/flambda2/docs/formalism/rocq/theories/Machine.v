(* Machine.v: the instantiated Flambda 2 machine (wave 4).

   Opsem.v defines the ch. 04 step relation abstractly over
   - a plain primitive denotation      (Section Variable denot),
   - a region-aware denotation         (Section Variable denot_r),
   - the external-call oracle          (Section Variable cext),
   - the effects classification        (Section Variables pure_prim,
                                        effectful_prim; catalog #21).
   This file ties the knot: the union denotation over the concrete
   relations denot_scalar (PrimScalar.v, ch. 05), denot_mem_a
   (PrimMemoryA.v) and denot_mem_b (PrimMemoryB.v) (both ch. 06);
   denot_region (PrimMemoryA.v); a Parameter for cextern (sanctioned
   oracle); and the classification predicates derived from
   PrimMemoryA.v's effects_of.

   NO rule ids live in this file: every rule is transcribed at its
   defining site; this file only instantiates. *)

(* String before List: List's length must win (Hopper). *)
From Stdlib Require Import ZArith Bool String List.
From Flambda2 Require Import Base Syntax Values Opsem.
From Flambda2 Require Import PrimScalar PrimMemoryA PrimMemoryB.

Set Implicit Arguments.

(* ================================================================ *)
(* 1. The primitive denotation: union of the three chapter halves   *)
(* ================================================================ *)

(* [[p]](vbar; H): the doc's single denotation is split across three
   files by chapter; the machine consumes their union.  The three
   relations are on disjoint operator ranges (each primitive's
   denotation rules live in exactly one file), so the union adds no
   new nondeterminism beyond what each half declares. *)
Definition denot_prim : denotation :=
  fun op vs H r =>
    denot_scalar op vs H r
    \/ denot_mem_a op vs H r
    \/ denot_mem_b op vs H r.

(* The region-aware denotation is single-sited (PrimMemoryA.v). *)
Definition denot_prim_r : denotation_r := denot_region.

(* ================================================================ *)
(* 2. External calls (sanctioned oracle)                            *)
(* ================================================================ *)

(* OS.Apply.CCall axiomatizes external calls; cextern is on the
   sanctioned-Parameter list (CORRESPONDENCE.md catalog #20: keyed
   by the callee VALUE). *)
Parameter cextern : cextern_rel.

(* ================================================================ *)
(* 3. Effects classification (ch. 06, P.Effects.Classification)     *)
(* ================================================================ *)

(* OS.Let.Prim.Pure fires for No_effects or Only_generative_effects;
   OS.Let.Prim.Effect for Arbitrary_effects (04-opsem.md section 3,
   matching catalog #21's Section-Variable split). *)
Definition pure_effects (e : effects) : Prop :=
  match e with
  | No_effects | Only_generative_effects _ => True
  | Arbitrary_effects => False
  end.

(* The machine is parameterized by the compilation flags because the
   effects classification is (effects_of : eff_flags -> prim_op ->
   ece, PrimMemoryA.v).  Note the split is the doc's CLASSIFICATION,
   not "heap unchanged": OS.Let.Prim.Pure and .Effect carry the same
   transition (both thread H' and R' through denot_R); allocators
   (Only_generative_effects) extend the heap on the Pure side. *)
Section MACHINE.

Variable flags : eff_flags.

Definition fl_pure_prim (p : prim_op) : Prop :=
  pure_effects (ece_effects (effects_of flags p)).

Definition fl_effectful_prim (p : prim_op) : Prop :=
  ece_effects (effects_of flags p) = Arbitrary_effects.

(* ================================================================ *)
(* 4. The instantiated machine                                      *)
(* ================================================================ *)

Definition fl_step : config -> label -> config -> Prop :=
  step denot_prim denot_prim_r cextern fl_pure_prim
    fl_effectful_prim.

Definition fl_undef_next : config -> Prop :=
  undef_next denot_prim denot_prim_r.

Definition fl_stuck : config -> Prop :=
  stuck denot_prim denot_prim_r cextern fl_pure_prim
    fl_effectful_prim.

(* Observable behaviors of a configuration (OS.Unit.Final,
   transcribed in Opsem.v's has_behavior), under the concrete
   denotations.  Terminating behaviors carry the final heap; the
   H(sym_mod) observation is taken by obs_equiv (Soundness.v). *)
Definition fl_has_behavior (c0 : config) : behavior -> Prop :=
  has_behavior denot_prim denot_prim_r cextern fl_pure_prim
    fl_effectful_prim c0.

(* Unit-level behaviors: OS.Unit.Init (Opsem.v's initial) composed
   with the instantiated has_behavior.  rho_pre/H0 are the ambient
   predefined-symbol environment and starting heap (see the ENCODING
   NOTE on initial); ch. 13 quantifies over them. *)
Definition fl_unit_behavior (u : flambda_unit) (rho_pre : env)
    (H0 : heap) (b : behavior) : Prop :=
  exists c0,
    initial u rho_pre H0 c0
    /\ fl_has_behavior c0 b.

End MACHINE.
