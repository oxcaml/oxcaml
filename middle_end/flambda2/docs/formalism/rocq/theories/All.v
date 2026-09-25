(* All.v -- the whole mechanization under one import (task #26).

   Compiling this file re-checks every theory file in the
   development.

   Coverage gate anchor: 453 unique rule ids across theories/, in
   exact set and role equality with the twenty chapter files
   (../../*.md), split 365 specification / 75 implementation / 13
   definition (ROLE keyword; the retired CLAIM vocabulary renamed
   1:1 by record 77, STATUS/conjectured dissolved under record 76's
   adjudicated partition).
   The check is the Traceability grep of ../CORRESPONDENCE.md
   against the chapters' rule blocks. *)

From Flambda2 Require Import
  Base Syntax Cmm Values WellFormed TypeGrammar CmmMemory Opsem
  PrimMemoryA PrimScalar PrimMemoryB Concretization ToCmmControl
  Machine Representation MeetJoin SimplifyStructure Inlining
  RewritesControl Unboxing RewritesPrim ToCmmData ToCmmSoundness
  Simplify Soundness Pilot.
