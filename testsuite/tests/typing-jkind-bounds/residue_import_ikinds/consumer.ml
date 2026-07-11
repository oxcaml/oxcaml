(* Stage-4d regression: consume a recursive-module fixpoint RESIDUE cross-unit.
   Res_lam.LamF.exp0 / .a are recursive-module decls whose stored (persisted)
   ikind carries a foreign-Param residue (mixmod5 family).  Under -ikinds the
   importer consumes that persisted residue; these decls must typecheck (a
   mishandled residue import would yield a wrong ikind and a spurious rejection).
   The importer's validation classifies the residue divergence as CLASS-B (stored
   trusted, 0 HARD) rather than a hard mismatch -- verify with:
     OXCAML_IKINDS_VALIDATE=1 ocamlc -I lib -c consumer.ml
   and observe `mismatches=0 ... residue_trusted>0` in the [ikind-validate]
   summary (validate-only telemetry, not part of the compile verdict). *)
type e = Res_lam.LamF.exp0
type a = Res_lam.LamF.a
type wrap = Wv of Res_lam.LamF.exp0
type holder = { h : Res_lam.LamF.exp0; k : Res_lam.LamF.a }
type v : value with Res_lam.LamF.exp0 = V of Res_lam.LamF.exp0
