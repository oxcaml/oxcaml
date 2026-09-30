(* Helpers shared by the artifact-plumbing tests in this directory.

   These tests only check that module-type facts travel through the build
   artifacts: present when the artifact is written, absent otherwise, and
   identical across artifact kinds.  The semantic expectations, i.e. which
   facts are recorded for which programs, are tested through the
   [module-type-impls] query in Merlin's test suite, where the programs are
   real files and the facts are observed through the product commands. *)

module Facts = Module_implementation_facts

let equal_facts (left : Facts.t) (right : Facts.t) =
  Facts.Check_set.equal left.checks right.checks
  && Facts.Dependency_set.equal left.dependencies right.dependencies
  && Facts.Context_equality_set.equal left.equalities right.equalities
  && Facts.Omission_set.equal left.omissions right.omissions

let heading title = Printf.printf "== %s\n" title
