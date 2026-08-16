(* TEST
 modules = "mtf_facts.ml";
 include ocamlcommon;
*)

(* Normalization and merging of fact sets, on values built here rather than
   extracted from a source, so that the ordering, deduplication and
   orientation the readers rely on are pinned down directly.

   The uids are compilation unit uids, which print as the unit name. *)

open Mtf_facts

let uid name =
  Shape.Uid.of_compilation_unit_name (Compilation_unit.Name.of_string name)

let key name = Facts.Key.Named (Def (uid name), uid name)

let anon name = Facts.Key.Anon (uid name)

let check expectation kind : Facts.Check.t =
  { implementation = Uid (uid "Impl");
    expectation;
    kind;
    site = Location.none
  }

let dependency derived source reason : Facts.Dependency.t =
  { derived; source; reason }

let equality left right : Facts.Context_equality.t =
  { left = Def (uid left); right = Def (uid right) }

let omission ?affected ?source reason : Facts.Omission.t =
  { affected; source; reason }

let print facts = List.iter print_endline (fact_lines (printer []) facts)

(* Deliberately out of order, with a duplicate of each kind of fact, a
   backwards equality and an equality of a context with itself. *)
let unsorted : Facts.t =
  { checks =
      [ check (key "B") Ascription;
        check (key "A") Interface;
        check (key "A") Ascription;
        check (key "A") Ascription
      ];
    dependencies =
      [ dependency (key "B") (key "A") Include;
        dependency (key "A") (anon "B") Alias;
        dependency (key "B") (key "A") Include
      ];
    equalities = [ equality "B" "A"; equality "A" "A"; equality "A" "B" ];
    omissions =
      [ omission ~source:(uid "B") Unresolved_module;
        omission ~affected:(key "A") Unsupported_path;
        omission ~source:(uid "B") Unresolved_module
      ]
  }

let () = heading "the facts as they were built, in the order they are stored"

let () = print unsorted

let () = heading "normalized: sorted, deduplicated, equalities oriented"

(* An equality of a context with itself carries no information and is dropped;
   a backwards one is turned around, so that a reader can compare the two
   sides without normalizing again. *)
let normalized = Facts.normalize unsorted

let () = print normalized

let () = heading "normalizing is idempotent"

let () =
  Printf.printf "normalize (normalize t) = normalize t: %b\n"
    (Facts.compare (Facts.normalize normalized) normalized = 0);
  Printf.printf "ensure_normalized t = normalize t: %b\n"
    (Facts.compare (Facts.ensure_normalized unsorted) normalized = 0);
  Printf.printf "compare normalizes what it is given: %b\n"
    (Facts.compare unsorted normalized = 0)

let () = heading "merging two overlapping sets"

let left : Facts.t =
  { checks = [ check (key "A") Ascription ];
    dependencies = [ dependency (key "B") (key "A") Include ];
    equalities = [ equality "A" "B" ];
    omissions = [ omission ~source:(uid "B") Unresolved_module ]
  }

let right : Facts.t =
  { checks = [ check (key "A") Ascription; check (key "C") Argument ];
    dependencies = [ dependency (key "C") (key "B") Alias ];
    equalities = [ equality "C" "A" ];
    omissions = [ omission ~affected:(key "C") Unresolved_module_type ]
  }

let merged = Facts.merge left right

let () = print merged

let () = heading "merging is commutative and merge_many folds it"

let () =
  Printf.printf "merge left right = merge right left: %b\n"
    (Facts.compare merged (Facts.merge right left) = 0);
  Printf.printf "merge t t = t: %b\n"
    (Facts.compare (Facts.merge left left) (Facts.normalize left) = 0);
  Printf.printf "merging with empty changes nothing: %b\n"
    (Facts.compare (Facts.merge merged Facts.empty) merged = 0);
  Printf.printf "merge_many [] = empty: %b\n"
    (Facts.compare (Facts.merge_many []) Facts.empty = 0);
  Printf.printf "merge_many [a; b; c] = merge (merge a b) c: %b\n"
    (Facts.compare
       (Facts.merge_many [ left; right; unsorted ])
       (Facts.merge (Facts.merge left right) unsorted)
     = 0);
  Printf.printf "distinct sets compare unequal: %b\n"
    (Facts.compare left right <> 0)
