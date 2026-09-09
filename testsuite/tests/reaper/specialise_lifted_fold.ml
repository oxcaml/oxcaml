(* TEST
   modules = "specialise_lifted_fold_lib.ml";
   flambda2;
   flags += "-O4 -flambda2-reaper -reaper-local-fields -dflambda-invariants";
   { native; }
 *)

module M = Specialise_lifted_fold_lib

let[@zero_alloc] simple (l @ local) =
  M.fold (fun x -> x * x) l [@nontail]

let[@zero_alloc] nested (l @ local) (l2 @ local) =
  M.fold (fun x -> M.fold (fun y -> x * y) l2 [@nontail]) l [@nontail]

(* Distinct callbacks must not redirect to each other's specialised code. *)
let[@zero_alloc] multiple (l @ local) =
  let squares = M.fold (fun x -> x * x) l in
  let doubled = M.fold (fun x -> x + x) l in
  let shifted = M.fold (fun x -> x + 3) l in
  squares + (10 * doubled) + (100 * shifted)

let () =
  let l = [1; 2; 3; 4] in
  Printf.printf "simple: %d %d\n" (simple []) (simple l);
  Printf.printf "nested: %d %d %d\n"
    (nested [] l) (nested l []) (nested l [2; 3]);
  Printf.printf "multiple: %d\n" (multiple l)
