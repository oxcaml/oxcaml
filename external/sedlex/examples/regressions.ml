(* This test that unicode_old.ml is a strict sub-set of new unicode.ml. *)

module CSet = Sedlex_ppx.Sedlex_cset
module Unicode = Sedlex_ppx.Unicode

let test_versions = ("16.0.0", "17.0.0")

let regressions =
  [ (* Example *)
    (* ("lt", CSet.union (CSet.singleton 0x1c5) (CSet.singleton (0x0001))) *) ]

let compare name (old_ : CSet.t) (new_ : CSet.t) =
  let diff = CSet.difference old_ new_ in
  let regressions =
    match List.assoc name regressions with
      | exception Not_found -> CSet.empty
      | x -> x
  in
  let regressions_intersect = CSet.intersection regressions old_ in
  let regressions = CSet.difference regressions regressions_intersect in
  let regressions_useless = CSet.difference regressions new_ in
  let diff = CSet.difference diff regressions in
  Seq.iter
    (fun x ->
      Printf.printf
        "Invalid regression for 0x%x in %s: already present in old set.\n" x
        name)
    (CSet.to_seq regressions_intersect);
  Seq.iter
    (fun x ->
      Printf.printf "Invalid regression for 0x%x in %s: absent in new set.\n" x
        name)
    (CSet.to_seq regressions_useless);
  Seq.iter
    (fun x -> Printf.printf "Code point 0x%x missing in %s!\n" x name)
    (CSet.to_seq diff)

let test new_l (name, old_l) =
  (* Cn is for unassigned code points, which are allowed to be
   * used in future version. *)
  let old_l = Sedlex_utils.Cset.to_list old_l in
  if name <> "cn" then (
    let old_l =
      List.fold_left
        (fun acc (a, b) -> CSet.union acc (CSet.interval a b))
        CSet.empty old_l
    in
    compare name old_l (List.assoc name new_l))

let () =
  if (Unicode_old.version, Unicode.version) <> test_versions then
    failwith
      (Printf.sprintf "Test written for versions: %s => %s\n%!"
         Unicode_old.version Unicode.version);
  Printf.printf "Testing Unicode regression: %s => %s\n%!" Unicode_old.version
    Unicode.version;
  List.iter (test Unicode.Categories.list) Unicode_old.Categories.list;
  List.iter (test Unicode.Properties.list) Unicode_old.Properties.list
