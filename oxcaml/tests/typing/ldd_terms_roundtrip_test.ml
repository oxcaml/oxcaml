(* Round-trip property test for [Ldd.to_terms]/[Ldd.of_terms] (stage 4c). These
   are the cmi-residue marshaling vehicle (stage 5), so the contract is
   load-bearing:
   - [of_terms (to_terms n)] is semantically equal to [n];
   - [to_terms (of_terms ts)] returns [ts] up to the canonical form (within-term
     [Name.compare] sort + duplicate name-set coeff-join);
   - edge cases: bot/top, const, duplicate name-sets, Unknown-atom Uid stability. *)

module L = Types.Ldd
module N = Types.Rigid_name

let atoms =
  let p name = Path.Pident (Ident.create_local name) in
  [ N.param 1;
    N.param 2;
    N.param 7;
    N.atomic (p "t") 0;
    N.atomic (p "t") 1;
    N.katom (p "k") ]

let node_of name = L.node_of_var (L.rigid name)

let coeffs = Axis_lattice.[bot; top; immutable_data; mutable_data; immediate]

let sem_eq a b = L.leq_with_reason a b = [] && L.leq_with_reason b a = []

let check_sem label a b =
  if not (sem_eq a b)
  then failwith (Format.asprintf "ldd round-trip (%s): not semantically equal" label)

(* Build a variety of nodes: constants, single/multi-atom terms, sums. *)
let sample_nodes () =
  let singles =
    List.concat_map
      (fun c -> List.map (fun a -> L.meet (L.const c) (node_of a)) atoms)
      coeffs
  in
  let pairs =
    [ L.meet (node_of (List.nth atoms 0)) (node_of (List.nth atoms 3));
      L.meet
        (L.const Axis_lattice.immutable_data)
        (L.meet (node_of (List.nth atoms 1)) (node_of (List.nth atoms 5))) ]
  in
  let sums =
    [ L.join (L.const Axis_lattice.immutable_data) (node_of (List.nth atoms 0));
      L.join
        (L.join (L.const Axis_lattice.mutable_data) (node_of (List.nth atoms 2)))
        (L.meet (node_of (List.nth atoms 3)) (node_of (List.nth atoms 4))) ]
  in
  List.concat
    [ [L.bot; L.const Axis_lattice.bot; L.const Axis_lattice.top];
      List.map (fun c -> L.const c) coeffs;
      singles;
      pairs;
      sums ]

let () =
  (* Edge contracts. *)
  if L.to_terms L.bot <> [] then failwith "to_terms bot <> []";
  check_sem "of_terms [] = bot" (L.of_terms []) L.bot;
  (match L.to_terms (L.const Axis_lattice.top) with
   | [(c, [])] when Axis_lattice.equal c Axis_lattice.top -> ()
   | _ -> failwith "to_terms top <> [(top, [])]");
  check_sem "of_terms [(c,[])] = const c"
    (L.of_terms [Axis_lattice.immutable_data, []])
    (L.const Axis_lattice.immutable_data);
  (* Duplicate name-sets: of_terms joins their coeffs. *)
  let a0 = List.nth atoms 0 in
  check_sem "duplicate name-sets join"
    (L.of_terms
       [Axis_lattice.immutable_data, [a0]; Axis_lattice.mutable_data, [a0]])
    (L.of_terms
       [Axis_lattice.join Axis_lattice.immutable_data Axis_lattice.mutable_data, [a0]]);
  (* of_terms (to_terms n) == n, semantically, on a corpus. *)
  List.iteri
    (fun i n ->
      check_sem
        (Format.asprintf "of_terms (to_terms n) [#%d]" i)
        (L.of_terms (L.to_terms n))
        n)
    (sample_nodes ());
  (* to_terms (of_terms ts) idempotent on canonical lists (to_terms output). *)
  List.iteri
    (fun i n ->
      let ts = L.to_terms n in
      let ts2 = L.to_terms (L.of_terms ts) in
      if ts <> ts2
      then
        failwith
          (Format.asprintf "to_terms (of_terms ts) not idempotent [#%d]" i))
    (sample_nodes ());
  (* Within-term names are sorted by Name.compare. *)
  List.iter
    (fun (_, names) ->
      if names <> List.sort N.compare names
      then failwith "to_terms within-term names not sorted")
    (L.to_terms
       (L.meet (node_of (List.nth atoms 4)) (node_of (List.nth atoms 0))));
  print_string "ldd_terms_roundtrip_test: OK\n"
