module L = Ldd.Make (struct
  type t = string

  let compare = String.compare

  let to_string x = x
end)

let leq a b = L.leq_with_reason a b = []

let assert_equiv label a b =
  if not (leq a b && leq b a)
  then
    failwith
      (Printf.sprintf "LDD equivalence failed for %s:\nleft:  %s\nright: %s"
         label (L.pp a) (L.pp b))

let assert_bool label actual expected =
  if actual <> expected
  then failwith (Printf.sprintf "LDD check failed for %s" label)

let assert_axis_lattice label actual expected =
  if not (Axis_lattice.equal actual expected)
  then
    failwith
      (Printf.sprintf
         "Axis_lattice check failed for %s:\nactual:   %s\nexpected: %s" label
         (Axis_lattice.to_string actual)
         (Axis_lattice.to_string expected))

let var name = L.node_of_var (L.rigid name)

let () =
  let top = L.const Axis_lattice.top in
  let x = var "x" in
  let y = var "y" in
  let z = var "z" in
  assert_equiv "bot => x" (L.imply L.bot x) top;
  assert_equiv "x => x" (L.imply x x) top;
  assert_equiv "x => bot" (L.imply x L.bot) L.bot;
  assert_equiv "(x meet y) => x" (L.imply (L.meet x y) x) top;
  assert_equiv "x => (x meet y)" (L.imply x (L.meet x y)) y;
  assert_equiv "(x join y) => x" (L.imply (L.join x y) x) x;
  let pending_gfp = L.new_var () in
  let pending_gfp_node = L.node_of_var pending_gfp in
  L.enqueue_gfp pending_gfp L.bot;
  assert_equiv "pending gfp is solved before implication"
    (L.imply pending_gfp_node L.bot)
    top;
  let immutable_data = Axis_lattice.immutable_data in
  let c = L.const immutable_data in
  let d = L.const Axis_lattice.value in
  assert_equiv "provenance variable implies constant" (L.imply x c) c;
  let implication_with_other_provenance = L.imply x (L.join c y) in
  assert_axis_lattice "round_down ignores other provenance"
    (L.round_down implication_with_other_provenance)
    immutable_data;
  assert_axis_lattice "round_up includes other provenance"
    (L.round_up implication_with_other_provenance)
    Axis_lattice.top;
  let terms =
    [ L.bot;
      top;
      c;
      d;
      x;
      y;
      z;
      L.join x y;
      L.join x c;
      L.join (L.meet x y) z;
      L.meet x y;
      L.meet x c;
      L.meet (L.join x y) (L.join y z) ]
  in
  List.iteri
    (fun i a ->
      List.iteri
        (fun j b ->
          let a_implies_b = L.imply a b in
          List.iteri
            (fun k h ->
              let lhs = leq (L.meet a h) b in
              let rhs = leq h a_implies_b in
              assert_bool
                (Printf.sprintf "adjunction a=%d b=%d h=%d" i j k)
                lhs rhs)
            terms)
        terms)
    terms
