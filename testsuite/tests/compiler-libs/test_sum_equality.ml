(* TEST
 include ocamlcommon;
 bytecode;
*)

let kind layout : Types.jkind_const_desc_lr =
  { base = Layout layout;
    mod_bounds = Btype.Jkind0.Mod_bounds.max;
    with_bounds = No_with_bounds }

let () =
  let module Layout = Jkind_types.Layout.Const in
  let module Sort = Jkind_types.Sort in
  let axes = Jkind_types.Scannable_axes.max in
  let bits = Layout.Static.of_base Bits32 axes in
  let sum = kind (Layout.sum [bits; bits]) in
  let variable = Sort.new_genvar () in
  let unknown = kind (Layout.genvar variable axes) in
  let equality = Jkind.Const.equal Env.empty sum unknown in
  assert (Sort.Var.is_root variable);
  assert (Result.is_error equality);
  let closed = Sort.Sum [Sort.bits32; Sort.bits32] in
  assert (Result.is_ok (Sort.equate ~allow_mutation:true
                          (Sort.of_var variable) closed));
  assert (Result.is_ok (Jkind.Const.equal Env.empty sum unknown))

let () =
  let module Layout = Jkind.Layout in
  let module Sort = Jkind_types.Sort in
  let axes = Jkind_types.Scannable_axes.max in
  let infer arm =
    let variable = Sort.new_genvar () in
    let inferred = Sort.of_var variable in
    let layout = Layout.Sum [Layout.Sort (Sort.void, axes); arm] in
    Jkind.Sub_result.is_le (Layout.sub layout (Layout.Sort (inferred, axes))),
    inferred
  in
  let accepted, inferred = infer (Layout.Sort (Sort.bits32, axes)) in
  assert accepted;
  assert (Sort.get inferred = Sort.Sum [Sort.void; Sort.bits32]);
  let arm = Jkind.Sort.new_var ~level:(Ctype.get_current_level ()) in
  let accepted, _ = infer (Layout.Sort (Sort.of_var arm, axes)) in
  assert (not accepted);
  assert (Sort.Var.is_root arm)
