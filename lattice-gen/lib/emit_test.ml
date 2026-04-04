open Model

let bprintf = Printf.bprintf

let values_var_name module_name = "values_" ^ Name.snake_case module_name

let const_module_name module_name = module_name ^ ".Const"

let field_module_name (field : field) ~in_op =
  if in_op && field.declared_opposite
  then field.lattice_name
  else if in_op then Name.op_module_name field.lattice_name else field.lattice_name

let axis_ctor_name field_name = String.capitalize_ascii field_name

let morphism_of_slot (embedding : embedding) slot =
  if slot = "embed"
  then embedding.embed
  else if String.length slot >= 4 && String.sub slot 0 4 = "left"
  then
    List.nth embedding.left_chain
      (int_of_string (String.sub slot 4 (String.length slot - 4)) - 1)
  else
    List.nth embedding.right_chain
      (int_of_string (String.sub slot 5 (String.length slot - 5)) - 1)

type exported_alias =
  { alias_name : string;
    module_name : string;
    morphism : morphism
  }

let collect_exported_aliases model =
  let seen = Hashtbl.create 16 in
  let add seen alias =
    if Hashtbl.mem seen alias.alias_name
    then failwith ("duplicate top-level alias export: " ^ alias.alias_name);
    Hashtbl.add seen alias.alias_name ()
  in
  List.fold_left
    (fun acc ->
      function
      | Lattice _ -> acc
      | Embedding embedding ->
        List.fold_left
          (fun acc (alias_name, slot) ->
            let alias =
              { alias_name;
                module_name = embedding.module_name;
                morphism = morphism_of_slot embedding slot
              }
            in
            add seen alias;
            acc @ [ alias ])
          acc
          embedding.aliases)
    []
    model.items

let add_test_runtime_ml buf root_module =
  bprintf buf "open %s\n\n" root_module;
  Buffer.add_string
    buf
    {|[@@@warning "-32"]

let exhaustive_threshold = 256

let repr_exhaustive_threshold = 4096

let sample_count = 200

let rng = Random.State.make [| 0x51ed; 0x1234; 0x2026 |]

let fail fmt = Printf.ksprintf failwith fmt

let ensure cond fmt =
  Printf.ksprintf (fun message -> if not cond then fail "%s" message) fmt

let safe_product limit a b =
  if a = 0 || b = 0
  then 0
  else if a > limit / b
  then limit + 1
  else a * b

let should_exhaust sizes =
  List.fold_left (safe_product exhaustive_threshold) 1 sizes
  <= exhaustive_threshold

let iter1 xs f =
  let xs = Array.of_list xs in
  let nx = Array.length xs in
  if should_exhaust [ nx ]
  then
    for i = 0 to nx - 1 do
      f xs.(i)
    done
  else
    for _ = 1 to sample_count do
      f xs.(Random.State.int rng nx)
    done

let iter2 xs ys f =
  let xs = Array.of_list xs in
  let ys = Array.of_list ys in
  let nx = Array.length xs in
  let ny = Array.length ys in
  if should_exhaust [ nx; ny ]
  then (
    for i = 0 to nx - 1 do
      for j = 0 to ny - 1 do
        f xs.(i) ys.(j)
      done
    done)
  else
    for _ = 1 to sample_count do
      f
        xs.(Random.State.int rng nx)
        ys.(Random.State.int rng ny)
    done

let iter3 xs ys zs f =
  let xs = Array.of_list xs in
  let ys = Array.of_list ys in
  let zs = Array.of_list zs in
  let nx = Array.length xs in
  let ny = Array.length ys in
  let nz = Array.length zs in
  if should_exhaust [ nx; ny; nz ]
  then (
    for i = 0 to nx - 1 do
      for j = 0 to ny - 1 do
        for k = 0 to nz - 1 do
          f xs.(i) ys.(j) zs.(k)
        done
      done
    done)
  else
    for _ = 1 to sample_count do
      f
        xs.(Random.State.int rng nx)
        ys.(Random.State.int rng ny)
        zs.(Random.State.int rng nz)
    done

let values_of_repr mask of_int_exn =
  if mask <= repr_exhaustive_threshold
  then (
    let acc = ref [] in
    for i = 0 to mask do
      try acc := of_int_exn i :: !acc with
      | Invalid_argument _ -> ()
    done;
    List.rev !acc)
  else (
    let acc = ref [] in
    let target = max exhaustive_threshold sample_count in
    let try_add i =
      try
        let value = of_int_exn i in
        if not (List.exists (( = ) value) !acc) then acc := value :: !acc
      with
      | Invalid_argument _ -> ()
    in
    List.iter try_add [ 0; mask; mask land lnot 1; mask lsr 1; 1 ];
    let attempts = ref 0 in
    while List.length !acc < target && !attempts < target * 32 do
      incr attempts;
      try_add (Random.State.int rng (mask + 1))
    done;
    List.rev !acc)

let check_repr ~name ~values ~mask ~to_int ~of_int_exn ~show =
  ensure (values <> []) "%s: no values enumerated" name;
  let valid = Array.make (mask + 1) false in
  List.iter
    (fun value ->
      let i = to_int value in
      ensure (0 <= i && i <= mask)
        "%s: representation %d for %s is outside mask %d"
        name i (show value) mask;
      ensure (not valid.(i))
        "%s: duplicate representation %d"
        name i;
      valid.(i) <- true;
      ensure (of_int_exn i = value)
        "%s: of_int_exn/to_int roundtrip failed for %s"
        name (show value))
    values;
  if mask <= repr_exhaustive_threshold
  then
    for i = 0 to mask do
      match try Some (of_int_exn i) with Invalid_argument _ -> None with
      | Some value ->
        ensure valid.(i)
          "%s: of_int_exn accepted invalid representation %d (%s)"
          name i (show value);
        ensure (to_int value = i)
          "%s: to_int/of_int_exn roundtrip failed for %d"
          name i
      | None ->
        ensure (not valid.(i))
          "%s: of_int_exn rejected valid representation %d"
          name i
    done
  else
    for _ = 1 to sample_count * 4 do
      let i = Random.State.int rng (mask + 1) in
      match try Some (of_int_exn i) with Invalid_argument _ -> None with
      | Some value ->
        ensure (to_int value = i)
          "%s: sampled to_int/of_int_exn roundtrip failed for %d"
          name i
      | None -> ()
    done

let check_name_roundtrip ~name ~values ~to_name ~of_name ~show =
  iter1 values
    (fun value ->
      let text = to_name value in
      match of_name text with
      | Some value' ->
        ensure (value = value')
          "%s: name/of_name roundtrip failed for %s"
          name (show value)
      | None ->
        fail "%s: of_name rejected %S" name text)

let check_valid ~name ~to_int ~of_int_exn ~show value =
  let i = to_int value in
  match try Some (of_int_exn i) with Invalid_argument _ -> None with
  | Some value' ->
    ensure (value' = value)
      "%s: invalid result representation %d for %s"
      name i (show value)
  | None ->
    fail "%s: invalid result representation %d for %s" name i (show value)

let check_lattice
    ~name
    ~values
    ~bottom
    ~top
    ~leq
    ~join
    ~meet
    ~sub
    ~imply
    ~to_int
    ~of_int_exn
    ~show
  =
  check_valid ~name ~to_int ~of_int_exn ~show bottom;
  check_valid ~name ~to_int ~of_int_exn ~show top;
  iter1 values
    (fun value ->
      ensure (leq value value)
        "%s: reflexivity failed for %s"
        name (show value);
      ensure (leq bottom value)
        "%s: bottom is not <= %s"
        name (show value);
      ensure (leq value top)
        "%s: %s is not <= top"
        name (show value));
  iter2 values values
    (fun x y ->
      let join_xy = join x y in
      let join_yx = join y x in
      let meet_xy = meet x y in
      let meet_yx = meet y x in
      let sub_xy = sub x y in
      let imply_xy = imply x y in
      check_valid ~name ~to_int ~of_int_exn ~show join_xy;
      check_valid ~name ~to_int ~of_int_exn ~show join_yx;
      check_valid ~name ~to_int ~of_int_exn ~show meet_xy;
      check_valid ~name ~to_int ~of_int_exn ~show meet_yx;
      check_valid ~name ~to_int ~of_int_exn ~show sub_xy;
      check_valid ~name ~to_int ~of_int_exn ~show imply_xy;
      ensure (join_xy = join_yx)
        "%s: join commutativity failed for %s and %s"
        name (show x) (show y);
      ensure (meet_xy = meet_yx)
        "%s: meet commutativity failed for %s and %s"
        name (show x) (show y);
      ensure (join x x = x)
        "%s: join idempotence failed for %s"
        name (show x);
      ensure (meet x x = x)
        "%s: meet idempotence failed for %s"
        name (show x);
      ensure ((leq x y) = (join x y = y))
        "%s: join/order mismatch for %s and %s"
        name (show x) (show y);
      ensure ((leq x y) = (meet x y = x))
        "%s: meet/order mismatch for %s and %s"
        name (show x) (show y);
      ensure (meet x (join x y) = x)
        "%s: absorption failed for meet/join on %s and %s"
        name (show x) (show y);
      ensure (join x (meet x y) = x)
        "%s: absorption failed for join/meet on %s and %s"
        name (show x) (show y));
  iter3 values values values
    (fun x y z ->
      let join_yz = join y z in
      let join_xy = join x y in
      let meet_yz = meet y z in
      let meet_xy = meet x y in
      let sub_xy = sub x y in
      let imply_xy = imply x y in
      check_valid ~name ~to_int ~of_int_exn ~show join_yz;
      check_valid ~name ~to_int ~of_int_exn ~show join_xy;
      check_valid ~name ~to_int ~of_int_exn ~show meet_yz;
      check_valid ~name ~to_int ~of_int_exn ~show meet_xy;
      check_valid ~name ~to_int ~of_int_exn ~show sub_xy;
      check_valid ~name ~to_int ~of_int_exn ~show imply_xy;
      ensure (join x join_yz = join join_xy z)
        "%s: join associativity failed"
        name;
      ensure (meet x meet_yz = meet meet_xy z)
        "%s: meet associativity failed"
        name;
      ensure ((leq sub_xy z) = (leq x join_yz))
        "%s: subtraction residuation failed"
        name;
      ensure ((leq z imply_xy) = (leq (meet z x) y))
        "%s: implication residuation failed"
        name)

let check_duality
    ~name
    ~values
    ~bottom
    ~top
    ~join
    ~meet
    ~sub
    ~imply
    ~op_bottom
    ~op_top
    ~op_join
    ~op_meet
    ~op_sub
    ~op_imply
    ~show
  =
  ensure (bottom = op_top)
    "%s: bottom/top duality failed"
    name;
  ensure (top = op_bottom)
    "%s: top/bottom duality failed"
    name;
  iter2 values values
    (fun x y ->
      ensure (join x y = op_meet x y)
        "%s: join/meet duality failed for %s and %s"
        name (show x) (show y);
      ensure (meet x y = op_join x y)
        "%s: meet/join duality failed for %s and %s"
        name (show x) (show y);
      ensure (sub x y = op_imply y x)
        "%s: sub/imply duality failed for %s and %s"
        name (show x) (show y);
      ensure (imply x y = op_sub y x)
        "%s: imply/sub duality failed for %s and %s"
        name (show x) (show y))

let check_reference_equivalence
    ~name
    ~values
    ~bottom
    ~top
    ~leq
    ~join
    ~meet
    ~sub
    ~imply
    ~ref_bottom
    ~ref_top
    ~ref_leq
    ~ref_join
    ~ref_meet
    ~ref_sub
    ~ref_imply
    ~to_int
    ~of_int_exn
    ~show
  =
  check_valid ~name ~to_int ~of_int_exn ~show ref_bottom;
  check_valid ~name ~to_int ~of_int_exn ~show ref_top;
  ensure (bottom = ref_bottom)
    "%s: bottom/reference mismatch"
    name;
  ensure (top = ref_top)
    "%s: top/reference mismatch"
    name;
  iter2 values values
    (fun x y ->
      let ref_join_xy = ref_join x y in
      let ref_meet_xy = ref_meet x y in
      let ref_sub_xy = ref_sub x y in
      let ref_imply_xy = ref_imply x y in
      check_valid ~name ~to_int ~of_int_exn ~show ref_join_xy;
      check_valid ~name ~to_int ~of_int_exn ~show ref_meet_xy;
      check_valid ~name ~to_int ~of_int_exn ~show ref_sub_xy;
      check_valid ~name ~to_int ~of_int_exn ~show ref_imply_xy;
      ensure (leq x y = ref_leq x y)
        "%s: leq/reference mismatch for %s and %s"
        name (show x) (show y);
      ensure (join x y = ref_join_xy)
        "%s: join/reference mismatch for %s and %s"
        name (show x) (show y);
      ensure (meet x y = ref_meet_xy)
        "%s: meet/reference mismatch for %s and %s"
        name (show x) (show y);
      ensure (sub x y = ref_sub_xy)
        "%s: sub/reference mismatch for %s and %s"
        name (show x) (show y);
      ensure (imply x y = ref_imply_xy)
        "%s: imply/reference mismatch for %s and %s"
        name (show x) (show y))

let check_adjunction
    ~name
    ~a_values
    ~b_values
    ~leq_a
    ~leq_b
    ~f
    ~g
    ~show_f
    ~show_g
    ~f_to_int
    ~f_of_int_exn
    ~g_to_int
    ~g_of_int_exn
  =
  iter2 a_values b_values
    (fun a b ->
      let fa = f a in
      let gb = g b in
      check_valid ~name ~to_int:f_to_int ~of_int_exn:f_of_int_exn ~show:show_f fa;
      check_valid ~name ~to_int:g_to_int ~of_int_exn:g_of_int_exn ~show:show_g gb;
      ensure ((leq_b fa b) = (leq_a a gb))
        "%s: adjunction failed"
        name)

let check_function_equality
    ~name
    ~values
    ~f
    ~g
    ~to_int
    ~of_int_exn
    ~show
  =
  iter1 values
    (fun value ->
      let fv = f value in
      let gv = g value in
      check_valid ~name ~to_int ~of_int_exn ~show fv;
      check_valid ~name ~to_int ~of_int_exn ~show gv;
      ensure (fv = gv)
        "%s: extensional equality failed"
        name)

let check_solver
    ~name
    ~values
    ~bottom
    ~top
    ~leq
    ~equal
    ~solver_min
    ~solver_max
    ~of_const
    ~to_const_exn
    ~floor_of_above
    ~ceil_of_below
    ~submode_const
    ~equate_const
    ~fresh_floor
    ~fresh_ceil
    ~show
  =
  ensure (to_const_exn solver_min = bottom)
    "%s: solver min mismatch"
    name;
  ensure (to_const_exn solver_max = top)
    "%s: solver max mismatch"
    name;
  ensure (fresh_floor () = bottom)
    "%s: fresh solver var floor mismatch"
    name;
  ensure (fresh_ceil () = top)
    "%s: fresh solver var ceil mismatch"
    name;
  iter1 values
    (fun value ->
      ensure (to_const_exn (of_const value) = value)
        "%s: solver const roundtrip failed for %s"
        name (show value);
      ensure (floor_of_above value = value)
        "%s: newvar_above floor mismatch for %s"
        name (show value);
      ensure (ceil_of_below value = value)
        "%s: newvar_below ceil mismatch for %s"
        name (show value));
  iter2 values values
    (fun x y ->
      ensure
        ((Result.is_ok (submode_const x y)) = leq x y)
        "%s: solver submode mismatch for %s and %s"
        name (show x) (show y);
      ensure
        ((Result.is_ok (equate_const x y)) = equal x y)
        "%s: solver equate mismatch for %s and %s"
        name (show x) (show y))

let check_solver_projection
    ~name
    ~product_values
    ~field_values
    ~product_of_const
    ~field_of_const
    ~field_to_const
    ~proj
    ~min_with_floor
    ~max_with_ceil
    ~newvar
    ~equate_const
    ~zap_to_floor
    ~zap_to_ceil
    ~field
    ~field_bot
    ~field_top
    ~product_show
    ~field_show
  =
  iter1 product_values
    (fun product ->
      ensure
        (field_to_const (proj (product_of_const product)) = field product)
        "%s: solver projection mismatch for %s"
        name (product_show product));
  iter1 field_values
    (fun field_value ->
      ensure
        (min_with_floor (field_of_const field_value) = field_bot field_value)
        "%s: solver min_with mismatch for %s"
        name (field_show field_value);
      ensure
        (max_with_ceil (field_of_const field_value) = field_top field_value)
        "%s: solver max_with mismatch for %s"
        name (field_show field_value);
      let v = newvar () in
      let projected = proj v in
      ensure (Result.is_ok (equate_const projected field_value))
        "%s: solver projected equate failed for %s"
        name (field_show field_value);
      ensure
        (field (zap_to_floor v) = field_value)
        "%s: solver projection sharing floor mismatch for %s"
        name (field_show field_value);
      ensure
        (field (zap_to_ceil v) = field_value)
        "%s: solver projection sharing ceil mismatch for %s"
        name (field_show field_value))

let check_product_roundtrip ~name ~values ~view ~of_view ~to_int ~of_int_exn ~show =
  iter1 values
    (fun value ->
      let rebuilt = of_view (view value) in
      check_valid ~name ~to_int ~of_int_exn ~show rebuilt;
      ensure (rebuilt = value)
        "%s: view/of_view roundtrip failed"
        name)

let check_field_projection
    ~name
    ~product_values
    ~field_values
    ~product_leq
    ~field_leq
    ~field
    ~with_field
    ~field_bot
    ~field_top
    ~product_to_int
    ~product_of_int_exn
    ~product_show
    ~field_to_int
    ~field_of_int_exn
    ~field_show
  =
  iter1 product_values
    (fun product ->
      let field_product = field product in
      let rebuilt = with_field field_product product in
      check_valid
        ~name
        ~to_int:field_to_int
        ~of_int_exn:field_of_int_exn
        ~show:field_show
        field_product;
      check_valid
        ~name
        ~to_int:product_to_int
        ~of_int_exn:product_of_int_exn
        ~show:product_show
        rebuilt;
      ensure (rebuilt = product)
        "%s: with_field/field roundtrip failed"
        name);
  iter2 field_values product_values
    (fun field_value product ->
      let updated = with_field field_value product in
      let projected = field updated in
      let lower = field_bot field_value in
      let upper = field_top field_value in
      check_valid
        ~name
        ~to_int:product_to_int
        ~of_int_exn:product_of_int_exn
        ~show:product_show
        updated;
      check_valid
        ~name
        ~to_int:field_to_int
        ~of_int_exn:field_of_int_exn
        ~show:field_show
        projected;
      check_valid
        ~name
        ~to_int:product_to_int
        ~of_int_exn:product_of_int_exn
        ~show:product_show
        lower;
      check_valid
        ~name
        ~to_int:product_to_int
        ~of_int_exn:product_of_int_exn
        ~show:product_show
        upper;
      ensure (projected = field_value)
        "%s: field/with_field coherence failed"
        name;
      ensure
        ((product_leq lower product)
         = field_leq field_value (field product))
        "%s: lower adjunction failed"
        name;
      ensure
        ((product_leq product upper)
         = field_leq (field product) field_value)
        "%s: upper adjunction failed"
        name)

|}

let add_values_bindings_ml buf model =
  List.iter
    (function
      | Lattice (Base base) ->
        let values_name = values_var_name base.name in
        let op_values_name = values_var_name (Name.op_module_name base.name) in
        bprintf
          buf
          "let %s = values_of_repr %s.Repr.mask %s.Repr.of_int_exn\n"
          values_name
          (const_module_name base.name)
          (const_module_name base.name);
        bprintf buf "let %s = %s\n\n" op_values_name values_name
      | Lattice (Product product) ->
        let values_name = values_var_name product.name in
        let op_values_name = values_var_name (Name.op_module_name product.name) in
        bprintf
          buf
          "let %s = values_of_repr %s.Repr.mask %s.Repr.of_int_exn\n"
          values_name
          (const_module_name product.name)
          (const_module_name product.name);
        bprintf buf "let %s = %s\n\n" op_values_name values_name
      | Embedding _ -> ())
    model.items

let add_solver_check_ml buf ~test_name ~module_name ~values_name =
  Buffer.add_string buf
    ("  check_solver ~name:"
    ^ Printf.sprintf "%S" test_name
    ^ " ~values:" ^ values_name
    ^ " ~bottom:" ^ const_module_name module_name ^ ".bottom"
    ^ " ~top:" ^ const_module_name module_name ^ ".top"
    ^ " ~leq:" ^ const_module_name module_name ^ ".leq"
    ^ " ~equal:" ^ const_module_name module_name ^ ".equal"
    ^ " ~solver_min:" ^ module_name ^ ".min"
    ^ " ~solver_max:" ^ module_name ^ ".max"
    ^ " ~of_const:" ^ module_name ^ ".of_const"
    ^ " ~to_const_exn:" ^ module_name ^ ".to_const_exn"
    ^ " ~floor_of_above:(fun value -> let v, _ = "
    ^ module_name ^ ".newvar_above (" ^ module_name
    ^ ".of_const value) in " ^ module_name ^ ".zap_to_floor v)"
    ^ " ~ceil_of_below:(fun value -> let v, _ = "
    ^ module_name ^ ".newvar_below (" ^ module_name
    ^ ".of_const value) in " ^ module_name ^ ".zap_to_ceil v)"
    ^ " ~submode_const:(fun x y -> " ^ module_name ^ ".submode ("
    ^ module_name ^ ".of_const x) (" ^ module_name
    ^ ".of_const y))"
    ^ " ~equate_const:(fun x y -> " ^ module_name ^ ".equate ("
    ^ module_name ^ ".of_const x) (" ^ module_name
    ^ ".of_const y))"
    ^ " ~fresh_floor:(fun () -> let v = " ^ module_name
    ^ ".newvar () in " ^ module_name ^ ".zap_to_floor v)"
    ^ " ~fresh_ceil:(fun () -> let v = " ^ module_name
    ^ ".newvar () in " ^ module_name ^ ".zap_to_ceil v)"
    ^ " ~show:" ^ const_module_name module_name ^ ".show;\n")

let add_base_test_ml buf (base : base) =
  let values_name = values_var_name base.name in
  let op_name = Name.op_module_name base.name in
  let op_values_name = values_var_name op_name in
  let const_name = const_module_name base.name in
  let op_const_name = const_module_name op_name in
  bprintf buf "let () =\n";
  bprintf
    buf
    "  check_repr ~name:%S ~values:%s ~mask:%s.Repr.mask ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    base.name
    values_name
    const_name
    const_name
    const_name
    const_name;
  bprintf
    buf
    "  check_name_roundtrip ~name:%S ~values:%s ~to_name:%s.name ~of_name:%s.of_name ~show:%s.show;\n"
    base.name
    values_name
    const_name
    const_name
    const_name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    base.name
    values_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    op_name
    op_values_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name;
  bprintf
    buf
    "  check_duality ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~op_bottom:%s.bottom ~op_top:%s.top ~op_join:%s.join ~op_meet:%s.meet ~op_sub:%s.sub ~op_imply:%s.imply ~show:%s.show;\n"
    base.name
    values_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    const_name;
  add_solver_check_ml buf ~test_name:base.name ~module_name:base.name ~values_name;
  add_solver_check_ml buf ~test_name:op_name ~module_name:op_name ~values_name:op_values_name;
  Buffer.add_string buf "  ()\n\n"

let add_product_reference_defs_ml buf (product : product) module_name ~in_op =
  let const_module = const_module_name module_name in
  let field_module field = const_module_name (field_module_name field ~in_op) in
  let add_view_record value_of_field =
    Buffer.add_string buf "    {\n";
    List.iter
      (fun (field : field) ->
        bprintf buf "      %s = %s;\n" field.name (value_of_field field))
      product.fields;
    Buffer.add_string buf "    }\n"
  in
  bprintf buf "  let ref_bottom =\n    %s.of_view\n" const_module;
  add_view_record (fun field ->
      Printf.sprintf "%s.bottom" (field_module field));
  Buffer.add_string buf "  in\n";
  bprintf buf "  let ref_top =\n    %s.of_view\n" const_module;
  add_view_record (fun field ->
      Printf.sprintf "%s.top" (field_module field));
  Buffer.add_string buf "  in\n";
  bprintf
    buf
    "  let ref_leq x y =\n\
    \    let xv = %s.view x in\n\
    \    let yv = %s.view y in\n\
    \    %s\n\
    \  in\n"
    const_module
    const_module
    (match product.fields with
     | [] -> "true"
     | fields ->
       String.concat
         "\n    && "
         (List.map
            (fun (field : field) ->
              Printf.sprintf
                "%s.leq xv.%s yv.%s"
                (field_module field)
                field.name
                field.name)
            fields));
  List.iter
    (fun op_name ->
      bprintf
        buf
        "  let ref_%s x y =\n\
        \    let xv = %s.view x in\n\
        \    let yv = %s.view y in\n\
        \    %s.of_view\n"
        op_name
        const_module
        const_module
        const_module;
      add_view_record (fun field ->
          Printf.sprintf
            "%s.%s xv.%s yv.%s"
            (field_module field)
            op_name
            field.name
            field.name);
      Buffer.add_string buf "  in\n")
    [ "join"; "meet"; "sub"; "imply" ]

let add_product_solver_test_ml buf (product : product) module_name values_name ~in_op =
  add_solver_check_ml buf ~test_name:module_name ~module_name ~values_name;
  List.iter
    (fun (field : field) ->
      let field_module = field_module_name field ~in_op in
      let field_const_module = const_module_name field_module in
      let product_const_module = const_module_name module_name in
      let field_values_name = values_var_name field_module in
      Buffer.add_string buf
        ("  check_solver_projection ~name:"
        ^ Printf.sprintf "%S" (module_name ^ ".Solver." ^ field.name)
        ^ " ~product_values:" ^ values_name
        ^ " ~field_values:" ^ field_values_name
        ^ " ~product_of_const:" ^ module_name ^ ".of_const"
        ^ " ~field_of_const:" ^ field_module ^ ".of_const"
        ^ " ~field_to_const:" ^ field_module ^ ".to_const_exn"
        ^ " ~proj:" ^ module_name ^ ".proj_" ^ field.name
        ^ " ~min_with_floor:(fun x -> " ^ module_name ^ ".zap_to_floor ("
        ^ module_name ^ ".min_with_" ^ field.name ^ " x))"
        ^ " ~max_with_ceil:(fun x -> " ^ module_name ^ ".zap_to_ceil ("
        ^ module_name ^ ".max_with_" ^ field.name ^ " x))"
        ^ " ~newvar:" ^ module_name ^ ".newvar"
        ^ " ~equate_const:(fun x y -> " ^ field_module
        ^ ".equate x (" ^ field_module ^ ".of_const y))"
        ^ " ~zap_to_floor:(fun v -> " ^ module_name
        ^ ".zap_to_floor (Obj.magic v))"
        ^ " ~zap_to_ceil:(fun v -> " ^ module_name
        ^ ".zap_to_ceil (Obj.magic v))"
        ^ " ~field:" ^ product_const_module ^ "." ^ field.name
        ^ " ~field_bot:" ^ product_const_module ^ "." ^ field.name ^ "_bot"
        ^ " ~field_top:" ^ product_const_module ^ "." ^ field.name ^ "_top"
        ^ " ~product_show:" ^ product_const_module ^ ".show"
        ^ " ~field_show:" ^ field_const_module ^ ".show;\n"))
    product.fields

let add_product_test_ml buf (product : product) =
  let values_name = values_var_name product.name in
  let op_name = Name.op_module_name product.name in
  let op_values_name = values_var_name op_name in
  let const_name = const_module_name product.name in
  let op_const_name = const_module_name op_name in
  bprintf buf "let () =\n";
  bprintf
    buf
    "  check_repr ~name:%S ~values:%s ~mask:%s.Repr.mask ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    const_name
    const_name
    const_name
    const_name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    op_name
    op_values_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name;
  bprintf
    buf
    "  check_duality ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~op_bottom:%s.bottom ~op_top:%s.top ~op_join:%s.join ~op_meet:%s.meet ~op_sub:%s.sub ~op_imply:%s.imply ~show:%s.show;\n"
    product.name
    values_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    const_name;
  add_product_reference_defs_ml buf product product.name ~in_op:false;
  bprintf
    buf
    "  check_reference_equivalence ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~ref_bottom ~ref_top ~ref_leq ~ref_join ~ref_meet ~ref_sub ~ref_imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name
    const_name;
  add_product_solver_test_ml buf product product.name values_name ~in_op:false;
  add_product_reference_defs_ml buf product op_name ~in_op:true;
  bprintf
    buf
    "  check_reference_equivalence ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~ref_bottom ~ref_top ~ref_leq ~ref_join ~ref_meet ~ref_sub ~ref_imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    op_name
    op_values_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name
    op_const_name;
  add_product_solver_test_ml buf product op_name op_values_name ~in_op:true;
  bprintf
    buf
    "  check_product_roundtrip ~name:%S ~values:%s ~view:%s.view ~of_view:%s.of_view ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    const_name
    const_name
    const_name
    const_name
    const_name;
  bprintf buf "  iter1 %s (fun value ->\n" values_name;
  bprintf buf "    let made = %s.make " const_name;
  List.iter
    (fun (field : field) ->
      bprintf buf "~%s:(%s.%s value) " field.name const_name field.name)
    product.fields;
  bprintf
    buf
    "in\n\
    \    check_valid ~name:%S ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show made;\n\
    \    ensure (made = value) %S"
    product.name
    const_name
    const_name
    const_name
    (product.name ^ ": make/projection roundtrip failed");
  Buffer.add_string buf ");\n";
  List.iter
    (fun (field : field) ->
      let field_module = field_module_name field ~in_op:false in
      let field_const_module = const_module_name field_module in
      let field_values_name = values_var_name field_module in
      bprintf
        buf
        "  check_field_projection ~name:%S ~product_values:%s ~field_values:%s ~product_leq:%s.leq ~field_leq:%s.leq ~field:%s.%s ~with_field:%s.with_%s ~field_bot:%s.%s_bot ~field_top:%s.%s_top ~product_to_int:%s.Repr.to_int ~product_of_int_exn:%s.Repr.of_int_exn ~product_show:%s.show ~field_to_int:%s.Repr.to_int ~field_of_int_exn:%s.Repr.of_int_exn ~field_show:%s.show;\n"
        (product.name ^ "." ^ field.name)
        values_name
        field_values_name
        const_name
        field_const_module
        const_name
        field.name
        const_name
        field.name
        const_name
        field.name
        const_name
        field.name
        const_name
        const_name
        const_name
        field_const_module
        field_const_module
        field_const_module;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.proj_%s ~g:%s.%s ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".proj_" ^ field.name)
        values_name
        const_name
        field.name
        const_name
        field.name
        field_const_module
        field_const_module
        field_const_module;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.min_with_%s ~g:%s.%s_bot ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".min_with_" ^ field.name)
        field_values_name
        const_name
        field.name
        const_name
        field.name
        const_name
        const_name
        const_name;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.max_with_%s ~g:%s.%s_top ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".max_with_" ^ field.name)
        field_values_name
        const_name
        field.name
        const_name
        field.name
        const_name
        const_name
        const_name;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:(%s.Const.proj %s.Axis.%s) ~g:%s.%s ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".Const.proj." ^ field.name)
        values_name
        product.name
        product.name
        (axis_ctor_name field.name)
        const_name
        field.name
        field_const_module
        field_const_module
        field_const_module;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:(%s.Const.min_with %s.Axis.%s) ~g:%s.%s_bot ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".Const.min_with." ^ field.name)
        field_values_name
        product.name
        product.name
        (axis_ctor_name field.name)
        const_name
        field.name
        const_name
        const_name
        const_name;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:(%s.Const.max_with %s.Axis.%s) ~g:%s.%s_top ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".Const.max_with." ^ field.name)
        field_values_name
        product.name
        product.name
        (axis_ctor_name field.name)
        const_name
        field.name
        const_name
        const_name
        const_name)
    product.fields;
  Buffer.add_string buf "  ()\n\n"

let add_embedding_test_ml buf (embedding : embedding) =
  let small_values = values_var_name embedding.small_name in
  let big_values = values_var_name embedding.big_name in
  let small_const = const_module_name embedding.small_name in
  let big_const = const_module_name embedding.big_name in
  bprintf buf "let () =\n";
  bprintf
    buf
    "  iter2 %s %s (fun x y ->\n\
    \    let ex = %s.embed x in\n\
    \    let ey = %s.embed y in\n\
    \    check_valid ~name:%S ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show ex;\n\
    \    check_valid ~name:%S ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show ey;\n\
    \    ensure ((not (%s.leq x y)) || %s.leq ex ey) %S;\n\
    \    ensure ((not (%s.leq ex ey)) || %s.leq x y) %S);\n"
    small_values
    small_values
    embedding.module_name
    embedding.module_name
    (embedding.module_name ^ ".embed(x)")
    big_const
    big_const
    big_const
    (embedding.module_name ^ ".embed(y)")
    big_const
    big_const
    big_const
    small_const
    big_const
    (embedding.module_name ^ ": order preservation failed")
    big_const
    small_const
    (embedding.module_name ^ ": order reflection failed");
  (match embedding.left_chain with
   | left1 :: rest ->
     bprintf
       buf
       "  check_adjunction ~name:%S ~a_values:%s ~b_values:%s ~leq_a:%s.leq ~leq_b:%s.leq ~f:%s.left1 ~g:%s.embed ~show_f:%s.show ~show_g:%s.show ~f_to_int:%s.Repr.to_int ~f_of_int_exn:%s.Repr.of_int_exn ~g_to_int:%s.Repr.to_int ~g_of_int_exn:%s.Repr.of_int_exn;\n"
       (embedding.module_name ^ ".left1/embed")
       (values_var_name left1.domain)
       (values_var_name left1.codomain)
       (const_module_name left1.domain)
       (const_module_name left1.codomain)
       embedding.module_name
       embedding.module_name
       (const_module_name left1.codomain)
       (const_module_name left1.domain)
       (const_module_name left1.codomain)
       (const_module_name left1.codomain)
       (const_module_name left1.domain)
       (const_module_name left1.domain);
     List.iteri
       (fun i (morphism : morphism) ->
         let prev = "left" ^ string_of_int (i + 1) in
         let curr = "left" ^ string_of_int (i + 2) in
         bprintf
           buf
           "  check_adjunction ~name:%S ~a_values:%s ~b_values:%s ~leq_a:%s.leq ~leq_b:%s.leq ~f:%s.%s ~g:%s.%s ~show_f:%s.show ~show_g:%s.show ~f_to_int:%s.Repr.to_int ~f_of_int_exn:%s.Repr.of_int_exn ~g_to_int:%s.Repr.to_int ~g_of_int_exn:%s.Repr.of_int_exn;\n"
           (embedding.module_name ^ "." ^ curr ^ "/" ^ prev)
           (values_var_name morphism.domain)
           (values_var_name morphism.codomain)
           (const_module_name morphism.domain)
           (const_module_name morphism.codomain)
           embedding.module_name
           curr
           embedding.module_name
           prev
           (const_module_name morphism.codomain)
           (const_module_name morphism.domain)
           (const_module_name morphism.codomain)
           (const_module_name morphism.codomain)
           (const_module_name morphism.domain)
           (const_module_name morphism.domain))
       rest
   | [] -> ());
  (match embedding.right_chain with
   | right1 :: rest ->
     ignore right1;
     bprintf
       buf
       "  check_adjunction ~name:%S ~a_values:%s ~b_values:%s ~leq_a:%s.leq ~leq_b:%s.leq ~f:%s.embed ~g:%s.right1 ~show_f:%s.show ~show_g:%s.show ~f_to_int:%s.Repr.to_int ~f_of_int_exn:%s.Repr.of_int_exn ~g_to_int:%s.Repr.to_int ~g_of_int_exn:%s.Repr.of_int_exn;\n"
       (embedding.module_name ^ ".embed/right1")
       small_values
       big_values
       small_const
       big_const
       embedding.module_name
       embedding.module_name
       big_const
       small_const
       big_const
       big_const
       small_const
       small_const;
     List.iteri
       (fun i (morphism : morphism) ->
         let prev = "right" ^ string_of_int (i + 1) in
         let curr = "right" ^ string_of_int (i + 2) in
         bprintf
           buf
           "  check_adjunction ~name:%S ~a_values:%s ~b_values:%s ~leq_a:%s.leq ~leq_b:%s.leq ~f:%s.%s ~g:%s.%s ~show_f:%s.show ~show_g:%s.show ~f_to_int:%s.Repr.to_int ~f_of_int_exn:%s.Repr.of_int_exn ~g_to_int:%s.Repr.to_int ~g_of_int_exn:%s.Repr.of_int_exn;\n"
           (embedding.module_name ^ "." ^ prev ^ "/" ^ curr)
           (values_var_name morphism.codomain)
           (values_var_name morphism.domain)
           (const_module_name morphism.codomain)
           (const_module_name morphism.domain)
           embedding.module_name
           prev
           embedding.module_name
           curr
           (const_module_name morphism.domain)
           (const_module_name morphism.codomain)
           (const_module_name morphism.domain)
           (const_module_name morphism.domain)
           (const_module_name morphism.codomain)
           (const_module_name morphism.codomain))
       rest
   | [] -> ());
  List.iter
    (fun (alias_name, slot) ->
      let morphism = morphism_of_slot embedding slot in
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.%s ~g:%s.%s ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (embedding.module_name ^ "." ^ alias_name)
        (values_var_name morphism.domain)
        embedding.module_name
        alias_name
        embedding.module_name
        slot
        (const_module_name morphism.codomain)
        (const_module_name morphism.codomain)
        (const_module_name morphism.codomain))
    embedding.aliases;
  Buffer.add_string buf "  ()\n\n"

let add_exported_aliases_test_ml buf model =
  let aliases = collect_exported_aliases model in
  List.iter
    (fun alias ->
      bprintf
        buf
        "let () =\n\
        \  check_function_equality ~name:%S ~values:%s ~f:%s ~g:%s.%s ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n\
        \  ()\n\n"
        alias.alias_name
        (values_var_name alias.morphism.domain)
        alias.alias_name
        alias.module_name
        alias.alias_name
        (const_module_name alias.morphism.codomain)
        (const_module_name alias.morphism.codomain)
        (const_module_name alias.morphism.codomain))
    aliases

let add_test_ml buf model ~root_module =
  add_test_runtime_ml buf root_module;
  add_values_bindings_ml buf model;
  List.iter
    (function
      | Lattice (Base base) -> add_base_test_ml buf base
      | Lattice (Product product) -> add_product_test_ml buf product
      | Embedding embedding -> add_embedding_test_ml buf embedding)
    model.items;
  add_exported_aliases_test_ml buf model
