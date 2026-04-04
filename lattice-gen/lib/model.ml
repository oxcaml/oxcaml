module String_map = Map.Make (String)
module String_set = Set.Make (String)

type round =
  { shift : int;
    mask : int
  }

type descriptor =
  { bits : int;
    mask : int;
    nat_mask : int;
    down_nat : round list;
    up_nat : round list;
    down_dual : round list;
    up_dual : round list
  }

type repr_validator =
  { mask : int;
    down : round list
  }

type finite_lattice =
  { element_names : string array;
    leq : bool array array;
    join : int array array;
    meet : int array array;
    bottom : int;
    top : int
  }

type base =
  { name : string;
    element_names : string array;
    element_values : int array;
    element_value_names : string array;
    repr_validator : repr_validator;
    descriptor : descriptor;
    op_descriptor : descriptor;
    logical : finite_lattice
  }

type field =
  { name : string;
    lattice_name : string;
    declared_opposite : bool;
    shift : int;
    raw_mask : int;
    layout_mask : int
  }

type axis_object =
  { name : string;
    ctor_name : string;
    carrier_name : string;
    declared_opposite : bool;
    shift : int;
    raw_mask : int;
    layout_mask : int;
    proj_name : string;
    with_name : string;
    bot_name : string;
    top_name : string;
    min_with_name : string;
    max_with_name : string
  }

type product =
  { name : string;
    fields : field list;
    axes : axis_object list;
    repr_validator : repr_validator;
    descriptor : descriptor;
    op_descriptor : descriptor
  }

type lattice =
  | Base of base
  | Product of product

type morphism =
  { domain : string;
    codomain : string;
    map : int array
  }

type embedding =
  { module_name : string;
    small_name : string;
    big_name : string;
    embed : morphism;
    left_chain : morphism list;
    right_chain : morphism list;
    aliases : (string * string) list
  }

type item =
  | Lattice of lattice
  | Embedding of embedding

type object_shape =
  | Base_object
  | Product_object of axis_object list

type lattice_object =
  { name : string;
    opposite_name : string;
    shape : object_shape;
    repr_validator : repr_validator;
    descriptor : descriptor;
    op_descriptor : descriptor
  }

type t =
  { items : item list;
    lattices : lattice String_map.t;
    objects : lattice_object String_map.t
  }

let opp_descriptor (desc : descriptor) =
  let dual_mask = desc.mask land lnot desc.nat_mask in
  { bits = desc.bits;
    mask = desc.mask;
    nat_mask = dual_mask;
    down_nat = desc.down_dual;
    up_nat = desc.up_dual;
    down_dual = desc.down_nat;
    up_dual = desc.up_nat
  }

let check_module_name name loc =
  if not (Name.is_module_name name)
  then Error.failf loc "invalid OCaml module name %S" name

let check_value_name kind name loc =
  if not (Name.is_value_name name)
  then Error.failf loc "invalid OCaml %s name %S" kind name

let reserved_base_names =
  String_set.of_list
    [ "bottom";
      "top";
      "leq";
      "equal";
      "join";
      "meet";
      "sub";
      "imply";
      "name";
      "of_name";
      "pp";
      "show"
    ]

let reserved_product_names =
  String_set.of_list
    [ "make";
      "view";
      "of_view";
      "bottom";
      "top";
      "leq";
      "equal";
      "join";
      "meet";
      "sub";
      "imply";
      "pp";
      "show"
    ]

let located_txt (x : string Location.located) = x.txt

let located_loc (x : 'a Location.located) = x.loc

let parse_alias_slot alias =
  let s = located_txt alias.Ast.slot in
  let len = String.length s in
  if s = "embed"
  then `Embed
  else if len >= 5 && String.sub s 0 4 = "left"
  then (
    try `Left (int_of_string (String.sub s 4 (len - 4))) with
    | Failure _ -> Error.failf (located_loc alias.slot) "invalid alias slot %S" s)
  else if len >= 6 && String.sub s 0 5 = "right"
  then (
    try `Right (int_of_string (String.sub s 5 (len - 5))) with
    | Failure _ -> Error.failf (located_loc alias.slot) "invalid alias slot %S" s)
  else Error.failf (located_loc alias.slot) "invalid alias slot %S" s

let ensure_unique_name used name loc kind =
  if String_set.mem name !used
  then Error.failf loc "duplicate generated %s name %S" kind name;
  used := String_set.add name !used

let matrix_make n value = Array.init n (fun _ -> Array.make n value)

let lattice_reverse (lattice : finite_lattice) =
  let n = Array.length lattice.element_names in
  let leq = matrix_make n false in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      leq.(i).(j) <- lattice.leq.(j).(i)
    done
  done;
  { element_names = lattice.element_names;
    leq;
    join = lattice.meet;
    meet = lattice.join;
    bottom = lattice.top;
    top = lattice.bottom
  }

let compute_unique_extreme kind leq n loc =
  let candidates =
    List.filter
      (fun i ->
        let ok = ref true in
        for j = 0 to n - 1 do
          ok :=
            !ok
            &&
            if kind = `Bottom then leq.(i).(j) else leq.(j).(i)
        done;
        !ok)
      (List.init n Fun.id)
  in
  match candidates with
  | [ x ] -> x
  | [] ->
    Error.failf loc
      "base lattice is missing a unique %s"
      (if kind = `Bottom then "bottom" else "top")
  | _ ->
    Error.failf loc
      "base lattice does not have a unique %s"
      (if kind = `Bottom then "bottom" else "top")

let compute_binary_op kind leq n loc =
  Array.init n (fun a ->
      Array.init n (fun b ->
          let candidates =
            List.filter
              (fun c ->
                if kind = `Join
                then leq.(a).(c) && leq.(b).(c)
                else leq.(c).(a) && leq.(c).(b))
              (List.init n Fun.id)
          in
          let chosen =
            List.filter
              (fun c ->
                List.for_all
                  (fun other ->
                    if kind = `Join then leq.(c).(other) else leq.(other).(c))
                  candidates)
              candidates
          in
          match chosen with
          | [ x ] -> x
          | [] ->
            Error.failf loc
              "base lattice is missing a unique %s for %d and %d"
              (if kind = `Join then "join" else "meet")
              a
              b
          | _ ->
            Error.failf loc
              "base lattice does not have a unique %s for %d and %d"
              (if kind = `Join then "join" else "meet")
              a
              b))

let check_distributive join meet n loc =
  for x = 0 to n - 1 do
    for y = 0 to n - 1 do
      for z = 0 to n - 1 do
        if meet.(x).(join.(y).(z)) <> join.(meet.(x).(y)).(meet.(x).(z))
        then Error.failf loc "base lattice is not distributive"
      done
    done
  done

let make_finite_lattice ~loc element_names edges =
  let n = Array.length element_names in
  let leq = matrix_make n false in
  for i = 0 to n - 1 do
    leq.(i).(i) <- true
  done;
  List.iter (fun (a, b) -> leq.(a).(b) <- true) edges;
  for k = 0 to n - 1 do
    for i = 0 to n - 1 do
      if leq.(i).(k)
      then
        for j = 0 to n - 1 do
          leq.(i).(j) <- leq.(i).(j) || (leq.(i).(k) && leq.(k).(j))
        done
    done
  done;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i <> j && leq.(i).(j) && leq.(j).(i)
      then Error.failf loc "base lattice contains a cycle"
    done
  done;
  let bottom = compute_unique_extreme `Bottom leq n loc in
  let top = compute_unique_extreme `Top leq n loc in
  let join = compute_binary_op `Join leq n loc in
  let meet = compute_binary_op `Meet leq n loc in
  check_distributive join meet n loc;
  { element_names; leq; join; meet; bottom; top }

let join_irreducibles (lattice : finite_lattice) =
  let n = Array.length lattice.element_names in
  List.filter
    (fun x ->
      x <> lattice.bottom
      &&
      let reducible = ref false in
      for a = 0 to n - 1 do
        for b = 0 to n - 1 do
          if a <> x
             && b <> x
             && lattice.leq.(a).(x)
             && lattice.leq.(b).(x)
             && lattice.join.(a).(b) = x
          then reducible := true
        done
      done;
      not !reducible)
    (List.init n Fun.id)

let down_schedule comparable order =
  let buckets = Hashtbl.create 8 in
  List.iteri
    (fun source_index source ->
      List.iteri
        (fun target_index target ->
          if target_index < source_index && comparable source target
          then (
            let shift = source_index - target_index in
            let mask =
              match Hashtbl.find_opt buckets shift with
              | Some mask -> mask
              | None -> 0
            in
            Hashtbl.replace buckets shift (mask lor (1 lsl source_index))))
        order)
    order;
  Hashtbl.to_seq buckets
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map (fun (shift, mask) -> { shift; mask })

let up_schedule comparable order =
  let buckets = Hashtbl.create 8 in
  List.iteri
    (fun source_index source ->
      List.iteri
        (fun target_index target ->
          if target_index > source_index && comparable source target
          then (
            let shift = target_index - source_index in
            let mask =
              match Hashtbl.find_opt buckets shift with
              | Some mask -> mask
              | None -> 0
            in
            Hashtbl.replace buckets shift (mask lor (1 lsl source_index))))
        order)
    order;
  Hashtbl.to_seq buckets
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map (fun (shift, mask) -> { shift; mask })

let list_fold_lefti f init xs =
  let rec loop index acc = function
    | [] -> acc
    | x :: rest -> loop (index + 1) (f acc index x) rest
  in
  loop 0 init xs

let build_base_descriptor decl_direction logical loc =
  let native =
    match decl_direction with
    | Ast.Lt -> logical
    | Ast.Gt -> lattice_reverse logical
  in
  let jis = join_irreducibles native in
  let ji_order =
    List.sort
      (fun a b ->
        let rank i =
          Array.fold_left
            (fun acc below -> if below then acc + 1 else acc)
            0
            native.leq.(i)
        in
        match Int.compare (rank b) (rank a) with
        | 0 -> String.compare native.element_names.(a) native.element_names.(b)
        | n -> n)
      jis
  in
  let bits = List.length ji_order in
  if bits >= Sys.int_size
  then Error.failf loc "too many join-irreducibles";
  let bit_of_element element =
    list_fold_lefti
      (fun acc index ji ->
        if native.leq.(ji).(element) then acc lor (1 lsl index) else acc)
      0
      ji_order
  in
  let element_values =
    Array.init (Array.length logical.element_names) (fun i -> bit_of_element i)
  in
  let down_nat =
    down_schedule
      (fun source target -> native.leq.(target).(source))
      ji_order
  in
  let up_nat =
    up_schedule
      (fun source target -> native.leq.(source).(target))
      ji_order
  in
  let mask = if bits = 0 then 0 else (1 lsl bits) - 1 in
  let native_desc =
    { bits;
      mask;
      nat_mask = mask;
      down_nat;
      up_nat;
      down_dual = down_nat;
      up_dual = up_nat
    }
  in
  let repr_validator = { mask; down = down_nat } in
  let descriptor, op_descriptor =
    match decl_direction with
    | Ast.Lt -> native_desc, opp_descriptor native_desc
    | Ast.Gt -> opp_descriptor native_desc, native_desc
  in
  element_values, repr_validator, descriptor, op_descriptor

let descriptor_of_lattice = function
  | Base base -> base.descriptor, base.op_descriptor
  | Product product -> product.descriptor, product.op_descriptor

let repr_validator_of_lattice = function
  | Base base -> base.repr_validator
  | Product product -> product.repr_validator

let check_generated_element_names base loc =
  let used = ref reserved_base_names in
  Array.iter
    (fun name ->
      let value_name = Name.snake_case name in
      check_value_name "value" value_name loc;
      ensure_unique_name used value_name loc "value")
    base.element_names

let check_product_field_names (fields : Ast.field list) loc =
  let used = ref reserved_product_names in
  List.iter
    (fun (field : Ast.field) ->
      let name = located_txt field.Ast.name in
      check_value_name "field" name (located_loc field.name);
      List.iter
        (fun generated ->
          ensure_unique_name used generated (located_loc field.name) "value")
        [ name;
          "with_" ^ name;
          name ^ "_bot";
          name ^ "_top";
          "proj_" ^ name;
          "min_with_" ^ name;
          "max_with_" ^ name
        ])
    fields;
  ignore loc

let axis_object_of_field (field : field) =
  { name = field.name;
    ctor_name = String.capitalize_ascii field.name;
    carrier_name = field.lattice_name;
    declared_opposite = field.declared_opposite;
    shift = field.shift;
    raw_mask = field.raw_mask;
    layout_mask = field.layout_mask;
    proj_name = "proj_" ^ field.name;
    with_name = "with_" ^ field.name;
    bot_name = field.name ^ "_bot";
    top_name = field.name ^ "_top";
    min_with_name = "min_with_" ^ field.name;
    max_with_name = "max_with_" ^ field.name
  }

let shift_round amount (round : round) =
  { round with mask = round.mask lsl amount }

let shift_descriptor amount desc =
  { bits = desc.bits;
    mask = desc.mask lsl amount;
    nat_mask = desc.nat_mask lsl amount;
    down_nat = List.map (shift_round amount) desc.down_nat;
    up_nat = List.map (shift_round amount) desc.up_nat;
    down_dual = List.map (shift_round amount) desc.down_dual;
    up_dual = List.map (shift_round amount) desc.up_dual
  }

let shift_repr_validator amount repr_validator =
  { mask = repr_validator.mask lsl amount;
    down = List.map (shift_round amount) repr_validator.down
  }

let merge_rounds rounds =
  let buckets = Hashtbl.create 8 in
  List.iter
    (fun ({ shift; mask } : round) ->
      let combined =
        match Hashtbl.find_opt buckets shift with
        | None -> mask
        | Some prev -> prev lor mask
      in
      Hashtbl.replace buckets shift combined)
    rounds;
  Hashtbl.to_seq buckets
  |> List.of_seq
  |> List.sort (fun (a, _) (b, _) -> Int.compare a b)
  |> List.map (fun (shift, mask) -> { shift; mask })

let combine_descriptors descs =
  let combined =
    List.fold_left
      (fun acc desc ->
        { bits = acc.bits + desc.bits;
          mask = acc.mask lor desc.mask;
          nat_mask = acc.nat_mask lor desc.nat_mask;
          down_nat = acc.down_nat @ desc.down_nat;
          up_nat = acc.up_nat @ desc.up_nat;
          down_dual = acc.down_dual @ desc.down_dual;
          up_dual = acc.up_dual @ desc.up_dual
        })
      { bits = 0;
        mask = 0;
        nat_mask = 0;
        down_nat = [];
        up_nat = [];
        down_dual = [];
        up_dual = []
      }
      descs
  in
  { combined with
    down_nat = merge_rounds combined.down_nat;
    up_nat = merge_rounds combined.up_nat;
    down_dual = merge_rounds combined.down_dual;
    up_dual = merge_rounds combined.up_dual
  }

let combine_repr_validators repr_validators =
  let combined =
    List.fold_left
      (fun acc repr_validator ->
        { mask = acc.mask lor repr_validator.mask;
          down = acc.down @ repr_validator.down
        })
      { mask = 0; down = [] }
      repr_validators
  in
  { combined with down = merge_rounds combined.down }

let name_of_lattice = function Base base -> base.name | Product product -> product.name

let object_of_lattice = function
  | Base base ->
    { name = base.name;
      opposite_name = Name.op_module_name base.name;
      shape = Base_object;
      repr_validator = base.repr_validator;
      descriptor = base.descriptor;
      op_descriptor = base.op_descriptor
    }
  | Product product ->
    { name = product.name;
      opposite_name = Name.op_module_name product.name;
      shape = Product_object product.axes;
      repr_validator = product.repr_validator;
      descriptor = product.descriptor;
      op_descriptor = product.op_descriptor
    }

let finite_of_lattice = function
  | Base base -> base.logical
  | Product _ -> invalid_arg "products are not enumerated in v1"

let build_aliases aliases function_names =
  let used = ref function_names in
  List.map
    (fun alias ->
      let slot =
        match parse_alias_slot alias with
        | `Embed -> "embed"
        | `Left n -> "left" ^ string_of_int n
        | `Right n -> "right" ^ string_of_int n
      in
      if not (String_set.mem slot function_names)
      then Error.failf (located_loc alias.slot) "alias slot %S refers to a missing function" slot;
      let alias_name = Name.snake_case (located_txt alias.name) in
      check_value_name "alias" alias_name (located_loc alias.name);
      ensure_unique_name used alias_name (located_loc alias.name) "embedding function";
      alias_name, slot)
    aliases

let greatest_under (lattice : finite_lattice) predicate =
  let n = Array.length lattice.element_names in
  let candidates = List.filter predicate (List.init n Fun.id) in
  match
    List.filter
      (fun x ->
        List.for_all
          (fun other -> lattice.leq.(other).(x))
          candidates)
      candidates
  with
  | [ x ] -> Some x
  | _ -> None

let least_over (lattice : finite_lattice) predicate =
  let n = Array.length lattice.element_names in
  let candidates = List.filter predicate (List.init n Fun.id) in
  match
    List.filter
      (fun x ->
        List.for_all
          (fun other -> lattice.leq.(x).(other))
          candidates)
      candidates
  with
  | [ x ] -> Some x
  | _ -> None

let left_adjoint (dom : finite_lattice) (cod : finite_lattice) map =
  Array.init (Array.length cod.element_names) (fun y ->
      match least_over dom (fun x -> cod.leq.(y).(map.(x))) with
      | Some x -> x
      | None -> raise Not_found)

let right_adjoint (dom : finite_lattice) (cod : finite_lattice) map =
  Array.init (Array.length cod.element_names) (fun y ->
      match greatest_under dom (fun x -> cod.leq.(map.(x)).(y)) with
      | Some x -> x
      | None -> raise Not_found)

let morphism_equal a b =
  a.domain = b.domain
  && a.codomain = b.codomain
  && Array.length a.map = Array.length b.map
  && Array.for_all2 Int.equal a.map b.map

let rec build_left_chain lattices seen current acc =
  let dom = finite_of_lattice (String_map.find current.domain lattices) in
  let cod = finite_of_lattice (String_map.find current.codomain lattices) in
  match
    try Some (left_adjoint dom cod current.map) with
    | Not_found -> None
  with
  | None -> List.rev acc
  | Some map ->
    let morphism = { domain = current.codomain; codomain = current.domain; map } in
    if List.exists (morphism_equal morphism) seen
    then List.rev acc
    else build_left_chain lattices (morphism :: seen) morphism (morphism :: acc)

let rec build_right_chain lattices seen current acc =
  let dom = finite_of_lattice (String_map.find current.domain lattices) in
  let cod = finite_of_lattice (String_map.find current.codomain lattices) in
  match
    try Some (right_adjoint dom cod current.map) with
    | Not_found -> None
  with
  | None -> List.rev acc
  | Some map ->
    let morphism = { domain = current.codomain; codomain = current.domain; map } in
    if List.exists (morphism_equal morphism) seen
    then List.rev acc
    else build_right_chain lattices (morphism :: seen) morphism (morphism :: acc)

let resolve ast =
  let global_modules = ref String_set.empty in
  let lattices = ref String_map.empty in
  let items = ref [] in
  let register_module name loc =
    check_module_name name loc;
    ensure_unique_name global_modules name loc "module"
  in
  let register_lattice_name name loc =
    register_module name loc;
    register_module (Name.op_module_name name) loc
  in
  List.iter
    (function
      | Ast.Base { name; clauses } ->
        let lattice_name = located_txt name in
        if String_map.mem lattice_name !lattices
        then Error.failf (located_loc name) "duplicate lattice name %S" lattice_name;
        register_lattice_name lattice_name (located_loc name);
        let element_order = ref [] in
        let element_index = Hashtbl.create 16 in
        let logical_direction = ref None in
        let edges = ref [] in
        List.iter
          (function
            | Ast.Singleton elt ->
              let elt_name = located_txt elt in
              if not (Hashtbl.mem element_index elt_name)
              then (
                Hashtbl.add element_index elt_name (List.length !element_order);
                element_order := !element_order @ [ elt_name ])
            | Ast.Chain { direction; names; loc } ->
              (match !logical_direction with
               | None -> logical_direction := Some direction
               | Some prev when prev = direction -> ()
               | Some _ ->
                 Error.failf loc
                   "all non-singleton clauses in a base lattice must use the same direction");
              List.iter
                (fun elt ->
                  let elt_name = located_txt elt in
                  if not (Hashtbl.mem element_index elt_name)
                  then (
                    Hashtbl.add element_index elt_name (List.length !element_order);
                    element_order := !element_order @ [ elt_name ]))
                names;
              let indices =
                List.map (fun elt -> Hashtbl.find element_index (located_txt elt)) names
              in
              let rec add_edges = function
                | a :: ((b :: _) as rest) ->
                  edges :=
                    (match direction with
                     | Ast.Lt -> a, b
                     | Ast.Gt -> b, a)
                    :: !edges;
                  add_edges rest
                | _ -> ()
              in
              add_edges indices)
          clauses;
        let element_names = Array.of_list !element_order in
        let loc =
          match clauses with
          | [] -> located_loc name
          | Ast.Singleton elt :: _ -> Location.merge (located_loc name) (located_loc elt)
          | Ast.Chain chain :: _ -> Location.merge (located_loc name) chain.loc
        in
        let logical =
          make_finite_lattice ~loc element_names (List.rev !edges)
        in
        let direction = Option.value !logical_direction ~default:Ast.Lt in
        let element_values, repr_validator, descriptor, op_descriptor =
          build_base_descriptor direction logical (located_loc name)
        in
        let base =
          { name = lattice_name;
            element_names;
            element_values;
            element_value_names = Array.map Name.snake_case element_names;
            repr_validator;
            descriptor;
            op_descriptor;
            logical
          }
        in
        check_generated_element_names base (located_loc name);
        lattices := String_map.add lattice_name (Base base) !lattices;
        items := !items @ [ Lattice (Base base) ]
      | Ast.Product { name; fields } ->
        let lattice_name = located_txt name in
        if String_map.mem lattice_name !lattices
        then Error.failf (located_loc name) "duplicate lattice name %S" lattice_name;
        register_lattice_name lattice_name (located_loc name);
        if fields = []
        then Error.failf (located_loc name) "products must have at least one field";
        check_product_field_names fields (located_loc name);
        let offset = ref 0 in
        let shifted_descs = ref [] in
        let shifted_repr_validators = ref [] in
        let resolved_fields =
          List.map
            (fun (field : Ast.field) ->
              let referenced =
                match String_map.find_opt (located_txt field.ty.lattice) !lattices with
                | Some lattice -> lattice
                | None ->
                  Error.failf (located_loc field.ty.lattice)
                    "unknown lattice %S"
                    (located_txt field.ty.lattice)
              in
              let repr_validator = repr_validator_of_lattice referenced in
              let descriptor, op_descriptor = descriptor_of_lattice referenced in
              let chosen = if field.ty.opposite then op_descriptor else descriptor in
              let shift = !offset in
              let shifted = shift_descriptor shift chosen in
              let shifted_repr_validator =
                shift_repr_validator shift repr_validator
              in
              offset := !offset + chosen.bits;
              shifted_descs := !shifted_descs @ [ shifted ];
              shifted_repr_validators :=
                !shifted_repr_validators @ [ shifted_repr_validator ];
              { name = located_txt field.name;
                lattice_name = name_of_lattice referenced;
                declared_opposite = field.ty.opposite;
                shift;
                raw_mask = chosen.mask;
                layout_mask = chosen.mask lsl shift
              })
            fields
        in
        let repr_validator = combine_repr_validators !shifted_repr_validators in
        let descriptor = combine_descriptors !shifted_descs in
        let op_descriptor = opp_descriptor descriptor in
        let product =
          { name = lattice_name;
            fields = resolved_fields;
            axes = List.map axis_object_of_field resolved_fields;
            repr_validator;
            descriptor;
            op_descriptor
          }
        in
        lattices := String_map.add lattice_name (Product product) !lattices;
        items := !items @ [ Lattice (Product product) ]
      | Ast.Embedding { small; big; mappings; aliases } ->
        let small_name = located_txt small in
        let big_name = located_txt big in
        let module_name = Name.embedding_module_name small_name big_name in
        register_module module_name (Location.merge (located_loc small) (located_loc big));
        let small_lattice =
          match String_map.find_opt small_name !lattices with
          | Some (Base base) -> base
          | Some _ ->
            Error.failf (located_loc small)
              "embeddings are only supported between base lattices in v1"
          | None -> Error.failf (located_loc small) "unknown lattice %S" small_name
        in
        let big_lattice =
          match String_map.find_opt big_name !lattices with
          | Some (Base base) -> base
          | Some _ ->
            Error.failf (located_loc big)
              "embeddings are only supported between base lattices in v1"
          | None -> Error.failf (located_loc big) "unknown lattice %S" big_name
        in
        let small_index = Hashtbl.create 16 in
        Array.iteri
          (fun i name -> Hashtbl.add small_index name i)
          small_lattice.element_names;
        let big_index = Hashtbl.create 16 in
        Array.iteri
          (fun i name -> Hashtbl.add big_index name i)
          big_lattice.element_names;
        let map = Array.make (Array.length small_lattice.element_names) (-1) in
        List.iter
          (fun (mapping : Ast.mapping) ->
            let source =
              match Hashtbl.find_opt small_index (located_txt mapping.small) with
              | Some i -> i
              | None ->
                Error.failf (located_loc mapping.small)
                  "unknown element %S in %S"
                  (located_txt mapping.small)
                  small_name
            in
            let target =
              match Hashtbl.find_opt big_index (located_txt mapping.big) with
              | Some i -> i
              | None ->
                Error.failf (located_loc mapping.big)
                  "unknown element %S in %S"
                  (located_txt mapping.big)
                  big_name
            in
            if map.(source) <> -1
            then
              Error.failf (located_loc mapping.small)
                "duplicate mapping for element %S"
                (located_txt mapping.small);
            map.(source) <- target)
          mappings;
        Array.iteri
          (fun _i target ->
            if target = -1
            then
              Error.failf (located_loc small)
                "embedding must map every element of %S"
                small_name)
          map;
        let seen = Hashtbl.create 16 in
        Array.iter
          (fun target ->
            if Hashtbl.mem seen target
            then Error.failf (located_loc small) "embedding must be injective";
            Hashtbl.add seen target ())
          map;
        let n_small = Array.length small_lattice.element_names in
        for a = 0 to n_small - 1 do
          for b = 0 to n_small - 1 do
            if small_lattice.logical.leq.(a).(b)
               && not big_lattice.logical.leq.(map.(a)).(map.(b))
            then Error.failf (located_loc small) "embedding must be monotone";
            if big_lattice.logical.leq.(map.(a)).(map.(b))
               && not small_lattice.logical.leq.(a).(b)
            then Error.failf (located_loc small) "embedding must be order-reflecting"
          done
        done;
        let embed = { domain = small_name; codomain = big_name; map } in
        let left_chain = build_left_chain !lattices [ embed ] embed [] in
        let right_chain = build_right_chain !lattices [ embed ] embed [] in
        let function_names =
          let names = ref String_set.empty in
          ensure_unique_name names "embed" (located_loc small) "embedding function";
          List.iteri
            (fun i _ ->
              ensure_unique_name names ("left" ^ string_of_int (i + 1)) (located_loc small) "embedding function")
            left_chain;
          List.iteri
            (fun i _ ->
              ensure_unique_name names ("right" ^ string_of_int (i + 1)) (located_loc small) "embedding function")
            right_chain;
          !names
        in
        let aliases = build_aliases aliases function_names in
        let embedding =
          { module_name;
            small_name;
            big_name;
            embed;
            left_chain;
            right_chain;
            aliases
          }
        in
        items := !items @ [ Embedding embedding ])
    ast;
  let objects =
    String_map.fold
      (fun _ lattice acc ->
        let object_ = object_of_lattice lattice in
        String_map.add object_.name object_ acc)
      !lattices
      String_map.empty
  in
  { items = !items; lattices = !lattices; objects }
