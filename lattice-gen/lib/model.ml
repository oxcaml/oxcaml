module String_map = Map.Make (String)
module String_set = Set.Make (String)

let no_loc =
  let pos =
    { Lexing.pos_fname = "";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = 0
    }
  in
  Location.make pos pos

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
    element_values : int array;
    leq : bool array array;
    join : int array array;
    meet : int array array;
    bottom : int;
    top : int
  }

type lattice_expr =
  { name : string;
    opposite : bool
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
    ty : lattice_expr;
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

type product_field_info =
  { field : field;
    finite : finite_lattice;
    stride : int
  }

type product_expr_info =
  { product : product;
    fields : product_field_info array;
    positions : int String_map.t;
    total : int
  }

type lattice =
  | Base of base
  | Product of product

type morph_core =
  { name : string;
    source : lattice_expr;
    target : lattice_expr;
    map : int array;
    left_adjoint : int array option;
    right_adjoint : int array option;
    left_name : string option;
    right_name : string option
  }

type primitive_morph =
  { core : morph_core
  }

type bridge_expr =
  | Source_field of string
  | Morph_apply of
      { morph_name : string;
        source_field : string
      }
  | Min
  | Max
  | Join of bridge_expr * bridge_expr
  | Meet of bridge_expr * bridge_expr

type bridge_assignment =
  { target_field : string;
    expr : bridge_expr
  }

type product_bridge =
  { core : morph_core;
    assignments : bridge_assignment list
  }

type morph =
  | Primitive of primitive_morph
  | Bridge of product_bridge

type item =
  | Lattice of lattice
  | Morph of morph

type t =
  { items : item list;
    lattices : lattice String_map.t;
    morphs : morph String_map.t
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

let located_txt (x : string Location.located) = x.txt

let located_loc (x : 'a Location.located) = x.loc

let check_module_name name loc =
  if not (Name.is_module_name name)
  then Error.failf loc "invalid OCaml module name %S" name

let check_value_name kind name loc =
  if not (Name.is_value_name name)
  then Error.failf loc "invalid OCaml %s name %S" kind name

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
    element_values = lattice.element_values;
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
  { element_names;
    element_values = Array.init n Fun.id;
    leq;
    join;
    meet;
    bottom;
    top
  }

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

let reserved_base_names =
  String_set.of_list
    [ "min";
      "max";
      "le";
      "equal";
      "join";
      "meet";
      "subtract";
      "imply";
      "print"
    ]

let reserved_product_names =
  String_set.of_list
    [ "make";
      "view";
      "of_view";
      "min";
      "max";
      "le";
      "equal";
      "join";
      "meet";
      "subtract";
      "imply";
      "print";
      "split";
      "merge";
      "proj"
    ]

let check_generated_element_names base loc =
  let used = ref reserved_base_names in
  Array.iter
    (fun name ->
      let value_name = Name.snake_case_value_name name in
      check_value_name "value" value_name loc;
      ensure_unique_name used value_name loc "value")
    base.element_names

let check_product_field_names (fields : Ast.field list) =
  let used = ref reserved_product_names in
  List.iter
    (fun (field : Ast.field) ->
      let name = located_txt field.Ast.name in
      let generated_name = Name.escape_value_name name in
      check_value_name "field" generated_name (located_loc field.name);
      List.iter
        (fun generated ->
          ensure_unique_name used generated (located_loc field.name) "value")
        [ generated_name;
          "with_" ^ generated_name;
          "proj_" ^ generated_name
        ])
    fields

let axis_object_of_field (field : field) =
  { name = field.name;
    ctor_name = String.capitalize_ascii field.name;
    carrier_name = field.ty.name;
    declared_opposite = field.ty.opposite;
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

let name_of_lattice = function Base base -> base.name | Product product -> product.name

let module_name_of_expr (expr : lattice_expr) =
  if expr.opposite then Name.op_module_name expr.name else expr.name

let lattice_expr_equal (a : lattice_expr) (b : lattice_expr) =
  a.name = b.name && Bool.equal a.opposite b.opposite

let flip_expr (expr : lattice_expr) = { expr with opposite = not expr.opposite }

let effective_field_expr root_expr (field : field) =
  if root_expr.opposite then flip_expr field.ty else field.ty

let morph_core_of = function
  | Primitive primitive -> primitive.core
  | Bridge bridge -> bridge.core

let with_morph_core morph core =
  match morph with
  | Primitive _ -> Primitive { core }
  | Bridge bridge -> Bridge { bridge with core }

let emitted_module_exprs (model : t) =
  let seen = Hashtbl.create 32 in
  let acc = ref [] in
  let rec visit (expr : lattice_expr) =
    if not (Hashtbl.mem seen expr)
    then (
      Hashtbl.add seen expr ();
      (match String_map.find expr.name model.lattices with
       | Base _ -> ()
       | Product product ->
         List.iter
           (fun field -> visit (effective_field_expr expr field))
           product.fields);
      acc := expr :: !acc)
  in
  List.iter
    (function
      | Lattice lattice ->
        visit { name = name_of_lattice lattice; opposite = false }
      | Morph morph ->
        let core = morph_core_of morph in
        visit core.source;
        visit core.target)
    model.items;
  List.rev !acc

let value_index (lattice : finite_lattice) =
  let indices = Hashtbl.create (Array.length lattice.element_values) in
  Array.iteri (fun i value -> Hashtbl.replace indices value i) lattice.element_values;
  indices

let find_value_index (lattice : finite_lattice) indices value =
  match Hashtbl.find_opt indices value with
  | Some index -> index
  | None ->
    invalid_arg
      (Printf.sprintf
         "unknown element value %d for finite lattice of size %d"
         value
         (Array.length lattice.element_values))

let greatest_under (lattice : finite_lattice) predicate =
  let n = Array.length lattice.element_names in
  let best = ref None in
  for x = 0 to n - 1 do
    if predicate x
    then
      match !best with
      | None -> best := Some x
      | Some current when lattice.leq.(current).(x) -> best := Some x
      | Some _ -> ()
  done;
  match !best with
  | None -> None
  | Some best ->
    let ok = ref true in
    for x = 0 to n - 1 do
      if predicate x && not lattice.leq.(x).(best) then ok := false
    done;
    if !ok then Some best else None

let least_over (lattice : finite_lattice) predicate =
  let n = Array.length lattice.element_names in
  let best = ref None in
  for x = 0 to n - 1 do
    if predicate x
    then
      match !best with
      | None -> best := Some x
      | Some current when lattice.leq.(x).(current) -> best := Some x
      | Some _ -> ()
  done;
  match !best with
  | None -> None
  | Some best ->
    let ok = ref true in
    for x = 0 to n - 1 do
      if predicate x && not lattice.leq.(best).(x) then ok := false
    done;
    if !ok then Some best else None

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

let int_array_equal a b =
  Array.length a = Array.length b && Array.for_all2 Int.equal a b

let resolve ast =
  let lattice_decls = Hashtbl.create 16 in
  let morph_decls = Hashtbl.create 16 in
  let lattice_order = ref [] in
  let morph_order = ref [] in
  let adjoint_chains = ref [] in
  let module_names = ref String_set.empty in
  let morph_names = ref String_set.empty in
  let register_lattice_name name loc =
    check_module_name name loc;
    ensure_unique_name module_names name loc "module";
    ensure_unique_name module_names (Name.op_module_name name) loc "module"
  in
  let register_morph_name name loc =
    let generated_name = Name.escape_value_name name in
    check_value_name "morphism" generated_name loc;
    ensure_unique_name morph_names generated_name loc "morphism"
  in
  List.iter
    (function
      | Ast.Base { name; _ } as decl ->
        let lattice_name = located_txt name in
        if Hashtbl.mem lattice_decls lattice_name
        then Error.failf (located_loc name) "duplicate lattice name %S" lattice_name;
        register_lattice_name lattice_name (located_loc name);
        Hashtbl.add lattice_decls lattice_name decl;
        lattice_order := !lattice_order @ [ lattice_name ]
      | Ast.Product { name; _ } as decl ->
        let lattice_name = located_txt name in
        if Hashtbl.mem lattice_decls lattice_name
        then Error.failf (located_loc name) "duplicate lattice name %S" lattice_name;
        register_lattice_name lattice_name (located_loc name);
        Hashtbl.add lattice_decls lattice_name decl;
        lattice_order := !lattice_order @ [ lattice_name ]
      | Ast.Primitive_morph { name; _ } as decl ->
        let morph_name = located_txt name in
        if Hashtbl.mem morph_decls morph_name
        then Error.failf (located_loc name) "duplicate morph name %S" morph_name;
        register_morph_name morph_name (located_loc name);
        Hashtbl.add morph_decls morph_name decl;
        morph_order := !morph_order @ [ morph_name ]
      | Ast.Product_bridge { name; _ } as decl ->
        let morph_name = located_txt name in
        if Hashtbl.mem morph_decls morph_name
        then Error.failf (located_loc name) "duplicate morph name %S" morph_name;
        register_morph_name morph_name (located_loc name);
        Hashtbl.add morph_decls morph_name decl;
        morph_order := !morph_order @ [ morph_name ]
      | Ast.Adjoint_chain chain ->
        adjoint_chains := !adjoint_chains @ [ chain ])
    ast;
  let parse_expr (expr : Ast.lattice_expr) =
    { name = located_txt expr.lattice; opposite = expr.opposite }
  in
  let resolved_lattices = ref String_map.empty in
  let rec resolve_lattice visiting name =
    match String_map.find_opt name !resolved_lattices with
    | Some lattice -> lattice
    | None ->
      if String_set.mem name visiting
      then Error.failf no_loc "cyclic product definition involving %S" name;
      let visiting = String_set.add name visiting in
      match Hashtbl.find_opt lattice_decls name with
      | None -> Error.failf no_loc "unknown lattice %S" name
      | Some (Ast.Base { name; clauses }) ->
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
        let logical = make_finite_lattice ~loc element_names (List.rev !edges) in
        let direction = Option.value !logical_direction ~default:Ast.Lt in
        let element_values, repr_validator, descriptor, op_descriptor =
          build_base_descriptor direction logical (located_loc name)
        in
        let logical = { logical with element_values } in
        let base =
          { name = located_txt name;
            element_names;
            element_values;
            element_value_names = Array.map Name.snake_case_value_name element_names;
            repr_validator;
            descriptor;
            op_descriptor;
            logical
          }
        in
        check_generated_element_names base (located_loc name);
        let lattice = Base base in
        resolved_lattices := String_map.add base.name lattice !resolved_lattices;
        lattice
      | Some (Ast.Product { name; fields }) ->
        if fields = []
        then Error.failf (located_loc name) "products must have at least one field";
        check_product_field_names fields;
        let offset = ref 0 in
        let shifted_descs = ref [] in
        let shifted_repr_validators = ref [] in
        let resolved_fields =
          List.map
            (fun (field : Ast.field) ->
              let ty = parse_expr field.ty in
              let referenced =
                resolve_lattice visiting ty.name
              in
              let repr_validator =
                match referenced with
                | Base base -> base.repr_validator
                | Product product -> product.repr_validator
              in
              let descriptor, op_descriptor =
                match referenced with
                | Base base -> base.descriptor, base.op_descriptor
                | Product product -> product.descriptor, product.op_descriptor
              in
              let chosen = if ty.opposite then op_descriptor else descriptor in
              let shift = !offset in
              let shifted = shift_descriptor shift chosen in
              let shifted_repr_validator = shift_repr_validator shift repr_validator in
              offset := !offset + chosen.bits;
              shifted_descs := !shifted_descs @ [ shifted ];
              shifted_repr_validators :=
                !shifted_repr_validators @ [ shifted_repr_validator ];
              { name = located_txt field.name;
                ty;
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
          { name = located_txt name;
            fields = resolved_fields;
            axes = List.map axis_object_of_field resolved_fields;
            repr_validator;
            descriptor;
            op_descriptor
          }
        in
        let lattice = Product product in
        resolved_lattices := String_map.add product.name lattice !resolved_lattices;
        lattice
      | Some _ -> assert false
  in
  List.iter (fun name -> ignore (resolve_lattice String_set.empty name)) !lattice_order;
  let resolved_lattices = !resolved_lattices in
  let find_base_expr (expr : lattice_expr) loc =
    match String_map.find expr.name resolved_lattices with
    | Base base -> base
    | Product _ ->
      Error.failf loc "primitive morph endpoints must be base lattices, got %S" expr.name
  in
  let product_fields_for_expr (expr : lattice_expr) =
    match String_map.find expr.name resolved_lattices with
    | Base _ -> None
    | Product product ->
      Some
        (List.map
           (fun (field : field) ->
             (field.name, if expr.opposite then flip_expr field.ty else field.ty))
           product.fields)
  in
  let find_field_type (fields : (string * lattice_expr) list) field_name =
    match List.find_opt (fun (name, _) -> String.equal name field_name) fields with
    | Some (_, ty) -> ty
    | None -> raise Not_found
  in
  let finite_cache = Hashtbl.create 32 in
  let rec finite_of_expr (expr : lattice_expr) =
    match Hashtbl.find_opt finite_cache expr with
    | Some finite -> finite
    | None ->
      let finite =
        match String_map.find expr.name resolved_lattices with
        | Base base ->
          if expr.opposite then lattice_reverse base.logical else base.logical
        | Product product ->
          let effective_fields : (field * finite_lattice) array =
            Array.of_list
              (List.map
                 (fun (field : field) ->
                   let field_expr =
                     if expr.opposite then flip_expr field.ty else field.ty
                   in
                   field, finite_of_expr field_expr)
                 product.fields)
          in
          let arity = Array.length effective_fields in
          let dims =
            Array.map
              (fun (_, (field_finite : finite_lattice)) ->
                Array.length field_finite.element_names)
              effective_fields
          in
          let total =
            Array.fold_left
              (fun acc dim ->
                if dim = 0 then 0 else acc * dim)
              1
              dims
          in
          let decode ordinal =
            let coords = Array.make arity 0 in
            let remainder = ref ordinal in
            for i = arity - 1 downto 0 do
              let dim = dims.(i) in
              coords.(i) <- !remainder mod dim;
              remainder := !remainder / dim
            done;
            coords
          in
          let coords = Array.init total decode in
          let pack coord =
            Array.fold_left
              (fun acc i ->
                let field, (field_finite : finite_lattice) = effective_fields.(i) in
                let value = field_finite.element_values.(coord.(i)) in
                acc lor ((value land field.raw_mask) lsl field.shift))
              0
              (Array.init arity Fun.id)
          in
          let element_values = Array.map pack coords in
          let element_names =
            Array.map
              (fun coord ->
                let pieces =
                  Array.to_list
                    (Array.mapi
                       (fun i field_index ->
                         let field, (field_finite : finite_lattice) = effective_fields.(i) in
                         field.name ^ "=" ^ field_finite.element_names.(field_index))
                       coord)
                in
                "{ " ^ String.concat "; " pieces ^ " }")
              coords
          in
          let indices = Hashtbl.create total in
          Array.iteri (fun i value -> Hashtbl.add indices value i) element_values;
          let finite_stub : finite_lattice =
            { element_names;
              element_values;
              leq = [||];
              join = [||];
              meet = [||];
              bottom = 0;
              top = 0
            }
          in
          let pointwise make =
            Array.init total (fun left ->
                Array.init total (fun right ->
                    let coord = Array.make arity 0 in
                    for i = 0 to arity - 1 do
                      let _, (field_finite : finite_lattice) = effective_fields.(i) in
                      coord.(i) <- make field_finite coords.(left).(i) coords.(right).(i)
                    done;
                    find_value_index finite_stub indices (pack coord)))
          in
          let leq =
            Array.init total (fun left ->
                Array.init total (fun right ->
                    let ok = ref true in
                    for i = 0 to arity - 1 do
                      let _, (field_finite : finite_lattice) = effective_fields.(i) in
                      ok :=
                        !ok
                        && field_finite.leq.(coords.(left).(i)).(coords.(right).(i))
                    done;
                    !ok))
          in
          let join =
            pointwise (fun field_finite left right -> field_finite.join.(left).(right))
          in
          let meet =
            pointwise (fun field_finite left right -> field_finite.meet.(left).(right))
          in
          let bottom_coord =
            Array.init arity (fun i ->
                let _, (field_finite : finite_lattice) = effective_fields.(i) in
                field_finite.bottom)
          in
          let top_coord =
            Array.init arity (fun i ->
                let _, (field_finite : finite_lattice) = effective_fields.(i) in
                field_finite.top)
          in
          let bottom = find_value_index finite_stub indices (pack bottom_coord) in
          let top = find_value_index finite_stub indices (pack top_coord) in
          { element_names; element_values; leq; join; meet; bottom; top }
      in
      Hashtbl.add finite_cache expr finite;
      finite
  in
  let product_info_of_expr (expr : lattice_expr) =
    let product =
      match String_map.find expr.name resolved_lattices with
      | Product product -> product
      | Base _ -> invalid_arg "expected product expression"
    in
    let effective_fields =
      Array.of_list
        (List.map
           (fun (field : field) ->
             let field_expr =
               if expr.opposite then flip_expr field.ty else field.ty
             in
             field, finite_of_expr field_expr)
           product.fields)
    in
    let arity = Array.length effective_fields in
    let dims =
      Array.map
        (fun (_, (field_finite : finite_lattice)) ->
          Array.length field_finite.element_names)
        effective_fields
    in
    let total =
      Array.fold_left
        (fun acc dim ->
          if dim = 0 then 0 else acc * dim)
        1
        dims
    in
    let strides = Array.make arity 1 in
    for i = arity - 2 downto 0 do
      strides.(i) <- strides.(i + 1) * dims.(i + 1)
    done;
    let fields =
      Array.mapi
        (fun i (field, finite) -> { field; finite; stride = strides.(i) })
        effective_fields
    in
    let positions =
      let positions = ref String_map.empty in
      Array.iteri
        (fun i (info : product_field_info) ->
          positions := String_map.add info.field.name i !positions)
        fields;
      !positions
    in
    { product; fields; positions; total }
  in
  let product_decode_coord (info : product_expr_info) ordinal =
    let arity = Array.length info.fields in
    let coords = Array.make arity 0 in
    let remainder = ref ordinal in
    for i = arity - 1 downto 0 do
      let dim = Array.length info.fields.(i).finite.element_names in
      coords.(i) <- !remainder mod dim;
      remainder := !remainder / dim
    done;
    coords
  in
  let product_encode_coord (info : product_expr_info) coord =
    let ordinal = ref 0 in
    Array.iteri
      (fun i field_index ->
        ordinal := !ordinal + (field_index * info.fields.(i).stride))
      coord;
    !ordinal
  in
  let check_monotone
      morph_name
      loc
      (source_finite : finite_lattice)
      (target_finite : finite_lattice)
      map
    =
    let n = Array.length source_finite.element_names in
    for i = 0 to n - 1 do
      for j = 0 to n - 1 do
        if source_finite.leq.(i).(j)
           && not target_finite.leq.(map.(i)).(map.(j))
        then Error.failf loc "morphism %S is not monotone" morph_name
      done
    done
  in
  let with_adjoint_metadata core =
    let source_finite = finite_of_expr core.source in
    let target_finite = finite_of_expr core.target in
    let left_adjoint =
      try Some (left_adjoint source_finite target_finite core.map) with
      | Not_found -> None
    in
    let right_adjoint =
      try Some (right_adjoint source_finite target_finite core.map) with
      | Not_found -> None
    in
    { core with left_adjoint; right_adjoint }
  in
  let resolved_morphs = ref String_map.empty in
  let rec resolve_morph visiting morph_name =
    match String_map.find_opt morph_name !resolved_morphs with
    | Some morph -> morph
    | None ->
      if String_set.mem morph_name visiting
      then Error.failf no_loc "cyclic morph definition involving %S" morph_name;
      let visiting = String_set.add morph_name visiting in
      let resolved =
        match Hashtbl.find morph_decls morph_name with
        | Ast.Primitive_morph { name; source; target; mappings } ->
          let source_expr = parse_expr source in
          let target_expr = parse_expr target in
          let source_base = find_base_expr source_expr (located_loc source.lattice) in
          let target_base = find_base_expr target_expr (located_loc target.lattice) in
          let source_index = Hashtbl.create 16 in
          Array.iteri (fun i elt -> Hashtbl.add source_index elt i) source_base.element_names;
          let target_index = Hashtbl.create 16 in
          Array.iteri (fun i elt -> Hashtbl.add target_index elt i) target_base.element_names;
          let map = Array.make (Array.length source_base.element_names) (-1) in
          List.iter
            (fun (mapping : Ast.primitive_mapping) ->
              let src_name = located_txt mapping.source in
              let dst_name = located_txt mapping.target in
              let src =
                match Hashtbl.find_opt source_index src_name with
                | Some i -> i
                | None ->
                  Error.failf
                    (located_loc mapping.source)
                    "unknown element %S in %S"
                    src_name
                    source_base.name
              in
              let dst =
                match Hashtbl.find_opt target_index dst_name with
                | Some i -> i
                | None ->
                  Error.failf
                    (located_loc mapping.target)
                    "unknown element %S in %S"
                    dst_name
                    target_base.name
              in
              if map.(src) <> -1
              then
                Error.failf
                  (located_loc mapping.source)
                  "duplicate mapping for element %S"
                  src_name;
              map.(src) <- dst)
            mappings;
          Array.iteri
            (fun _ target ->
              if target = -1
              then
                Error.failf
                  (located_loc name)
                  "morphism must map every element of %S"
                  source_base.name)
            map;
          let source_finite = finite_of_expr source_expr in
          let target_finite = finite_of_expr target_expr in
          check_monotone morph_name (located_loc name) source_finite target_finite map;
          Primitive
            { core =
                with_adjoint_metadata
                  { name = morph_name;
                    source = source_expr;
                    target = target_expr;
                    map;
                    left_adjoint = None;
                    right_adjoint = None;
                    left_name = None;
                    right_name = None
                  }
            }
        | Ast.Product_bridge { name; source; target; assignments } ->
          let source_expr = parse_expr source in
          let target_expr = parse_expr target in
          let source_fields =
            match product_fields_for_expr source_expr with
            | Some fields -> fields
            | None ->
              Error.failf
                (located_loc source.lattice)
                "product bridge source must be a product, got %S"
                source_expr.name
          in
          let target_fields =
            match product_fields_for_expr target_expr with
            | Some fields -> fields
            | None ->
              Error.failf
                (located_loc target.lattice)
                "product bridge target must be a product, got %S"
                target_expr.name
          in
          let seen_targets = Hashtbl.create 16 in
          let rec resolve_bridge_expr
              (target_field : string)
              (target_ty : lattice_expr)
              (expr : Ast.bridge_expr)
            =
            match expr with
            | Ast.Source_field field ->
              let source_field = located_txt field in
              let source_ty =
                match find_field_type source_fields source_field with
                | ty -> ty
                | exception Not_found ->
                  Error.failf
                    (located_loc field)
                    "unknown source field %S"
                    source_field
              in
              if not (lattice_expr_equal source_ty target_ty)
              then
                Error.failf
                  (located_loc field)
                  "field %S expects %s but source field %S has %s"
                  target_field
                  (module_name_of_expr target_ty)
                  source_field
                  (module_name_of_expr source_ty);
              Source_field source_field
            | Ast.Morph_apply { morph; field } ->
              let referenced = resolve_morph visiting (located_txt morph) in
              let referenced_core = morph_core_of referenced in
              let source_field = located_txt field in
              let source_ty =
                match find_field_type source_fields source_field with
                | ty -> ty
                | exception Not_found ->
                  Error.failf
                    (located_loc field)
                    "unknown source field %S"
                    source_field
              in
              if not (lattice_expr_equal referenced_core.source source_ty)
              then
                Error.failf
                  (located_loc morph)
                  "morphism %S expects %s but source field %S has %s"
                  referenced_core.name
                  (module_name_of_expr referenced_core.source)
                  source_field
                  (module_name_of_expr source_ty);
              if not (lattice_expr_equal referenced_core.target target_ty)
              then
                Error.failf
                  (located_loc morph)
                  "morphism %S returns %s but target field %S expects %s"
                  referenced_core.name
                  (module_name_of_expr referenced_core.target)
                  target_field
                  (module_name_of_expr target_ty);
              Morph_apply { morph_name = referenced_core.name; source_field }
            | Ast.Min _ -> Min
            | Ast.Max _ -> Max
            | Ast.Join { left; right; _ } ->
              Join
                ( resolve_bridge_expr target_field target_ty left,
                  resolve_bridge_expr target_field target_ty right )
            | Ast.Meet { left; right; _ } ->
              Meet
                ( resolve_bridge_expr target_field target_ty left,
                  resolve_bridge_expr target_field target_ty right )
          in
          let resolved_assignments =
            List.map
              (fun (assignment : Ast.bridge_assignment) ->
                let target_field = located_txt assignment.target in
                if Hashtbl.mem seen_targets target_field
                then
                  Error.failf
                    (located_loc assignment.target)
                    "duplicate assignment for target field %S"
                    target_field;
                Hashtbl.add seen_targets target_field ();
                let target_ty =
                  match find_field_type target_fields target_field with
                  | ty -> ty
                  | exception Not_found ->
                    Error.failf
                      (located_loc assignment.target)
                      "unknown target field %S"
                      target_field
                in
                let expr =
                  resolve_bridge_expr target_field target_ty assignment.expr
                in
                { target_field; expr })
              assignments
          in
          List.iter
            (fun (target_field, _) ->
              if not (Hashtbl.mem seen_targets target_field)
              then
                Error.failf
                  (located_loc name)
                  "missing assignment for target field %S"
                  target_field)
            target_fields;
          let source_info = product_info_of_expr source_expr in
          let target_info = product_info_of_expr target_expr in
          let source_field_pos name =
            match String_map.find_opt name source_info.positions with
            | Some pos -> pos
            | None -> raise Not_found
          in
          let rec eval_bridge_expr
              (source_coord : int array)
              (target_finite : finite_lattice)
              (expr : bridge_expr)
            =
            match expr with
            | Source_field source_field_name ->
              source_coord.(source_field_pos source_field_name)
            | Morph_apply { morph_name; source_field } ->
              let referenced_core =
                morph_core_of (resolve_morph visiting morph_name)
              in
              referenced_core.map
                .(source_coord.(source_field_pos source_field))
            | Min -> target_finite.bottom
            | Max -> target_finite.top
            | Join (left, right) ->
              target_finite.join
                .(eval_bridge_expr source_coord target_finite left)
                .(eval_bridge_expr source_coord target_finite right)
            | Meet (left, right) ->
              target_finite.meet
                .(eval_bridge_expr source_coord target_finite left)
                .(eval_bridge_expr source_coord target_finite right)
          in
          let assignment_by_target =
            Array.init (Array.length target_info.fields) (fun i ->
                let target_field_name = target_info.fields.(i).field.name in
                match
                  List.find_opt
                    (fun assignment ->
                      String.equal assignment.target_field target_field_name)
                    resolved_assignments
                with
                | Some assignment -> assignment
                | None -> assert false)
          in
          let rec check_bridge_expr_monotone = function
            | Source_field _ | Min | Max -> ()
            | Morph_apply { morph_name; _ } ->
              ignore (resolve_morph visiting morph_name)
            | Join (left, right) | Meet (left, right) ->
              check_bridge_expr_monotone left;
              check_bridge_expr_monotone right
          in
          let map =
            Array.init source_info.total (fun source_index ->
                let source_coord = product_decode_coord source_info source_index in
                let target_coord = Array.make (Array.length target_info.fields) 0 in
                Array.iteri
                  (fun target_pos (target_spec : product_field_info) ->
                    let assignment = assignment_by_target.(target_pos) in
                    target_coord.(target_pos) <-
                      eval_bridge_expr source_coord target_spec.finite assignment.expr)
                  target_info.fields;
                product_encode_coord target_info target_coord)
          in
          List.iter
            (fun assignment -> check_bridge_expr_monotone assignment.expr)
            resolved_assignments;
          let core_without_adjoint_metadata =
            { name = morph_name;
              source = source_expr;
              target = target_expr;
              map;
              left_adjoint = None;
              right_adjoint = None;
              left_name = None;
              right_name = None
            }
          in
          let core =
            let neutral_bounds kind =
              Array.init (Array.length source_info.fields) (fun i ->
                  match kind with
                  | `Left -> source_info.fields.(i).finite.bottom
                  | `Right -> source_info.fields.(i).finite.top)
            in
            let merge_bounds kind left right =
              Array.mapi
                (fun i left_index ->
                  let source_field_finite = source_info.fields.(i).finite in
                  let right_index = right.(i) in
                  match kind with
                  | `Left -> source_field_finite.join.(left_index).(right_index)
                  | `Right -> source_field_finite.meet.(left_index).(right_index))
                left
            in
            let singleton_bounds kind source_pos contribution =
              let bounds = neutral_bounds kind in
              bounds.(source_pos) <- contribution;
              bounds
            in
            let rec bridge_expr_adjoint kind target_finite target_index expr =
              match expr with
              | Source_field source_field_name ->
                let source_pos = source_field_pos source_field_name in
                Some (singleton_bounds kind source_pos target_index)
              | Morph_apply { morph_name; source_field } ->
                let referenced_core = morph_core_of (resolve_morph visiting morph_name) in
                let adjoint =
                  match kind with
                  | `Left -> referenced_core.left_adjoint
                  | `Right -> referenced_core.right_adjoint
                in
                Option.map
                  (fun adjoint ->
                    let source_pos = source_field_pos source_field in
                    singleton_bounds kind source_pos adjoint.(target_index))
                  adjoint
              | Min ->
                if kind = `Left && target_index <> target_finite.bottom
                then None
                else Some (neutral_bounds kind)
              | Max ->
                if kind = `Right && target_index <> target_finite.top
                then None
                else Some (neutral_bounds kind)
              | Join (left, right) ->
                (match kind with
                 | `Left -> None
                 | `Right ->
                   Option.bind
                     (bridge_expr_adjoint kind target_finite target_index left)
                     (fun left_bounds ->
                       Option.map
                         (merge_bounds kind left_bounds)
                         (bridge_expr_adjoint kind target_finite target_index right)))
              | Meet (left, right) ->
                (match kind with
                 | `Left ->
                   Option.bind
                     (bridge_expr_adjoint kind target_finite target_index left)
                     (fun left_bounds ->
                       Option.map
                         (merge_bounds kind left_bounds)
                         (bridge_expr_adjoint kind target_finite target_index right))
                 | `Right -> None)
            in
            let structural_adjoint kind =
              let compute_target_bound target_index =
                let target_coord = product_decode_coord target_info target_index in
                let initial = neutral_bounds kind in
                let rec loop target_pos bounds =
                  if target_pos = Array.length target_info.fields
                  then Some (product_encode_coord source_info bounds)
                  else
                    let target_spec = target_info.fields.(target_pos) in
                    let assignment = assignment_by_target.(target_pos) in
                    let target_field_index = target_coord.(target_pos) in
                    Option.bind
                      (bridge_expr_adjoint
                         kind
                         target_spec.finite
                         target_field_index
                         assignment.expr)
                      (fun field_bounds -> loop (target_pos + 1) (merge_bounds kind bounds field_bounds))
                in
                loop 0 initial
              in
              let adjoint = Array.init target_info.total compute_target_bound in
              if Array.for_all Option.is_some adjoint
              then Some (Array.map Option.get adjoint)
              else None
            in
            { core_without_adjoint_metadata with
              left_adjoint = structural_adjoint `Left;
              right_adjoint = structural_adjoint `Right
            }
          in
          Bridge { core; assignments = resolved_assignments }
        | Ast.Base _ | Ast.Product _ | Ast.Adjoint_chain _ -> assert false
      in
      resolved_morphs := String_map.add morph_name resolved !resolved_morphs;
      resolved
  in
  List.iter
    (fun morph_name -> ignore (resolve_morph String_set.empty morph_name))
    !morph_order;
  let find_matching_morph source target map =
    String_map.to_seq !resolved_morphs
    |> Seq.find_map (fun (name, morph) ->
         let core = morph_core_of morph in
         if lattice_expr_equal core.source source
            && lattice_expr_equal core.target target
            && int_array_equal core.map map
         then Some name
         else None)
  in
  let resolved_morphs =
    String_map.mapi
      (fun _ morph ->
        let core = morph_core_of morph in
        let left_name =
          Option.bind core.left_adjoint (fun map ->
              find_matching_morph core.target core.source map)
        in
        let right_name =
          Option.bind core.right_adjoint (fun map ->
              find_matching_morph core.target core.source map)
        in
        with_morph_core morph { core with left_name; right_name })
      !resolved_morphs
  in
  List.iter
    (fun (chain : Ast.adjoint_chain) ->
      let rec check_pairs = function
        | [] | [ _ ] -> ()
        | left_name :: ((right_name :: _) as rest) ->
          let left_morph_name = located_txt left_name in
          let right_morph_name = located_txt right_name in
          let left =
            match String_map.find_opt left_morph_name resolved_morphs with
            | Some morph -> morph
            | None ->
              Error.failf
                (located_loc left_name)
                "unknown morph %S in adjoint chain"
                left_morph_name
          in
          let right =
            match String_map.find_opt right_morph_name resolved_morphs with
            | Some morph -> morph
            | None ->
              Error.failf
                (located_loc right_name)
                "unknown morph %S in adjoint chain"
                right_morph_name
          in
          let left_core = morph_core_of left in
          let right_core = morph_core_of right in
          if not (lattice_expr_equal left_core.target right_core.source)
             || not (lattice_expr_equal left_core.source right_core.target)
          then
            Error.failf
              (Location.merge (located_loc left_name) (located_loc right_name))
              "adjoint chain type mismatch between %S : %s -> %s and %S : %s -> %s"
              left_core.name
              (module_name_of_expr left_core.source)
              (module_name_of_expr left_core.target)
              right_core.name
              (module_name_of_expr right_core.source)
              (module_name_of_expr right_core.target);
          (match left_core.right_adjoint with
           | None ->
             Error.failf
               (located_loc left_name)
               "morphism %S has no right adjoint (required to match %S in adjoint chain)"
               left_core.name
               right_core.name
           | Some right_adjoint ->
             if not (int_array_equal right_adjoint right_core.map)
             then
               Error.failf
                 (Location.merge (located_loc left_name) (located_loc right_name))
                 "morphism %S is not left adjoint to %S"
                 left_core.name
                 right_core.name);
          (match right_core.left_adjoint with
           | None ->
             Error.failf
               (located_loc right_name)
               "morphism %S has no left adjoint (required to match %S in adjoint chain)"
               right_core.name
               left_core.name
           | Some left_adjoint ->
             if not (int_array_equal left_adjoint left_core.map)
             then
               Error.failf
                 (Location.merge (located_loc left_name) (located_loc right_name))
                 "morphism %S is not right adjoint to %S"
                 right_core.name
                 left_core.name);
          check_pairs rest
      in
      check_pairs chain.names)
    !adjoint_chains;
  let items =
    List.map (fun name -> Lattice (String_map.find name resolved_lattices)) !lattice_order
    @ List.map (fun name -> Morph (String_map.find name resolved_morphs)) !morph_order
  in
  { items; lattices = resolved_lattices; morphs = resolved_morphs }
