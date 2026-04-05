open Model

let bprintf = Printf.bprintf

type outputs =
  { ml : string;
    mli : string
  }

let dual_mask (desc : descriptor) = desc.mask land lnot desc.nat_mask

let add_schedule_fn_ml ?(indent = "  ") buf name rounds direction =
  if rounds = []
  then false
  else (
    bprintf buf "%slet[@inline] %s x =\n" indent name;
    List.iter
      (fun (round : round) ->
        bprintf
          buf
          "%s  let x = x lor ((x land %d) %s %d) in\n"
          indent
          round.mask
          (match direction with `Down -> "lsr" | `Up -> "lsl")
          round.shift)
      rounds;
    bprintf buf "%s  x\n" indent;
    true)

let add_repr_validator_ml ?(indent = "  ") buf repr_validator =
  let has_close_repr =
    add_schedule_fn_ml ~indent buf "close_repr" repr_validator.down `Down
  in
  bprintf buf "%slet mask = %d\n" indent repr_validator.mask;
  bprintf buf "%slet validate_repr_exn x =\n" indent;
  bprintf
    buf
    "%s  if x land lnot mask <> 0 then invalid_arg \"invalid representation\";\n"
    indent;
  if has_close_repr
  then
    bprintf
      buf
      "%s  if close_repr x <> x then invalid_arg \"invalid representation\";\n"
      indent;
  bprintf buf "%s  x\n" indent

let add_specialized_ops_ml ?(indent = "  ") buf desc =
  let nat_mask = desc.nat_mask in
  let dual_mask = dual_mask desc in
  if nat_mask = desc.mask
  then (
    let has_close_down_nat =
      add_schedule_fn_ml ~indent buf "close_down_nat" desc.down_nat `Down
    in
    let has_close_up_nat =
      add_schedule_fn_ml ~indent buf "close_up_nat" desc.up_nat `Up
    in
    bprintf buf "%slet min = 0\n" indent;
    bprintf buf "%slet max = %d\n" indent desc.mask;
    bprintf buf "%slet[@inline] le x y = (x land y) = x\n" indent;
    bprintf buf "%slet[@inline] equal (x : t) (y : t) = x = y\n" indent;
    bprintf buf "%slet[@inline] join x y = x lor y\n" indent;
    bprintf buf "%slet[@inline] meet x y = x land y\n" indent;
    bprintf buf "%slet[@inline] subtract x y =\n" indent;
    bprintf buf "%s  let zxy = x land lnot y in\n" indent;
    bprintf
      buf
      "%s  %s\n"
      indent
      (if has_close_down_nat then "close_down_nat zxy" else "zxy");
    bprintf buf "%slet[@inline] imply x y =\n" indent;
    bprintf buf "%s  let zxy = x land lnot y in\n" indent;
    bprintf
      buf
      "%s  %s\n"
      indent
      (if has_close_up_nat
       then Printf.sprintf "lnot (close_up_nat zxy) land %d" desc.mask
       else Printf.sprintf "lnot zxy land %d" desc.mask))
  else if nat_mask = 0
  then (
    let has_close_down_dual =
      add_schedule_fn_ml ~indent buf "close_down_dual" desc.down_dual `Down
    in
    let has_close_up_dual =
      add_schedule_fn_ml ~indent buf "close_up_dual" desc.up_dual `Up
    in
    bprintf buf "%slet min = %d\n" indent desc.mask;
    bprintf buf "%slet max = 0\n" indent;
    bprintf buf "%slet[@inline] le x y = (x land y) = y\n" indent;
    bprintf buf "%slet[@inline] equal (x : t) (y : t) = x = y\n" indent;
    bprintf buf "%slet[@inline] join x y = x land y\n" indent;
    bprintf buf "%slet[@inline] meet x y = x lor y\n" indent;
    bprintf buf "%slet[@inline] subtract x y =\n" indent;
    bprintf buf "%s  let zyx = y land lnot x in\n" indent;
    bprintf
      buf
      "%s  %s\n"
      indent
      (if has_close_up_dual
       then Printf.sprintf "lnot (close_up_dual zyx) land %d" desc.mask
       else Printf.sprintf "lnot zyx land %d" desc.mask);
    bprintf buf "%slet[@inline] imply x y =\n" indent;
    bprintf buf "%s  let zyx = y land lnot x in\n" indent;
    bprintf
      buf
      "%s  %s\n"
      indent
      (if has_close_down_dual then "close_down_dual zyx" else "zyx"))
  else (
    let has_close_down_nat =
      add_schedule_fn_ml ~indent buf "close_down_nat" desc.down_nat `Down
    in
    let has_close_up_nat =
      add_schedule_fn_ml ~indent buf "close_up_nat" desc.up_nat `Up
    in
    let has_close_down_dual =
      add_schedule_fn_ml ~indent buf "close_down_dual" desc.down_dual `Down
    in
    let has_close_up_dual =
      add_schedule_fn_ml ~indent buf "close_up_dual" desc.up_dual `Up
    in
    bprintf buf "%slet min = %d\n" indent dual_mask;
    bprintf buf "%slet max = %d\n" indent nat_mask;
    bprintf buf "%slet[@inline] le x y =\n" indent;
    bprintf buf "%s  (((x land lnot y) land %d) = 0)\n" indent nat_mask;
    bprintf buf "%s  && (((y land lnot x) land %d) = 0)\n" indent dual_mask;
    bprintf buf "%slet[@inline] equal (x : t) (y : t) = x = y\n" indent;
    bprintf buf "%slet[@inline] join x y =\n" indent;
    bprintf buf "%s  let o = x lor y in\n" indent;
    bprintf buf "%s  let a = x land y in\n" indent;
    bprintf buf "%s  (o land %d) lor (a land %d)\n" indent nat_mask dual_mask;
    bprintf buf "%slet[@inline] meet x y =\n" indent;
    bprintf buf "%s  let o = x lor y in\n" indent;
    bprintf buf "%s  let a = x land y in\n" indent;
    bprintf buf "%s  (a land %d) lor (o land %d)\n" indent nat_mask dual_mask;
    bprintf buf "%slet[@inline] subtract x y =\n" indent;
    bprintf buf "%s  let zxy = x land lnot y in\n" indent;
    bprintf buf "%s  let zyx = y land lnot x in\n" indent;
    bprintf
      buf
      "%s  ((%s) land %d)\n"
      indent
      (if has_close_down_nat then "close_down_nat zxy" else "zxy")
      nat_mask;
    bprintf
      buf
      "%s  lor ((%s) land %d)\n"
      indent
      (if has_close_up_dual then "lnot (close_up_dual zyx)" else "lnot zyx")
      dual_mask;
    bprintf buf "%slet[@inline] imply x y =\n" indent;
    bprintf buf "%s  let zxy = x land lnot y in\n" indent;
    bprintf buf "%s  let zyx = y land lnot x in\n" indent;
    bprintf
      buf
      "%s  ((%s) land %d)\n"
      indent
      (if has_close_up_nat then "lnot (close_up_nat zxy)" else "lnot zxy")
      nat_mask;
    bprintf
      buf
      "%s  lor ((%s) land %d)\n"
      indent
      (if has_close_down_dual then "close_down_dual zyx" else "zyx")
      dual_mask)

let field_module_name (field : field) ~in_op =
  let opposite = if in_op then not field.ty.opposite else field.ty.opposite in
  if opposite then Name.op_module_name field.ty.name else field.ty.name

let proj_name (field : field) = "proj_" ^ field.name

let with_name (field : field) = "with_" ^ field.name

let min_with_name (field : field) = "min_with_" ^ field.name

let max_with_name (field : field) = "max_with_" ^ field.name

let legacy_value_name name =
  match name with
  | "Locality" | "Regionality" -> "global"
  | "Uniqueness" -> "aliased"
  | "Linearity" -> "many"
  | "Portability" -> "nonportable"
  | "Contention" -> "uncontended"
  | "Forkable" -> "forkable"
  | "Yielding" -> "unyielding"
  | "Statefulness" -> "stateful"
  | "Visibility" -> "read_write"
  | "Staticity" -> "dynamic"
  | _ -> "max"

let view_ctor_name value_name = String.capitalize_ascii value_name

let add_common_sig_items_base buf =
  Buffer.add_string
    buf
    "  type t\n\
    \n\
    \  module Repr : sig\n\
    \    type nonrec t = t\n\
    \    val to_int_unsafe : t -> int\n\
    \    val from_int_unsafe : int -> t\n\
    \  end\n\
    \n\
    \  val min : t\n\
    \  val max : t\n\
    \  val le : t -> t -> bool\n\
    \  val equal : t -> t -> bool\n\
    \  val join : t -> t -> t\n\
    \  val meet : t -> t -> t\n\
    \  val subtract : t -> t -> t\n\
    \  val imply : t -> t -> t\n\
    \  val print : Format.formatter -> t -> unit\n\
    \  val show : t -> string\n\
    \  val name : t -> string\n\
    \  val of_name : string -> t option\n\
    \  val legacy : t\n"

let add_common_sig_items_product buf =
  Buffer.add_string
    buf
    "  type t\n\
    \n\
    \  module Repr : sig\n\
    \    type nonrec t = t\n\
    \    val to_int_unsafe : t -> int\n\
    \    val from_int_unsafe : int -> t\n\
    \  end\n\
    \n\
    \  val min : t\n\
    \  val max : t\n\
    \  val le : t -> t -> bool\n\
    \  val equal : t -> t -> bool\n\
    \  val join : t -> t -> t\n\
    \  val meet : t -> t -> t\n\
    \  val subtract : t -> t -> t\n\
    \  val imply : t -> t -> t\n\
    \  val print : Format.formatter -> t -> unit\n\
    \  val show : t -> string\n\
    \  val name : t -> string\n"

let add_common_ml_items buf desc repr_validator =
  Buffer.add_string buf "  type t = int\n\n";
  Buffer.add_string buf "  module Repr = struct\n";
  Buffer.add_string buf "    type nonrec t = t\n";
  Buffer.add_string buf "    let[@inline] to_int_unsafe x = x\n";
  Buffer.add_string buf "    let[@inline] from_int_unsafe x = x\n";
  Buffer.add_string buf "  end\n\n";
  add_repr_validator_ml buf repr_validator;
  Buffer.add_char buf '\n';
  add_specialized_ops_ml buf desc;
  Buffer.add_char buf '\n'

let add_name_functions_ml buf cases legacy =
  Buffer.add_string buf "  let name x =\n";
  List.iteri
    (fun i (value_name, display_name) ->
      if i = 0
      then bprintf buf "    if equal x %s then %S\n" value_name display_name
      else bprintf buf "    else if equal x %s then %S\n" value_name display_name)
    cases;
  Buffer.add_string buf "    else invalid_arg \"unknown lattice element\"\n\n";
  Buffer.add_string buf "  let of_name = function\n";
  List.iter
    (fun (value_name, display_name) ->
      bprintf buf "    | %S -> Some %s\n" display_name value_name)
    cases;
  Buffer.add_string buf "    | _ -> None\n\n";
  Buffer.add_string buf "  let print ppf x = Format.pp_print_string ppf (name x)\n";
  Buffer.add_string buf "  let show x = name x\n";
  bprintf buf "  let legacy = %s\n" legacy

let add_base_sig buf (base : base) module_name =
  bprintf buf "module %s : sig\n" module_name;
  add_common_sig_items_base buf;
  Buffer.add_string buf "  type view =\n";
  Array.iter
    (fun value_name ->
      bprintf buf "    | %s\n" (view_ctor_name value_name))
    base.element_value_names;
  Buffer.add_char buf '\n';
  Buffer.add_string buf "  val view : t -> view\n";
  Buffer.add_string buf "  val of_view : view -> t\n";
  Array.iteri
    (fun i value_name ->
      bprintf buf "  val %s : t\n" value_name;
      if i = Array.length base.element_value_names - 1 then ())
    base.element_value_names;
  Buffer.add_string buf "end\n\n"

let add_base_ml buf (base : base) module_name desc =
  bprintf buf "module %s = struct\n" module_name;
  add_common_ml_items buf desc base.repr_validator;
  Buffer.add_string buf "  type view =\n";
  Array.iter
    (fun value_name ->
      bprintf buf "    | %s\n" (view_ctor_name value_name))
    base.element_value_names;
  Buffer.add_char buf '\n';
  Array.iteri
    (fun i value_name ->
      bprintf buf "  let %s = %d\n" value_name base.element_values.(i))
    base.element_value_names;
  if Array.length base.element_value_names > 0 then Buffer.add_char buf '\n';
  let cases =
    Array.to_list
      (Array.mapi
         (fun i value_name -> value_name, base.element_names.(i))
         base.element_value_names)
  in
  add_name_functions_ml buf cases (legacy_value_name base.name);
  Buffer.add_string buf "\n  let view x =\n";
  Array.iteri
    (fun i value_name ->
      let ctor = view_ctor_name value_name in
      if i = 0
      then bprintf buf "    if equal x %s then %s\n" value_name ctor
      else bprintf buf "    else if equal x %s then %s\n" value_name ctor)
    base.element_value_names;
  Buffer.add_string buf "    else invalid_arg \"unknown lattice element\"\n\n";
  Buffer.add_string buf "  let of_view = function\n";
  Array.iter
    (fun value_name ->
      bprintf
        buf
        "    | %s -> %s\n"
        (view_ctor_name value_name)
        value_name)
    base.element_value_names;
  Buffer.add_string buf "end\n\n"

let add_product_sig buf (product : product) module_name ~in_op =
  bprintf buf "module %s : sig\n" module_name;
  add_common_sig_items_product buf;
  Buffer.add_string buf "\n  type view = {\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    %s : %s.t;\n"
        field.name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  }\n\n";
  Buffer.add_string buf "  type nonrec product = t\n\n";
  Buffer.add_string buf "  val of_view : view -> t\n";
  Buffer.add_string buf "  val view : t -> view\n";
  Buffer.add_string buf "  val make :\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    %s:%s.t ->\n"
        field.name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "    t\n";
  List.iter
    (fun (field : field) ->
      let field_mod = field_module_name field ~in_op in
      bprintf buf "  val %s : t -> %s.t\n" (proj_name field) field_mod;
      bprintf buf "  val %s : %s.t -> t -> t\n" (with_name field) field_mod;
      bprintf buf "  val %s : %s.t -> t\n" (min_with_name field) field_mod;
      bprintf buf "  val %s : %s.t -> t\n" (max_with_name field) field_mod)
    product.fields;
  Buffer.add_string buf "end\n\n"

let add_product_ml buf (product : product) module_name desc ~in_op =
  bprintf buf "module %s = struct\n" module_name;
  add_common_ml_items buf desc product.repr_validator;
  Buffer.add_string buf "  type view = {\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    %s : %s.t;\n"
        field.name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  }\n\n";
  Buffer.add_string buf "  type nonrec product = t\n\n";
  Buffer.add_string buf "  let of_view view =\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    let %s = (view.%s land %d) lsl %d in\n"
        field.name
        field.name
        field.raw_mask
        field.shift)
    product.fields;
  Buffer.add_string buf "    ";
  List.iteri
    (fun i (field : field) ->
      if i > 0 then Buffer.add_string buf " lor ";
      Buffer.add_string buf field.name)
    product.fields;
  Buffer.add_string buf "\n\n";
  Buffer.add_string buf "  let view t =\n";
  Buffer.add_string buf "    {\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      %s = (t lsr %d) land %d;\n"
        field.name
        field.shift
        field.raw_mask)
    product.fields;
  Buffer.add_string buf "    }\n\n";
  Buffer.add_string buf "  let make =\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "    fun ~%s ->\n" field.name)
    product.fields;
  Buffer.add_string buf "    of_view {\n";
  List.iter
    (fun (field : field) -> bprintf buf "      %s;\n" field.name)
    product.fields;
  Buffer.add_string buf "    }\n\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "  let %s t = (t lsr %d) land %d\n"
        (proj_name field)
        field.shift
        field.raw_mask;
      bprintf
        buf
        "  let %s x t = (t land lnot %d) lor ((x land %d) lsl %d)\n"
        (with_name field)
        field.layout_mask
        field.raw_mask
        field.shift;
      bprintf
        buf
        "  let %s x = %s x min\n"
        (min_with_name field)
        (with_name field);
      bprintf
        buf
        "  let %s x = %s x max\n"
        (max_with_name field)
        (with_name field);
      Buffer.add_char buf '\n')
    product.fields;
  Buffer.add_string buf "  let name x =\n";
  Buffer.add_string buf "    let view = view x in\n";
  Buffer.add_string buf "    let b = Buffer.create 64 in\n";
  bprintf buf "    Buffer.add_string b %S;\n" (module_name ^ " { ");
  List.iteri
    (fun i (field : field) ->
      if i > 0 then Buffer.add_string buf "    Buffer.add_string b \"; \";\n";
      bprintf buf "    Buffer.add_string b %S;\n" (field.name ^ " = ");
      bprintf
        buf
        "    Buffer.add_string b (%s.show view.%s);\n"
        (field_module_name field ~in_op)
        field.name)
    product.fields;
  Buffer.add_string buf "    Buffer.add_string b \" }\";\n";
  Buffer.add_string buf "    Buffer.contents b\n\n";
  Buffer.add_string buf "  let print ppf x = Format.pp_print_string ppf (name x)\n";
  Buffer.add_string buf "  let show x = name x\n";
  Buffer.add_string buf "end\n\n"

let add_primitive_morph_sig buf (primitive : primitive_morph) =
  let core = primitive.core in
  bprintf
    buf
    "val %s : %s.t -> %s.t\n"
    core.name
    (module_name_of_expr core.source)
    (module_name_of_expr core.target)

let lattice_expr_module_sig_and_ml buf_mli buf_ml lattice expr =
  match lattice, expr.opposite with
  | Base base, false ->
    add_base_sig buf_mli base base.name;
    add_base_ml buf_ml base base.name base.descriptor
  | Base base, true ->
    add_base_sig buf_mli base (Name.op_module_name base.name);
    add_base_ml buf_ml base (Name.op_module_name base.name) base.op_descriptor
  | Product product, false ->
    add_product_sig buf_mli product product.name ~in_op:false;
    add_product_ml buf_ml product product.name product.descriptor ~in_op:false
  | Product product, true ->
    add_product_sig buf_mli product (Name.op_module_name product.name) ~in_op:true;
    add_product_ml
      buf_ml
      product
      (Name.op_module_name product.name)
      product.op_descriptor
      ~in_op:true

let source_element_name lattice_name index model =
  match String_map.find lattice_name model.lattices with
  | Base base -> base.element_value_names.(index)
  | Product _ -> invalid_arg "primitive morph endpoints must be base lattices"

type bit_term =
  { negated : bool;
    mask : int;
    shift : int
  }

type bit_kernel =
  { const : int;
    terms : bit_term list
  }

let popcount x =
  let rec loop n acc =
    if n = 0 then acc else loop (n land (n - 1)) (acc + 1)
  in
  loop x 0

let bit_positions mask =
  let rec loop bit acc =
    if bit >= Sys.int_size - 1
    then List.rev acc
    else
      let acc = if mask land (1 lsl bit) <> 0 then bit :: acc else acc in
      loop (bit + 1) acc
  in
  loop 0 []

let descriptor_of_expr model (expr : lattice_expr) =
  match String_map.find expr.name model.lattices with
  | Base base -> if expr.opposite then base.op_descriptor else base.descriptor
  | Product product ->
    if expr.opposite then product.op_descriptor else product.descriptor

let mask_of_expr model expr = (descriptor_of_expr model expr).mask

let bottom_value_of_expr model expr =
  let desc = descriptor_of_expr model expr in
  dual_mask desc

let top_value_of_expr model expr = (descriptor_of_expr model expr).nat_mask

let normalize_kernel target_mask (kernel : bit_kernel) =
  let groups = Hashtbl.create 8 in
  List.iter
    (fun (term : bit_term) ->
      if term.mask <> 0
      then (
        let key = (term.negated, term.shift) in
        let prev = Option.value (Hashtbl.find_opt groups key) ~default:0 in
        Hashtbl.replace groups key (prev lor term.mask)))
    kernel.terms;
  let terms =
    Hashtbl.to_seq groups
    |> List.of_seq
    |> List.map (fun ((negated, shift), mask) -> { negated; shift; mask })
    |> List.sort (fun left right ->
         match Bool.compare left.negated right.negated with
         | 0 ->
           let by_shift = Int.compare left.shift right.shift in
           if by_shift <> 0 then by_shift else Int.compare left.mask right.mask
         | n -> n)
  in
  { const = kernel.const land target_mask; terms }

let find_field (product : product) field_name =
  match
    List.find_opt
      (fun (field : field) -> String.equal field.name field_name)
      product.fields
  with
  | Some field -> field
  | None -> invalid_arg ("unknown field " ^ field_name)

let effective_field_type root_expr (field : field) =
  if root_expr.opposite then flip_expr field.ty else field.ty

let synthesize_or_literal_cover ~inputs ~desired =
  let n = Array.length desired in
  if Array.for_all Fun.id desired
  then Some (`Const_true, [])
  else if Array.for_all not desired
  then Some (`Const_false, [])
  else (
    let valid_inputs =
      List.filter
        (fun (_, truths) ->
          let ok = ref true in
          for i = 0 to n - 1 do
            if truths.(i) && not desired.(i) then ok := false
          done;
          !ok)
        inputs
    in
    let valid_inputs = Array.of_list valid_inputs in
    let m = Array.length valid_inputs in
    let best = ref None in
    for subset = 1 to (1 lsl m) - 1 do
      let subset_size = popcount subset in
      if
        match !best with
        | None -> true
        | Some (_, best_size) -> subset_size < best_size
      then (
        let covered = Array.make n false in
        for literal = 0 to m - 1 do
          if subset land (1 lsl literal) <> 0
          then (
            let _, truths = valid_inputs.(literal) in
            for i = 0 to n - 1 do
              covered.(i) <- covered.(i) || truths.(i)
            done)
        done;
        if Array.for_all2 Bool.equal covered desired
        then
          let chosen =
            Array.to_list valid_inputs
            |> List.mapi (fun i literal -> (i, literal))
            |> List.filter_map (fun (i, literal) ->
                 if subset land (1 lsl i) <> 0 then Some (fst literal) else None)
          in
          best := Some (chosen, subset_size))
    done;
    Option.map (fun (chosen, _) -> (`Const_false, chosen)) !best)

let synthesize_primitive_kernel model (primitive : primitive_morph) =
  let core = primitive.core in
  let source_base =
    match String_map.find core.source.name model.lattices with
    | Base base -> base
    | Product _ -> invalid_arg "primitive morph endpoints must be base lattices"
  in
  let target_base =
    match String_map.find core.target.name model.lattices with
    | Base base -> base
    | Product _ -> invalid_arg "primitive morph endpoints must be base lattices"
  in
  let source_mask = mask_of_expr model core.source in
  let target_mask = mask_of_expr model core.target in
  let source_bits = bit_positions source_mask in
  let target_bits = bit_positions target_mask in
  let source_values = source_base.element_values in
  let target_values = Array.map (fun index -> target_base.element_values.(index)) core.map in
  let n = Array.length target_values in
  let literals =
    List.concat_map
      (fun bit ->
        let truths = Array.init n (fun i -> source_values.(i) land (1 lsl bit) <> 0) in
        let neg_truths = Array.init n (fun i -> not truths.(i)) in
        [ ((false, bit), truths); ((true, bit), neg_truths) ])
      source_bits
  in
  let terms = ref [] in
  let const = ref 0 in
  let failed = ref false in
  List.iter
    (fun target_bit ->
      if not !failed
      then (
        let desired =
          Array.init n (fun i -> target_values.(i) land (1 lsl target_bit) <> 0)
        in
        match synthesize_or_literal_cover ~inputs:literals ~desired with
        | None -> failed := true
        | Some (`Const_true, _) -> const := !const lor (1 lsl target_bit)
        | Some (`Const_false, chosen) ->
          List.iter
            (fun (negated, source_bit) ->
              terms :=
                { negated; mask = 1 lsl source_bit; shift = target_bit - source_bit }
                :: !terms)
            chosen))
    target_bits;
  if !failed
  then None
  else Some (normalize_kernel target_mask { const = !const; terms = List.rev !terms })

let render_term ~source_mask (term : bit_term) =
  let dropped_low_bits shift =
    if shift <= 0 then 0 else (1 lsl shift) - 1
  in
  let mask_is_redundant_for_right_shift =
    if term.negated || term.shift >= 0
    then false
    else (
      let shift = -term.shift in
      let required_mask = source_mask land lnot (dropped_low_bits shift) in
      term.mask = required_mask)
  in
  let atom =
    match term.negated, term.mask = source_mask with
    | false, true -> "x"
    | true, true when term.shift = 0 ->
      Printf.sprintf "(x lxor %d)" source_mask
    | false, false when mask_is_redundant_for_right_shift -> "x"
    | false, false -> Printf.sprintf "(x land %d)" term.mask
    | true, _ -> Printf.sprintf "((lnot x) land %d)" term.mask
  in
  match term.shift with
  | 0 -> atom
  | shift when shift > 0 -> Printf.sprintf "(%s lsl %d)" atom shift
  | shift -> Printf.sprintf "(%s lsr %d)" atom (-shift)

let render_kernel_expr ~source_mask (kernel : bit_kernel) =
  let pieces =
    (if kernel.const <> 0 then [ string_of_int kernel.const ] else [])
    @ List.map (render_term ~source_mask) kernel.terms
  in
  match pieces with
  | [] -> "0"
  | [ piece ] -> piece
  | piece :: rest ->
    piece
    ^ List.fold_left
        (fun acc next -> acc ^ "\n      lor " ^ next)
        ""
        rest

let add_kernel_ml buf model name source_expr kernel =
  let source_mask = mask_of_expr model source_expr in
  bprintf buf "  %s x =\n" name;
  bprintf buf "    %s\n" (render_kernel_expr ~source_mask kernel)

let lift_kernel_through_field
    ~(target_field : field)
    ~(source_field : field)
    ~(referenced : bit_kernel)
  =
  let target_mask = target_field.raw_mask lsl target_field.shift in
  normalize_kernel
    target_mask
    { const = (referenced.const land target_field.raw_mask) lsl target_field.shift;
      terms =
        List.map
          (fun (term : bit_term) ->
            { negated = term.negated;
              mask = term.mask lsl source_field.shift;
              shift = term.shift + target_field.shift - source_field.shift
            })
          referenced.terms
    }

let product_field_module_name model (expr : lattice_expr) field_name =
  match String_map.find expr.name model.lattices with
  | Base _ -> invalid_arg "expected product lattice"
  | Product product ->
    let field =
      match List.find_opt (fun (field : field) -> String.equal field.name field_name) product.fields with
      | Some field -> field
      | None -> invalid_arg "unknown bridge target field"
    in
    let effective_ty =
      if expr.opposite then flip_expr field.ty else field.ty
    in
    module_name_of_expr effective_ty

let kernel_of_bridge_assignment
    model
    resolve_kernel
    (source_expr : lattice_expr)
    (target_expr : lattice_expr)
    (product : product)
    (assignment : bridge_assignment)
  =
  let target_field = find_field product assignment.target_field in
  let target_ty = effective_field_type target_expr target_field in
  match assignment.expr with
  | Source_field source_field_name ->
    let source_product =
      match String_map.find source_expr.name model.lattices with
      | Product product -> product
      | Base _ -> invalid_arg "expected product bridge source"
    in
    let source_field = find_field source_product source_field_name in
    Some
      (normalize_kernel
         (target_field.raw_mask lsl target_field.shift)
         { const = 0;
           terms =
             [ { negated = false;
                 mask = source_field.raw_mask lsl source_field.shift;
                 shift = target_field.shift - source_field.shift
               } ]
         })
  | Morph_apply { morph_name; source_field } ->
    let source_product =
      match String_map.find source_expr.name model.lattices with
      | Product product -> product
      | Base _ -> invalid_arg "expected product bridge source"
    in
    let source_field = find_field source_product source_field in
    (match resolve_kernel morph_name with
     | None -> None
     | Some referenced ->
       Some
         (lift_kernel_through_field
            ~target_field
            ~source_field
            ~referenced))
  | Min ->
    Some
      { const = (bottom_value_of_expr model target_ty land target_field.raw_mask) lsl target_field.shift;
        terms = []
      }
  | Max ->
    Some
      { const = (top_value_of_expr model target_ty land target_field.raw_mask) lsl target_field.shift;
        terms = []
      }

let build_morph_kernels model =
  let kernels = Hashtbl.create (String_map.cardinal model.morphs) in
  let visiting = Hashtbl.create 8 in
  let rec resolve morph_name =
    match Hashtbl.find_opt kernels morph_name with
    | Some kernel -> Some kernel
    | None ->
      if Hashtbl.mem visiting morph_name then None
      else (
        Hashtbl.add visiting morph_name ();
        let result =
          match String_map.find morph_name model.morphs with
          | Primitive primitive -> synthesize_primitive_kernel model primitive
          | Bridge bridge ->
            let core : morph_core = bridge.core in
            let source_expr = core.source in
            let target_expr = core.target in
            let target_product =
              match String_map.find target_expr.name model.lattices with
              | Product product -> product
              | Base _ -> invalid_arg "expected bridge target product"
            in
            let partials =
              List.map
                (fun assignment ->
                  kernel_of_bridge_assignment
                    model
                    resolve
                    source_expr
                    target_expr
                    target_product
                    assignment)
                bridge.assignments
            in
            if List.exists Option.is_none partials
            then None
            else
              let partials = List.map Option.get partials in
              Some
                (normalize_kernel
                   (mask_of_expr model target_expr)
                   (List.fold_left
                      (fun acc partial ->
                        { const = acc.const lor partial.const;
                          terms = acc.terms @ partial.terms
                        })
                      { const = 0; terms = [] }
                      partials))
        in
        Hashtbl.remove visiting morph_name;
        Option.iter (fun kernel -> Hashtbl.add kernels morph_name kernel) result;
        result)
  in
  String_map.iter (fun name _ -> ignore (resolve name)) model.morphs;
  kernels

let add_primitive_morph_ml buf model kernels (primitive : primitive_morph) =
  let core = primitive.core in
  match Hashtbl.find_opt kernels core.name with
  | Some kernel -> add_kernel_ml buf model core.name core.source kernel
  | None ->
    let src_module = module_name_of_expr core.source in
    let dst_module = module_name_of_expr core.target in
    bprintf buf "  %s x =\n" core.name;
    Array.iteri
      (fun i target ->
        let src_name = source_element_name core.source.name i model in
        let dst_name = source_element_name core.target.name target model in
        if i = 0
        then
          bprintf
            buf
            "    if %s.equal x %s.%s then %s.%s\n"
            src_module
            src_module
            src_name
            dst_module
            dst_name
        else
          bprintf
            buf
            "    else if %s.equal x %s.%s then %s.%s\n"
            src_module
            src_module
            src_name
            dst_module
            dst_name)
      core.map;
    bprintf buf "    else invalid_arg %S\n" core.name

let add_bridge_ml buf model kernels (bridge : product_bridge) =
  let core = bridge.core in
  match Hashtbl.find_opt kernels core.name with
  | Some kernel -> add_kernel_ml buf model core.name core.source kernel
  | None ->
    let src_module = module_name_of_expr core.source in
    let dst_module = module_name_of_expr core.target in
    bprintf buf "  %s x =\n" core.name;
    bprintf buf "    %s.make\n" dst_module;
    List.iter
      (fun (assignment : bridge_assignment) ->
        bprintf buf "      ~%s:(" assignment.target_field;
        (match assignment.expr with
         | Source_field source_field ->
           bprintf buf "%s.proj_%s x" src_module source_field
         | Morph_apply { morph_name; source_field } ->
           bprintf buf "%s (%s.proj_%s x)" morph_name src_module source_field
         | Min ->
           bprintf
             buf
             "%s.min"
             (product_field_module_name model core.target assignment.target_field)
         | Max ->
           bprintf
             buf
             "%s.max"
             (product_field_module_name model core.target assignment.target_field));
        Buffer.add_string buf ")\n")
      bridge.assignments

let add_morphs_sig buf model =
  String_map.iter
    (fun _ morph ->
      match morph with
      | Primitive primitive -> add_primitive_morph_sig buf primitive
      | Bridge bridge ->
        let core = bridge.core in
        bprintf
          buf
          "val %s : %s.t -> %s.t\n"
          core.name
          (module_name_of_expr core.source)
          (module_name_of_expr core.target))
    model.morphs

let add_morphs_ml buf model =
  let morphs = String_map.bindings model.morphs |> List.map snd in
  let kernels = build_morph_kernels model in
  match morphs with
  | [] -> ()
  | first :: rest ->
    Buffer.add_string buf "let rec\n";
    (match first with
     | Primitive primitive -> add_primitive_morph_ml buf model kernels primitive
     | Bridge bridge -> add_bridge_ml buf model kernels bridge);
    List.iter
      (fun morph ->
        Buffer.add_string buf "\nand\n";
        match morph with
        | Primitive primitive -> add_primitive_morph_ml buf model kernels primitive
        | Bridge bridge -> add_bridge_ml buf model kernels bridge)
      rest;
    Buffer.add_string buf "\n"

let render ~root_module:_ model =
  let ml = Buffer.create 16384 in
  let mli = Buffer.create 8192 in
  List.iter
    (fun (expr : lattice_expr) ->
      let lattice = String_map.find expr.name model.lattices in
      lattice_expr_module_sig_and_ml mli ml lattice expr)
    (emitted_module_exprs model);
  add_morphs_sig mli model;
  add_morphs_ml ml model;
  { ml = Buffer.contents ml; mli = Buffer.contents mli }
