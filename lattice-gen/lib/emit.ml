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

let add_repr_validator_ml ?(indent = "    ") buf repr_validator =
  let has_close_repr =
    add_schedule_fn_ml ~indent buf "close_repr" repr_validator.down `Down
  in
  bprintf buf "%slet mask = %d\n" indent repr_validator.mask;
  bprintf buf "%slet of_int_exn x =\n" indent;
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
    bprintf buf "%slet bottom = 0\n" indent;
    bprintf buf "%slet top = %d\n" indent desc.mask;
    bprintf buf "%slet[@inline] leq x y = (x land y) = x\n" indent;
    bprintf buf "%slet[@inline] equal (x : t) (y : t) = x = y\n" indent;
    bprintf buf "%slet[@inline] join x y = x lor y\n" indent;
    bprintf buf "%slet[@inline] meet x y = x land y\n" indent;
    bprintf buf "%slet[@inline] sub x y =\n" indent;
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
    bprintf buf "%slet bottom = %d\n" indent desc.mask;
    bprintf buf "%slet top = 0\n" indent;
    bprintf buf "%slet[@inline] leq x y = (x land y) = y\n" indent;
    bprintf buf "%slet[@inline] equal (x : t) (y : t) = x = y\n" indent;
    bprintf buf "%slet[@inline] join x y = x land y\n" indent;
    bprintf buf "%slet[@inline] meet x y = x lor y\n" indent;
    bprintf buf "%slet[@inline] sub x y =\n" indent;
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
    bprintf buf "%slet bottom = %d\n" indent dual_mask;
    bprintf buf "%slet top = %d\n" indent nat_mask;
    bprintf buf "%slet[@inline] leq x y =\n" indent;
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
    bprintf buf "%slet[@inline] sub x y =\n" indent;
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

let axis_ctor_name field_name = String.capitalize_ascii field_name

let proj_name (field : field) = "proj_" ^ field.name

let with_name (field : field) = "with_" ^ field.name

let bot_name (field : field) = field.name ^ "_bot"

let top_name (field : field) = field.name ^ "_top"

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
  | _ -> "top"

let add_common_sig_items buf =
  Buffer.add_string
    buf
    "  type t = int\n\
    \n\
    \  module Repr : sig\n\
    \    type nonrec t = t\n\
    \    val mask : int\n\
    \    val of_int_exn : int -> t\n\
    \  end\n\
    \n\
    \  val bottom : t\n\
    \  val top : t\n\
    \  val leq : t -> t -> bool\n\
    \  val equal : t -> t -> bool\n\
    \  val join : t -> t -> t\n\
    \  val meet : t -> t -> t\n\
    \  val sub : t -> t -> t\n\
    \  val imply : t -> t -> t\n\
    \  val pp : Format.formatter -> t -> unit\n\
    \  val show : t -> string\n\
    \  val name : t -> string\n\
    \  val of_name : string -> t option\n\
    \  val legacy : t\n"

let add_common_ml_items buf desc repr_validator =
  Buffer.add_string buf "  type t = int\n\n";
  Buffer.add_string buf "  module Repr = struct\n";
  Buffer.add_string buf "    type nonrec t = t\n";
  add_repr_validator_ml buf repr_validator;
  Buffer.add_string buf "  end\n\n";
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
  Buffer.add_string buf "  let pp ppf x = Format.pp_print_string ppf (name x)\n";
  Buffer.add_string buf "  let show x = name x\n";
  bprintf buf "  let legacy = %s\n" legacy

let add_base_sig buf (base : base) module_name =
  bprintf buf "module %s : sig\n" module_name;
  add_common_sig_items buf;
  Array.iteri
    (fun i value_name ->
      bprintf buf "  val %s : t\n" value_name;
      if i = Array.length base.element_value_names - 1 then ())
    base.element_value_names;
  Buffer.add_string buf "end\n\n"

let add_base_ml buf (base : base) module_name desc =
  bprintf buf "module %s = struct\n" module_name;
  add_common_ml_items buf desc base.repr_validator;
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
  Buffer.add_string buf "end\n\n"

let add_product_sig buf (product : product) module_name ~in_op =
  bprintf buf "module %s : sig\n" module_name;
  add_common_sig_items buf;
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
  Buffer.add_string buf "  module Axis : sig\n";
  Buffer.add_string buf "    type _ t = ..\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    type _ t += %s : %s.t t\n"
        (axis_ctor_name field.name)
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string
    buf
    "    type packed = Pack : 'a t -> packed\n\
    \    val packed : 'a t -> packed\n\
    \    val all : packed list\n\
    \    val name : 'a t -> string\n\
    \    val pp : Format.formatter -> 'a t -> unit\n\
    \    val proj : 'a t -> product -> 'a\n\
    \    val set : 'a t -> 'a -> product -> product\n\
    \    val min_with : 'a t -> 'a -> product\n\
    \    val max_with : 'a t -> 'a -> product\n\
    \  end\n\n";
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
      bprintf buf "  val %s : %s.t -> t\n" (max_with_name field) field_mod;
      bprintf buf "  val %s : t\n" (bot_name field);
      bprintf buf "  val %s : t\n" (top_name field))
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
        "  let %s x = %s x bottom\n"
        (min_with_name field)
        (with_name field);
      bprintf
        buf
        "  let %s x = %s x top\n"
        (max_with_name field)
        (with_name field);
      bprintf buf "  let %s = %s bottom\n" (bot_name field) (min_with_name field);
      bprintf buf "  let %s = %s top\n\n" (top_name field) (max_with_name field))
    product.fields;
  Buffer.add_string buf "  module Axis = struct\n";
  Buffer.add_string buf "    type _ t = ..\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    type _ t += %s : %s.t t\n"
        (axis_ctor_name field.name)
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string
    buf
    "    type packed = Pack : 'a t -> packed\n\
    \n\
    \    let packed axis = Pack axis\n\
    \n\
    \    let all =\n\
    \      [\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "        Pack %s;\n" (axis_ctor_name field.name))
    product.fields;
  Buffer.add_string
    buf
    "      ]\n\
    \n\
    \    let name : type a. a t -> string = function\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      | %s -> %S\n"
        (axis_ctor_name field.name)
        field.name)
    product.fields;
  Buffer.add_string buf "      | _ -> invalid_arg \"unknown axis\"\n";
  Buffer.add_string
    buf
    "\n\
    \    let pp ppf axis = Format.pp_print_string ppf (name axis)\n\
    \n\
    \    let proj : type a. a t -> product -> a = function\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      | %s -> %s\n"
        (axis_ctor_name field.name)
        (proj_name field))
    product.fields;
  Buffer.add_string buf "      | _ -> invalid_arg \"unknown axis\"\n";
  Buffer.add_string
    buf
    "\n\
    \    let set : type a. a t -> a -> product -> product = function\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      | %s -> %s\n"
        (axis_ctor_name field.name)
        (with_name field))
    product.fields;
  Buffer.add_string buf "      | _ -> invalid_arg \"unknown axis\"\n";
  Buffer.add_string
    buf
    "\n\
    \    let min_with : type a. a t -> a -> product = function\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      | %s -> %s\n"
        (axis_ctor_name field.name)
        (min_with_name field))
    product.fields;
  Buffer.add_string buf "      | _ -> invalid_arg \"unknown axis\"\n";
  Buffer.add_string
    buf
    "\n\
    \    let max_with : type a. a t -> a -> product = function\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      | %s -> %s\n"
        (axis_ctor_name field.name)
        (max_with_name field))
    product.fields;
  Buffer.add_string buf "      | _ -> invalid_arg \"unknown axis\"\n";
  Buffer.add_string buf "  end\n\n";
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
  Buffer.add_string buf "  let pp ppf x = Format.pp_print_string ppf (name x)\n";
  Buffer.add_string buf "  let show x = name x\n";
  Buffer.add_string buf "  let of_name _ = None\n";
  Buffer.add_string buf "  let legacy =\n";
  Buffer.add_string buf "    make\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      ~%s:%s.legacy\n"
        field.name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "end\n\n"

let add_primitive_morph_sig buf (primitive : primitive_morph) =
  let core = primitive.core in
  bprintf
    buf
    "val %s : %s.t -> %s.t\n"
    core.name
    (module_name_of_expr core.source)
    (module_name_of_expr core.target)

let lattice_module_sig_and_ml buf_mli buf_ml lattice =
  match lattice with
  | Base base ->
    add_base_sig buf_mli base base.name;
    add_base_sig buf_mli base (Name.op_module_name base.name);
    add_base_ml buf_ml base base.name base.descriptor;
    add_base_ml buf_ml base (Name.op_module_name base.name) base.op_descriptor
  | Product product ->
    add_product_sig buf_mli product product.name ~in_op:false;
    add_product_sig buf_mli product (Name.op_module_name product.name) ~in_op:true;
    add_product_ml buf_ml product product.name product.descriptor ~in_op:false;
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

let add_primitive_morph_ml buf model (primitive : primitive_morph) =
  let core = primitive.core in
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

let add_bridge_ml buf model (bridge : product_bridge) =
  let core = bridge.core in
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
           "%s.bottom"
           (product_field_module_name model core.target assignment.target_field)
       | Max ->
         bprintf
           buf
           "%s.top"
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
  match morphs with
  | [] -> ()
  | first :: rest ->
    Buffer.add_string buf "let rec\n";
    (match first with
     | Primitive primitive -> add_primitive_morph_ml buf model primitive
     | Bridge bridge -> add_bridge_ml buf model bridge);
    List.iter
      (fun morph ->
        Buffer.add_string buf "\nand\n";
        match morph with
        | Primitive primitive -> add_primitive_morph_ml buf model primitive
        | Bridge bridge -> add_bridge_ml buf model bridge)
      rest;
    Buffer.add_string buf "\n"

let render ~root_module:_ model =
  let ml = Buffer.create 16384 in
  let mli = Buffer.create 8192 in
  List.iter
    (function
      | Lattice lattice -> lattice_module_sig_and_ml mli ml lattice
      | Morph _ -> ())
    model.items;
  add_morphs_sig mli model;
  add_morphs_ml ml model;
  { ml = Buffer.contents ml; mli = Buffer.contents mli }
