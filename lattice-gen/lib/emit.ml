open Model

let bprintf = Printf.bprintf

type outputs =
  { ml : string;
    mli : string;
    test_ml : string
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

let add_repr_validator_ml buf repr_validator =
  let has_close_repr =
    add_schedule_fn_ml ~indent:"    " buf "close_repr" repr_validator.down `Down
  in
  Buffer.add_string buf "    let of_int_exn x =\n";
  Buffer.add_string
    buf
    "      if x land lnot mask <> 0 then invalid_arg \"invalid representation\";\n";
  if has_close_repr
  then
    Buffer.add_string
      buf
      "      if close_repr x <> x then invalid_arg \"invalid representation\";\n";
  Buffer.add_string buf "      x\n"

let add_specialized_ops_ml buf desc =
  let nat_mask = desc.nat_mask in
  let dual_mask = dual_mask desc in
  if nat_mask = desc.mask
  then (
    let has_close_down_nat =
      add_schedule_fn_ml buf "close_down_nat" desc.down_nat `Down
    in
    let has_close_up_nat =
      add_schedule_fn_ml buf "close_up_nat" desc.up_nat `Up
    in
    bprintf
      buf
      "  let bottom = 0\n\
      \  let top = %d\n\
      \  let[@inline] leq x y = (x land y) = x\n\
      \  let[@inline] equal (x : t) (y : t) = x = y\n\
      \  let[@inline] join x y = x lor y\n\
      \  let[@inline] meet x y = x land y\n\
      \  let[@inline] sub x y =\n\
      \    let zxy = x land lnot y in\n\
      \    %s\n\
      \  let[@inline] imply x y =\n\
      \    let zxy = x land lnot y in\n\
      \    %s\n"
      desc.mask
      (if has_close_down_nat then "close_down_nat zxy" else "zxy")
      (if has_close_up_nat
       then Printf.sprintf "lnot (close_up_nat zxy) land %d" desc.mask
       else Printf.sprintf "lnot zxy land %d" desc.mask))
  else if nat_mask = 0
  then (
    let has_close_down_dual =
      add_schedule_fn_ml buf "close_down_dual" desc.down_dual `Down
    in
    let has_close_up_dual =
      add_schedule_fn_ml buf "close_up_dual" desc.up_dual `Up
    in
    bprintf
      buf
      "  let bottom = %d\n\
      \  let top = 0\n\
      \  let[@inline] leq x y = (x land y) = y\n\
      \  let[@inline] equal (x : t) (y : t) = x = y\n\
      \  let[@inline] join x y = x land y\n\
      \  let[@inline] meet x y = x lor y\n\
      \  let[@inline] sub x y =\n\
      \    let zyx = y land lnot x in\n\
      \    %s\n\
      \  let[@inline] imply x y =\n\
      \    let zyx = y land lnot x in\n\
      \    %s\n"
      desc.mask
      (if has_close_up_dual
       then Printf.sprintf "lnot (close_up_dual zyx) land %d" desc.mask
       else Printf.sprintf "lnot zyx land %d" desc.mask)
      (if has_close_down_dual then "close_down_dual zyx" else "zyx"))
  else (
    let has_close_down_nat =
      add_schedule_fn_ml buf "close_down_nat" desc.down_nat `Down
    in
    let has_close_up_nat =
      add_schedule_fn_ml buf "close_up_nat" desc.up_nat `Up
    in
    let has_close_down_dual =
      add_schedule_fn_ml buf "close_down_dual" desc.down_dual `Down
    in
    let has_close_up_dual =
      add_schedule_fn_ml buf "close_up_dual" desc.up_dual `Up
    in
    bprintf
      buf
      "  let bottom = %d\n\
      \  let top = %d\n\
      \  let[@inline] leq x y =\n\
      \    (((x land lnot y) land %d) = 0)\n\
      \    && (((y land lnot x) land %d) = 0)\n\
      \  let[@inline] equal (x : t) (y : t) = x = y\n\
      \  let[@inline] join x y =\n\
      \    let o = x lor y in\n\
      \    let a = x land y in\n\
      \    (o land %d) lor (a land %d)\n\
      \  let[@inline] meet x y =\n\
      \    let o = x lor y in\n\
      \    let a = x land y in\n\
      \    (a land %d) lor (o land %d)\n\
      \  let[@inline] sub x y =\n\
      \    let zxy = x land lnot y in\n\
      \    let zyx = y land lnot x in\n\
      \    ((%s) land %d)\n\
      \    lor ((%s) land %d)\n\
      \  let[@inline] imply x y =\n\
      \    let zxy = x land lnot y in\n\
      \    let zyx = y land lnot x in\n\
      \    ((%s) land %d)\n\
      \    lor ((%s) land %d)\n"
      dual_mask
      nat_mask
      nat_mask
      dual_mask
      nat_mask
      dual_mask
      nat_mask
      dual_mask
      (if has_close_down_nat then "close_down_nat zxy" else "zxy")
      nat_mask
      (if has_close_up_dual then "lnot (close_up_dual zyx)" else "lnot zyx")
      dual_mask
      (if has_close_up_nat then "lnot (close_up_nat zxy)" else "lnot zxy")
      nat_mask
      (if has_close_down_dual then "close_down_dual zyx" else "zyx")
      dual_mask)

let field_module_name (field : field) ~in_op =
  let opposite = if in_op then not field.declared_opposite else field.declared_opposite in
  if opposite then Name.op_module_name field.lattice_name else field.lattice_name

let axis_ctor_name field_name = String.capitalize_ascii field_name

let field_module_alias_name (field : field) = "Field_" ^ axis_ctor_name field.name

let axis_module_name (axis : axis_object) ~in_op =
  let opposite = if in_op then not axis.declared_opposite else axis.declared_opposite in
  if opposite then Name.op_module_name axis.carrier_name else axis.carrier_name

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

let add_repr_sig buf =
  Buffer.add_string
    buf
    "  module Repr : sig\n\
    \    val bits : int\n\
    \    val mask : int\n\
    \    val to_int : t -> int\n\
    \    val of_int_exn : int -> t\n\
    \  end\n"

let add_base_sig buf (base : base) module_name =
  let descriptor =
    if module_name = base.name then base.descriptor else base.op_descriptor
  in
  ignore descriptor;
  bprintf buf "module %s : sig\n" module_name;
  if module_name = base.name
  then Buffer.add_string buf "  type t = private int\n\n"
  else bprintf buf "  type t = %s.t\n\n" base.name;
  Array.iteri
    (fun i (_ : string) ->
      bprintf buf "  val %s : t\n" base.element_value_names.(i))
    base.element_names;
  Buffer.add_string buf "\n";
  Buffer.add_string
    buf
    "  val bottom : t\n\
    \  val top : t\n\n\
    \  val leq : t -> t -> bool\n\
    \  val equal : t -> t -> bool\n\
    \  val join : t -> t -> t\n\
    \  val meet : t -> t -> t\n\
    \  val sub : t -> t -> t\n\
    \  val imply : t -> t -> t\n\n\
    \  val name : t -> string\n\
    \  val of_name : string -> t option\n\
    \  val pp : Format.formatter -> t -> unit\n\
    \  val show : t -> string\n\n";
  add_repr_sig buf;
  bprintf
    buf
    "  module Const : sig\n\
    \    type nonrec t = t\n\
    \n\
    \    val min : t\n\
    \    val max : t\n\
    \    val le : t -> t -> bool\n\
    \    val equal : t -> t -> bool\n\
    \    val join : t -> t -> t\n\
    \    val meet : t -> t -> t\n\
    \    val print : Format.formatter -> t -> unit\n\
    \    val legacy : t\n\
    \  end\n";
  Buffer.add_string buf "end\n\n"

let add_product_view_sig buf (product : product) _module_name ~in_op =
  bprintf buf "  type view = {\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    %s : %s.t;\n"
        field.name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  }\n\n"

let add_product_sig buf (product : product) module_name ~in_op =
  bprintf buf "module %s : sig\n" module_name;
  if not in_op
  then Buffer.add_string buf "  type t = private int\n\n"
  else bprintf buf "  type t = %s.t\n\n" product.name;
  add_product_view_sig buf product module_name ~in_op;
  Buffer.add_string buf "  val make : ";
  List.iteri
    (fun i (field : field) ->
      if i > 0 then Buffer.add_string buf " -> ";
      bprintf
        buf
        "%s:%s.t"
        field.name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf " -> t\n";
  Buffer.add_string buf "  val view : t -> view\n";
  Buffer.add_string buf "  val of_view : view -> t\n\n";
  List.iter
    (fun (field : field) ->
      let field_module = field_module_name field ~in_op in
      bprintf buf "  val %s : t -> %s.t\n" field.name field_module;
      bprintf buf "  val proj_%s : t -> %s.t\n" field.name field_module;
      bprintf buf "  val with_%s : %s.t -> t -> t\n" field.name field_module;
      bprintf buf "  val %s_bot : %s.t -> t\n" field.name field_module;
      bprintf buf "  val %s_top : %s.t -> t\n" field.name field_module;
      bprintf buf "  val min_with_%s : %s.t -> t\n" field.name field_module;
      bprintf buf "  val max_with_%s : %s.t -> t\n" field.name field_module)
    product.fields;
  Buffer.add_string
    buf
    "\n  val bottom : t\n\
    \  val top : t\n\n\
    \  val leq : t -> t -> bool\n\
    \  val equal : t -> t -> bool\n\
    \  val join : t -> t -> t\n\
    \  val meet : t -> t -> t\n\
    \  val sub : t -> t -> t\n\
    \  val imply : t -> t -> t\n\n\
    \  val pp : Format.formatter -> t -> unit\n\
    \  val show : t -> string\n\n\
    \  module Layout : sig\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "    val %s_shift : int\n" field.name;
      bprintf buf "    val %s_mask : int\n" field.name)
    product.fields;
  Buffer.add_string buf "  end\n\n";
  add_repr_sig buf;
  Buffer.add_string buf "  module Axis : sig\n";
  Buffer.add_string buf "    type _ t =\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | %s : %s.t t\n"
        axis.ctor_name
        (axis_module_name axis ~in_op))
    product.axes;
  Buffer.add_string
    buf
    "\n\
    \    type packed = P : 'a t -> packed\n\
    \n\
    \    val all : packed list\n\
    \    val print : Format.formatter -> 'a t -> unit\n\
    \  end\n\n";
  bprintf
    buf
    "  module Const : sig\n\
    \    type nonrec t = t\n\
    \    type 'a axis = 'a Axis.t\n\
    \n\
    \    val min : t\n\
    \    val max : t\n\
    \    val le : t -> t -> bool\n\
    \    val equal : t -> t -> bool\n\
    \    val join : t -> t -> t\n\
    \    val meet : t -> t -> t\n\
    \    val print : Format.formatter -> t -> unit\n\
    \    val legacy : t\n\
    \n\
    \    val split : t -> view\n\
    \    val merge : view -> t\n\
    \n\
    \    val proj : 'a axis -> t -> 'a\n\
    \    val min_with : 'a axis -> 'a -> t\n\
    \    val max_with : 'a axis -> 'a -> t\n\
    \  end\n";
  Buffer.add_string buf "end\n\n"

let add_embedding_sig buf (embedding : embedding) =
  bprintf buf "module %s : sig\n" embedding.module_name;
  bprintf buf "  val embed : %s.t -> %s.t\n" embedding.small_name embedding.big_name;
  List.iteri
    (fun i (morphism : morphism) ->
      bprintf
        buf
        "  val left%d : %s.t -> %s.t\n"
        (i + 1)
        morphism.domain
        morphism.codomain)
    embedding.left_chain;
  List.iteri
    (fun i (morphism : morphism) ->
      bprintf
        buf
        "  val right%d : %s.t -> %s.t\n"
        (i + 1)
        morphism.domain
        morphism.codomain)
    embedding.right_chain;
  List.iter
    (fun (alias_name, target) ->
      let morphism =
        if target = "embed"
        then embedding.embed
        else if String.length target >= 4 && String.sub target 0 4 = "left"
        then List.nth embedding.left_chain (int_of_string (String.sub target 4 (String.length target - 4)) - 1)
        else List.nth embedding.right_chain (int_of_string (String.sub target 5 (String.length target - 5)) - 1)
      in
      bprintf buf "  val %s : %s.t -> %s.t\n" alias_name morphism.domain morphism.codomain)
    embedding.aliases;
  Buffer.add_string buf "end\n\n"

let add_base_ml buf (base : base) module_name descriptor =
  bprintf buf "module %s = struct\n" module_name;
  if module_name = base.name
  then Buffer.add_string buf "  type t = int\n"
  else bprintf buf "  type t = %s.t\n" base.name;
  Array.iteri
    (fun i (_ : string) ->
      bprintf buf "  let %s = %d\n" base.element_value_names.(i) base.element_values.(i))
    base.element_names;
  add_specialized_ops_ml buf descriptor;
  Buffer.add_string buf "  let name = function\n";
  Array.iteri
    (fun i elt ->
      bprintf buf "    | %d -> %S\n" base.element_values.(i) elt)
    base.element_names;
  Buffer.add_string buf "    | _ -> invalid_arg \"invalid lattice element\"\n";
  Buffer.add_string buf "  let of_name = function\n";
  Array.iteri
    (fun i elt ->
      bprintf buf "    | %S -> Some %s\n" elt base.element_value_names.(i))
    base.element_names;
  Buffer.add_string buf "    | _ -> None\n";
  Buffer.add_string
    buf
    "  let pp ppf = function\n";
  Array.iteri
    (fun i elt ->
      bprintf
        buf
        "    | %d -> Format.pp_print_string ppf %S\n"
        base.element_values.(i)
        elt)
    base.element_names;
  Buffer.add_string
    buf
    "    | _ -> invalid_arg \"invalid lattice element\"\n\
    \  let show t = Format.asprintf \"%a\" pp t\n\
    \  module Repr = struct\n";
  bprintf
    buf
    "    let bits = %d\n\
    \    let mask = %d\n\
    \    let to_int (x : t) = x\n"
    descriptor.bits
    descriptor.mask;
  add_repr_validator_ml buf base.repr_validator;
  bprintf
    buf
    "  end\n\
    \  module Const = struct\n\
    \    type nonrec t = t\n\
    \n\
    \    let min = bottom\n\
    \    let max = top\n\
    \    let le = leq\n\
    \    let equal = equal\n\
    \    let join = join\n\
    \    let meet = meet\n\
    \    let print = pp\n\
    \    let legacy = %s\n\
    \  end\n\
    end\n\n"
    (legacy_value_name base.name)

let add_product_ml buf (product : product) module_name descriptor ~in_op =
  bprintf buf "module %s = struct\n" module_name;
  if not in_op
  then Buffer.add_string buf "  type t = int\n"
  else bprintf buf "  type t = %s.t\n" product.name;
  bprintf buf "  type view = {\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "    %s : %s.t;\n" field.name (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  }\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "  module %s = %s\n"
        (field_module_alias_name field)
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  module Layout = struct\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "    let %s_shift = %d\n" field.name field.shift;
      bprintf buf "    let %s_mask = %d\n" field.name field.layout_mask)
    product.fields;
  Buffer.add_string buf "  end\n";
  Buffer.add_string buf "  let[@inline] make ";
  List.iter
    (fun (field : field) -> bprintf buf "~%s " field.name)
    product.fields;
  Buffer.add_string buf "=\n    ";
  let parts =
    List.map
      (fun (field : field) ->
        if field.shift = 0
        then field.name
        else Printf.sprintf "(%s lsl %d)" field.name field.shift)
      product.fields
  in
  Buffer.add_string buf (String.concat " lor " parts);
  Buffer.add_string buf "\n";
  List.iter
    (fun (field : field) ->
      let field_module = field_module_name field ~in_op in
      bprintf
        buf
        "  let[@inline] %s t = (%s land %d : %s.t)\n"
        field.name
        (if field.shift = 0
         then "t"
         else Printf.sprintf "(t lsr %d)" field.shift)
        field.raw_mask
        field_module;
      bprintf buf "  let proj_%s = %s\n" field.name field.name;
      bprintf
        buf
        "  let[@inline] with_%s value t = (t land lnot Layout.%s_mask) lor %s\n"
        field.name
        field.name
        (if field.shift = 0
         then "value"
         else Printf.sprintf "(value lsl %d)" field.shift))
    product.fields;
  Buffer.add_string buf "  let[@inline] of_view view =\n";
  Buffer.add_string buf "    make ";
  List.iter
    (fun (field : field) -> bprintf buf "~%s:view.%s " field.name field.name)
    product.fields;
  Buffer.add_string buf "\n";
  Buffer.add_string buf "  let[@inline] view t =\n    {\n";
  List.iter
    (fun (field : field) -> bprintf buf "      %s = %s t;\n" field.name field.name)
    product.fields;
  Buffer.add_string buf "    }\n";
  add_specialized_ops_ml buf descriptor;
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "  let[@inline] %s_bot value = with_%s value bottom\n"
        field.name
        field.name;
      bprintf
        buf
        "  let[@inline] %s_top value = with_%s value top\n"
        field.name
        field.name;
      bprintf buf "  let min_with_%s = %s_bot\n" field.name field.name;
      bprintf buf "  let max_with_%s = %s_top\n" field.name field.name)
    product.fields;
  Buffer.add_string buf "  let pp ppf t =\n";
  if product.fields = []
  then Buffer.add_string buf "    Format.pp_print_string ppf \"{}\"\n"
  else (
    Buffer.add_string buf "    let v = view t in\n";
    Buffer.add_string buf "    Format.pp_print_string ppf \"{ \";\n";
    List.iteri
      (fun i field ->
        let field : field = field in
        if i > 0
        then Buffer.add_string buf "    Format.pp_print_string ppf \"; \";\n";
        bprintf
          buf
          "    Format.pp_print_string ppf %S;\n\
          \    %s.pp ppf v.%s;\n"
          (field.name ^ " = ")
          (field_module_name field ~in_op)
          field.name)
      product.fields;
    Buffer.add_string buf "    Format.pp_print_string ppf \" }\"\n");
  Buffer.add_string
    buf
    "  let show t = Format.asprintf \"%a\" pp t\n\
    \  module Repr = struct\n";
  bprintf
    buf
    "    let bits = %d\n\
    \    let mask = %d\n\
    \    let to_int (x : t) = x\n"
    descriptor.bits
    descriptor.mask;
  add_repr_validator_ml buf product.repr_validator;
  Buffer.add_string buf "  end\n";
  Buffer.add_string buf "  module Axis = struct\n";
  Buffer.add_string buf "    type _ t =\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | %s : %s.t t\n"
        axis.ctor_name
        (axis_module_name axis ~in_op))
    product.axes;
  Buffer.add_string buf "\n    type packed = P : 'a t -> packed\n\n";
  Buffer.add_string buf "    let all = [\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf buf "      P %s;\n" axis.ctor_name)
    product.axes;
  Buffer.add_string buf "    ]\n";
  Buffer.add_string
    buf
    "    let print : type a. Format.formatter -> a t -> unit = fun ppf -> function\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | %s -> Format.pp_print_string ppf %S\n"
        axis.ctor_name
        axis.name)
    product.axes;
  Buffer.add_string buf "  end\n";
  Buffer.add_string
    buf
    "  module Const = struct\n\
    \    type nonrec t = t\n\
    \    type 'a axis = 'a Axis.t\n\
    \n\
    \    let min = bottom\n\
    \    let max = top\n\
    \    let le = leq\n\
    \    let equal = equal\n\
    \    let join = join\n\
    \    let meet = meet\n\
    \    let print = pp\n\
    \n\
    \    let split = view\n\
    \    let merge = of_view\n\
    \n";
  Buffer.add_string buf "    let legacy =\n      make ";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "~%s:%s.Const.legacy "
        field.name
        (field_module_alias_name field))
    product.fields;
  Buffer.add_string buf "\n";
  Buffer.add_string
    buf
    "    let[@inline] proj : type a. a axis -> t -> a = fun axis t ->\n\
    \      match axis with\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | Axis.%s -> %s t\n"
        axis.ctor_name
        axis.name)
    product.axes;
  Buffer.add_string
    buf
    "    let[@inline] min_with : type a. a axis -> a -> t = fun axis value ->\n\
    \      match axis with\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | Axis.%s -> %s_bot value\n"
        axis.ctor_name
        axis.name)
    product.axes;
  Buffer.add_string
    buf
    "    let[@inline] max_with : type a. a axis -> a -> t = fun axis value ->\n\
    \      match axis with\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | Axis.%s -> %s_top value\n"
        axis.ctor_name
        axis.name)
    product.axes;
  Buffer.add_string buf "  end\nend\n\n"

let find_base model name =
  match String_map.find name model.lattices with
  | Base base -> base
  | Product _ -> invalid_arg "expected a base lattice"

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

let add_morphism_ml buf model name morphism =
  let domain = find_base model morphism.domain in
  let codomain = find_base model morphism.codomain in
  bprintf buf "  let %s = function\n" name;
  Array.iteri
    (fun i target ->
      bprintf
        buf
        "    | %d -> %s.%s\n"
        domain.element_values.(i)
        codomain.name
        codomain.element_value_names.(target))
    morphism.map;
  Buffer.add_string buf "    | _ -> invalid_arg \"invalid lattice element\"\n"

let add_embedding_ml buf model (embedding : embedding) =
  bprintf buf "module %s = struct\n" embedding.module_name;
  add_morphism_ml buf model "embed" embedding.embed;
  List.iteri
    (fun i (morphism : morphism) ->
      add_morphism_ml buf model ("left" ^ string_of_int (i + 1)) morphism)
    embedding.left_chain;
  List.iteri
    (fun i (morphism : morphism) ->
      add_morphism_ml buf model ("right" ^ string_of_int (i + 1)) morphism)
    embedding.right_chain;
  List.iter
    (fun (alias_name, slot) ->
      bprintf buf "  let %s = %s\n" alias_name slot;
      ignore (morphism_of_slot embedding slot))
    embedding.aliases;
  Buffer.add_string buf "end\n\n"

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

let add_exported_aliases_sig buf aliases =
  List.iter
    (fun alias ->
      bprintf
        buf
        "val %s : %s.t -> %s.t\n"
        alias.alias_name
        alias.morphism.domain
        alias.morphism.codomain)
    aliases;
  if aliases <> [] then Buffer.add_string buf "\n"

let add_exported_aliases_ml buf aliases =
  List.iter
    (fun alias ->
      bprintf
        buf
        "let %s = %s.%s\n"
        alias.alias_name
        alias.module_name
        alias.alias_name)
    aliases;
  if aliases <> [] then Buffer.add_string buf "\n"

let add_exported_aliases_top_sig buf model =
  let aliases = collect_exported_aliases model in
  add_exported_aliases_sig buf aliases

let add_exported_aliases_top_ml buf model =
  let aliases = collect_exported_aliases model in
  add_exported_aliases_ml buf aliases

let values_var_name module_name = "values_" ^ Name.snake_case module_name

let add_test_runtime_ml buf root_module =
  bprintf buf "open %s\n\n" root_module;
  Buffer.add_string
    buf
    {|let exhaustive_threshold = 256

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
          base.name
          base.name;
        bprintf buf "let %s = %s\n\n" op_values_name values_name
      | Lattice (Product product) ->
        let values_name = values_var_name product.name in
        let op_values_name = values_var_name (Name.op_module_name product.name) in
        bprintf
          buf
          "let %s = values_of_repr %s.Repr.mask %s.Repr.of_int_exn\n"
          values_name
          product.name
          product.name;
        bprintf buf "let %s = %s\n\n" op_values_name values_name
      | Embedding _ -> ())
    model.items

let add_base_test_ml buf (base : base) =
  let values_name = values_var_name base.name in
  let op_name = Name.op_module_name base.name in
  let op_values_name = values_var_name op_name in
  bprintf buf "let () =\n";
  bprintf
    buf
    "  check_repr ~name:%S ~values:%s ~mask:%s.Repr.mask ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    base.name
    values_name
    base.name
    base.name
    base.name
    base.name;
  bprintf
    buf
    "  check_name_roundtrip ~name:%S ~values:%s ~to_name:%s.name ~of_name:%s.of_name ~show:%s.show;\n"
    base.name
    values_name
    base.name
    base.name
    base.name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    base.name
    values_name
    base.name
    base.name
    base.name
    base.name
    base.name
    base.name
    base.name
    base.name
    base.name
    base.name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    op_name
    op_values_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name;
  bprintf
    buf
    "  check_duality ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~op_bottom:%s.bottom ~op_top:%s.top ~op_join:%s.join ~op_meet:%s.meet ~op_sub:%s.sub ~op_imply:%s.imply ~show:%s.show;\n\
    \  ()\n\n"
    base.name
    values_name
    base.name
    base.name
    base.name
    base.name
    base.name
    base.name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    base.name

let add_product_reference_defs_ml buf (product : product) module_name ~in_op =
  let field_module field = field_module_name field ~in_op in
  let add_view_record value_of_field =
    Buffer.add_string buf "    {\n";
    List.iter
      (fun (field : field) ->
        bprintf buf "      %s = %s;\n" field.name (value_of_field field))
      product.fields;
    Buffer.add_string buf "    }\n"
  in
  bprintf buf "  let ref_bottom =\n    %s.of_view\n" module_name;
  add_view_record (fun field ->
      Printf.sprintf "%s.bottom" (field_module field));
  Buffer.add_string buf "  in\n";
  bprintf buf "  let ref_top =\n    %s.of_view\n" module_name;
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
    module_name
    module_name
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
        module_name
        module_name
        module_name;
      add_view_record (fun field ->
          Printf.sprintf
            "%s.%s xv.%s yv.%s"
            (field_module field)
            op_name
            field.name
            field.name);
      Buffer.add_string buf "  in\n")
    [ "join"; "meet"; "sub"; "imply" ]

let add_product_test_ml buf (product : product) =
  let values_name = values_var_name product.name in
  let op_name = Name.op_module_name product.name in
  let op_values_name = values_var_name op_name in
  bprintf buf "let () =\n";
  bprintf
    buf
    "  check_repr ~name:%S ~values:%s ~mask:%s.Repr.mask ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    product.name
    product.name
    product.name
    product.name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name;
  bprintf
    buf
    "  check_lattice ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    op_name
    op_values_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name;
  bprintf
    buf
    "  check_duality ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~op_bottom:%s.bottom ~op_top:%s.top ~op_join:%s.join ~op_meet:%s.meet ~op_sub:%s.sub ~op_imply:%s.imply ~show:%s.show;\n"
    product.name
    values_name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    product.name;
  add_product_reference_defs_ml buf product product.name ~in_op:false;
  bprintf
    buf
    "  check_reference_equivalence ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~ref_bottom ~ref_top ~ref_leq ~ref_join ~ref_meet ~ref_sub ~ref_imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name
    product.name;
  add_product_reference_defs_ml buf product op_name ~in_op:true;
  bprintf
    buf
    "  check_reference_equivalence ~name:%S ~values:%s ~bottom:%s.bottom ~top:%s.top ~leq:%s.leq ~join:%s.join ~meet:%s.meet ~sub:%s.sub ~imply:%s.imply ~ref_bottom ~ref_top ~ref_leq ~ref_join ~ref_meet ~ref_sub ~ref_imply ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    op_name
    op_values_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name
    op_name;
  bprintf
    buf
    "  check_product_roundtrip ~name:%S ~values:%s ~view:%s.view ~of_view:%s.of_view ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
    product.name
    values_name
    product.name
    product.name
    product.name
    product.name
    product.name;
  bprintf buf "  iter1 %s (fun value ->\n" values_name;
  bprintf buf "    let made = %s.make " product.name;
  List.iter
    (fun (field : field) ->
      bprintf buf "~%s:(%s.%s value) " field.name product.name field.name)
    product.fields;
  bprintf
    buf
    "in\n\
    \    check_valid ~name:%S ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show made;\n\
    \    ensure (made = value) %S"
    product.name
    product.name
    product.name
    product.name
    (product.name ^ ": make/projection roundtrip failed");
  Buffer.add_string buf ");\n";
  List.iter
    (fun (field : field) ->
      let field_module = field_module_name field ~in_op:false in
      let field_values_name = values_var_name field_module in
      bprintf
        buf
        "  check_field_projection ~name:%S ~product_values:%s ~field_values:%s ~product_leq:%s.leq ~field_leq:%s.leq ~field:%s.%s ~with_field:%s.with_%s ~field_bot:%s.%s_bot ~field_top:%s.%s_top ~product_to_int:%s.Repr.to_int ~product_of_int_exn:%s.Repr.of_int_exn ~product_show:%s.show ~field_to_int:%s.Repr.to_int ~field_of_int_exn:%s.Repr.of_int_exn ~field_show:%s.show;\n"
        (product.name ^ "." ^ field.name)
        values_name
        field_values_name
        product.name
        field_module
        product.name
        field.name
        product.name
        field.name
        product.name
        field.name
        product.name
        field.name
        product.name
        product.name
        product.name
        field_module
        field_module
        field_module;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.proj_%s ~g:%s.%s ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".proj_" ^ field.name)
        values_name
        product.name
        field.name
        product.name
        field.name
        field_module
        field_module
        field_module;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.min_with_%s ~g:%s.%s_bot ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".min_with_" ^ field.name)
        field_values_name
        product.name
        field.name
        product.name
        field.name
        product.name
        product.name
        product.name;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:%s.max_with_%s ~g:%s.%s_top ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".max_with_" ^ field.name)
        field_values_name
        product.name
        field.name
        product.name
        field.name
        product.name
        product.name
        product.name;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:(%s.Const.proj %s.Axis.%s) ~g:%s.%s ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".Const.proj." ^ field.name)
        values_name
        product.name
        product.name
        (axis_ctor_name field.name)
        product.name
        field.name
        field_module
        field_module
        field_module;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:(%s.Const.min_with %s.Axis.%s) ~g:%s.%s_bot ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".Const.min_with." ^ field.name)
        field_values_name
        product.name
        product.name
        (axis_ctor_name field.name)
        product.name
        field.name
        product.name
        product.name
        product.name;
      bprintf
        buf
        "  check_function_equality ~name:%S ~values:%s ~f:(%s.Const.max_with %s.Axis.%s) ~g:%s.%s_top ~to_int:%s.Repr.to_int ~of_int_exn:%s.Repr.of_int_exn ~show:%s.show;\n"
        (product.name ^ ".Const.max_with." ^ field.name)
        field_values_name
        product.name
        product.name
        (axis_ctor_name field.name)
        product.name
        field.name
        product.name
        product.name
        product.name)
    product.fields;
  Buffer.add_string buf "  ()\n\n"

let add_embedding_test_ml buf (embedding : embedding) =
  let small_values = values_var_name embedding.small_name in
  let big_values = values_var_name embedding.big_name in
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
    embedding.big_name
    embedding.big_name
    embedding.big_name
    (embedding.module_name ^ ".embed(y)")
    embedding.big_name
    embedding.big_name
    embedding.big_name
    embedding.small_name
    embedding.big_name
    (embedding.module_name ^ ": order preservation failed")
    embedding.big_name
    embedding.small_name
    (embedding.module_name ^ ": order reflection failed");
  (match embedding.left_chain with
   | left1 :: rest ->
     bprintf
       buf
       "  check_adjunction ~name:%S ~a_values:%s ~b_values:%s ~leq_a:%s.leq ~leq_b:%s.leq ~f:%s.left1 ~g:%s.embed ~show_f:%s.show ~show_g:%s.show ~f_to_int:%s.Repr.to_int ~f_of_int_exn:%s.Repr.of_int_exn ~g_to_int:%s.Repr.to_int ~g_of_int_exn:%s.Repr.of_int_exn;\n"
       (embedding.module_name ^ ".left1/embed")
       (values_var_name left1.domain)
       (values_var_name left1.codomain)
       left1.domain
       left1.codomain
       embedding.module_name
       embedding.module_name
       left1.codomain
       left1.domain
       left1.codomain
       left1.codomain
       left1.domain
       left1.domain;
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
           morphism.domain
           morphism.codomain
           embedding.module_name
           curr
           embedding.module_name
           prev
           morphism.codomain
           morphism.domain
           morphism.codomain
           morphism.codomain
           morphism.domain
           morphism.domain)
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
       embedding.small_name
       embedding.big_name
       embedding.module_name
       embedding.module_name
       embedding.big_name
       embedding.small_name
       embedding.big_name
       embedding.big_name
       embedding.small_name
       embedding.small_name;
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
           morphism.codomain
           morphism.domain
           embedding.module_name
           prev
           embedding.module_name
           curr
           morphism.domain
           morphism.codomain
           morphism.domain
           morphism.domain
           morphism.codomain
           morphism.codomain)
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
        morphism.codomain
        morphism.codomain
        morphism.codomain)
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
        alias.morphism.codomain
        alias.morphism.codomain
        alias.morphism.codomain)
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

let render ~root_module model =
  let mli = Buffer.create 4096 in
  let ml = Buffer.create 8192 in
  let test_ml = Buffer.create 8192 in
  add_test_ml test_ml model ~root_module;
  List.iter
    (function
      | Lattice (Base base) ->
        add_base_sig mli base base.name;
        add_base_sig mli base (Name.op_module_name base.name);
        add_base_ml ml base base.name base.descriptor;
        add_base_ml ml base (Name.op_module_name base.name) base.op_descriptor
      | Lattice (Product product) ->
        add_product_sig mli product product.name ~in_op:false;
        add_product_sig mli product (Name.op_module_name product.name) ~in_op:true;
        add_product_ml ml product product.name product.descriptor ~in_op:false;
        add_product_ml
          ml
          product
          (Name.op_module_name product.name)
          product.op_descriptor
          ~in_op:true
      | Embedding embedding ->
        add_embedding_sig mli embedding;
        add_embedding_ml ml model embedding)
    model.items;
  add_exported_aliases_top_sig mli model;
  add_exported_aliases_top_ml ml model;
  { ml = Buffer.contents ml;
    mli = Buffer.contents mli;
    test_ml = Buffer.contents test_ml
  }
