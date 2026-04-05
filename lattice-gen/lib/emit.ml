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

let add_repr_validator_ml ?(indent = "    ") buf repr_validator =
  let has_close_repr =
    add_schedule_fn_ml ~indent buf "close_repr" repr_validator.down `Down
  in
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
  Emit_common.field_module_name field ~in_op

let axis_ctor_name = Emit_common.axis_ctor_name

let field_module_alias_name (field : field) = "Field_" ^ axis_ctor_name field.name

let field_const_module_name (field : field) =
  field_module_alias_name field ^ ".Const"

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

let solver_object_ctor_name_of_name name = Model.solver_object_ctor_name name

let solver_objects_in_order model =
  let add acc name =
    let object_ = String_map.find name model.solver_objects in
    acc @ [ object_ ]
  in
  List.fold_left
    (fun acc ->
      function
      | Lattice lattice ->
        let name = name_of_lattice lattice in
        let acc = add acc name in
        add acc (Name.op_module_name name)
      | Embedding _ -> acc)
    []
    model.items

let add_solver_items_sig buf =
  Buffer.add_string
    buf
    "  type 'd t constraint 'd = 'l * 'r\n\
    \n\
    \  type l = (Allowance.allowed * Allowance.disallowed) t\n\
    \  type r = (Allowance.disallowed * Allowance.allowed) t\n\
    \  type lr = (Allowance.allowed * Allowance.allowed) t\n\
    \n\
    \  type simple_error = {\n\
    \    left : const;\n\
    \    right : const;\n\
    \  }\n\
    \n\
    \  type error = simple_error\n\
    \n\
    \  type equate_step =\n\
    \    | Left_le_right\n\
    \    | Right_le_left\n\
    \n\
    \  type equate_error = equate_step * error\n\
    \n\
    \  val min : lr\n\
    \  val max : lr\n\
    \  val legacy : lr\n\
    \n\
    \  val of_const : const -> ('l * 'r) t\n\
    \  val to_const_exn : lr -> const\n\
    \n\
    \  val newvar : unit -> ('l * 'r) t\n\
    \  val newvar_above : l -> ('l * 'r) t * bool\n\
    \  val newvar_below : r -> ('l * 'r) t * bool\n\
    \n\
    \  val to_simple_error : error -> simple_error\n\
    \  val print_error : Format.formatter -> error -> unit\n\
    \  val print_equate_error : Format.formatter -> equate_error -> unit\n\
    \n\
    \  val submode : l -> r -> (unit, error) result\n\
    \  val submode_exn : l -> r -> unit\n\
    \  val equate : lr -> lr -> (unit, equate_error) result\n\
    \  val equate_exn : lr -> lr -> unit\n\
    \n\
    \  val join : l list -> l\n\
    \  val meet : r list -> r\n\
    \n\
    \  val print : ?verbose:bool -> Format_doc.formatter -> ('l * 'r) t -> unit\n\
    \  val show : ?verbose:bool -> ('l * 'r) t -> string\n\
    \n\
    \  val zap_to_floor : l -> const\n\
    \  val zap_to_ceil : r -> const\n"

let add_product_solver_items_sig buf (product : product) ~in_op:_ =
  Buffer.add_string
    buf
    "  type 'd t constraint 'd = 'l * 'r\n\
    \n\
    \  type l = (Allowance.allowed * Allowance.disallowed) t\n\
    \  type r = (Allowance.disallowed * Allowance.allowed) t\n\
    \  type lr = (Allowance.allowed * Allowance.allowed) t\n\
    \n\
    \  type 'a simple_axerror\n\
    \n\
    \  type simple_error = Error : 'a Axis.t * 'a simple_axerror -> simple_error\n\
    \n\
    \  type error = simple_error\n\
    \n\
    \  type equate_step =\n\
    \    | Left_le_right\n\
    \    | Right_le_left\n\
    \n\
    \  type equate_error = equate_step * error\n\
    \n\
    \  val min : lr\n\
    \  val max : lr\n\
    \  val legacy : lr\n\
    \n\
    \  val of_const : const -> ('l * 'r) t\n\
    \  val to_const_exn : lr -> const\n\
    \n\
    \  val newvar : unit -> ('l * 'r) t\n\
    \  val newvar_above : l -> ('l * 'r) t * bool\n\
    \  val newvar_below : r -> ('l * 'r) t * bool\n\
    \n\
    \  val to_simple_error : error -> simple_error\n\
    \  val print_error : Format.formatter -> error -> unit\n\
    \  val print_equate_error : Format.formatter -> equate_error -> unit\n\
    \n\
    \  val submode : l -> r -> (unit, error) result\n\
    \  val submode_exn : l -> r -> unit\n\
    \  val equate : lr -> lr -> (unit, equate_error) result\n\
    \  val equate_exn : lr -> lr -> unit\n\
    \n\
    \  val join : l list -> l\n\
    \  val meet : r list -> r\n\
    \n\
    \  val print : ?verbose:bool -> Format_doc.formatter -> ('l * 'r) t -> unit\n\
    \  val show : ?verbose:bool -> ('l * 'r) t -> string\n\
    \n\
    \  val zap_to_floor : l -> const\n\
    \  val zap_to_ceil : r -> const\n\
    \n\
    \  type ('a, 'd) axis_mode constraint 'd = 'l * 'r\n\
    \n\
    \  val proj : 'a Axis.t -> ('l * 'r) t -> ('a, 'l * 'r) axis_mode\n\
    \  val min_with : 'a Axis.t -> ('a, 'l * 'r) axis_mode -> ('l * Allowance.disallowed) t\n\
    \  val max_with : 'a Axis.t -> ('a, 'l * 'r) axis_mode -> (Allowance.disallowed * 'r) t\n";
  List.iter
    (fun (field : field) ->
      let field_module = field_module_alias_name field in
      bprintf
        buf
        "  val proj_%s : ('l * 'r) t -> ('l * 'r) %s.t\n\
        \  val min_with_%s : ('l * 'r) %s.t -> ('l * Allowance.disallowed) t\n\
        \  val max_with_%s : ('l * 'r) %s.t -> (Allowance.disallowed * 'r) t\n"
        field.name
        field_module
        field.name
        field_module
        field.name
        field_module)
    product.fields

let add_base_sig buf (base : base) module_name ~include_solver =
  let descriptor =
    if module_name = base.name then base.descriptor else base.op_descriptor
  in
  ignore descriptor;
  bprintf buf "module %s : sig\n" module_name;
  Buffer.add_string buf "  module Const : sig\n";
  if module_name = base.name
  then Buffer.add_string buf "    type t = private int\n\n"
  else bprintf buf "    type t = %s.Const.t\n\n" base.name;
  Array.iteri
    (fun i (_ : string) ->
      bprintf buf "    val %s : t\n" base.element_value_names.(i))
    base.element_names;
  Buffer.add_string buf "\n";
  Buffer.add_string
    buf
    "    val bottom : t\n\
    \    val top : t\n\n\
    \    val leq : t -> t -> bool\n\
    \    val equal : t -> t -> bool\n\
    \    val join : t -> t -> t\n\
    \    val meet : t -> t -> t\n\
    \    val sub : t -> t -> t\n\
    \    val imply : t -> t -> t\n\n\
    \    val name : t -> string\n\
    \    val of_name : string -> t option\n\
    \    val pp : Format.formatter -> t -> unit\n\
    \    val show : t -> string\n\n";
  Buffer.add_string
    buf
    "    module Repr : sig\n\
    \      val bits : int\n\
    \      val mask : int\n\
    \      val to_int : t -> int\n\
    \      val of_int_exn : int -> t\n\
    \    end\n\
    \n\
    \    val min : t\n\
    \    val max : t\n\
    \    val le : t -> t -> bool\n\
    \    val print : Format.formatter -> t -> unit\n\
    \    val legacy : t\n\
    \  end\n";
  Buffer.add_string buf "  type const = Const.t\n\n";
  if include_solver
  then (
    add_solver_items_sig buf;
    Buffer.add_string buf "\n";
    Array.iteri
      (fun i (_ : string) ->
        bprintf buf "  val %s : lr\n" base.element_value_names.(i))
      base.element_names;
    Buffer.add_string buf "\n");
  Buffer.add_string buf "end\n\n"

let add_product_view_sig buf (product : product) _module_name ~in_op:_ =
  bprintf buf "    type view = {\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      %s : %s.Const.t;\n"
        field.name
        (field_module_alias_name field))
    product.fields;
  Buffer.add_string buf "    }\n\n"

let add_product_sig buf (product : product) module_name ~in_op ~include_solver =
  bprintf buf "module %s : sig\n" module_name;
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "  module %s = %s\n"
        (field_module_alias_name field)
        (field_module_name field ~in_op))
    product.fields;
  if product.fields <> [] then Buffer.add_string buf "\n";
  Buffer.add_string buf "  module Axis : sig\n";
  Buffer.add_string buf "    type _ t =\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | %s : %s.Const.t t\n"
        axis.ctor_name
        ("Field_" ^ axis.ctor_name))
    product.axes;
  Buffer.add_string
    buf
    "\n\
    \    type packed = P : 'a t -> packed\n\
    \n\
    \    val compare : 'a t -> 'b t -> ('a, 'b) Misc.comparison\n\
    \n\
    \    val all : packed list\n\
    \    val print : Format.formatter -> 'a t -> unit\n\
    \  end\n\n";
  Buffer.add_string buf "  module Const : sig\n";
  if not in_op
  then Buffer.add_string buf "    type t = private int\n\n"
  else bprintf buf "    type t = %s.Const.t\n\n" product.name;
  add_product_view_sig buf product module_name ~in_op;
  Buffer.add_string buf "    val make : ";
  List.iteri
    (fun i (field : field) ->
      if i > 0 then Buffer.add_string buf " -> ";
      bprintf
        buf
        "%s:%s.Const.t"
        field.name
        (field_module_alias_name field))
    product.fields;
  Buffer.add_string buf " -> t\n";
  Buffer.add_string buf "    val view : t -> view\n";
  Buffer.add_string buf "    val of_view : view -> t\n\n";
  List.iter
    (fun (field : field) ->
      let field_module = field_module_alias_name field in
      bprintf buf "    val %s : t -> %s.Const.t\n" field.name field_module;
      bprintf buf "    val proj_%s : t -> %s.Const.t\n" field.name field_module;
      bprintf buf "    val with_%s : %s.Const.t -> t -> t\n" field.name field_module;
      bprintf buf "    val %s_bot : %s.Const.t -> t\n" field.name field_module;
      bprintf buf "    val %s_top : %s.Const.t -> t\n" field.name field_module;
      bprintf buf "    val min_with_%s : %s.Const.t -> t\n" field.name field_module;
      bprintf buf "    val max_with_%s : %s.Const.t -> t\n" field.name field_module)
    product.fields;
  Buffer.add_string
    buf
    "\n    val bottom : t\n\
    \    val top : t\n\n\
    \    val leq : t -> t -> bool\n\
    \    val equal : t -> t -> bool\n\
    \    val join : t -> t -> t\n\
    \    val meet : t -> t -> t\n\
    \    val sub : t -> t -> t\n\
    \    val imply : t -> t -> t\n\n\
    \    val pp : Format.formatter -> t -> unit\n\
    \    val show : t -> string\n\n\
    \    module Layout : sig\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "      val %s_shift : int\n" field.name;
      bprintf buf "      val %s_mask : int\n" field.name)
    product.fields;
  Buffer.add_string
    buf
    "    end\n\n\
    \    module Repr : sig\n\
    \      val bits : int\n\
    \      val mask : int\n\
    \      val to_int : t -> int\n\
    \      val of_int_exn : int -> t\n\
    \    end\n\
    \n";
  bprintf
    buf
    "    val min : t\n\
    \    val max : t\n\
    \    val le : t -> t -> bool\n\
    \    val print : Format.formatter -> t -> unit\n\
    \    val legacy : t\n\
    \n\
    \    val split : t -> view\n\
    \    val merge : view -> t\n\
    \n\
    \    val proj : 'a Axis.t -> t -> 'a\n\
    \    val min_with : 'a Axis.t -> 'a -> t\n\
    \    val max_with : 'a Axis.t -> 'a -> t\n\
    \  end\n";
  Buffer.add_string buf "  type const = Const.t\n\n";
  if include_solver
  then (
    add_product_solver_items_sig buf product ~in_op;
    Buffer.add_string buf "\n");
  Buffer.add_string buf "end\n\n"

let add_embedding_sig buf (embedding : embedding) =
  bprintf buf "module %s : sig\n" embedding.module_name;
  bprintf
    buf
    "  val embed : %s.const -> %s.const\n"
    embedding.small_name
    embedding.big_name;
  List.iteri
    (fun i (morphism : morphism) ->
      bprintf
        buf
        "  val left%d : %s.const -> %s.const\n"
        (i + 1)
        morphism.domain
        morphism.codomain)
    embedding.left_chain;
  List.iteri
    (fun i (morphism : morphism) ->
      bprintf
        buf
        "  val right%d : %s.const -> %s.const\n"
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
      bprintf
        buf
        "  val %s : %s.const -> %s.const\n"
        alias_name
        morphism.domain
        morphism.codomain)
    embedding.aliases;
  Buffer.add_string buf "end\n\n"

let add_base_const_module_ml buf (base : base) module_name descriptor =
  if module_name = base.name
  then (
    bprintf buf "  module %s = struct\n" module_name;
    Buffer.add_string buf "    type t = int\n";
    Array.iteri
      (fun i (_ : string) ->
        bprintf
          buf
          "    let %s = %d\n"
          base.element_value_names.(i)
          base.element_values.(i))
      base.element_names;
    add_specialized_ops_ml ~indent:"    " buf descriptor;
    Buffer.add_string buf "    let name = function\n";
    Array.iteri
      (fun i elt ->
        bprintf buf "      | %d -> %S\n" base.element_values.(i) elt)
      base.element_names;
    Buffer.add_string buf "      | _ -> invalid_arg \"invalid lattice element\"\n";
    Buffer.add_string buf "    let of_name = function\n";
    Array.iteri
      (fun i elt ->
        bprintf buf "      | %S -> Some %s\n" elt base.element_value_names.(i))
      base.element_names;
    Buffer.add_string buf "      | _ -> None\n";
    Buffer.add_string buf "    let pp ppf = function\n";
    Array.iteri
      (fun i elt ->
        bprintf
          buf
          "      | %d -> Format.pp_print_string ppf %S\n"
          base.element_values.(i)
          elt)
      base.element_names;
    Buffer.add_string
      buf
      "      | _ -> invalid_arg \"invalid lattice element\"\n\
      \    let show t = Format.asprintf \"%a\" pp t\n\
      \    module Repr = struct\n";
    bprintf
      buf
      "      let bits = %d\n\
      \      let mask = %d\n\
      \      let to_int (x : t) = x\n"
      descriptor.bits
      descriptor.mask;
    add_repr_validator_ml ~indent:"      " buf base.repr_validator;
    bprintf
      buf
      "    end\n\
      \n\
      \    let min = bottom\n\
      \    let max = top\n\
      \    let le = leq\n\
      \    let print = pp\n\
      \    let legacy = %s\n\
      \  end\n\n"
      (legacy_value_name base.name))
  else (
    bprintf buf "  module %s = struct\n" module_name;
    bprintf buf "    include Make_const_op (%s)\n" base.name;
    Array.iteri
      (fun i (_ : string) ->
        bprintf
          buf
          "    let %s = %s.%s\n"
          base.element_value_names.(i)
          base.name
          base.element_value_names.(i))
      base.element_names;
    bprintf buf "    let name = %s.name\n" base.name;
    bprintf buf "    let of_name = %s.of_name\n" base.name;
    Buffer.add_string buf "  end\n\n")

let add_product_const_module_ml
    buf
    (product : product)
    module_name
    descriptor
    ~in_op
  =
  bprintf buf "  module %s = struct\n" module_name;
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "    module %s = %s\n"
        (field_module_alias_name field)
        (field_module_name field ~in_op))
    product.fields;
  if in_op
  then (
    bprintf buf "    include Make_const_op (%s)\n" product.name;
    bprintf buf "    type view = %s.view = {\n" product.name;
    List.iter
      (fun (field : field) ->
        bprintf
          buf
          "      %s : %s.t;\n"
          field.name
          (field_module_alias_name field))
      product.fields;
    Buffer.add_string buf "    }\n\n";
    bprintf buf "    module Layout = %s.Layout\n" product.name;
    bprintf buf "    module Axis = %s.Axis\n" product.name;
    bprintf buf "    let make = %s.make\n" product.name;
    bprintf buf "    let view = %s.view\n" product.name;
    bprintf buf "    let of_view = %s.of_view\n" product.name;
    List.iter
      (fun (field : field) ->
        bprintf buf "    let %s = %s.%s\n" field.name product.name field.name;
        bprintf buf "    let proj_%s = %s.proj_%s\n" field.name product.name field.name;
        bprintf buf "    let with_%s = %s.with_%s\n" field.name product.name field.name;
        bprintf buf "    let %s_bot = %s.%s_bot\n" field.name product.name field.name;
        bprintf buf "    let %s_top = %s.%s_top\n" field.name product.name field.name;
        bprintf
          buf
          "    let min_with_%s = %s.min_with_%s\n"
          field.name
          product.name
          field.name;
        bprintf
          buf
          "    let max_with_%s = %s.max_with_%s\n"
          field.name
          product.name
          field.name)
      product.fields;
    bprintf buf "    let split = %s.split\n" product.name;
    bprintf buf "    let merge = %s.merge\n" product.name;
    bprintf buf "    let proj = %s.proj\n" product.name;
    bprintf buf "    let min_with = %s.min_with\n" product.name;
    bprintf buf "    let max_with = %s.max_with\n" product.name;
    Buffer.add_string buf "  end\n\n")
  else (
    Buffer.add_string buf "    type t = int\n";
    Buffer.add_string buf "    type view = {\n";
    List.iter
      (fun (field : field) ->
        bprintf
          buf
          "      %s : %s.t;\n"
          field.name
          (field_module_alias_name field))
      product.fields;
    Buffer.add_string buf "    }\n\n";
    Buffer.add_string buf "    module Layout = struct\n";
    List.iter
      (fun (field : field) ->
        bprintf buf "      let %s_shift = %d\n" field.name field.shift;
        bprintf buf "      let %s_mask = %d\n" field.name field.layout_mask)
      product.fields;
    Buffer.add_string buf "    end\n";
    Buffer.add_string buf "    let[@inline] make ";
    List.iter (fun (field : field) -> bprintf buf "~%s " field.name) product.fields;
    Buffer.add_string buf "=\n      ";
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
        let field_module = field_module_alias_name field in
        bprintf
          buf
          "    let[@inline] %s t = (%s land %d : %s.t)\n"
          field.name
          (if field.shift = 0
           then "t"
           else Printf.sprintf "(t lsr %d)" field.shift)
          field.raw_mask
          field_module;
        bprintf buf "    let proj_%s = %s\n" field.name field.name;
        bprintf
          buf
          "    let[@inline] with_%s value t = (t land lnot Layout.%s_mask) lor %s\n"
          field.name
          field.name
          (if field.shift = 0
           then "value"
           else Printf.sprintf "(value lsl %d)" field.shift))
      product.fields;
    Buffer.add_string buf "    let[@inline] of_view view =\n";
    Buffer.add_string buf "      make ";
    List.iter
      (fun (field : field) -> bprintf buf "~%s:view.%s " field.name field.name)
      product.fields;
    Buffer.add_string buf "\n";
    Buffer.add_string buf "    let[@inline] view t =\n      {\n";
    List.iter
      (fun (field : field) -> bprintf buf "        %s = %s t;\n" field.name field.name)
      product.fields;
    Buffer.add_string buf "      }\n";
    add_specialized_ops_ml ~indent:"    " buf descriptor;
    List.iter
      (fun (field : field) ->
        bprintf
          buf
          "    let[@inline] %s_bot value = with_%s value bottom\n"
          field.name
          field.name;
        bprintf
          buf
          "    let[@inline] %s_top value = with_%s value top\n"
          field.name
          field.name;
        bprintf buf "    let min_with_%s = %s_bot\n" field.name field.name;
        bprintf buf "    let max_with_%s = %s_top\n" field.name field.name)
      product.fields;
    Buffer.add_string buf "    let pp ppf t =\n";
    if product.fields = []
    then Buffer.add_string buf "      Format.pp_print_string ppf \"{}\"\n"
    else (
      Buffer.add_string buf "      let v = view t in\n";
      Buffer.add_string buf "      Format.pp_print_string ppf \"{ \";\n";
      List.iteri
        (fun i field ->
          let field : field = field in
          if i > 0
          then Buffer.add_string buf "      Format.pp_print_string ppf \"; \";\n";
          bprintf
            buf
            "      Format.pp_print_string ppf %S;\n\
            \      %s.pp ppf v.%s;\n"
            (field.name ^ " = ")
            (field_module_alias_name field)
            field.name)
        product.fields;
      Buffer.add_string buf "      Format.pp_print_string ppf \" }\"\n");
    Buffer.add_string
      buf
      "    let show t = Format.asprintf \"%a\" pp t\n\
      \    module Repr = struct\n";
    bprintf
      buf
      "      let bits = %d\n\
      \      let mask = %d\n\
      \      let to_int (x : t) = x\n"
      descriptor.bits
      descriptor.mask;
    add_repr_validator_ml ~indent:"      " buf product.repr_validator;
    Buffer.add_string
      buf
      "    end\n\
      \n\
      \    let min = bottom\n\
      \    let max = top\n\
      \    let le = leq\n\
      \    let print = pp\n\
      \n";
    Buffer.add_string buf "    let split = view\n    let merge = of_view\n\n";
    Buffer.add_string buf "    let legacy =\n      make ";
    List.iter
      (fun (field : field) ->
        bprintf
          buf
          "~%s:%s.legacy "
          field.name
          (field_module_alias_name field))
      product.fields;
    Buffer.add_string buf "\n";
    Buffer.add_string buf "    module Axis = struct\n";
    Buffer.add_string buf "      type _ t =\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf
          buf
          "        | %s : %s.t t\n"
          axis.ctor_name
          (field_module_alias_name
             (List.find (fun (field : field) -> field.name = axis.name) product.fields)))
      product.axes;
    Buffer.add_string buf "\n      type packed = P : 'a t -> packed\n\n";
    Buffer.add_string
      buf
      "      let compare : type a b. a t -> b t -> (a, b) Misc.comparison =\n\
      \        fun left right ->\n\
      \          match left, right with\n";
    List.iteri
      (fun i (left_axis : axis_object) ->
        bprintf
          buf
          "          | %s, %s -> Misc.Equal\n"
          left_axis.ctor_name
          left_axis.ctor_name;
        List.iteri
          (fun j (right_axis : axis_object) ->
            if i <> j
            then
              bprintf
                buf
                "          | %s, %s -> Misc.%s\n"
                left_axis.ctor_name
                right_axis.ctor_name
                (if i < j then "Less_than" else "Greater_than"))
          product.axes)
      product.axes;
    Buffer.add_string buf "\n";
    Buffer.add_string buf "      let all = [\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf buf "        P %s;\n" axis.ctor_name)
      product.axes;
    Buffer.add_string buf "      ]\n";
    Buffer.add_string
      buf
      "      let print : type a. Format.formatter -> a t -> unit = fun ppf -> function\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf
          buf
          "        | %s -> Format.pp_print_string ppf %S\n"
          axis.ctor_name
          axis.name)
      product.axes;
    Buffer.add_string
      buf
      "      let proj : type a. a t -> int -> a = fun axis t ->\n\
      \        match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf buf "        | %s -> %s t\n" axis.ctor_name axis.name)
      product.axes;
    Buffer.add_string
      buf
      "      let set : type a. a t -> a -> int -> int = fun axis value t ->\n\
      \        match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf buf "        | %s -> with_%s value t\n" axis.ctor_name axis.name)
      product.axes;
    Buffer.add_string
      buf
      "      let min_with : type a. a t -> a -> int = fun axis value ->\n\
      \        match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf buf "        | %s -> %s_bot value\n" axis.ctor_name axis.name)
      product.axes;
    Buffer.add_string
      buf
      "      let max_with : type a. a t -> a -> int = fun axis value ->\n\
      \        match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf buf "        | %s -> %s_top value\n" axis.ctor_name axis.name)
      product.axes;
    Buffer.add_string
      buf
      "      let _ = set\n\
      \    end\n\
      \n\
      \    let proj = Axis.proj\n\
      \    let min_with = Axis.min_with\n\
      \    let max_with = Axis.max_with\n\
      \  end\n\n")

let add_lattices_const_ml buf model =
  let solver_objects = solver_objects_in_order model in
  Buffer.add_string buf "module Lattices_const = struct\n  [@@@warning \"-32-37\"]\n";
  List.iter
    (function
      | Lattice (Base base) ->
        add_base_const_module_ml buf base base.name base.descriptor;
        add_base_const_module_ml
          buf
          base
          (Name.op_module_name base.name)
          base.op_descriptor
      | Lattice (Product product) ->
        add_product_const_module_ml
          buf
          product
          product.name
          product.descriptor
          ~in_op:false;
        add_product_const_module_ml
          buf
          product
          (Name.op_module_name product.name)
          product.op_descriptor
          ~in_op:true
      | Embedding _ -> ())
    model.items;
  Buffer.add_string buf "  type _ obj =\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "    | %s : %s.t obj\n"
        object_.object_ctor_name
        object_.name)
    solver_objects;
  Buffer.add_string buf "\n";
  Buffer.add_string buf "  let obj_index : type a. a obj -> int = function\n";
  List.iteri
    (fun i (object_ : solver_object) ->
      bprintf buf "    | %s -> %d\n" object_.object_ctor_name i)
    solver_objects;
  Buffer.add_string buf "\n";
  let emit_obj_dispatch fn =
    let header =
      match fn with
      | "min" | "max" | "legacy" ->
        Printf.sprintf "  let %s : type a. a obj -> a = function\n" fn
      | "print" ->
        "  let print : type a. a obj -> Format_doc.formatter -> a -> unit = fun obj ppf x ->\n      match obj with\n"
      | "le" | "equal" ->
        Printf.sprintf
          "  let %s : type a. a obj -> a -> a -> bool = fun obj x y ->\n      match obj with\n"
          fn
      | "join" | "meet" | "sub" | "imply" ->
        Printf.sprintf
          "  let %s : type a. a obj -> a -> a -> a = fun obj x y ->\n      match obj with\n"
          fn
      | _ -> invalid_arg "emit_obj_dispatch"
    in
    Buffer.add_string buf header;
    List.iter
      (fun (object_ : solver_object) ->
        let prefix = object_.name in
        match fn with
        | "min" ->
          bprintf buf "    | %s -> %s.bottom\n" object_.object_ctor_name prefix
        | "max" ->
          bprintf buf "    | %s -> %s.top\n" object_.object_ctor_name prefix
        | "legacy" ->
          bprintf buf "    | %s -> %s.legacy\n" object_.object_ctor_name prefix
        | "le" ->
          bprintf buf "      | %s -> %s.leq x y\n" object_.object_ctor_name prefix
        | "equal" ->
          bprintf buf "      | %s -> %s.equal x y\n" object_.object_ctor_name prefix
        | "join" ->
          bprintf buf "      | %s -> %s.join x y\n" object_.object_ctor_name prefix
        | "meet" ->
          bprintf buf "      | %s -> %s.meet x y\n" object_.object_ctor_name prefix
        | "sub" ->
          bprintf buf "      | %s -> %s.sub x y\n" object_.object_ctor_name prefix
        | "imply" ->
          bprintf buf "      | %s -> %s.imply x y\n" object_.object_ctor_name prefix
        | "print" ->
          bprintf
            buf
            "      | %s -> Format_doc.pp_print_string ppf (%s.show x)\n"
            object_.object_ctor_name
            prefix
        | _ -> ())
      solver_objects;
    Buffer.add_string buf "\n"
  in
  List.iter emit_obj_dispatch [ "min"; "max"; "legacy"; "le"; "equal"; "join"; "meet"; "sub"; "imply"; "print" ];
  Buffer.add_string buf "  let _ = legacy, sub, imply\n\n";
  Buffer.add_string
    buf
    "  let equal_obj : type a b. a obj -> b obj -> (a, b) Misc.eq option =\n\
    \   fun obj1 obj2 ->\n\
    \    match obj1, obj2 with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "    | %s, %s -> Some Misc.Refl\n"
        object_.object_ctor_name
        object_.object_ctor_name)
    solver_objects;
  Buffer.add_string
    buf
    "    | _ -> None\n\
    \n\
    \  let compare_obj : type a b. a obj -> b obj -> (a, b) Misc.comparison =\n\
    \   fun obj1 obj2 ->\n\
    \    match equal_obj obj1 obj2 with\n\
    \    | Some Misc.Refl -> Misc.Equal\n\
    \    | None ->\n\
    \      if obj_index obj1 < obj_index obj2 then Misc.Less_than else Misc.Greater_than\n\
    \n\
    \  let print_obj : type a. Format_doc.formatter -> a obj -> unit =\n\
    \   fun ppf -> function\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "    | %s -> Format_doc.fprintf ppf %S\n"
        object_.object_ctor_name
        object_.name)
    solver_objects;
  Buffer.add_string buf "\nend\n\n"

let add_lattices_univ_ml buf model =
  let solver_objects = solver_objects_in_order model in
  Buffer.add_string
    buf
    "module Lattices_univ = struct\n\
    \  [@@@warning \"-32-37\"]\n\
    \  include Lattices_const\n\
    \  open Allowance\n\
    \  open Misc\n\
    \  module Fmt = Format_doc\n\
    \n\
    \  type ('a, 'b, 'd) morph =\n\
    \    | Id : ('a, 'a, 'l * 'r) morph\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s : (%s.t, %s.t, 'l * 'r) morph\n\
              \    | %s : (%s.t, %s.t, 'l * disallowed) morph\n\
              \    | %s : (%s.t, %s.t, disallowed * 'r) morph\n"
              axis.proj_ctor_name
              axis.carrier_object_name
              object_.name
              axis.min_with_ctor_name
              axis.carrier_object_name
              object_.name
              axis.max_with_ctor_name
              axis.carrier_object_name
              object_.name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "    | Compose :\n\
    \        ('b, 'c, 'l * 'r) morph * ('a, 'b, 'l * 'r) morph\n\
    \        -> ('a, 'c, 'l * 'r) morph\n\
    \    constraint 'd = _ * _\n\
    \  [@@ocaml.warning \"-62\"]\n\
    \n\
    \  include Magic_allow_disallow (struct\n\
    \    type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph constraint 'd = 'l * 'r\n\
    \n\
    \    let disallow_right : type a b l r.\n\
    \        (a, b, l * r) sided -> (a, b, l * disallowed) sided = Obj.magic\n\
    \    let disallow_left : type a b l r.\n\
    \        (a, b, l * r) sided -> (a, b, disallowed * r) sided = Obj.magic\n\
    \    let allow_right : type a b l r.\n\
    \        (a, b, l * allowed) sided -> (a, b, l * r) sided = Obj.magic\n\
    \    let allow_left : type a b l r.\n\
    \        (a, b, allowed * r) sided -> (a, b, l * r) sided = Obj.magic\n\
    \  end)\n\
    \n\
    \  let rec src : type a b l r. b obj -> (a, b, l * r) morph -> a obj =\n\
    \   fun dst -> function\n\
    \    | Id -> dst\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s -> %s\n\
              \    | %s -> %s\n\
              \    | %s -> %s\n"
              axis.proj_ctor_name
              (solver_object_ctor_name_of_name object_.name)
              axis.min_with_ctor_name
              (solver_object_ctor_name_of_name axis.carrier_object_name)
              axis.max_with_ctor_name
              (solver_object_ctor_name_of_name axis.carrier_object_name))
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "    | Compose (f, g) -> src (src dst f) g\n\
    \n\
    \  let id = Id\n\
    \n\
    \  let maybe_compose : type a b c l r.\n\
    \      c obj -> (b, c, l * r) morph -> (a, b, l * r) morph -> (a, c, l * r) morph =\n\
    \   fun _dst f g ->\n\
    \    match f, g with\n\
    \    | Id, g -> g\n\
    \    | f, Id -> f\n\
    \    | _ -> Compose (f, g)\n\
    \n\
    \  let compose = maybe_compose\n\
    \n\
    \  let rec left_adjoint : type a b l.\n\
    \      b obj -> (a, b, l * allowed) morph -> (b, a, left_only) morph =\n\
    \   fun dst -> function\n\
    \    | Id -> Id\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s -> %s\n\
              \    | %s -> %s\n"
              axis.proj_ctor_name
              axis.min_with_ctor_name
              axis.max_with_ctor_name
              axis.proj_ctor_name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "    | Compose (f, g) ->\n\
    \      Compose (left_adjoint (src dst f) g, left_adjoint dst f)\n\
    \n\
    \  let rec right_adjoint : type a b r.\n\
    \      b obj -> (a, b, allowed * r) morph -> (b, a, right_only) morph =\n\
    \   fun dst -> function\n\
    \    | Id -> Id\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s -> %s\n\
              \    | %s -> %s\n"
              axis.proj_ctor_name
              axis.max_with_ctor_name
              axis.min_with_ctor_name
              axis.proj_ctor_name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "    | Compose (f, g) ->\n\
    \      Compose (right_adjoint (src dst f) g, right_adjoint dst f)\n\
    \n\
    \  let rec apply : type a b l r. b obj -> (a, b, l * r) morph -> a -> b =\n\
    \   fun dst morph x ->\n\
    \    match morph with\n\
    \    | Id -> x\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s -> %s.Axis.proj %s.Axis.%s x\n\
              \    | %s -> %s.Axis.min_with %s.Axis.%s x\n\
              \    | %s -> %s.Axis.max_with %s.Axis.%s x\n"
              axis.proj_ctor_name
              object_.name
              object_.name
              axis.axis.ctor_name
              axis.min_with_ctor_name
              object_.name
              object_.name
              axis.axis.ctor_name
              axis.max_with_ctor_name
              object_.name
              object_.name
              axis.axis.ctor_name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "    | Compose (f, g) -> apply dst f (apply (src dst f) g x)\n\
    \n\
    \  let rec morph_key : type a b l r. (a, b, l * r) morph -> string = function\n\
    \    | Id -> \"Id\"\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s -> %S\n\
              \    | %s -> %S\n\
              \    | %s -> %S\n"
              axis.proj_ctor_name
              axis.proj_ctor_name
              axis.min_with_ctor_name
              axis.min_with_ctor_name
              axis.max_with_ctor_name
              axis.max_with_ctor_name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "    | Compose (f, g) -> \"Compose(\" ^ morph_key f ^ \",\" ^ morph_key g ^ \")\"\n\
    \n\
    \  let equal_morph : type a0 a1 b l0 r0 l1 r1.\n\
    \      b obj ->\n\
    \      (a0, b, l0 * r0) morph ->\n\
    \      (a1, b, l1 * r1) morph ->\n\
    \      (a0, a1) Misc.eq option =\n\
    \   fun _dst m1 m2 ->\n\
    \    if String.equal (morph_key m1) (morph_key m2)\n\
    \    then Some (Obj.magic Misc.Refl)\n\
    \    else None\n\
    \n\
    \  let compare_morph : type a0 a1 b l0 r0 l1 r1.\n\
    \      b obj ->\n\
    \      (a0, b, l0 * r0) morph ->\n\
    \      (a1, b, l1 * r1) morph ->\n\
    \      (a0, a1) Misc.comparison =\n\
    \   fun _dst m1 m2 ->\n\
    \    if String.equal (morph_key m1) (morph_key m2)\n\
    \    then Obj.magic Equal\n\
    \    else if String.compare (morph_key m1) (morph_key m2) < 0\n\
    \    then Less_than\n\
    \    else Greater_than\n\
    \n\
    \  let print_morph : type a b l r. b obj -> Fmt.formatter -> (a, b, l * r) morph -> unit =\n\
    \   fun _dst ppf morph -> Fmt.fprintf ppf \"%s\" (morph_key morph)\n\
    \n\
    end\n\
    \n\
    module Solver_support = Solver_runtime.Make (Lattices_univ)\n\n"

let add_const_op_functor_ml buf =
  Buffer.add_string
    buf
    "module Make_const_op (Base : sig\n\
    \  type t\n\
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
    \  module Repr : sig\n\
    \    val bits : int\n\
    \    val mask : int\n\
    \    val to_int : t -> int\n\
    \    val of_int_exn : int -> t\n\
    \  end\n\
    \  val legacy : t\n\
    \  end) = struct\n\
    \  type t = Base.t\n\
    \  let bottom = Base.top\n\
    \  let top = Base.bottom\n\
    \  let leq x y = Base.leq y x\n\
    \  let equal = Base.equal\n\
    \  let join = Base.meet\n\
    \  let meet = Base.join\n\
    \  let sub x y = Base.imply y x\n\
    \  let imply x y = Base.sub y x\n\
    \  let pp = Base.pp\n\
    \  let show = Base.show\n\
    \  module Repr = Base.Repr\n\
    \  let min = bottom\n\
    \  let max = top\n\
    \  let le = leq\n\
    \  let print = pp\n\
    \  let legacy = Base.legacy\n\
    end\n\n"

let add_solver_wrapper_functor_ml buf =
  Buffer.add_string
    buf
    "module Make_solver_core\n\
    \    (Const_desc : sig\n\
    \      type t\n\
    \      module Repr : sig\n\
    \        val to_int : t -> int\n\
    \      end\n\
    \      val legacy : t\n\
    \    end)\n\
    \    (Obj_desc : sig\n\
    \      val obj : int Solver_support.obj\n\
    \    end) =\n\
    struct\n\
    \  include Solver_support.Positive_gen (Obj_desc)\n\
    \n\
    \  let of_const_value c = of_const (Const_desc.Repr.to_int c)\n\
    \  let legacy = of_const_value Const_desc.legacy\n\
    \  let show ?verbose t = Format_doc.asprintf \"%a\" (print ?verbose) t\n\
    end\n\
    \n\
    module Make_solver_module\n\
    \    (Const_desc : sig\n\
    \      type t\n\
    \      val pp : Format.formatter -> t -> unit\n\
    \      module Repr : sig\n\
    \        val to_int : t -> int\n\
    \        val of_int_exn : int -> t\n\
    \      end\n\
    \      val legacy : t\n\
    \    end)\n\
    \    (Obj_desc : sig\n\
    \      val obj : int Solver_support.obj\n\
    \    end) =\n\
    struct\n\
    \  include Make_solver_core (struct\n\
    \    type t = Const_desc.t\n\
    \    module Repr = struct\n\
    \      let to_int = Const_desc.Repr.to_int\n\
    \    end\n\
    \    let legacy = Const_desc.legacy\n\
    \  end) (Obj_desc)\n\
    \n\
    \  type simple_error = {\n\
    \    left : Const_desc.t;\n\
    \    right : Const_desc.t;\n\
    \  }\n\
    \n\
    \  type error = simple_error\n\
    \n\
    \  type equate_step =\n\
    \    | Left_le_right\n\
    \    | Right_le_left\n\
    \n\
    \  type equate_error = equate_step * error\n\
    \n\
    \  let error_of_raw ({ left; right; _ } : int Solver_support.Raw.error_raw) =\n\
    \    {\n\
    \      left = Const_desc.Repr.of_int_exn left;\n\
    \      right = Const_desc.Repr.of_int_exn right;\n\
    \    }\n\
    \n\
    \  let to_simple_error error = error\n\
    \n\
    \  let print_error ppf { left; right } =\n\
    \    Format.fprintf ppf \"%a <= %a does not hold\" Const_desc.pp left Const_desc.pp right\n\
    \n\
    \  let print_equate_error ppf = function\n\
    \    | Left_le_right, error ->\n\
    \      Format.fprintf ppf \"Left_le_right: %a\" print_error error\n\
    \    | Right_le_left, error ->\n\
    \      Format.fprintf ppf \"Right_le_left: %a\" print_error error\n\
    \n\
    \  let submode a b = Result.map_error error_of_raw (submode_raw a b)\n\
    \n\
    \  let submode_exn a b =\n\
    \    match submode a b with\n\
    \    | Ok () -> ()\n\
    \    | Error error -> invalid_arg (Format.asprintf \"%a\" print_error error)\n\
    \n\
    \  let equate a b =\n\
    \    Result.map_error\n\
    \      (fun (forward, error) ->\n\
    \        ((if forward then Left_le_right else Right_le_left), error_of_raw error))\n\
    \      (equate_raw a b)\n\
    \n\
    \  let equate_exn a b =\n\
    \    match equate a b with\n\
    \    | Ok () -> ()\n\
    \    | Error error -> invalid_arg (Format.asprintf \"%a\" print_equate_error error)\n\
    end\n\n"

let add_base_ml buf (base : base) module_name _descriptor ~include_solver =
  bprintf buf "module %s = struct\n" module_name;
  bprintf buf "  module Const = Lattices_const.%s\n" module_name;
  Buffer.add_string buf "  type const = Const.t\n";
  if include_solver
  then (
    bprintf
      buf
      "\n\
      \  include Make_solver_module (Const) (struct\n\
      \    let obj = Lattices_univ.%s\n\
      \  end)\n"
      (solver_object_ctor_name_of_name module_name);
    Array.iteri
      (fun i (_ : string) ->
        bprintf
          buf
          "  let %s = of_const_value Const.%s\n"
          base.element_value_names.(i)
          base.element_value_names.(i))
      base.element_names);
  Buffer.add_string buf "end\n\n"

let add_product_ml
    buf
    (product : product)
    module_name
    _descriptor
    ~in_op
    ~include_solver
  =
  bprintf buf "module %s = struct\n" module_name;
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "  module %s = %s\n"
        (field_module_alias_name field)
        (field_module_name field ~in_op))
    product.fields;
  bprintf buf "  module Axis = Lattices_const.%s.Axis\n" module_name;
  bprintf buf "  module Const = Lattices_const.%s\n" module_name;
  Buffer.add_string buf "  type const = Const.t\n";
  if include_solver
  then (
    bprintf
      buf
      "\n\
      \  include Make_solver_core (Const) (struct\n\
      \    let obj = Lattices_univ.%s\n\
      \  end)\n\
      \n"
      (solver_object_ctor_name_of_name module_name);
    Buffer.add_string
      buf
      "  type 'a simple_axerror = {\n\
      \    left : 'a;\n\
      \    right : 'a;\n\
      \  }\n\
      \n\
      \  type simple_error = Error : 'a Axis.t * 'a simple_axerror -> simple_error\n\
      \n\
      \  type error = simple_error\n\
      \n\
      \  type equate_step =\n\
      \    | Left_le_right\n\
      \    | Right_le_left\n\
      \n\
      \  type equate_error = equate_step * error\n\
      \n\
      \  let error_of_raw ({ left; right; _ } : int Solver_support.Raw.error_raw) =\n\
      \    let left = Const.Repr.of_int_exn left in\n\
      \    let right = Const.Repr.of_int_exn right in\n\
      \    let left_view = Const.view left in\n\
      \    let right_view = Const.view right in\n";
    List.iteri
      (fun i (axis : axis_object) ->
        let field =
          List.find (fun (field : field) -> field.name = axis.name) product.fields
        in
        let field_module = field_module_alias_name field in
        bprintf
          buf
          "  %sif not (%s.Const.le left_view.%s right_view.%s) then\n\
          \    Error (Axis.%s, { left = left_view.%s; right = right_view.%s })\n"
          (if i = 0 then "" else "else ")
          field_module
          field.name
          field.name
          axis.ctor_name
          field.name
          field.name)
      product.axes;
    Buffer.add_string
      buf
      "  else\n\
      \    invalid_arg \"product error_of_raw: no failing axis\"\n\
      \n\
      \  let to_simple_error error = error\n\
      \n\
      \  let print_axerror pp ppf { left; right } =\n\
      \    Format.fprintf ppf \"%a <= %a does not hold\" pp left pp right\n\
      \n\
      \  let print_error : Format.formatter -> error -> unit =\n\
      \   fun ppf (Error (axis, error)) ->\n\
      \    match axis, error with\n";
    List.iter
      (fun (axis : axis_object) ->
        let field =
          List.find (fun (field : field) -> field.name = axis.name) product.fields
        in
        let field_module = field_module_alias_name field in
        bprintf
          buf
          "    | Axis.%s, error ->\n\
          \      Format.fprintf ppf \"%%a: %%a\"\n\
          \        Axis.print\n\
          \        Axis.%s\n\
          \        (print_axerror %s.Const.pp)\n\
          \        error\n"
          axis.ctor_name
          axis.ctor_name
          field_module)
      product.axes;
    Buffer.add_string
      buf
      "\n\
      \  let print_equate_error ppf = function\n\
      \    | Left_le_right, error ->\n\
      \      Format.fprintf ppf \"Left_le_right: %a\" print_error error\n\
      \    | Right_le_left, error ->\n\
      \      Format.fprintf ppf \"Right_le_left: %a\" print_error error\n\
      \n\
      \  let submode a b = Result.map_error error_of_raw (submode_raw a b)\n\
      \n\
      \  let submode_exn a b =\n\
      \    match submode a b with\n\
      \    | Ok () -> ()\n\
      \    | Error error -> invalid_arg (Format.asprintf \"%a\" print_error error)\n\
      \n\
      \  let equate a b =\n\
      \    Result.map_error\n\
      \      (fun (forward, error) ->\n\
      \        ((if forward then Left_le_right else Right_le_left), error_of_raw error))\n\
      \      (equate_raw a b)\n\
      \n\
      \  let equate_exn a b =\n\
      \    match equate a b with\n\
      \    | Ok () -> ()\n\
      \    | Error error -> invalid_arg (Format.asprintf \"%a\" print_equate_error error)\n\
      \n\
      \  type ('a, 'd) axis_mode = (int, 'd) Solver_support.Raw.mode constraint 'd = 'l * 'r\n\
      \n\
      \  let proj : type a l r. a Axis.t -> (l * r) t -> (a, l * r) axis_mode =\n\
      \   fun axis m ->\n\
      \    match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf
          buf
          "    | Axis.%s ->\n\
          \      %s\n"
          axis.ctor_name
          (Printf.sprintf
             "Solver_support.Raw.apply\n\
             \        Lattices_univ.%s\n\
             \        Lattices_univ.%s\n\
             \        m"
             (solver_object_ctor_name_of_name (axis_module_name axis ~in_op))
             (solver_proj_ctor_name module_name axis)))
      product.axes;
    Buffer.add_string
      buf
      "  let min_with : type a l r. a Axis.t -> (a, l * r) axis_mode -> (l * Allowance.disallowed) t =\n\
      \   fun axis m ->\n\
      \    match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf
          buf
          "    | Axis.%s ->\n\
          \      %s\n"
          axis.ctor_name
          (Printf.sprintf
             "Solver_support.Raw.apply\n\
             \        Lattices_univ.%s\n\
             \        Lattices_univ.%s\n\
             \        (Solver_support.Raw.disallow_right m)"
             (solver_object_ctor_name_of_name module_name)
             (solver_min_with_ctor_name module_name axis)))
      product.axes;
    Buffer.add_string
      buf
      "  let max_with : type a l r. a Axis.t -> (a, l * r) axis_mode -> (Allowance.disallowed * r) t =\n\
      \   fun axis m ->\n\
      \    match axis with\n";
    List.iter
      (fun (axis : axis_object) ->
        bprintf
          buf
          "    | Axis.%s ->\n\
          \      %s\n"
          axis.ctor_name
          (Printf.sprintf
             "Solver_support.Raw.apply\n\
             \        Lattices_univ.%s\n\
             \        Lattices_univ.%s\n\
             \        (Solver_support.Raw.disallow_left m)"
             (solver_object_ctor_name_of_name module_name)
             (solver_max_with_ctor_name module_name axis)))
      product.axes;
    List.iter
      (fun (field : field) ->
        let axis = axis_ctor_name field.name in
        bprintf
          buf
          "  let proj_%s m = Obj.magic (proj Axis.%s m)\n\
          \  let min_with_%s m = min_with Axis.%s (Obj.magic m)\n\
          \  let max_with_%s m = max_with Axis.%s (Obj.magic m)\n"
          field.name
          axis
          field.name
          axis
          field.name
          axis)
      product.fields;
    Buffer.add_string buf "\n");
  Buffer.add_string buf "end\n\n"

let find_base model name =
  match String_map.find name model.lattices with
  | Base base -> base
  | Product _ -> invalid_arg "expected a base lattice"

let morphism_of_slot = Emit_common.morphism_of_slot

let add_morphism_ml buf model name morphism =
  let domain = find_base model morphism.domain in
  let codomain = find_base model morphism.codomain in
  bprintf buf "  let %s = function\n" name;
  Array.iteri
    (fun i target ->
      bprintf
        buf
        "    | %d -> %s.Const.%s\n"
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

type exported_alias = Emit_common.exported_alias =
  { alias_name : string;
    module_name : string;
    morphism : morphism
  }

let collect_exported_aliases = Emit_common.collect_exported_aliases

let add_exported_aliases_sig buf aliases =
  List.iter
    (fun alias ->
      bprintf
        buf
        "val %s : %s.const -> %s.const\n"
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

let add_test_ml = Emit_test.add_test_ml

let render ?(config = Render_config.default) ~root_module model =
  let mli = Buffer.create 4096 in
  let ml = Buffer.create 8192 in
  let test_ml = Buffer.create 8192 in
  add_test_ml test_ml model ~root_module;
  add_const_op_functor_ml ml;
  add_lattices_const_ml ml model;
  if config.include_solver
  then (
    add_lattices_univ_ml ml model;
    add_solver_wrapper_functor_ml ml);
  List.iter
    (function
      | Lattice (Base base) ->
        add_base_sig
          mli
          base
          base.name
          ~include_solver:config.include_solver;
        add_base_sig
          mli
          base
          (Name.op_module_name base.name)
          ~include_solver:config.include_solver;
        add_base_ml
          ml
          base
          base.name
          base.descriptor
          ~include_solver:config.include_solver;
        add_base_ml
          ml
          base
          (Name.op_module_name base.name)
          base.op_descriptor
          ~include_solver:config.include_solver
      | Lattice (Product product) ->
        add_product_sig
          mli
          product
          product.name
          ~in_op:false
          ~include_solver:config.include_solver;
        add_product_sig
          mli
          product
          (Name.op_module_name product.name)
          ~in_op:true
          ~include_solver:config.include_solver;
        add_product_ml
          ml
          product
          product.name
          product.descriptor
          ~in_op:false
          ~include_solver:config.include_solver;
        add_product_ml
          ml
          product
          (Name.op_module_name product.name)
          product.op_descriptor
          ~in_op:true
          ~include_solver:config.include_solver
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
