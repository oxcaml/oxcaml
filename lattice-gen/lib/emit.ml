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
  let opposite = if in_op then not field.declared_opposite else field.declared_opposite in
  if opposite then Name.op_module_name field.lattice_name else field.lattice_name

let axis_ctor_name field_name = String.capitalize_ascii field_name

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

let solver_ops_module_name name = "Solver_obj_" ^ name

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

let chop_op_suffix name =
  let suffix = "_op" in
  let name_len = String.length name in
  let suffix_len = String.length suffix in
  if name_len >= suffix_len
     && String.sub name (name_len - suffix_len) suffix_len = suffix
  then Some (String.sub name 0 (name_len - suffix_len))
  else None

let underlying_lattice_of_solver_object model (object_ : solver_object) =
  match String_map.find_opt object_.name model.lattices with
  | Some lattice -> lattice
  | None -> (
    match chop_op_suffix object_.name with
    | Some name -> String_map.find name model.lattices
    | None -> invalid_arg ("unknown solver object: " ^ object_.name))

let add_solver_items_sig buf =
  Buffer.add_string
    buf
    "  type 'd t constraint 'd = 'l * 'r\n\
    \n\
    \  type l = (Allowance.allowed * Allowance.disallowed) t\n\
    \  type r = (Allowance.disallowed * Allowance.allowed) t\n\
    \  type lr = (Allowance.allowed * Allowance.allowed) t\n\
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
    \  val submode : l -> r -> (unit, string) result\n\
    \  val submode_exn : l -> r -> unit\n\
    \  val equate : lr -> lr -> (unit, string) result\n\
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
  add_solver_items_sig buf;
  Buffer.add_string
    buf
    "\n\
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

let add_solver_object_ops_ml buf model (object_ : solver_object) =
  let ops_module = solver_ops_module_name object_.name in
  bprintf buf "  module %s = struct\n" ops_module;
  Buffer.add_string buf "    type t = int\n";
  add_specialized_ops_ml ~indent:"    " buf object_.descriptor;
  (match underlying_lattice_of_solver_object model object_, object_.shape with
   | Base base, Solver_base ->
     Buffer.add_string buf "    let pp ppf = function\n";
     Array.iteri
       (fun i elt ->
         bprintf
           buf
           "      | %d -> Fmt.pp_print_string ppf %S\n"
           base.element_values.(i)
           elt)
       base.element_names;
     Buffer.add_string
       buf
       "      | _ -> invalid_arg \"invalid lattice element\"\n"
   | Product product, Solver_product axes ->
     Buffer.add_string buf "    let pp ppf t =\n";
     if product.fields = []
     then Buffer.add_string buf "      Fmt.pp_print_string ppf \"{}\"\n"
     else (
       Buffer.add_string buf "      Fmt.pp_print_string ppf \"{ \";\n";
       List.iteri
         (fun i (field : field) ->
           let axis =
             List.find
               (fun (axis : solver_axis) -> axis.axis.name = field.name)
               axes
           in
           let carrier_ops = solver_ops_module_name axis.carrier_object_name in
           if i > 0
           then Buffer.add_string buf "      Fmt.pp_print_string ppf \"; \";\n";
           bprintf
             buf
             "      Fmt.pp_print_string ppf %S;\n\
             \      %s.pp ppf (%s);\n"
             (field.name ^ " = ")
             carrier_ops
             (if field.shift = 0
              then Printf.sprintf "(t land %d)" field.raw_mask
              else
                Printf.sprintf "((t lsr %d) land %d)" field.shift field.raw_mask))
         product.fields;
       Buffer.add_string buf "      Fmt.pp_print_string ppf \" }\"\n")
   | Base _, Solver_product _ | Product _, Solver_base ->
     invalid_arg "solver object shape mismatch");
  Buffer.add_string buf "    let _ = sub, imply\n";
  Buffer.add_string buf "  end\n\n"

let add_solver_support_ml buf model =
  let solver_objects = solver_objects_in_order model in
  Buffer.add_string buf "module Solver_support_base = struct\n";
  Buffer.add_string buf "  open Allowance\n";
  Buffer.add_string buf "  open Misc\n";
  Buffer.add_string buf "  module Fmt = Format_doc\n\n";
  List.iter (add_solver_object_ops_ml buf model) solver_objects;
  Buffer.add_string buf "  type _ obj =\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "    | %s : int obj\n"
        object_.object_ctor_name)
    solver_objects;
  Buffer.add_string buf "\n";
  Buffer.add_string buf "  let obj_index : type a. a obj -> int = function\n";
  List.iteri
    (fun i (object_ : solver_object) ->
      bprintf buf "    | %s -> %d\n" object_.object_ctor_name i)
    solver_objects;
  Buffer.add_string buf "\n";
  Buffer.add_string buf "  type (_, _, _) morph =\n";
  Buffer.add_string buf "    | Id : ('a, 'a, 'l * 'r) morph\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "    | %s : (int, int, 'l * 'r) morph\n\
              \    | %s : (int, int, 'l * disallowed) morph\n\
              \    | %s : (int, int, disallowed * 'r) morph\n"
              axis.proj_ctor_name
              axis.min_with_ctor_name
              axis.max_with_ctor_name)
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
    \  module C = struct\n\
    \    type nonrec 'a obj = 'a obj\n\
    \    type nonrec ('a, 'b, 'd) morph = ('a, 'b, 'd) morph constraint 'd = 'l * 'r\n\
    \n\
    \    let rec src : type a b l r. b obj -> (a, b, l * r) morph -> a obj =\n\
    \     fun dst -> function\n\
    \      | Id -> dst\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "      | %s -> %s\n\
              \      | %s -> %s\n\
              \      | %s -> %s\n"
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
    "      | Compose (f, g) -> src (src dst f) g\n\
    \n\
    \    let id = Id\n\
    \n\
    \    let compose : type a b c l r.\n\
    \        c obj -> (b, c, l * r) morph -> (a, b, l * r) morph -> (a, c, l * r) morph =\n\
    \     fun _dst f g ->\n\
    \      match f, g with\n\
    \      | Id, g -> g\n\
    \      | f, Id -> f\n\
    \      | _ -> Compose (f, g)\n\
    \n\
    \    let rec left_adjoint : type a b l.\n\
    \        b obj -> (a, b, l * allowed) morph -> (b, a, left_only) morph =\n\
    \     fun dst -> function\n\
    \      | Id -> Id\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "      | %s -> %s\n\
              \      | %s -> %s\n"
              axis.proj_ctor_name
              axis.min_with_ctor_name
              axis.max_with_ctor_name
              axis.proj_ctor_name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "      | Compose (f, g) ->\n\
    \        Compose (left_adjoint (src dst f) g, left_adjoint dst f)\n\
    \n\
    \    let rec right_adjoint : type a b r.\n\
    \        b obj -> (a, b, allowed * r) morph -> (b, a, right_only) morph =\n\
    \     fun dst -> function\n\
    \      | Id -> Id\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "      | %s -> %s\n\
              \      | %s -> %s\n"
              axis.proj_ctor_name
              axis.max_with_ctor_name
              axis.min_with_ctor_name
              axis.proj_ctor_name)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "      | Compose (f, g) ->\n\
    \        Compose (right_adjoint (src dst f) g, right_adjoint dst f)\n\
    \n\
    \    include Magic_allow_disallow (struct\n\
    \      type ('a, 'b, 'd) sided = ('a, 'b, 'd) morph constraint 'd = 'l * 'r\n\
    \n\
    \      let disallow_right : type a b l r.\n\
    \          (a, b, l * r) sided -> (a, b, l * disallowed) sided = Obj.magic\n\
    \      let disallow_left : type a b l r.\n\
    \          (a, b, l * r) sided -> (a, b, disallowed * r) sided = Obj.magic\n\
    \      let allow_right : type a b l r.\n\
    \          (a, b, l * allowed) sided -> (a, b, l * r) sided = Obj.magic\n\
    \      let allow_left : type a b l r.\n\
    \          (a, b, allowed * r) sided -> (a, b, l * r) sided = Obj.magic\n\
    \    end)\n\
    \n\
    \    let rec apply : type a b l r. b obj -> (a, b, l * r) morph -> a -> b =\n\
    \     fun dst morph x ->\n\
    \      match morph with\n\
    \      | Id -> x\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        let bottom_expr =
          Printf.sprintf "%s.bottom" (solver_ops_module_name object_.name)
        in
        let top_expr =
          Printf.sprintf "%s.top" (solver_ops_module_name object_.name)
        in
        List.iter
          (fun (axis : solver_axis) ->
            let field = axis.axis in
            let placed =
              if field.shift = 0
              then "x"
              else Printf.sprintf "(x lsl %d)" field.shift
            in
            bprintf
              buf
              "      | %s -> %s\n\
              \      | %s -> ((%s land lnot %d) lor %s)\n\
              \      | %s -> ((%s land lnot %d) lor %s)\n"
              axis.proj_ctor_name
              (if field.shift = 0
               then Printf.sprintf "(x land %d)" field.raw_mask
               else
                 Printf.sprintf "((x lsr %d) land %d)" field.shift field.raw_mask)
              axis.min_with_ctor_name
              bottom_expr
              field.layout_mask
              placed
              axis.max_with_ctor_name
              top_expr
              field.layout_mask
              placed)
          axes)
    solver_objects;
  Buffer.add_string
    buf
    "      | Compose (f, g) -> apply dst f (apply (src dst f) g x)\n\
    \n\
    \    let rec morph_key : type a b l r. (a, b, l * r) morph -> string = function\n\
    \      | Id -> \"Id\"\n";
  List.iter
    (fun (object_ : solver_object) ->
      match object_.shape with
      | Solver_base -> ()
      | Solver_product axes ->
        List.iter
          (fun (axis : solver_axis) ->
            bprintf
              buf
              "      | %s -> %S\n\
              \      | %s -> %S\n\
              \      | %s -> %S\n"
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
    "      | Compose (f, g) -> \"Compose(\" ^ morph_key f ^ \",\" ^ morph_key g ^ \")\"\n\
    \n\
    \    let equal_morph : type a0 a1 b l0 r0 l1 r1.\n\
    \        b obj ->\n\
    \        (a0, b, l0 * r0) morph ->\n\
    \        (a1, b, l1 * r1) morph ->\n\
    \        (a0, a1) Misc.eq option =\n\
    \     fun _dst m1 m2 ->\n\
    \      if String.equal (morph_key m1) (morph_key m2)\n\
    \      then Some (Obj.magic Misc.Refl)\n\
    \      else None\n\
    \n\
    \    let compare_morph : type a0 a1 b l0 r0 l1 r1.\n\
    \        b obj ->\n\
    \        (a0, b, l0 * r0) morph ->\n\
    \        (a1, b, l1 * r1) morph ->\n\
    \        (a0, a1) Misc.comparison =\n\
    \     fun _dst m1 m2 ->\n\
    \      if String.equal (morph_key m1) (morph_key m2)\n\
    \      then Obj.magic Equal\n\
    \      else if String.compare (morph_key m1) (morph_key m2) < 0\n\
    \      then Less_than\n\
    \      else Greater_than\n\
    \n\
    \    let print_morph : type a b l r. b obj -> Fmt.formatter -> (a, b, l * r) morph -> unit =\n\
    \     fun _dst ppf morph -> Fmt.fprintf ppf \"%s\" (morph_key morph)\n\
    \n\
    \    let min : type a. a obj -> a = function\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.bottom\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string buf "\n    let max : type a. a obj -> a = function\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.top\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string buf "\n    let le : type a. a obj -> a -> a -> bool = fun obj x y ->\n";
  Buffer.add_string buf "      match obj with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.leq x y\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string
    buf
    "\n\
    \    let equal : type a. a obj -> a -> a -> bool = fun obj x y ->\n\
    \      match obj with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.equal x y\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string
    buf
    "\n\
    \    let join : type a. a obj -> a -> a -> a = fun obj x y ->\n\
    \      match obj with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.join x y\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string
    buf
    "\n\
    \    let meet : type a. a obj -> a -> a -> a = fun obj x y ->\n\
    \      match obj with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.meet x y\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string
    buf
    "\n\
    \    let print : type a. a obj -> Fmt.formatter -> a -> unit = fun obj ppf x ->\n\
    \      match obj with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> %s.pp ppf x\n"
        object_.object_ctor_name
        (solver_ops_module_name object_.name))
    solver_objects;
  Buffer.add_string
    buf
    "\n\
    \    let equal_obj : type a b. a obj -> b obj -> (a, b) Misc.eq option =\n\
    \     fun obj1 obj2 ->\n\
    \      match obj1, obj2 with\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s, %s -> Some Misc.Refl\n"
        object_.object_ctor_name
        object_.object_ctor_name)
    solver_objects;
  Buffer.add_string buf "      | _ -> None\n";
  Buffer.add_string
    buf
    "\n\
    \    let compare_obj : type a b. a obj -> b obj -> (a, b) Misc.comparison =\n\
    \     fun obj1 obj2 ->\n\
    \      match equal_obj obj1 obj2 with\n\
    \      | Some Misc.Refl -> Equal\n\
    \      | None ->\n\
    \        if obj_index obj1 < obj_index obj2 then Less_than else Greater_than\n\
    \n\
    \    let print_obj : type a. Fmt.formatter -> a obj -> unit =\n\
    \     fun ppf -> function\n";
  List.iter
    (fun (object_ : solver_object) ->
      bprintf
        buf
        "      | %s -> Fmt.fprintf ppf %S\n"
        object_.object_ctor_name
        object_.name)
    solver_objects;
  Buffer.add_string
    buf
    "  end\n\
    \n\
    end\n\
    \n\
    module Solver_support = struct\n\
    \  include Solver_support_base\n\
    \  include Solver_runtime.Make (Solver_support_base.C)\n\
    end\n\n"

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
    "module Make_solver_module\n\
    \    (Const_desc : sig\n\
    \      type t\n\
    \      module Repr : sig\n\
    \        val to_int : t -> int\n\
    \      end\n\
    \      val legacy : t\n\
    \    end)\n\
    \    (Obj_desc : sig\n\
    \      val obj : int Solver_support_base.obj\n\
    \    end) =\n\
    struct\n\
    \  include Solver_support.Positive_gen (Obj_desc)\n\
    \n\
    \  let of_const_value c = of_const (Const_desc.Repr.to_int c)\n\
    \  let legacy = of_const_value Const_desc.legacy\n\
    \  let show ?verbose t = Format_doc.asprintf \"%a\" (print ?verbose) t\n\
    end\n\n"

let add_base_ml buf (base : base) module_name descriptor ~include_solver =
  if module_name = base.name
  then (
    bprintf buf "module %s = struct\n" module_name;
    Buffer.add_string buf "  module Const = struct\n";
    Buffer.add_string buf "    type t = int\n";
    Array.iteri
      (fun i (_ : string) ->
        bprintf buf "    let %s = %d\n" base.element_value_names.(i) base.element_values.(i))
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
      \  end\n"
      (legacy_value_name base.name);
    Buffer.add_string buf "  type const = Const.t\n";
    if include_solver
    then (
      bprintf
        buf
        "\n\
        \  include Make_solver_module (Const) (struct\n\
        \    let obj = Solver_support_base.%s\n\
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
    Buffer.add_string buf "end\n\n")
  else (
    bprintf buf "module %s = struct\n" module_name;
    bprintf buf "  module Const = struct\n    include Make_const_op (%s.Const)\n" base.name;
    Array.iteri
      (fun i (_ : string) ->
        bprintf
          buf
          "    let %s = %s.Const.%s\n"
          base.element_value_names.(i)
          base.name
          base.element_value_names.(i))
      base.element_names;
    Buffer.add_string
      buf
      "    let name = "
      ;
    bprintf buf "%s.Const.name\n" base.name;
    bprintf buf "    let of_name = %s.Const.of_name\n" base.name;
    Buffer.add_string buf "  end\n";
    Buffer.add_string buf "  type const = Const.t\n";
    if include_solver
    then (
      bprintf
        buf
        "\n\
        \  include Make_solver_module (Const) (struct\n\
        \    let obj = Solver_support_base.%s\n\
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
    Buffer.add_string buf "end\n\n")

let add_product_ml
    buf
    (product : product)
    module_name
    descriptor
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
  Buffer.add_string buf "  module Axis = struct\n";
  Buffer.add_string buf "    type _ t =\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | %s : %s.Const.t t\n"
        axis.ctor_name
        (field_module_alias_name
           (List.find (fun (field : field) -> field.name = axis.name) product.fields)))
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
  Buffer.add_string buf "  module Const = struct\n";
  if in_op
  then bprintf buf "    include Make_const_op (%s.Const)\n" product.name
  else Buffer.add_string buf "    type t = int\n";
  Buffer.add_string buf "    type view = {\n";
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      %s : %s.Const.t;\n"
        field.name
        (field_module_alias_name field))
    product.fields;
  Buffer.add_string buf "    }\n\n";
  if in_op
  then bprintf buf "    module Layout = %s.Const.Layout\n" product.name
  else (
    Buffer.add_string buf "    module Layout = struct\n";
    List.iter
      (fun (field : field) ->
        bprintf buf "      let %s_shift = %d\n" field.name field.shift;
        bprintf buf "      let %s_mask = %d\n" field.name field.layout_mask)
      product.fields;
    Buffer.add_string buf "    end\n");
  Buffer.add_string buf "    let[@inline] make ";
  List.iter (fun (field : field) -> bprintf buf "~%s " field.name) product.fields;
  if in_op
  then (
    Buffer.add_string buf "=\n      ";
    bprintf buf "%s.Const.make " product.name;
    List.iter (fun (field : field) -> bprintf buf "~%s " field.name) product.fields;
    Buffer.add_string buf "\n")
  else (
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
    Buffer.add_string buf "\n");
  List.iter
    (fun (field : field) ->
      let field_module = field_const_module_name field in
      if in_op
      then
        bprintf
          buf
          "    let[@inline] %s = %s.Const.%s\n"
          field.name
          product.name
          field.name
      else
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
      if in_op
      then
        bprintf
          buf
          "    let[@inline] with_%s = %s.Const.with_%s\n"
          field.name
          product.name
          field.name
      else
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
  if not in_op then add_specialized_ops_ml ~indent:"    " buf descriptor;
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
  if in_op
  then (
    bprintf buf "    let pp = %s.Const.pp\n" product.name;
    bprintf buf "    let show = %s.Const.show\n" product.name)
  else (
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
            (field_const_module_name field)
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
      \n");
  Buffer.add_string buf "    let split = view\n    let merge = of_view\n\n";
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
    "    let[@inline] proj : type a. a Axis.t -> t -> a = fun axis t ->\n\
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
    "    let[@inline] min_with : type a. a Axis.t -> a -> t = fun axis value ->\n\
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
    "    let[@inline] max_with : type a. a Axis.t -> a -> t = fun axis value ->\n\
    \      match axis with\n";
  List.iter
    (fun (axis : axis_object) ->
      bprintf
        buf
        "      | Axis.%s -> %s_top value\n"
        axis.ctor_name
        axis.name)
    product.axes;
  Buffer.add_string buf "  end\n";
  Buffer.add_string buf "  type const = Const.t\n";
  if include_solver
  then (
    bprintf
      buf
      "\n\
      \  include Make_solver_module (Const) (struct\n\
      \    let obj = Solver_support_base.%s\n\
      \  end)\n\
      \n\
      \  type ('a, 'd) axis_mode = (int, 'd) Solver_support.Raw.mode constraint 'd = 'l * 'r\n\
      \n\
      \  let proj : type a l r. a Axis.t -> (l * r) t -> (a, l * r) axis_mode =\n\
      \   fun axis m ->\n\
      \    match axis with\n"
      (solver_object_ctor_name_of_name module_name);
    List.iter
      (fun (axis : axis_object) ->
        bprintf
          buf
          "    | Axis.%s ->\n\
          \      %s\n"
          axis.ctor_name
          (Printf.sprintf
             "Solver_support.Raw.apply\n\
             \        Solver_support_base.%s\n\
             \        Solver_support_base.%s\n\
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
             \        Solver_support_base.%s\n\
             \        Solver_support_base.%s\n\
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
             \        Solver_support_base.%s\n\
             \        Solver_support_base.%s\n\
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
  if config.include_solver
  then (
    add_solver_support_ml ml model;
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
