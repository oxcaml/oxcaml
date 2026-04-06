open Model
module Bitwise = Bitwise

let bprintf = Printf.bprintf

type outputs =
  { ml : string;
    mli : string
  }

let dual_mask (desc : descriptor) = desc.mask land lnot desc.nat_mask
let bits_literal = Bitwise.bits_literal

let add_indented_block buf indent text =
  String.split_on_char '\n' text
  |> List.iter (fun line -> bprintf buf "%s%s\n" indent line)

let add_inline_or_block buf indent head expr =
  let compact = Bitwise.render expr in
  let inline_line = Printf.sprintf "%s%s = %s" indent head compact in
  if String.length inline_line <= 80
  then bprintf buf "%s\n" inline_line
  else (
    bprintf buf "%s%s =\n" indent head;
    add_indented_block
      buf
      (indent ^ "  ")
      (Bitwise.render_pretty ~width:(max 32 (80 - String.length indent - 2)) expr))

let render_bitwise_function_ml ?(indent = "  ") ?(bindings = []) buf name args expr =
  let avoid = args @ List.map fst bindings in
  let cse_bindings, expr = Bitwise.cse ~avoid expr in
  let head = Printf.sprintf "let[@inline] %s %s" name (String.concat " " args) in
  if bindings = [] && cse_bindings = []
  then add_inline_or_block buf indent head expr
  else (
    bprintf buf "%s%s =\n" indent head;
    let binding_width = max 32 (80 - String.length indent - 4) in
    List.iter
      (fun (binding_name, binding_expr) ->
        let rendered = Bitwise.render_pretty ~width:binding_width binding_expr in
        match String.split_on_char '\n' rendered with
        | [ line ] ->
          bprintf
            buf
            "%s  let %s = %s in\n"
            indent
            binding_name
            line
        | _ ->
          bprintf buf "%s  let %s =\n" indent binding_name;
          add_indented_block buf (indent ^ "    ") rendered;
          bprintf buf "%s  in\n" indent)
      bindings;
    List.iter
      (fun (binding : Bitwise.binding) ->
        let rendered = Bitwise.render_pretty ~width:binding_width binding.expr in
        match String.split_on_char '\n' rendered with
        | [ line ] ->
          bprintf
            buf
            "%s  let %s = %s in\n"
            indent
            binding.name
            line
        | _ ->
          bprintf buf "%s  let %s =\n" indent binding.name;
          add_indented_block buf (indent ^ "    ") rendered;
          bprintf buf "%s  in\n" indent)
      cse_bindings;
    add_indented_block
      buf
      (indent ^ "  ")
      (Bitwise.render_pretty ~width:(max 32 (80 - String.length indent - 2)) expr))

let build_schedule_expr expr rounds direction =
  List.fold_left
    (fun acc (round : round) ->
      let shifted =
        match direction with
        | `Down -> Bitwise.shift_r (Bitwise.mask acc round.mask) round.shift
        | `Up -> Bitwise.shift_l (Bitwise.mask acc round.mask) round.shift
      in
      Bitwise.or_ [ acc; shifted ])
    expr
    rounds

let with_named_expr ~mask ~name ~expr ~needed =
  if needed then [ (name, expr) ], Bitwise.var ~mask name else [], expr

let close_down rounds expr =
  if rounds = [] then expr else build_schedule_expr expr rounds `Down

let close_up_complement ~mask rounds expr =
  if rounds = []
  then Bitwise.xor_mask expr mask
  else Bitwise.xor_mask (build_schedule_expr expr rounds `Up) mask

let add_specialized_ops_ml ?(indent = "  ") buf desc =
  let nat_mask = desc.nat_mask in
  let dual_mask = dual_mask desc in
  let x = Bitwise.var ~mask:desc.mask "x" in
  let y = Bitwise.var ~mask:desc.mask "y" in
  let x_nat = Bitwise.mask x nat_mask in
  let y_nat = Bitwise.mask y nat_mask in
  let x_dual = Bitwise.mask x dual_mask in
  let y_dual = Bitwise.mask y dual_mask in
  let use_shared_diff = nat_mask <> 0 && dual_mask <> 0 in
  let diff_expr = Bitwise.xor_ x y in
  let diff_bindings, diff =
    with_named_expr ~mask:desc.mask ~name:"d" ~expr:diff_expr ~needed:use_shared_diff
  in
  let zxy_nat_expr =
    if use_shared_diff
    then Bitwise.and_ [ Bitwise.mask diff nat_mask; x_nat ]
    else Bitwise.and_ [ x_nat; Bitwise.xor_mask y_nat nat_mask ]
  in
  let zyx_dual_expr =
    if use_shared_diff
    then Bitwise.and_ [ Bitwise.mask diff dual_mask; y_dual ]
    else Bitwise.and_ [ y_dual; Bitwise.xor_mask x_dual dual_mask ]
  in
  bprintf buf "%slet min = %s\n" indent (bits_literal dual_mask);
  bprintf buf "%slet max = %s\n" indent (bits_literal nat_mask);
  if nat_mask = desc.mask
  then bprintf buf "%slet[@inline] le x y = (x land y) = x\n" indent
  else if nat_mask = 0
  then bprintf buf "%slet[@inline] le x y = (x land y) = y\n" indent
  else
    bprintf
      buf
      "%slet[@inline] le x y = ((x lxor y) land ((x land %s) lor (y land %s))) = 0\n"
      indent
      (bits_literal nat_mask)
      (bits_literal dual_mask);
  bprintf buf "%slet[@inline] equal (x : t) (y : t) = x = y\n" indent;
  render_bitwise_function_ml
    ~indent
    buf
    "join"
    [ "x"; "y" ]
    (Bitwise.or_
       [ Bitwise.or_ [ x_nat; y_nat ];
         Bitwise.and_ [ x_dual; y_dual ]
       ]);
  render_bitwise_function_ml
    ~indent
    buf
    "meet"
    [ "x"; "y" ]
    (Bitwise.or_
       [ Bitwise.and_ [ x_nat; y_nat ];
         Bitwise.or_ [ x_dual; y_dual ]
       ]);
  let subtract_zxy_bindings, subtract_zxy =
    with_named_expr
      ~mask:nat_mask
      ~name:"zxy"
      ~expr:zxy_nat_expr
      ~needed:(nat_mask <> 0 && desc.down_nat <> [])
  in
  let subtract_zyx_bindings, subtract_zyx =
    with_named_expr
      ~mask:dual_mask
      ~name:"zyx"
      ~expr:zyx_dual_expr
      ~needed:(dual_mask <> 0 && desc.up_dual <> [])
  in
  let nat_sub = close_down desc.down_nat subtract_zxy in
  let dual_sub =
    close_up_complement ~mask:dual_mask desc.up_dual subtract_zyx
  in
  render_bitwise_function_ml
    ~indent
    ~bindings:(diff_bindings @ subtract_zxy_bindings @ subtract_zyx_bindings)
    buf
    "subtract"
    [ "x"; "y" ]
    (Bitwise.or_ [ nat_sub; dual_sub ]);
  let imply_zxy_bindings, imply_zxy =
    with_named_expr
      ~mask:nat_mask
      ~name:"zxy"
      ~expr:zxy_nat_expr
      ~needed:(nat_mask <> 0 && desc.up_nat <> [])
  in
  let imply_zyx_bindings, imply_zyx =
    with_named_expr
      ~mask:dual_mask
      ~name:"zyx"
      ~expr:zyx_dual_expr
      ~needed:(dual_mask <> 0 && desc.down_dual <> [])
  in
  let nat_imply = close_up_complement ~mask:nat_mask desc.up_nat imply_zxy in
  let dual_imply = close_down desc.down_dual imply_zyx in
  render_bitwise_function_ml
    ~indent
    ~bindings:(diff_bindings @ imply_zxy_bindings @ imply_zyx_bindings)
    buf
    "imply"
    [ "x"; "y" ]
    (Bitwise.or_ [ nat_imply; dual_imply ])

let field_module_name (field : field) ~in_op =
  let opposite = if in_op then not field.ty.opposite else field.ty.opposite in
  if opposite then Name.op_module_name field.ty.name else field.ty.name

let emitted_field_name (field : field) = Name.escape_value_name field.name

let proj_name (field : field) = "proj_" ^ emitted_field_name field

let with_name (field : field) = "with_" ^ emitted_field_name field

let emitted_morph_name name = Name.escape_value_name name

let view_ctor_name name = String.capitalize_ascii (Name.snake_case name)

let lattice_module_type_text =
  "module type Lattice = sig\n\
  \  type t\n\
  \  type view\n\
  \n\
  \  val view : t -> view\n\
  \  val of_view : view -> t\n\
  \n\
  \  val le : t -> t -> bool\n\
  \  val equal : t -> t -> bool\n\
  \  val min : t\n\
  \  val max : t\n\
  \  val join : t -> t -> t\n\
  \  val meet : t -> t -> t\n\
  \  val subtract : t -> t -> t\n\
  \  val imply : t -> t -> t\n\
  \n\
  \  val print : Format.formatter -> t -> unit\n\
  \n\
  \  module Repr : sig\n\
  \    val to_int_unsafe : t -> int\n\
  \    val from_int_unsafe : int -> t\n\
  \  end\n\
  end\n\n"

let add_module_type_s buf = Buffer.add_string buf lattice_module_type_text

let add_module_type_s_ml buf = Buffer.add_string buf lattice_module_type_text

let add_common_prelude_ml buf =
  Buffer.add_string
    buf
    "let[@cold] raise_unknown_lattice_element () =\n\
\  invalid_arg \"unknown lattice element\"\n\n"

let add_common_ml_items buf desc =
  Buffer.add_string buf "  type t = int\n\n";
  add_specialized_ops_ml buf desc;
  Buffer.add_string buf "\n  module Repr = struct\n";
  Buffer.add_string buf "    let[@inline] to_int_unsafe x = x\n";
  Buffer.add_string buf "    let[@inline] from_int_unsafe x = x\n";
  Buffer.add_string buf "  end\n\n"

let cover_edges (finite : finite_lattice) =
  let n = Array.length finite.element_names in
  let edges = ref [] in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      if i <> j && finite.leq.(i).(j)
      then (
        let covered = ref false in
        for k = 0 to n - 1 do
          if k <> i && k <> j && finite.leq.(i).(k) && finite.leq.(k).(j)
          then covered := true
        done;
        if not !covered then edges := (i, j) :: !edges)
    done
  done;
  List.rev !edges

let maximal_chains (finite : finite_lattice) =
  let covers = cover_edges finite in
  let succs = Hashtbl.create 8 in
  List.iter
    (fun (src, dst) ->
      let prev = Option.value (Hashtbl.find_opt succs src) ~default:[] in
      Hashtbl.replace succs src (dst :: prev))
    covers;
  let rec dfs node =
    if node = finite.top
    then [ [ node ] ]
    else
      Hashtbl.find_opt succs node
      |> Option.value ~default:[]
      |> List.sort_uniq Int.compare
      |> List.concat_map (fun next ->
           List.map (fun chain -> node :: chain) (dfs next))
  in
  dfs finite.bottom

let add_base_order_comment buf (base : base) finite =
  let chains =
    maximal_chains finite
    |> List.map (fun chain ->
         String.concat
           " < "
           (List.map (fun index -> base.element_value_names.(index)) chain))
  in
  Buffer.add_string buf "  (* Order:\n";
  List.iter (fun chain -> bprintf buf "     %s\n" chain) chains;
  Buffer.add_string buf "  *)\n"

let add_base_sig buf (base : base) module_name finite =
  bprintf buf "module %s : sig\n" module_name;
  add_base_order_comment buf base finite;
  Buffer.add_char buf '\n';
  Buffer.add_string buf "  type view =\n";
  Array.iter
    (fun name ->
      bprintf buf "    | %s\n" (view_ctor_name name))
    base.element_names;
  Buffer.add_char buf '\n';
  Buffer.add_string
    buf
    "  include Lattice with type view := view\n\n";
  Array.iteri
    (fun i value_name ->
      bprintf buf "  val %s : t\n" value_name;
      if i = Array.length base.element_value_names - 1 then ())
    base.element_value_names;
  Buffer.add_string buf "end\n\n"

let add_base_ml buf (base : base) module_name desc =
  bprintf buf "module %s = struct\n" module_name;
  Buffer.add_string buf "  type view =\n";
  Array.iter
    (fun name ->
      bprintf buf "    | %s\n" (view_ctor_name name))
    base.element_names;
  Buffer.add_char buf '\n';
  Array.iteri
    (fun i value_name ->
      bprintf buf "  let %s = %d\n" value_name base.element_values.(i))
    base.element_value_names;
  if Array.length base.element_value_names > 0 then Buffer.add_string buf "\n";
  add_common_ml_items buf desc;
  Buffer.add_string buf "  let[@inline] view = function\n";
  Array.iteri
    (fun i name ->
      let ctor = view_ctor_name name in
      bprintf buf "    | %d -> %s\n" base.element_values.(i) ctor)
    base.element_names;
  Buffer.add_string buf "    | _ -> raise_unknown_lattice_element ()\n\n";
  Buffer.add_string buf "  let[@inline] of_view = function\n";
  Array.iteri
    (fun i name ->
      bprintf
        buf
        "    | %s -> %s\n"
        (view_ctor_name name)
        base.element_value_names.(i))
    base.element_names;
  Buffer.add_string buf "\n  let print ppf x =\n";
  Buffer.add_string buf "    let s =\n";
  Buffer.add_string buf "      match x with\n";
  Array.iteri
    (fun i _ ->
      let display_name = base.element_value_names.(i) in
      bprintf
        buf
        "      | %d -> %S\n"
        base.element_values.(i)
        display_name)
    base.element_value_names;
  Buffer.add_string buf "      | _ -> raise_unknown_lattice_element ()\n";
  Buffer.add_string buf "    in\n";
  Buffer.add_string buf "    Format.pp_print_string ppf s\n";
  Buffer.add_string buf "end\n\n"

let add_product_sig buf (product : product) module_name ~in_op =
  bprintf buf "module %s : sig\n" module_name;
  Buffer.add_string buf "  type view = {\n";
  List.iter
    (fun (field : field) ->
      let field_name = emitted_field_name field in
      bprintf
        buf
        "    %s : %s.t;\n"
        field_name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  }\n\n";
  Buffer.add_string
    buf
    "  include Lattice with type view := view\n\n";
  Buffer.add_string buf "  val make :\n";
  List.iter
    (fun (field : field) ->
      let field_name = emitted_field_name field in
      bprintf
        buf
        "    %s:%s.t ->\n"
        field_name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "    t\n\n";
  let last_index = List.length product.fields - 1 in
  List.iteri
    (fun i (field : field) ->
      let field_mod = field_module_name field ~in_op in
      bprintf buf "  val %s : t -> %s.t\n" (proj_name field) field_mod;
      bprintf buf "  val %s : %s.t -> t -> t\n" (with_name field) field_mod;
      if i <> last_index then Buffer.add_char buf '\n')
    product.fields;
  Buffer.add_string buf "end\n\n"

let add_product_ml buf (product : product) module_name (desc : descriptor) ~in_op =
  let t_expr = Bitwise.var ~mask:desc.mask "t" in
  let make_field_expr (field : field) =
    Bitwise.shift_l
      (Bitwise.var ~mask:field.raw_mask (emitted_field_name field))
      field.shift
  in
  let view_field_expr (field : field) =
    Bitwise.mask (Bitwise.shift_r t_expr field.shift) field.raw_mask
  in
  let with_field_expr (field : field) =
    Bitwise.or_
      [ Bitwise.and_
          [ t_expr;
            Bitwise.const (desc.mask land lnot field.layout_mask)
          ];
        Bitwise.shift_l (Bitwise.var ~mask:field.raw_mask "x") field.shift
      ]
  in
  bprintf buf "module %s = struct\n" module_name;
  add_common_ml_items buf desc;
  Buffer.add_string buf "  type view = {\n";
  List.iter
    (fun (field : field) ->
      let field_name = emitted_field_name field in
      bprintf
        buf
        "    %s : %s.t;\n"
        field_name
        (field_module_name field ~in_op))
    product.fields;
  Buffer.add_string buf "  }\n\n";
  Buffer.add_string buf "  let[@inline] make\n";
  List.iter
    (fun (field : field) ->
      bprintf buf "      ~%s\n" (emitted_field_name field))
    product.fields;
  Buffer.add_string buf "    =\n";
  Buffer.add_string buf "    ";
  Buffer.add_string
    buf
    (Bitwise.render (Bitwise.or_ (List.map make_field_expr product.fields)));
  Buffer.add_string buf "\n\n";
  Buffer.add_string buf "  let[@inline] of_view view =\n";
  Buffer.add_string buf "    make\n";
  List.iter
    (fun (field : field) ->
      let field_name = emitted_field_name field in
      bprintf buf "      ~%s:view.%s\n" field_name field_name)
    product.fields;
  Buffer.add_string buf "\n";
  Buffer.add_string buf "  let[@inline] view t =\n";
  Buffer.add_string buf "    {\n";
  List.iter
    (fun (field : field) ->
      let field_name = emitted_field_name field in
      bprintf
        buf
        "      %s = %s;\n"
        field_name
        (Bitwise.render (view_field_expr field)))
    product.fields;
  Buffer.add_string buf "    }\n\n";
  List.iter
    (fun (field : field) ->
      add_inline_or_block
        buf
        "  "
        (Printf.sprintf "let[@inline] %s t" (proj_name field))
        (view_field_expr field);
      add_inline_or_block
        buf
        "  "
        (Printf.sprintf "let[@inline] %s x t" (with_name field))
        (with_field_expr field);
      Buffer.add_char buf '\n')
    product.fields;
  Buffer.add_string buf "  let print ppf x =\n";
  Buffer.add_string buf "    let view = view x in\n";
  Buffer.add_string buf "    Format.fprintf\n";
  Buffer.add_string buf "      ppf\n";
  bprintf
    buf
    "      %S\n"
    (String.concat "," (List.map (fun _ -> "%a") product.fields));
  List.iter
    (fun (field : field) ->
      bprintf
        buf
        "      %s.print view.%s\n"
        (field_module_name field ~in_op)
        (emitted_field_name field))
    product.fields;
  Buffer.add_string buf "end\n\n"

let add_primitive_morph_sig buf (primitive : primitive_morph) =
  let core = primitive.core in
  bprintf
    buf
    "  val %s : %s.t -> %s.t\n"
    (emitted_morph_name core.name)
    (module_name_of_expr core.source)
    (module_name_of_expr core.target)

let lattice_expr_module_sig_and_ml buf_mli buf_ml lattice expr =
  match lattice, expr.opposite with
  | Base base, false ->
    add_base_sig buf_mli base base.name base.logical;
    add_base_ml buf_ml base base.name base.descriptor
  | Base base, true ->
    add_base_sig
      buf_mli
      base
      (Name.op_module_name base.name)
      (lattice_reverse base.logical);
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

let base_of_expr model (expr : lattice_expr) =
  match String_map.find expr.name model.lattices with
  | Base base -> base
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

let bitwise_of_term ~source_mask (term : bit_term) =
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
  let x = Bitwise.var ~mask:source_mask "x" in
  let atom =
    match term.negated, term.mask = source_mask with
    | false, true -> x
    | true, true -> Bitwise.xor_mask x source_mask
    | false, false when mask_is_redundant_for_right_shift -> x
    | false, false -> Bitwise.mask x term.mask
    | true, _ -> Bitwise.mask (Bitwise.xor_mask x source_mask) term.mask
  in
  match term.shift with
  | 0 -> atom
  | shift when shift > 0 -> Bitwise.shift_l atom shift
  | shift -> Bitwise.shift_r atom (-shift)

let bitwise_of_kernel ~source_mask (kernel : bit_kernel) =
  Bitwise.or_
    ((if kernel.const <> 0 then [ Bitwise.const kernel.const ] else [])
     @ List.map (bitwise_of_term ~source_mask) kernel.terms)

let add_kernel_ml ?(indent = "") buf model name source_expr kernel =
  let source_mask = mask_of_expr model source_expr in
  render_bitwise_function_ml
    ~indent
    buf
    (emitted_morph_name name)
    [ "x" ]
    (bitwise_of_kernel ~source_mask kernel)

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

let rec morph_expr_apply_ml arg = function
  | Morph_name morph_name ->
    Printf.sprintf "%s (%s)" (emitted_morph_name morph_name) arg
  | Compose (left, right) ->
    morph_expr_apply_ml (morph_expr_apply_ml arg right) left

let rec add_bridge_expr_ml buf src_module target_ty = function
  | Source_field source_field ->
    bprintf
      buf
      "%s.proj_%s x"
      src_module
      (Name.escape_value_name source_field)
  | Morph_apply { morph; source_field } ->
    bprintf
      buf
      "%s"
      (morph_expr_apply_ml
         (Printf.sprintf
            "%s.proj_%s x"
            src_module
            (Name.escape_value_name source_field))
         morph)
  | Min -> bprintf buf "%s.min" (module_name_of_expr target_ty)
  | Max -> bprintf buf "%s.max" (module_name_of_expr target_ty)
  | Join (left, right) ->
    bprintf buf "%s.join (" (module_name_of_expr target_ty);
    add_bridge_expr_ml buf src_module target_ty left;
    bprintf buf ") (";
    add_bridge_expr_ml buf src_module target_ty right;
    Buffer.add_char buf ')'
  | Meet (left, right) ->
    bprintf buf "%s.meet (" (module_name_of_expr target_ty);
    add_bridge_expr_ml buf src_module target_ty left;
    bprintf buf ") (";
    add_bridge_expr_ml buf src_module target_ty right;
    Buffer.add_char buf ')'

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
  | Morph_apply { morph = Morph_name morph_name; source_field } ->
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
  | Morph_apply { morph = Compose _; _ } ->
    None
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
  | Join _ | Meet _ -> None

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
          | Composed _ -> None
        in
        Hashtbl.remove visiting morph_name;
        Option.iter (fun kernel -> Hashtbl.add kernels morph_name kernel) result;
        result)
  in
  String_map.iter (fun name _ -> ignore (resolve name)) model.morphs;
  kernels

let add_primitive_morph_ml ?(indent = "") buf model kernels (primitive : primitive_morph) =
  let core = primitive.core in
  match Hashtbl.find_opt kernels core.name with
  | Some kernel -> add_kernel_ml ~indent buf model core.name core.source kernel
  | None ->
    let source_base = base_of_expr model core.source in
    let dst_module = module_name_of_expr core.target in
    bprintf buf "%slet %s x =\n" indent (emitted_morph_name core.name);
    bprintf buf "%s  match x with\n" indent;
    Array.iteri
      (fun i target ->
        let dst_name = source_element_name core.target.name target model in
        bprintf
          buf
          "%s  | %d -> %s.%s\n"
          indent
          source_base.element_values.(i)
          dst_module
          dst_name)
      core.map;
    bprintf buf "%s  | _ -> invalid_arg %S\n" indent (emitted_morph_name core.name)

let add_bridge_ml ?(indent = "") buf model kernels (bridge : product_bridge) =
  let core = bridge.core in
  match Hashtbl.find_opt kernels core.name with
  | Some kernel -> add_kernel_ml ~indent buf model core.name core.source kernel
  | None ->
    let src_module = module_name_of_expr core.source in
    let dst_module = module_name_of_expr core.target in
    bprintf buf "%slet %s x =\n" indent (emitted_morph_name core.name);
    bprintf buf "%s  %s.make\n" indent dst_module;
    List.iter
      (fun (assignment : bridge_assignment) ->
        let target_ty =
          let target_product =
            match String_map.find core.target.name model.lattices with
            | Product product -> product
            | Base _ -> invalid_arg "expected bridge target product"
          in
          effective_field_type
            core.target
            (find_field target_product assignment.target_field)
        in
        bprintf
          buf
          "%s    ~%s:("
          indent
          (Name.escape_value_name assignment.target_field);
        add_bridge_expr_ml buf src_module target_ty assignment.expr;
        Buffer.add_string buf ")\n")
      bridge.assignments

let add_composed_ml ?(indent = "") buf (composed : composed_morph) =
  let core = composed.core in
  bprintf
    buf
    "%slet %s x = %s\n"
    indent
    (emitted_morph_name core.name)
    (morph_expr_apply_ml "x" composed.expr)

let add_morph_sig buf = function
  | Primitive primitive -> add_primitive_morph_sig buf primitive
  | Bridge bridge ->
    let core = bridge.core in
    bprintf
      buf
      "  val %s : %s.t -> %s.t\n"
      (emitted_morph_name core.name)
      (module_name_of_expr core.source)
      (module_name_of_expr core.target)
  | Composed composed ->
    let core = composed.core in
    bprintf
      buf
      "  val %s : %s.t -> %s.t\n"
      (emitted_morph_name core.name)
      (module_name_of_expr core.source)
      (module_name_of_expr core.target)

let rec morph_names_of_morph_expr = function
  | Morph_name morph_name -> [ morph_name ]
  | Compose (left, right) ->
    morph_names_of_morph_expr left @ morph_names_of_morph_expr right

let rec morph_names_of_bridge_expr = function
  | Morph_apply { morph; _ } -> morph_names_of_morph_expr morph
  | Source_field _ | Min | Max -> []
  | Join (left, right) | Meet (left, right) ->
    morph_names_of_bridge_expr left @ morph_names_of_bridge_expr right

let morph_dependencies = function
  | Primitive _ -> []
  | Bridge bridge ->
    List.concat_map
      (fun (assignment : bridge_assignment) ->
        morph_names_of_bridge_expr assignment.expr)
      bridge.assignments
  | Composed composed -> morph_names_of_morph_expr composed.expr

let ordered_morphs model =
  let decl_order =
    List.filter_map
      (function Model.Morph morph -> Some morph | Lattice _ -> None)
      model.items
  in
  let by_name =
    List.to_seq decl_order
    |> Seq.map (fun morph -> (morph_core_of morph).name, morph)
    |> String_map.of_seq
  in
  let visiting = Hashtbl.create 16 in
  let done_ = Hashtbl.create 16 in
  let acc = ref [] in
  let rec visit name =
    if not (Hashtbl.mem done_ name)
    then (
      if Hashtbl.mem visiting name then invalid_arg ("cyclic morph definition involving " ^ name);
      Hashtbl.add visiting name ();
      let morph = String_map.find name by_name in
      List.iter visit (morph_dependencies morph);
      Hashtbl.remove visiting name;
      Hashtbl.add done_ name ();
      acc := morph :: !acc)
  in
  List.iter (fun morph -> visit (morph_core_of morph).name) decl_order;
  List.rev !acc

let split_morphs morphs =
  List.partition
    (function Primitive _ -> true | Bridge _ | Composed _ -> false)
    morphs

let add_module_section_comment buf title =
  bprintf buf "(* %s *)\n\n" title

let add_morphs_sig buf model =
  let morphs =
    List.filter_map
      (function Model.Morph morph -> Some morph | Lattice _ -> None)
      model.items
  in
  if morphs <> []
  then (
    let primitive_morphs, derived_morphs = split_morphs morphs in
    Buffer.add_string buf "module Morphs : sig\n";
    if primitive_morphs <> []
    then (
      Buffer.add_string buf "  (* Primitive morphs *)\n";
      List.iter (add_morph_sig buf) primitive_morphs;
      if derived_morphs <> [] then Buffer.add_char buf '\n');
    if derived_morphs <> []
    then (
      Buffer.add_string buf "  (* Derived morphs *)\n";
      List.iter (add_morph_sig buf) derived_morphs);
    Buffer.add_string buf "end\n\n")

let add_morphs_ml buf model =
  let morphs = ordered_morphs model in
  let kernels = build_morph_kernels model in
  if morphs <> []
  then (
    let primitive_morphs, derived_morphs = split_morphs morphs in
    Buffer.add_string buf "module Morphs = struct\n";
    if primitive_morphs <> []
    then (
      Buffer.add_string buf "  (* Primitive morphs *)\n";
      List.iter
        (fun primitive ->
          add_primitive_morph_ml ~indent:"  " buf model kernels primitive;
          Buffer.add_char buf '\n')
        (List.map
           (function
             | Primitive primitive -> primitive
             | Bridge _ | Composed _ -> invalid_arg "impossible")
           primitive_morphs);
      if derived_morphs <> [] then Buffer.add_char buf '\n');
    if derived_morphs <> []
    then (
      Buffer.add_string buf "  (* Derived morphs *)\n";
      List.iter
        (fun morph ->
          (match morph with
           | Bridge bridge -> add_bridge_ml ~indent:"  " buf model kernels bridge
           | Composed composed -> add_composed_ml ~indent:"  " buf composed
           | Primitive _ -> invalid_arg "impossible");
          Buffer.add_char buf '\n')
        derived_morphs);
    Buffer.add_string buf "end\n\n")

let section_name_for_expr model (expr : lattice_expr) =
  if expr.opposite
  then "Opposite Lattices"
  else
    match String_map.find expr.name model.lattices with
    | Base _ -> "Base Lattices"
    | Product _ -> "Product Lattices"

let render ~root_module:_ model =
  let ml = Buffer.create 16384 in
  let mli = Buffer.create 8192 in
  add_module_type_s_ml ml;
  add_common_prelude_ml ml;
  add_module_type_s mli;
  let current_section = ref None in
  List.iter
    (fun (expr : lattice_expr) ->
      let section = section_name_for_expr model expr in
      if !current_section <> Some section
      then (
        add_module_section_comment mli section;
        add_module_section_comment ml section;
        current_section := Some section);
      let lattice = String_map.find expr.name model.lattices in
      lattice_expr_module_sig_and_ml mli ml lattice expr)
    (emitted_module_exprs model);
  add_morphs_sig mli model;
  add_morphs_ml ml model;
  { ml = Buffer.contents ml; mli = Buffer.contents mli }
