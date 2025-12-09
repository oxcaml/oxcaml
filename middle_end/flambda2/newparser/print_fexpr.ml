open! Fexpr

let pp_list ~sep f ppf =
  Format.pp_print_list f ~pp_sep:(fun ppf () -> Format.fprintf ppf sep) ppf

let pp_star_list f = pp_list ~sep:" *@ " f

let pp_comma_list f = pp_list ~sep:",@ " f

let pp_semi_list f = pp_list ~sep:";@ " f

let pp_pipe_list f = pp_list ~sep:"@ |" f

let empty_fmt : (unit, Format.formatter, unit) format = ""

let space_fmt : (unit, Format.formatter, unit) format = "@ "

let pp_with ?(prefix = empty_fmt) ?(suffix = empty_fmt) ppf =
  Format.kdprintf (fun pp ->
      Format.fprintf ppf prefix;
      pp ppf;
      Format.fprintf ppf suffix)

let pp_like fmt f ppf = Format.fprintf ppf fmt f

type spacing =
  | Before
  | After
  | Neither

let pp_spaced ~space ppf =
  let prefix, suffix =
    match space with
    | Before -> space_fmt, empty_fmt
    | After -> empty_fmt, space_fmt
    | Neither -> empty_fmt, empty_fmt
  in
  pp_with ~prefix ~suffix ppf

let pp_option ~space f ppf = function
  | None -> ()
  | Some a -> pp_spaced ~space ppf "%a" f a

let recursive ~space ppf r =
  match (r : is_recursive) with
  | Nonrecursive -> ()
  | Recursive -> pp_spaced ~space ppf "rec"

let continuation_sort ppf sort =
  Format.pp_print_string ppf
  @@
  match sort with
  | Normal -> "normal"
  | Exn -> "exn"
  | Define_root_symbol -> "define_root_symbol"

let char_between c (low, high) =
  Char.compare low c <= 0 && Char.compare c high <= 0

let is_identstart c =
  Char.equal c '_' || char_between c ('a', 'z') || char_between c ('A', 'Z')

let is_identchar c =
  is_identstart c || Char.equal c '\'' || char_between c ('0', '9')

let is_unquoted_symbol s =
  (not (String.equal s "")) && Misc.Stdlib.String.for_all is_identchar s

let is_unquoted_ident s =
  (not (String.equal s ""))
  && is_identstart s.[0]
  && Misc.Stdlib.String.for_all is_identchar s

let symbol_part ppf s =
  if is_unquoted_symbol s
  then Format.pp_print_string ppf s
  else Format.fprintf ppf "`%s`" s

let symbol ppf { txt = cunit, s; loc = _ } =
  Format.pp_print_char ppf '$';
  cunit
  |> Option.iter (fun { ident; linkage_name } ->
         symbol_part ppf ident;
         linkage_name
         |> Option.iter (fun linkage_name ->
                Format.fprintf ppf "/%a" symbol_part linkage_name);
         Format.pp_print_char ppf '.');
  symbol_part ppf s

let ident ppf s =
  if is_unquoted_ident s && not (Flambda_lex.is_keyword s)
  then Format.pp_print_string ppf s
  else Format.fprintf ppf "`%s`" s

let variable ppf { txt = s; loc = _ } = ident ppf s

let value_slot ppf { txt = s; loc = _ } = ident ppf s

let code_id ppf ({ txt = s; loc = _ } : code_id) = ident ppf s

let function_slot ppf ({ txt = s; loc = _ } : function_slot) = ident ppf s

let continuation_id ppf ({ txt = s; loc = _ } : continuation_id) = ident ppf s

let special_continuation ppf special_cont =
  match special_cont with
  | Done -> Format.fprintf ppf "done"
  | Error -> Format.fprintf ppf "error"

let continuation ppf (cont : continuation) =
  match cont with
  | Named id -> continuation_id ppf id
  | Special special_cont -> special_continuation ppf special_cont

let result_continuation ppf rcont =
  match rcont with
  | Return c -> continuation ppf c
  | Never_returns -> Format.fprintf ppf "never"

let exn_continuation ppf c = Format.fprintf ppf "* %a" continuation c

let region ppf (r : region) =
  match r with
  | Named v -> variable ppf v
  | Toplevel -> Format.pp_print_string ppf "toplevel"

let naked_number_kind ppf (nnk : Flambda_kind.Naked_number_kind.t) =
  Format.pp_print_string ppf
  @@
  match nnk with
  | Naked_immediate -> "imm"
  | Naked_float32 -> "float32"
  | Naked_float -> "float"
  | Naked_int8 -> "int8"
  | Naked_int16 -> "int16"
  | Naked_int32 -> "int32"
  | Naked_int64 -> "int64"
  | Naked_nativeint -> "nativeint"
  | Naked_vec128 -> "vec128"
  | Naked_vec256 -> "vec256"
  | Naked_vec512 -> "vec512"

let rec subkind ppf (k : subkind) =
  let str s = Format.pp_print_string ppf s in
  match k with
  | Anything -> str "val"
  | Float_block { num_fields } -> Format.fprintf ppf "float ^ %d" num_fields
  | Boxed_float -> str "float boxed"
  | Boxed_float32 -> str "float32 boxed"
  | Boxed_int32 -> str "int32 boxed"
  | Boxed_int64 -> str "int64 boxed"
  | Boxed_nativeint -> str "nativeint boxed"
  | Boxed_vec128 -> str "vec128 boxed"
  | Boxed_vec256 -> str "vec256 boxed"
  | Boxed_vec512 -> str "vec512 boxed"
  | Variant { consts; non_consts } -> variant_subkind ppf consts non_consts
  | Tagged_immediate -> str "imm tagged"
  | Float_array -> str "float array"
  | Immediate_array -> str "imm array"
  | Value_array -> str "val array"
  | Generic_array -> str "any array"

and variant_subkind ppf consts non_consts =
  match consts, non_consts with
  | [], [] ->
    Format.pp_print_string ppf "[ ]" (* Empty variant? Sure, whatever *)
  | _, _ ->
    (* Align first | in line with opening [*)
    Format.fprintf ppf "@[<hov 0>[ ";
    pp_pipe_list (fun ppf -> Format.fprintf ppf "%Ld") ppf consts;
    let () =
      match consts, non_consts with
      | [], _ | _, [] -> ()
      | _ :: _, _ :: _ -> Format.fprintf ppf "@ | "
    in
    let pp_pair ppf (tag, sk) =
      Format.fprintf ppf "@[<hov 2>%d of %a@]" tag
        (pp_star_list kind_with_subkind)
        sk
    in
    pp_pipe_list pp_pair ppf non_consts;
    Format.fprintf ppf "@ ]@]"

and kind_with_subkind ppf (k : kind_with_subkind) =
  let str s = Format.pp_print_string ppf s in
  match k with
  | Naked_number nnk -> naked_number_kind ppf nnk
  | Rec_info -> str "rec_info"
  | Region -> str "region"
  | Value sk -> subkind ppf sk

let arity ppf (a : arity) =
  match a with
  | [] -> Format.pp_print_string ppf "unit"
  | _ -> Format.fprintf ppf "@[<hv>%a@]" (pp_star_list kind_with_subkind) a

let kinded_variable ppf (v, (k : kind_with_subkind option)) =
  match k with
  | None -> variable ppf v
  | Some k ->
    Format.fprintf ppf "@[<2>%a :@ %a@]" variable v kind_with_subkind k

let field_of_block ppf : field_of_block -> unit = function
  | Symbol s -> symbol ppf s
  | Dynamically_computed v -> variable ppf v
  | Tagged_immediate i -> Format.fprintf ppf "%s" i

type parens =
  | Never
  | If_complex

let rec rec_info ~parens ppf (ri : Fexpr.rec_info) =
  let with_parens ~f ppf =
    match parens with
    | Never -> f ppf ()
    | If_complex -> Format.fprintf ppf "(%a)" f ()
  in
  match ri with
  | Depth d -> Format.fprintf ppf "%d" d
  | Infinity -> Format.pp_print_string ppf "inf"
  | Do_not_inline -> Format.pp_print_string ppf "do_not_inline"
  | Var dv -> variable ppf dv
  | Succ ri ->
    with_parens ppf ~f:(fun ppf () ->
        Format.fprintf ppf "succ %a" (rec_info ~parens:If_complex) ri)
  | Unroll (d, ri) ->
    with_parens ppf ~f:(fun ppf () ->
        Format.fprintf ppf "unroll %d %a" d (rec_info ~parens:If_complex) ri)

let coercion ppf : coercion -> unit = function
  | Id -> Format.pp_print_string ppf "id"
  | Change_depth { from; to_ } ->
    Format.fprintf ppf "depth %a -> %a" (rec_info ~parens:Never) from
      (rec_info ~parens:Never) to_

let float ppf f = Format.fprintf ppf "%h" f

let const ppf (c : Fexpr.const) =
  match c with
  | Naked_immediate i -> Format.fprintf ppf "%si" i
  | Tagged_immediate i -> Format.fprintf ppf "%s" i
  | Naked_float f -> float ppf f
  | Naked_float32 f -> Format.fprintf ppf "%hs" f
  | Naked_int32 i -> Format.fprintf ppf "%lil" i
  | Naked_int64 i -> Format.fprintf ppf "%LiL" i
  | Naked_nativeint i -> Format.fprintf ppf "%Lin" i
  | Naked_vec128 { word0; word1 } ->
    Format.fprintf ppf "vec128[%016Lx:%016Lx]" word0 word1
  | Naked_vec256 { word0; word1; word2; word3 } ->
    Format.fprintf ppf "vec256[%016Lx:%016Lx:%016Lx:%016Lx]" word0 word1 word2
      word3
  | Naked_vec512 { word0; word1; word2; word3; word4; word5; word6; word7 } ->
    Format.fprintf ppf
      "vec512[%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx]" word0
      word1 word2 word3 word4 word5 word6 word7

let rec simple ppf : simple -> unit = function
  | Symbol s -> symbol ppf s
  | Var v -> variable ppf v
  | Const c -> const ppf c
  | Coerce (s, co) -> Format.fprintf ppf "%a ~ %a" simple s coercion co

let simple_args ~space ~omit_if_empty ppf = function
  | [] when omit_if_empty -> ()
  | args -> pp_spaced ~space ppf "(@[<hv>%a@])" (pp_comma_list simple) args

let mutability ~space ppf mut =
  let str =
    match mut with
    | Mutable -> Some "mutable"
    | Immutable -> None
    | Immutable_unique -> Some "immutable_unique"
  in
  pp_option ~space Format.pp_print_string ppf str

let empty_array_kind ~space ppf (ak : empty_array_kind) =
  let str =
    match ak with
    | Values_or_immediates_or_naked_floats -> None
    | Naked_float32s -> Some "float32"
    | Naked_int32s -> Some "int32"
    | Naked_int64s -> Some "int64"
    | Naked_nativeints -> Some "nativeint"
    | Naked_vec128s -> Some "vec128"
    | Naked_vec256s -> Some "vec256"
    | Naked_vec512s -> Some "vec512"
    | Unboxed_products -> Some "unboxed_product"
  in
  pp_option ~space Format.pp_print_string ppf str

let alloc_mode_for_applications_opt ppf (alloc : alloc_mode_for_applications)
    ~space =
  match alloc with
  | Heap -> ()
  | Local { region = r; ghost_region = r' } ->
    pp_spaced ~space ppf "&%a &%a" region r region r'

let boxed_variable ppf var ~kind =
  Format.fprintf ppf "%a : %s boxed" variable var kind

let float_or_variable ppf : float or_variable -> unit = function
  | Const f -> float ppf f
  | Var v -> variable ppf v

let static_data ppf : static_data -> unit = function
  | Block { tag; mutability = mut; elements = elts } ->
    Format.fprintf ppf "Block %a%i (@[<hv>%a@])" (mutability ~space:After) mut
      tag
      (pp_comma_list field_of_block)
      elts
  | Boxed_float32 (Const f) -> Format.fprintf ppf "%hs" f
  | Boxed_float (Const f) -> Format.fprintf ppf "%h" f
  | Boxed_int32 (Const i) -> Format.fprintf ppf "%lil" i
  | Boxed_int64 (Const i) -> Format.fprintf ppf "%LiL" i
  | Boxed_nativeint (Const i) -> Format.fprintf ppf "%Lin" i
  | Boxed_vec128 (Const { word0; word1 }) ->
    Format.fprintf ppf "vec128[%016Lx:%016Lx]" word0 word1
  | Boxed_vec256 (Const { word0; word1; word2; word3 }) ->
    Format.fprintf ppf "vec256[%016Lx:%016Lx:%016Lx:%016Lx]" word0 word1 word2
      word3
  | Boxed_vec512
      (Const { word0; word1; word2; word3; word4; word5; word6; word7 }) ->
    Format.fprintf ppf
      "vec512[%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx:%016Lx]" word0
      word1 word2 word3 word4 word5 word6 word7
  | Boxed_float (Var v) -> boxed_variable ppf v ~kind:"float"
  | Boxed_float32 (Var v) -> boxed_variable ppf v ~kind:"float32"
  | Boxed_int32 (Var v) -> boxed_variable ppf v ~kind:"int32"
  | Boxed_int64 (Var v) -> boxed_variable ppf v ~kind:"int64"
  | Boxed_nativeint (Var v) -> boxed_variable ppf v ~kind:"nativeint"
  | Boxed_vec128 (Var v) -> boxed_variable ppf v ~kind:"vec128"
  | Boxed_vec256 (Var v) -> boxed_variable ppf v ~kind:"vec256"
  | Boxed_vec512 (Var v) -> boxed_variable ppf v ~kind:"vec512"
  | Immutable_float_block elements ->
    Format.fprintf ppf "Float_block (%a)"
      (pp_comma_list float_or_variable)
      elements
  | Immutable_float_array elements ->
    Format.fprintf ppf "Float_array [|%a|]"
      (pp_semi_list float_or_variable)
      elements
  | Immutable_value_array elements ->
    Format.fprintf ppf "Value_array [|%a|]"
      (pp_semi_list field_of_block)
      elements
  | Empty_array kind ->
    Format.fprintf ppf "Empty_array%a" (empty_array_kind ~space:Before) kind
  | Mutable_string { initial_value = s } ->
    Format.fprintf ppf "mutable \"%s\"" (s |> String.escaped)
  | Immutable_string s -> Format.fprintf ppf "\"%s\"" (s |> String.escaped)

let static_data_binding ppf { symbol = s; defining_expr = sp } =
  Format.fprintf ppf "%a =@ %a" symbol s static_data sp

let prim_param ppf = function
  | Flag f -> Format.fprintf ppf ".`%s`" f
  | Positional p -> Format.fprintf ppf ".[`%s`]" p.txt
  | Labeled { label; value } -> Format.fprintf ppf ".`%s`[`%s`]" label value.txt

let prim_params ppf params =
  Format.fprintf ppf "%a"
    (Format.pp_print_list ~pp_sep:(fun _ () -> ()) prim_param) params

let prim_op ppf ({ prim; params } : prim_op) =
  Format.fprintf ppf "@[<2>%s%a@]"
    prim
    prim_params params

let prim ppf (op, args : prim) =
  Format.fprintf ppf "@[<2>%a%a@]"
    prim_op op
    (simple_args ~space:Before ~omit_if_empty:false) args

let parameter ppf { param; kind = k } = kinded_variable ppf (param, k)

let kinded_parameters ~space ppf = function
  | [] -> ()
  | args -> pp_spaced ~space ppf "(@[<hv>%a@])" (pp_comma_list parameter) args

let cont_recursive ~space ppf recu =
  match (recu : is_cont_recursive) with
  | Nonrecursive -> ()
  | Recursive l ->
    pp_spaced ~space ppf "rec%a" (kinded_parameters ~space:Neither) l

let raise_kind ppf rt =
  Format.pp_print_string ppf
  @@
  match rt with
  | Regular -> "regular"
  | Reraise -> "reraise"
  | No_trace -> "notrace"

let trap_action ppf = function
  | Push { exn_handler } ->
    Format.fprintf ppf "push(%a)" continuation exn_handler
  | Pop { exn_handler; raise_kind = rk } ->
    Format.fprintf ppf "@[<h>pop(%a%a)@]"
      (pp_option ~space:After raise_kind)
      rk continuation exn_handler

let apply_cont ppf (ac : Fexpr.apply_cont) =
  match ac with
  | { cont; trap_action = action; args } ->
    Format.fprintf ppf "@[<hv2>%a%a%a@]" continuation cont
      (pp_option ~space:Before trap_action)
      action
      (simple_args ~space:Before ~omit_if_empty:true)
      args

let switch_case ppf (v, c) = Format.fprintf ppf "@;| %i -> %a" v apply_cont c

let value_slots ppf = function
  | None -> ()
  | Some ces ->
    Format.fprintf ppf "@ @[<hv2>with {";
    pp_list ~sep:";"
      (fun ppf ({ var; value } : one_value_slot) ->
        Format.fprintf ppf "@ @[<hv2>%a =@ %a@]" value_slot var simple value)
      ppf ces;
    Format.fprintf ppf "@;<1 -2>}@]"

let fun_decl ppf (decl : fun_decl) =
  let pp_at_function_slot ppf cid =
    pp_option ~space:Before (pp_like "@@%a" function_slot) ppf cid
  in
  Format.fprintf ppf "@[<2>closure@ %a%a@]" code_id decl.code_id
    pp_at_function_slot decl.function_slot

let named ppf = function
  | (Simple s : named) -> simple ppf s
  | Prim p -> prim ppf p
  | (Closure decl : named) -> fun_decl ppf decl
  | Rec_info ri ->
    Format.fprintf ppf "@[<hv 2>rec_info@ %a@]" (rec_info ~parens:If_complex) ri

let static_closure_binding ppf (scb : static_closure_binding) =
  Format.fprintf ppf "%a =@ %a" symbol scb.symbol fun_decl scb.fun_decl

let call_kind ~space ppf ck =
  match ck with
  | Function (Indirect alloc) ->
    alloc_mode_for_applications_opt ppf alloc ~space
  | Function (Direct { code_id = c; function_slot = cl; alloc }) ->
    pp_spaced ~space ppf "@[direct(%a%a%a)@]" code_id c
      (pp_option ~space:Before (pp_like "@@%a" function_slot))
      cl
      (alloc_mode_for_applications_opt ~space:Before)
      alloc
  | C_call { alloc } ->
    let noalloc_kwd = if alloc then None else Some "noalloc" in
    pp_spaced ~space ppf "ccall%a"
      (pp_option ~space:Before Format.pp_print_string)
      noalloc_kwd

let inline_attribute ~space ppf (i : Inline_attribute.t) =
  let str =
    match i with
    | Always_inline -> Some "inline(always)"
    | Available_inline -> Some "inline(hint)"
    | Never_inline -> Some "inline(never)"
    | Unroll i -> Some (Format.sprintf "unroll(%d)" i)
    | Default_inline -> None
  in
  pp_option ~space Format.pp_print_string ppf str

let inline_attribute_opt ~space ppf i =
  pp_option ~space (inline_attribute ~space:Neither) ppf i

let inlined_attribute ~space ppf (i : Fexpr.inlined_attribute) =
  let str =
    match i with
    | Always_inlined -> Some "inlined(always)"
    | Hint_inlined -> Some "inlined(hint)"
    | Never_inlined -> Some "inlined(never)"
    | Unroll i -> Some (Format.sprintf "unroll(%d)" i)
    | Default_inlined -> None
  in
  pp_option ~space Format.pp_print_string ppf str

let inlined_attribute_opt ~space ppf i =
  pp_option ~space (inlined_attribute ~space:Neither) ppf i

let inlining_state ppf { depth } = Format.fprintf ppf "depth(%d)" depth

let loopify_attribute ppf (loopify : loopify_attribute) =
  Format.pp_print_string ppf
  @@
  match loopify with
  | Always_loopify -> "loopify(always)"
  | Never_loopify -> "loopify(never)"
  | Already_loopified -> "loopify(done)"
  | Default_loopify_and_tailrec -> "loopify(default tailrec)"
  | Default_loopify_and_not_tailrec -> "loopify(default)"

let loopify_attribute_opt ~space ppf l =
  pp_option ~space loopify_attribute ppf l

let code_size ppf code_size = Format.fprintf ppf "%d" code_size

let or_blank f ppf ob =
  match ob with None -> Format.pp_print_string ppf "_" | Some a -> f ppf a

let func_name_with_optional_arities ppf (n, arities) =
  match arities with
  | None -> simple ppf n
  | Some { params_arity; ret_arity } ->
    Format.fprintf ppf "@[<1>(%a@ : @[%a ->@ %a@]@,)@]" simple n
      (or_blank arity) params_arity arity ret_arity

type scope =
  | Outer
  | Where_body
  | Continuation_body

let parens ~if_scope_is scope ppf f =
  match if_scope_is, scope with
  | Outer, Outer | Where_body, Where_body | Continuation_body, Continuation_body
    ->
    Format.fprintf ppf "(%t)" (f Outer)
  | (Outer | Where_body | Continuation_body), _ -> f scope ppf

let rec expr scope ppf = function
  | Invalid { message } ->
    Format.fprintf ppf "@[invalid \"%s\"@]" (message |> String.escaped)
  | Apply_cont ac -> Format.fprintf ppf "@[cont %a@]" apply_cont ac
  | Let let_ ->
    parens ~if_scope_is:Where_body scope ppf (fun scope ppf ->
        let_expr scope ppf let_)
  | Let_cont
      { recursive = recu;
        body;
        bindings = { name; params; sort; handler } :: rem_cont
      } ->
    parens ~if_scope_is:Continuation_body scope ppf (fun _scope ppf ->
        Format.fprintf ppf
          "@[<v 2>%a@ @[<v>@[<v 2>@[where%a @]@[<hv 2>%a%a%a@] =@ %a@]%a@]@]"
          (expr Where_body) body
          (cont_recursive ~space:Before)
          recu continuation_id name
          (pp_option continuation_sort ~space:Before)
          sort
          (kinded_parameters ~space:Before)
          params (expr Continuation_body) handler andk rem_cont)
  | Let_cont _ -> Format.pp_print_string ppf "<malformed letk>"
  | Let_symbol l ->
    parens ~if_scope_is:Where_body scope ppf (fun scope ppf ->
        let_symbol_expr scope ppf l)
  | Switch { scrutinee; cases } ->
    Format.fprintf ppf "@[<v 2>switch %a%a@]" simple scrutinee
      (pp_list ~sep:"" switch_case)
      cases
    (* (fun ppf () -> if cases <> [] then Format.pp_print_cut ppf ()) () *)
  | Apply
      { call_kind = kind;
        inlined;
        inlining_state = is;
        continuation = ret;
        exn_continuation = ek;
        args;
        func;
        arities
      } ->
    let pp_inlining_state ppf () =
      pp_option ~space:Before
        (pp_like "inlining_state(%a)" inlining_state)
        ppf is
    in
    Format.fprintf ppf
      "@[<hv 2>apply@[<2>%a%a%a@]@ @[<hv 2>%a%a@ @[<hov>-> %a@ %a@]@]@]"
      (call_kind ~space:Before) kind
      (inlined_attribute_opt ~space:Before)
      inlined pp_inlining_state () func_name_with_optional_arities
      (func, arities)
      (simple_args ~space:Before ~omit_if_empty:true)
      args result_continuation ret exn_continuation ek

and let_expr scope ppf : let_ -> unit = function
  | { bindings = first :: rest; body; value_slots = ces } ->
    Format.fprintf ppf "@[<v>@[<hv>@[<hv2>let %a =@ %a@]" variable first.var
      named first.defining_expr;
    List.iter
      (fun ({ var; defining_expr } : let_binding) ->
        Format.fprintf ppf "@ @[<hv2>and %a =@ %a@]" variable var named
          defining_expr)
      rest;
    Format.fprintf ppf "%a@ in@]@ %a@]" value_slots ces (expr scope) body
  | _ -> failwith "empty let?"

and let_symbol_expr scope ppf = function
  | { bindings; value_slots; body } ->
    Format.fprintf ppf "@[<v>@[<hv>@[<hv2>let %a@]@ in@]@ %a@]" symbol_bindings
      (bindings, value_slots) (expr scope) body

and andk ppf l =
  let cont { name; params; sort; handler } =
    Format.fprintf ppf "@ @[<v 2>andwhere%a %a@[<hv2>%a@] =@ %a@]"
      (pp_option continuation_sort ~space:Before)
      sort continuation_id name
      (kinded_parameters ~space:Before)
      params (expr Continuation_body) handler
  in
  List.iter cont l

and symbol_bindings ppf (bindings, elements) =
  let first = ref true in
  let pp_and ppf () = if not !first then Format.fprintf ppf "@;<1 -2>and " in
  List.iter
    (fun b ->
      Format.fprintf ppf "%a%a" pp_and () symbol_binding b;
      first := false)
    bindings;
  value_slots ppf elements

and symbol_binding ppf (sb : symbol_binding) =
  match sb with
  | Data ss -> static_data_binding ppf ss
  | Code code -> code_binding ppf code
  | Deleted_code id ->
    Format.fprintf ppf "@[<hov 1>code@ %a@ deleted@]" code_id id
  | Closure clo -> static_closure_binding ppf clo
  | Set_of_closures soc ->
    Format.fprintf ppf "@[<hv>@[<hv2>set_of_closures@ ";
    (* Somewhat clumsily reuse the logic in [symbol_bindings] *)
    let closure_bindings_as_symbol_bindings =
      List.map
        (fun binding : Fexpr.symbol_binding -> Closure binding)
        soc.bindings
    in
    symbol_bindings ppf (closure_bindings_as_symbol_bindings, soc.elements);
    Format.fprintf ppf "@]@ end@]"

and code_binding ppf
    ({ recursive = rec_;
       inline;
       id;
       newer_version_of;
       param_arity = _;
       ret_arity;
       params_and_body;
       code_size = cs;
       is_tupled;
       loopify;
       result_mode
     } :
      code) =
  Format.fprintf ppf
    "@[<hv 2>code@[<h>%a%a%a@ size(%a)%a%a@]@ @[<hv2>@[<hv 2>%a"
    (recursive ~space:Before) rec_
    (inline_attribute_opt ~space:Before)
    inline
    (loopify_attribute_opt ~space:Before)
    loopify code_size cs
    (pp_option ~space:Before (pp_like "newer_version_of(%a)" code_id))
    newer_version_of
    (fun ppf is_tupled -> if is_tupled then Format.fprintf ppf "@ tupled@ ")
    is_tupled code_id id;
  let { params;
        closure_var;
        region_var;
        ghost_region_var;
        depth_var;
        ret_cont;
        exn_cont;
        body
      } =
    params_and_body
  in
  Format.fprintf ppf
    "%a@]@ @[<hov 2>%a@ %a@ %a %a@]@ @[<hv 2>-> %a@ * %a@]%a%s@]@] =@ %a"
    (kinded_parameters ~space:Before)
    params variable closure_var variable region_var variable ghost_region_var
    variable depth_var continuation_id ret_cont continuation_id exn_cont
    (pp_option ~space:Before (pp_like ": %a" arity))
    ret_arity
    (match result_mode with Heap -> "" | Local -> " local")
    (expr Outer) body

let flambda_unit ppf ({ body } : flambda_unit) =
  Format.fprintf ppf "@[<v>@[%a@]@ @]" (expr Outer) body

let expect_test_spec ppf ({ before; after } : expect_test_spec) =
  Format.fprintf ppf "@[<v>%a===>@ %a@]" flambda_unit before flambda_unit after

let markdown_doc ppf nodes =
  Format.fprintf ppf "@[<v>";
  List.iter
    (fun (node : markdown_node) ->
      match node with
      | Text text -> Format.pp_print_string ppf text
      | Expect spec ->
        Format.fprintf ppf "```flexpect@ %a@ ```@ " expect_test_spec spec)
    nodes;
  Format.fprintf ppf "@]"
