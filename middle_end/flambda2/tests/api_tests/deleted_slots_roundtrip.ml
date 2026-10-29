(* ACTUAL COMPILER: check slot structure directly, rather than relying on
   Compare.flambda_units, which only counts deleted slots. *)
open Flambda2_parser

type slot =
  | Live of string
  | Deleted of string * int

type mode =
  | Heap_toplevel
  | Heap of string
  | Local of string * string

type group =
  { slots : slot list;
    mode : mode;
    static_symbols : string list option
  }

let fail format = Printf.ksprintf failwith format

let parse filename =
  match Parse_flambda.parse_fexpr filename with
  | Ok unit -> unit
  | Error (Parsing_error (message, loc)) ->
    Format.eprintf "%a: %s@." Location.print_loc loc message;
    fail "parsing %s failed" filename
  | Error (Lexing_error (error, loc)) ->
    Format.eprintf "%a: %a@." Location.print_loc loc Flambda_lex.pp_error error;
    fail "lexing %s failed" filename

let slot (decl : Fexpr.fun_decl) =
  match decl.code with
  | Code_id { code_id; function_slot } ->
    let slot = Option.value function_slot ~default:code_id in
    Live slot.txt
  | Deleted { function_slot; function_slot_size } ->
    Deleted (function_slot.txt, function_slot_size)

let mode (decl : Fexpr.fun_decl) =
  match decl.alloc with
  | Heap { alloc_region = Toplevel_alloc_region } -> Heap_toplevel
  | Heap { alloc_region = Named region } -> Heap region.txt
  | Local { alloc_region = Named alloc_region; region = Named region } ->
    Local (alloc_region.txt, region.txt)
  | Local _ -> fail "unexpected toplevel region in local closure"

let group ?static_symbols decls =
  match decls with
  | [] -> fail "empty set of closures"
  | first :: _ ->
    let alloc_mode = mode first in
    if not (List.for_all (fun decl -> alloc_mode = mode decl) decls)
    then fail "allocation modes differ within a set of closures";
    { slots = List.map slot decls; mode = alloc_mode; static_symbols }

let groups ({ body } : Fexpr.flambda_unit) =
  let found = ref [] in
  let add group = found := group :: !found in
  let static_group bindings =
    let symbols, declarations =
      List.split
        (List.map
           (fun ({ symbol; fun_decl } : Fexpr.static_closure_binding) ->
             let _, name = symbol.txt in
             name, fun_decl)
           bindings)
    in
    add (group ~static_symbols:symbols declarations)
  in
  let rec visit (expr : Fexpr.expr) =
    match expr with
    | Let { bindings; body; _ } ->
      let declarations =
        List.filter_map
          (fun ({ defining_expr; _ } : Fexpr.let_binding) ->
            match defining_expr with
            | Closure decl -> Some decl
            | Simple _ | Prim _ | Rec_info _ -> None)
          bindings
      in
      if declarations <> [] then add (group declarations);
      visit body
    | Let_symbol { bindings; body; _ } ->
      let implicit =
        List.filter_map
          (fun (binding : Fexpr.symbol_binding) ->
            match binding with
            | Closure binding -> Some binding
            | Data _ | Code _ | Deleted_code _ | Set_of_closures _ -> None)
          bindings
      in
      if implicit <> [] then static_group implicit;
      List.iter
        (fun (binding : Fexpr.symbol_binding) ->
          match binding with
          | Set_of_closures { bindings; _ } -> static_group bindings
          | Code { params_and_body = { body; _ }; _ } -> visit body
          | Data _ | Deleted_code _ | Closure _ -> ())
        bindings;
      visit body
    | Let_cont { bindings; body; _ } ->
      visit body;
      List.iter
        (fun ({ handler; _ } : Fexpr.continuation_binding) -> visit handler)
        bindings
    | Apply _ | Apply_cont _ | Switch _ | Invalid -> ()
  in
  visit body;
  !found

let check stage unit =
  let expected =
    [ Live "slot_a";
      Deleted ("slot_dead2", 2);
      Live "slot_b";
      Deleted ("slot_dead3", 3) ]
  in
  let expected_symbols =
    ["Static_a"; "Static_dead2"; "Static_b"; "Static_dead3"]
  in
  let gs = groups unit in
  if List.length gs <> 3 then fail "%s: expected three closure groups" stage;
  List.iter
    (fun { slots; _ } ->
      if slots <> expected then fail "%s: wrong slot identities or order" stage)
    gs;
  let static = List.filter (fun g -> Option.is_some g.static_symbols) gs in
  let dynamic = List.filter (fun g -> Option.is_none g.static_symbols) gs in
  (match static with
  | [{ mode; static_symbols; _ }] ->
    if mode <> Heap_toplevel || static_symbols <> Some expected_symbols
    then fail "%s: static slot/symbol layout changed" stage
  | [] | _ :: _ :: _ -> fail "%s: static slot/symbol layout changed" stage);
  let heap =
    List.filter_map
      (fun g ->
        match g.mode with
        | Heap name -> Some name
        | Heap_toplevel | Local _ -> None)
      dynamic
  in
  let local =
    List.filter_map
      (fun g ->
        match g.mode with
        | Local (ar, r) -> Some (ar, r)
        | Heap_toplevel | Heap _ -> None)
      dynamic
  in
  match heap, local with
  | [heap_region], [(local_alloc_region, local_region)]
    when heap_region = local_alloc_region && local_region <> heap_region ->
    ()
  | _ -> fail "%s: dynamic allocation modes or region identities changed" stage

let converted unit_info fexpr =
  (Fexpr_to_flambda.conv (Unit_info.modname unit_info) fexpr).unit
  |> Flambda_to_fexpr.conv

let write filename contents =
  let output = open_out filename in
  Fun.protect
    ~finally:(fun () -> close_out output)
    (fun () -> output_string output contents)

let check_roundtrip filename =
  let unit_info = Parse_flambda.make_unit_info ~filename in
  Env.set_current_unit unit_info;
  let input = parse filename in
  check "parsed fixture" input;
  let printed = converted unit_info input in
  check "flambda to fexpr" printed;
  let output = Filename.temp_file "deleted_slots_roundtrip" ".fl" in
  Fun.protect
    ~finally:(fun () -> Sys.remove output)
    (fun () ->
      write output (Format.asprintf "%a" Print_fexpr.flambda_unit printed);
      let reparsed = parse output in
      check "printed then parsed" reparsed;
      check "second conversion" (converted unit_info reparsed))

let contains text part =
  let text_length = String.length text in
  let part_length = String.length part in
  let rec check i =
    if i + part_length > text_length
    then false
    else if String.sub text i part_length = part
    then true
    else check (i + 1)
  in
  check 0

let check_consistent filename =
  let unit_info = Parse_flambda.make_unit_info ~filename in
  Env.set_current_unit unit_info;
  let input = parse filename in
  ignore (Fexpr_to_flambda.conv (Unit_info.modname unit_info) input)

let check_rejected filename =
  let unit_info = Parse_flambda.make_unit_info ~filename in
  Env.set_current_unit unit_info;
  let input = parse filename in
  let ppf = Format.err_formatter in
  let original = Format.pp_get_formatter_out_functions ppf () in
  let output = Buffer.create 128 in
  let captured =
    { original with
      out_string =
        (fun text offset length ->
          Buffer.add_substring output text offset length);
      out_flush = (fun () -> ())
    }
  in
  let rejected =
    Format.pp_print_flush ppf ();
    Format.pp_set_formatter_out_functions ppf captured;
    Fun.protect
      ~finally:(fun () ->
        Format.pp_print_flush ppf ();
        Format.pp_set_formatter_out_functions ppf original)
      (fun () ->
        match Fexpr_to_flambda.conv (Unit_info.modname unit_info) input with
        | _ -> false
        | exception Misc.Fatal_error -> true)
  in
  let diagnostic = Buffer.contents output in
  if not rejected then fail "conflicting sizes of the same slot were accepted";
  if
    (not (contains diagnostic "Inconsistent sizes for function slot"))
    || not (contains diagnostic "conflicting")
  then fail "unexpected diagnostic for conflicting sizes: %s" diagnostic

(* Compare region identities to their own binders, since converting Flambda back
   to fexpr may freshen the textual names of those binders. *)
let check_ccalls stage ({ body } : Fexpr.flambda_unit) =
  let signatures = ref [] in
  let same_region (expected : Fexpr.variable) (actual : Fexpr.region) =
    match actual with
    | Named actual -> String.equal expected.txt actual.txt
    | Toplevel_alloc_region -> false
  in
  let check_call
      (region_vars : Fexpr.variable Fexpr.alloc_mode_for_applications)
      (body : Fexpr.expr) =
    let alloc, mode =
      match body with
      | Apply { call_kind = C_call { alloc }; alloc_mode; _ } ->
        alloc, alloc_mode
      | Apply { call_kind = Function _ | Method _; _ }
      | Let _ | Let_cont _ | Let_symbol _ | Apply_cont _ | Switch _ | Invalid ->
        fail "%s: expected a C call in fixture" stage
    in
    let local =
      match region_vars, mode with
      | ( Not_alloc_stack { alloc_region = expected },
          Not_alloc_stack { alloc_region = actual } ) ->
        if not (same_region expected actual)
        then fail "%s: C-call allocation region changed" stage;
        false
      | ( Maybe_alloc_stack
            { alloc_region = expected_alloc;
              region = expected_region;
              ghost_region = expected_ghost
            },
          Maybe_alloc_stack
            { alloc_region = actual_alloc;
              region = actual_region;
              ghost_region = actual_ghost
            } ) ->
        if
          not
            (same_region expected_alloc actual_alloc
            && same_region expected_region actual_region
            && same_region expected_ghost actual_ghost)
        then fail "%s: C-call local region identities changed" stage;
        true
      | Not_alloc_stack _, Maybe_alloc_stack _
      | Maybe_alloc_stack _, Not_alloc_stack _ ->
        fail "%s: C-call allocation mode changed" stage
    in
    signatures := (alloc, local) :: !signatures
  in
  (match body with
  | Let_symbol { bindings; _ } ->
    List.iter
      (fun (binding : Fexpr.symbol_binding) ->
        match binding with
        | Code { params_and_body = { body; region_vars; _ }; _ } ->
          check_call region_vars body
        | Data _ | Deleted_code _ | Closure _ | Set_of_closures _ -> ())
      bindings
  | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid ->
    fail "%s: expected C-call code definitions" stage);
  if
    List.sort compare !signatures
    <> [false, false; false, true; true, false; true, true]
  then fail "%s: C-call allocation flags or modes changed" stage

let check_ccall_roundtrip filename =
  let unit_info = Parse_flambda.make_unit_info ~filename in
  Env.set_current_unit unit_info;
  let input = parse filename in
  check_ccalls "parsed C-call fixture" input;
  let printed = converted unit_info input in
  check_ccalls "C calls converted to Flambda and back" printed;
  let output = Filename.temp_file "ccall_roundtrip" ".fl" in
  Fun.protect
    ~finally:(fun () -> Sys.remove output)
    (fun () ->
      write output (Format.asprintf "%a" Print_fexpr.flambda_unit printed);
      let reparsed = parse output in
      check_ccalls "printed then parsed C calls" reparsed;
      check_ccalls "second C-call conversion" (converted unit_info reparsed))

(* Printed names may get fresh numeric stamps; all other slash components are
   part of the name. A slash at position zero is not a stamp separator. *)
let name_without_stamp name =
  match String.rindex_opt name '/' with
  | Some slash when slash > 0 && slash + 1 < String.length name ->
    let rec all_digits i =
      i = String.length name
      || Char.compare name.[i] '0' >= 0
         && Char.compare name.[i] '9' <= 0
         && all_digits (i + 1)
    in
    if all_digits (slash + 1) then String.sub name 0 slash else name
  | None | Some _ -> name

let check_slashes stage ({ body } : Fexpr.flambda_unit) =
  let fail_shape () = fail "%s: unexpected slash fixture structure" stage in
  let params, ret, code_body =
    match body with
    | Let_symbol { bindings; _ } -> (
      match bindings with
      | [binding] -> (
        match (binding : Fexpr.symbol_binding) with
        | Code code ->
          let { Fexpr.params; ret_cont; body; _ } = code.params_and_body in
          params, ret_cont.txt, body
        | Data _ | Deleted_code _ | Closure _ | Set_of_closures _ ->
          fail_shape ())
      | [] | _ :: _ :: _ -> fail_shape ())
    | Let _ | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid ->
      fail_shape ()
  in
  let param_names =
    List.map (fun ({ param; _ } : Fexpr.kinded_parameter) -> param.txt) params
  in
  let expected_bases =
    [ "pair/shared/alpha";
      "pair/shared/beta";
      "item/path";
      "item/path";
      "nonnumeric/end";
      "trailing/path/";
      "/123" ]
  in
  if List.map name_without_stamp param_names <> expected_bases
  then fail "%s: slash-containing parameter names changed" stage;
  if List.length (List.sort_uniq String.compare param_names) <> 7
  then fail "%s: parameter identities merged" stage;
  let continuations = ref [] in
  let calls = ref [] in
  let rec visit (expr : Fexpr.expr) =
    match expr with
    | Let_cont { bindings; body; _ } ->
      List.iter
        (fun ({ name; params; handler; _ } : Fexpr.continuation_binding) ->
          let params =
            List.map
              (fun ({ param; _ } : Fexpr.kinded_parameter) -> param.txt)
              params
          in
          continuations := (name.txt, params) :: !continuations;
          visit handler)
        bindings;
      visit body
    | Apply_cont { cont = Named cont; args; _ } ->
      let args =
        List.map
          (fun (arg : Fexpr.simple) ->
            match arg with
            | Var var -> var.txt
            | Symbol _ | Const _ | Coerce _ -> fail_shape ())
          args
      in
      calls := (cont.txt, args) :: !calls
    | Apply_cont { cont = Special _; _ }
    | Let _ | Let_symbol _ | Apply _ | Switch _ | Invalid ->
      fail_shape ()
  in
  visit code_body;
  let continuation base =
    match
      List.filter
        (fun (name, _) -> name_without_stamp name = base)
        !continuations
    with
    | [entry] -> entry
    | [] | _ :: _ :: _ -> fail_shape ()
  in
  let check_call target expected =
    let matching = List.filter (fun (name, _) -> name = target) !calls in
    match matching with
    | [(_, args)] when args = expected -> ()
    | [] | [_] | _ :: _ :: _ -> fail "%s: continuation arguments changed" stage
  in
  let first, first_params = continuation "jump/path/one" in
  let second, second_params = continuation "jump/path/two/nonnum" in
  if
    first = second || List.length !continuations <> 2 || List.length !calls <> 3
  then fail "%s: continuation identities merged" stage;
  check_call first param_names;
  check_call second first_params;
  match second_params with
  | first_param :: _ -> check_call ret [first_param]
  | [] -> fail_shape ()

let check_slash_roundtrip filename =
  let unit_info = Parse_flambda.make_unit_info ~filename in
  Env.set_current_unit unit_info;
  let input = parse filename in
  check_slashes "parsed slash fixture" input;
  let printed = converted unit_info input in
  check_slashes "slash names converted to Flambda and back" printed;
  let output = Filename.temp_file "slash_roundtrip" ".fl" in
  Fun.protect
    ~finally:(fun () -> Sys.remove output)
    (fun () ->
      write output (Format.asprintf "%a" Print_fexpr.flambda_unit printed);
      let reparsed = parse output in
      check_slashes "printed then parsed slash names" reparsed;
      check_slashes "second slash conversion" (converted unit_info reparsed))

let () =
  Misc.Style.setup (Some Misc.Color.Never);
  check_roundtrip Sys.argv.(1);
  check_consistent Sys.argv.(2);
  check_rejected Sys.argv.(3);
  check_ccall_roundtrip Sys.argv.(4);
  check_slash_roundtrip Sys.argv.(5)
