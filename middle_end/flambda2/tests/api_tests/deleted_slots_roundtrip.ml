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
            match defining_expr with Closure decl -> Some decl | _ -> None)
          bindings
      in
      if declarations <> [] then add (group declarations);
      visit body
    | Let_symbol { bindings; body; _ } ->
      let implicit =
        List.filter_map
          (function Fexpr.Closure binding -> Some binding | _ -> None)
          bindings
      in
      if implicit <> [] then static_group implicit;
      List.iter
        (function
          | Fexpr.Set_of_closures { bindings; _ } -> static_group bindings
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
  | [{ mode = Heap_toplevel; static_symbols = Some symbols; _ }]
    when symbols = expected_symbols ->
    ()
  | _ -> fail "%s: static slot/symbol layout changed" stage);
  let heap =
    List.filter_map
      (fun g -> match g.mode with Heap name -> Some name | _ -> None)
      dynamic
  in
  let local =
    List.filter_map
      (fun g -> match g.mode with Local (ar, r) -> Some (ar, r) | _ -> None)
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

let () =
  check_roundtrip Sys.argv.(1);
  check_consistent Sys.argv.(2);
  check_rejected Sys.argv.(3)
