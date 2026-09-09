(* Check specialisation parsing, comparison, approximants and costs. *)

open Import

let base =
  {|let code size(1) f (x : val, y : val)
      specialised { x = sx; y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let $f = closure f &toplevel synthetic { sx = 0; sy = 1 } in
let $camlCompare = Block 0 ($f) in
cont done ($camlCompare)
|}

let occurrences text pattern =
  let n = String.length pattern in
  let rec go i acc =
    if i + n > String.length text
    then List.rev acc
    else if String.equal (String.sub text i n) pattern
    then go (i + n) (i :: acc)
    else go (i + 1) acc
  in
  go 0 []

let replace_at text ~pattern ~with_ i =
  let n = String.length pattern in
  String.sub text 0 i ^ with_
  ^ String.sub text (i + n) (String.length text - i - n)

let replace_once text ~pattern ~with_ =
  match occurrences text pattern with
  | [i] -> replace_at text ~pattern ~with_ i
  | [] | _ :: _ :: _ ->
    Misc.fatal_errorf "Expected exactly one occurrence of %S" pattern

let replace_all text ~pattern ~with_ =
  match occurrences text pattern with
  | [] -> Misc.fatal_errorf "Expected an occurrence of %S" pattern
  | occurrences ->
    List.fold_left
      (fun text i -> replace_at text ~pattern ~with_ i)
      text (List.rev occurrences)

let delete_line text ~containing =
  let lines = String.split_on_char '\n' text in
  let lines' =
    List.filter
      (fun line ->
        match occurrences line containing with [] -> true | _ -> false)
      lines
  in
  if List.compare_lengths lines lines' = 0
  then Misc.fatal_errorf "No line contains %S" containing;
  String.concat "\n" lines'

let missing = delete_line base ~containing:"specialised {"

let missing_one = replace_once base ~pattern:"x = sx; y = sy" ~with_:"x = sx"

let swapped =
  replace_once base ~pattern:"x = sx; y = sy" ~with_:"x = sy; y = sx"

let ordinary = replace_once missing ~pattern:" synthetic {" ~with_:" with {"

let renamed =
  base
  |> replace_once ~pattern:"(x : val, y : val)" ~with_:"(u : val, v : val)"
  |> replace_once ~pattern:"x = sx; y = sy" ~with_:"u = su; v = sv"
  |> replace_once ~pattern:"cont k (x)" ~with_:"cont k (u)"
  |> replace_once ~pattern:"sx = 0; sy = 1" ~with_:"su = 0; sv = 1"

let reordered =
  replace_once base ~pattern:"x = sx; y = sy" ~with_:"y = sy; x = sx"

let different_body =
  base |> replace_once ~pattern:"cont k (x)" ~with_:"cont k (y)"

let renamed_different_body =
  replace_once renamed ~pattern:"cont k (u)" ~with_:"cont k (v)"

let mark_site text =
  replace_once text ~pattern:"closure f" ~with_:"closure specialisation_site f"

let site = mark_site base

let site_different_body =
  replace_once site ~pattern:"cont k (x)" ~with_:"cont k (y)"

let empty =
  replace_once missing ~pattern:" synthetic { sx = 0; sy = 1 }" ~with_:""

let empty_site = mark_site empty

let empty_site_different_body =
  replace_once empty_site ~pattern:"cont k (x)" ~with_:"cont k (y)"

let malformed_site = mark_site ordinary

let duplicate_values = replace_once base ~pattern:"sy = 1" ~with_:"sy = 0"

let duplicate_values_reordered =
  replace_once duplicate_values ~pattern:"x = sx; y = sy"
    ~with_:"y = sy; x = sx"

let early_slots =
  {|let $f = closure f &toplevel synthetic { sx = 0; sy = 0 }
and code size(1) f (x : val, y : val)
      specialised { x = sx; y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let $camlCompare = Block 0 ($f) in
cont done ($camlCompare)
|}

let early_slots_swapped =
  replace_once early_slots ~pattern:"x = sx; y = sy" ~with_:"x = sy; y = sx"

let indistinguishable_slots =
  delete_line early_slots ~containing:"specialised {"

let indistinguishable_slots_renamed =
  indistinguishable_slots
  |> replace_all ~pattern:"sx =" ~with_:"su ="
  |> replace_all ~pattern:"sy =" ~with_:"sv ="

let ordinary_slots =
  {|let code size(1) f (x : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
and code size(1) g (x : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (0)
in
let $f = closure f &toplevel
and $g = closure g &toplevel
with { sx = 0; sy = 0 }
in
let x = %project_value_slot.[f].[sx] ($f) in
let y = %project_value_slot.[f].[sx] ($f) in
let other = %project_function_slot.[f].[g] ($f) in
let $camlCompare = Block 0 (x, y, other) in
cont done ($camlCompare)
|}

let ordinary_slots_renamed =
  ordinary_slots
  |> replace_all ~pattern:"sx" ~with_:"su"
  |> replace_all ~pattern:"sy" ~with_:"sv"
  |> replace_all ~pattern:"closure f" ~with_:"closure f @first"
  |> replace_all ~pattern:"closure g" ~with_:"closure g @second"
  |> replace_all ~pattern:"[f]" ~with_:"[first]"
  |> replace_all ~pattern:"[g]" ~with_:"[second]"

let ordinary_value_slot_not_bijective =
  replace_once ordinary_slots ~pattern:"let y = %project_value_slot.[f].[sx]"
    ~with_:"let y = %project_value_slot.[f].[sy]"

let ordinary_function_slot_not_bijective =
  replace_once ordinary_slots ~pattern:"%project_function_slot.[f].[g]"
    ~with_:"%project_function_slot.[f].[f]"

let mixed_duplicate_values =
  replace_once duplicate_values ~pattern:"sx = 0; sy = 0"
    ~with_:"sx = 0; sy = 0; sz = 0"

let mixed_duplicate_values_reordered =
  replace_once mixed_duplicate_values ~pattern:"x = sx; y = sy"
    ~with_:"y = sy; x = sx"

let mixed_duplicate_values_changed =
  replace_once mixed_duplicate_values_reordered ~pattern:"sz = 0"
    ~with_:"sz = 1"

let missing_sy =
  replace_once duplicate_values ~pattern:"sx = 0; sy = 0" ~with_:"sx = 0"

let missing_sx =
  replace_once duplicate_values ~pattern:"sx = 0; sy = 0" ~with_:"sy = 0"

let mixed_kinds =
  base
  |> replace_once ~pattern:"y : val" ~with_:"y : float"
  |> replace_once ~pattern:"sy = 1" ~with_:"sy : float = 1.0"

let mixed_kinds_different_body =
  replace_once mixed_kinds ~pattern:"cont k (x)" ~with_:"cont k (0)"

let cyclic =
  replace_once base ~pattern:"sx = 0; sy = 1" ~with_:"sx = $f; sy = $f"

let conflicting_synthetic_values =
  replace_once base ~pattern:"sx = 0; sy = 1" ~with_:"sx = 0; sx = 1; sy = 1"

let duplicate_specialised_param =
  replace_once base ~pattern:"x = sx; y = sy" ~with_:"x = sx; x = sy"

let local_site =
  replace_once base ~pattern:"let $camlCompare ="
    ~with_:
      "let f_local =\n\
      \  closure specialisation_site f &toplevel &toplevel synthetic { sx = 0 }\n\
       in\n\
       let $camlCompare ="

let mismatched_slot_kind =
  replace_once base ~pattern:"y : val" ~with_:"y : float"

let synthetic_projection =
  replace_once base ~pattern:"let $camlCompare ="
    ~with_:"let v = %project_value_slot.[f].[sx] ($f) in\nlet $camlCompare ="

let cyclic_renamed =
  cyclic
  |> replace_all ~pattern:"$f" ~with_:"$g"
  |> replace_all ~pattern:"sx" ~with_:"su"
  |> replace_all ~pattern:"sy =" ~with_:"sv ="
  |> replace_all ~pattern:"= sy" ~with_:"= sv"

let with_text text ~f =
  let dir = Filename.temp_dir "specialised_params_test" "" in
  (* A fixed basename puts all inputs in the same compilation unit,
     [Compare]. *)
  let filename = Filename.concat dir "compare.fl" in
  Misc.try_finally
    ~always:(fun () ->
      Misc.remove_file filename;
      Sys.rmdir dir)
    (fun () ->
      Env.set_current_unit (Parse_flambda.make_unit_info ~filename);
      Out_channel.with_open_text filename (fun out -> output_string out text);
      f filename)

let parse text =
  with_text text ~f:(fun filename ->
      match Parse_flambda.parse filename with
      | Ok unit -> unit
      | Error _ -> Misc.fatal_errorf "Could not parse:@ %s" text)

let parse_fexpr text =
  with_text text ~f:(fun filename ->
      match Parse_flambda.parse_fexpr filename with
      | Ok unit -> unit
      | Error _ -> Misc.fatal_errorf "Could not parse:@ %s" text)

type summary =
  { sites : int;
    specialised_params : int;
    synthetic_value_slots : int
  }

let add_fun_decl summary (decl : Fexpr.fun_decl) =
  { summary with
    sites = (summary.sites + if decl.is_specialisation_site then 1 else 0);
    synthetic_value_slots =
      (summary.synthetic_value_slots
      +
      match decl.synthetic_value_slots with
      | None -> 0
      | Some slots -> List.length slots)
  }

let rec summarise summary (expr : Fexpr.expr) =
  match expr with
  | Let { bindings; value_slots = _; body } ->
    let summary =
      List.fold_left
        (fun summary ({ defining_expr; var = _ } : Fexpr.let_binding) ->
          match defining_expr with
          | Closure decl -> add_fun_decl summary decl
          | Simple _ | Prim _ | Rec_info _ -> summary)
        summary bindings
    in
    summarise summary body
  | Let_cont { recursive = _; body; bindings } ->
    List.fold_left
      (fun summary ({ handler; _ } : Fexpr.continuation_binding) ->
        summarise summary handler)
      (summarise summary body) bindings
  | Let_symbol { bindings; value_slots = _; body } ->
    let summary =
      List.fold_left
        (fun summary (binding : Fexpr.symbol_binding) ->
          match binding with
          | Code code ->
            let summary =
              { summary with
                specialised_params =
                  summary.specialised_params
                  + List.length code.params_and_body.specialised_params
              }
            in
            summarise summary code.params_and_body.body
          | Closure { fun_decl; symbol = _ } -> add_fun_decl summary fun_decl
          | Set_of_closures { bindings; elements = _ } ->
            List.fold_left
              (fun summary ({ fun_decl; _ } : Fexpr.static_closure_binding) ->
                add_fun_decl summary fun_decl)
              summary bindings
          | Data _ | Deleted_code _ -> summary)
        summary bindings
    in
    summarise summary body
  | Switch { scrutinee = _; cases } ->
    List.fold_left
      (fun summary (_, (cont : Fexpr.apply_or_inlined_cont)) ->
        match cont with
        | Inlined_goto expr -> summarise summary expr
        | Named_cont _ -> summary)
      summary cases
  | Apply _ | Apply_cont _ | Invalid _ -> summary

let summary_of_unit (unit : Fexpr.flambda_unit) =
  summarise
    { sites = 0; specialised_params = 0; synthetic_value_slots = 0 }
    unit.body

let failures = ref 0

let fail fmt =
  Format.kfprintf
    (fun ppf ->
      Format.fprintf ppf "@.";
      incr failures)
    Format.err_formatter ("FAIL " ^^ fmt)

(* As for [fldiff], the approximant returned is that of the second unit. *)
let compare ~left ~right = Compare.flambda_units (parse right) (parse left)

let check_equivalent name ~left ~right =
  match compare ~left ~right with
  | Equivalent -> ()
  | Different _ -> fail "%s: expected equivalent" name

let check_different name ~left ~right =
  match compare ~left ~right with
  | Different _ -> ()
  | Equivalent -> fail "%s: expected different" name

let check_both_directions ~expected name left right =
  let check = if expected then check_equivalent else check_different in
  check name ~left ~right;
  check (name ^ " (reverse)") ~left:right ~right:left

let equivalent = check_both_directions ~expected:true

let different = check_both_directions ~expected:false

let code_in_recursive_handler () =
  let unit =
    parse
      {|let code size(1) g (x : val)
          my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
(apply direct(g &toplevel) (0) -> finish * error)
where finish (result : val) =
  let $camlCompare = Block 0 (result) in
  cont done ($camlCompare)
|}
  in
  let open Flambda in
  match Expr.descr (Flambda_unit.body unit) with
  | Let code_binding ->
    Let.pattern_match code_binding ~f:(fun bound_static ~body ->
        match Expr.descr body with
        | Let_cont (Non_recursive { handler; _ }) ->
          Non_recursive_let_cont_handler.pattern_match handler
            ~f:(fun finish ~body:call ->
              let start = Continuation.create ~name:"start" () in
              let use = Continuation.create ~name:"use" () in
              let jump cont =
                Apply_cont.create cont ~args:[] ~dbg:Debuginfo.none
                |> Expr.create_apply_cont
              in
              let no_params =
                Flambda2_bound_identifiers.Bound_parameters.empty
              in
              let make_handler body =
                Continuation_handler.create no_params ~handler:body
                  ~free_names_of_handler:Unknown ~is_exn_handler:false
                  ~is_cold:false
              in
              let start_handler =
                Let.create bound_static
                  (Let.defining_expr code_binding)
                  ~body:(jump use) ~free_names_of_body:Unknown
                |> Expr.create_let |> make_handler
              in
              (* [g] dominates its use, but [use] precedes [start] in storage
                 order. Comparing [use] first cannot yet match the code IDs. *)
              let body =
                Let_cont.create_recursive ~invariant_params:no_params
                  (Continuation.Lmap.of_list
                     [use, make_handler call; start, start_handler])
                  ~body:(jump start)
              in
              let body =
                Let_cont.create_non_recursive finish
                  (Non_recursive_let_cont_handler.handler handler)
                  ~body ~free_names_of_body:Unknown
              in
              Flambda_unit.with_body unit body)
        | Let_cont (Recursive _)
        | Let _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
          Misc.fatal_error "Expected the finish continuation")
  | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
    Misc.fatal_error "Expected the code binding"

let code_in_nested_recursive_handlers () =
  let open Flambda2_bound_identifiers in
  let open Flambda2_kinds in
  let text =
    {|let code size(1) g (x : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let code size(1) h (x : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
(apply direct(h &toplevel) (0) -> finish * error)
where finish (result : val) =
  let $camlCompare = Block 0 (result) in
  cont done ($camlCompare)
|}
  in
  let unit = parse text in
  let open Flambda in
  let let_exn expr =
    match Expr.descr expr with
    | Let t -> t
    | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
      Misc.fatal_error "Expected a let binding"
  in
  let g_binding = let_exn (Flambda_unit.body unit) in
  Let.pattern_match g_binding ~f:(fun g_pattern ~body ->
      let g_id =
        match Let.defining_expr g_binding with
        | Static_consts group ->
          Code.code_id
            (Option.get
               (Static_const_or_code.to_code
                  (List.hd (Static_const_group.to_list group))))
        | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ ->
          Misc.fatal_error "Expected code"
      in
      let h_binding = let_exn body in
      Let.pattern_match h_binding ~f:(fun h_pattern ~body ->
          let finish_handler =
            match Expr.descr body with
            | Let_cont (Non_recursive { handler; _ }) -> handler
            | Let_cont (Recursive _)
            | Let _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
              Misc.fatal_error "Expected the finish continuation"
          in
          Non_recursive_let_cont_handler.pattern_match finish_handler
            ~f:(fun finish ~body:call ->
              let start = Continuation.create ~name:"start" () in
              let use = Continuation.create ~name:"use" () in
              let inner_start = Continuation.create ~name:"inner_start" () in
              let inner_use = Continuation.create ~name:"inner_use" () in
              let no_params = Bound_parameters.empty in
              let jump cont =
                Expr.create_apply_cont
                  (Apply_cont.create cont ~args:[] ~dbg:Debuginfo.none)
              in
              let handler ?(params = no_params) body =
                Continuation_handler.create params ~handler:body
                  ~free_names_of_handler:Unknown ~is_exn_handler:false
                  ~is_cold:false
              in
              let h_call =
                match Expr.descr call with
                | Apply a -> a
                | Let _ | Let_cont _ | Apply_cont _ | Switch _ | Invalid _ ->
                  Misc.fatal_error "Expected an apply"
              in
              let ignored = Variable.create "ignored" Flambda_kind.value in
              let inner_start_handler =
                Let.create h_pattern
                  (Let.defining_expr h_binding)
                  ~body:(jump inner_use) ~free_names_of_body:Unknown
                |> Expr.create_let
                |> handler
                     ~params:
                       (Bound_parameters.create
                          [ Bound_parameter.create ignored
                              Flambda_kind.With_subkind.any_value
                              Flambda_debug_uid.none ])
              in
              let g_call =
                let call =
                  Apply.with_call_kind h_call
                    (Call_kind.direct_function_call g_id)
                in
                Apply.with_continuation call (Return inner_start)
                |> Expr.create_apply
              in
              (* [g] gates traversal of the inner handlers, where [h] is only
                 discovered after its use. This needs a third pass. *)
              let inner_handlers =
                Continuation.Lmap.of_list
                  [inner_use, handler call; inner_start, inner_start_handler]
              in
              let use_handler =
                Let_cont.create_recursive ~invariant_params:no_params
                  inner_handlers ~body:g_call
                |> handler
              in
              let start_handler =
                Let.create g_pattern
                  (Let.defining_expr g_binding)
                  ~body:(jump use) ~free_names_of_body:Unknown
                |> Expr.create_let |> handler
              in
              let body =
                Let_cont.create_recursive ~invariant_params:no_params
                  (Continuation.Lmap.of_list
                     [use, use_handler; start, start_handler])
                  ~body:(jump start)
              in
              let body =
                Let_cont.create_non_recursive finish
                  (Non_recursive_let_cont_handler.handler finish_handler)
                  ~body ~free_names_of_body:Unknown
              in
              Flambda_unit.with_body unit body)))

let () =
  List.iter
    (fun (name, make) ->
      let left = make () in
      let right = make () in
      List.iter
        (fun (left, right) ->
          match Compare.flambda_units left right with
          | Equivalent -> ()
          | Different _ -> fail "%s: expected equivalent" name)
        [left, right; right, left])
    [ "dominator-scoped code", code_in_recursive_handler;
      "nested dominator-scoped code", code_in_nested_recursive_handlers ]

let () =
  different "missing specialised parameters" base missing;
  different "missing one specialised parameter" base missing_one;
  different "swapped specialised parameters" base swapped;
  different "ordinary versus synthetic slots" ordinary missing;
  equivalent "alpha-renamed parameters and slots" base renamed;
  equivalent "reordered annotation entries" base reordered;
  different "specialisation-site marker" base site;
  different "empty specialisation-site marker" empty empty_site;
  equivalent "reordered equal-valued slots" duplicate_values
    duplicate_values_reordered;
  equivalent "mapped and unmapped equal-valued slots" mixed_duplicate_values
    mixed_duplicate_values_reordered;
  different "changed unmapped slot alongside equal-valued slots"
    mixed_duplicate_values mixed_duplicate_values_changed;
  different "mapped slots missing from the opposite set" missing_sy missing_sx;
  equivalent "alpha-renamed cyclic synthetic slots" cyclic cyclic_renamed;
  equivalent "equal-valued slots before swapped annotations" early_slots
    early_slots_swapped;
  equivalent "indistinguishable slots" indistinguishable_slots
    indistinguishable_slots_renamed;
  different "different numbers of indistinguishable slots"
    indistinguishable_slots
    (replace_once indistinguishable_slots ~pattern:"sx = 0; sy = 0"
       ~with_:"sx = 0");
  equivalent "ordinary slot alpha-renaming" ordinary_slots
    ordinary_slots_renamed;
  different "ordinary value-slot bijection" ordinary_slots
    ordinary_value_slot_not_bijective;
  different "ordinary function-slot bijection" ordinary_slots
    ordinary_function_slot_not_bijective

(* Approximants must preserve annotations as well as alpha-equivalence. *)
let check_approximant name ~original ~changed =
  match compare ~left:original ~right:changed with
  | Equivalent -> fail "%s: expected different" name
  | Different { approximant } ->
    let printed =
      Flambda2_ui.Flambda_colours.without_colours ~f:(fun () ->
          Format.asprintf "%a" Print_fexpr.flambda_unit
            (Flambda_to_fexpr.conv approximant))
    in
    let expected = summary_of_unit (parse_fexpr changed) in
    let actual = summary_of_unit (parse_fexpr printed) in
    if expected.sites <> actual.sites
    then fail "%s: approximant changed the specialisation-site markers" name;
    if expected.specialised_params <> actual.specialised_params
    then fail "%s: approximant changed the specialised parameters" name;
    if expected.synthetic_value_slots <> actual.synthetic_value_slots
    then fail "%s: approximant changed the synthetic value slots" name;
    (match Compare.flambda_units approximant (parse changed) with
    | Equivalent -> ()
    | Different _ ->
      fail "%s: approximant is not equivalent to the second unit" name);
    check_equivalent
      (name ^ " (printed approximant)")
      ~left:changed ~right:printed

let check_approximant_both_directions name original changed =
  check_approximant name ~original ~changed;
  check_approximant (name ^ " (reverse)") ~original:changed ~changed:original

let () =
  check_approximant_both_directions "different body" base different_body;
  check_approximant_both_directions "alpha-renamed different body" base
    renamed_different_body;
  check_approximant_both_directions "specialisation-site different body" site
    site_different_body;
  check_approximant_both_directions "empty specialisation-site roundtrip"
    empty_site empty_site_different_body;
  check_approximant_both_directions "missing annotation" missing base;
  check_approximant_both_directions "missing one annotation" missing_one base;
  check_approximant_both_directions "swapped annotations" base swapped;
  check_approximant_both_directions "marker difference" base site;
  check_approximant_both_directions "empty marker difference" empty empty_site;
  check_approximant_both_directions "equal-valued slot approximant" missing_one
    duplicate_values_reordered;
  check_approximant_both_directions "changed unmapped slot approximant"
    mixed_duplicate_values mixed_duplicate_values_changed;
  check_approximant_both_directions "missing slot approximant" missing_sy
    missing_sx;
  check_approximant_both_directions "mixed value/float slot roundtrip"
    mixed_kinds mixed_kinds_different_body;
  let early_slots_changed =
    replace_once early_slots_swapped ~pattern:"cont k (x)" ~with_:"cont k (y)"
  in
  check_approximant_both_directions "early equal-valued slot approximant"
    early_slots early_slots_changed;
  check_approximant_both_directions "indistinguishable slot approximant"
    indistinguishable_slots
    (replace_once indistinguishable_slots_renamed ~pattern:"cont k (x)"
       ~with_:"cont k (y)")

let check_parse_error name text ~message =
  let ppf = Format.err_formatter in
  Format.pp_print_flush ppf ();
  let errors = Buffer.create 128 in
  let out_functions = Format.pp_get_formatter_out_functions ppf () in
  Format.pp_set_formatter_out_functions ppf
    { out_functions with
      out_string = Buffer.add_substring errors;
      out_flush = (fun () -> ())
    };
  let rejected =
    Misc.try_finally
      ~always:(fun () ->
        Format.pp_print_flush ppf ();
        Format.pp_set_formatter_out_functions ppf out_functions)
      (fun () ->
        match parse text with exception Misc.Fatal_error -> true | _ -> false)
  in
  if not rejected
  then fail "%s: was accepted" name
  else if occurrences (Buffer.contents errors) message = []
  then fail "%s: unexpected error: %s" name (Buffer.contents errors)

let () =
  check_parse_error "marked site with ordinary value slots" malformed_site
    ~message:"A specialisation site cannot have runtime value slots";
  check_parse_error "conflicting synthetic values" conflicting_synthetic_values
    ~message:"Synthetic value slot sx is defined more than once";
  check_parse_error "duplicate specialised parameter"
    duplicate_specialised_param
    ~message:"Specialised parameter x is given more than once";
  check_parse_error "locally allocated site" local_site
    ~message:"A specialisation site must have heap allocation mode";
  check_parse_error "explicit slot kind mismatch" mismatched_slot_kind
    ~message:"does not match kind";
  check_parse_error "synthetic slot projection" synthetic_projection
    ~message:"is used both as a synthetic and as an ordinary value slot"

(* Sites have no allocation cost, but duplicating their live code has a cost. *)
let () =
  let open Flambda2_kinds in
  let compilation_unit = Current_unit.get_cu_exn () in
  let code_id name =
    Code_id.create ~name ~debug:Debuginfo.none compilation_unit
  in
  let slot name =
    Function_slot.create compilation_unit ~name ~is_always_immediate:false
      Flambda_kind.value
  in
  let f = code_id "cost_f" in
  let g = code_id "cost_g" in
  let f_slot = slot "cost_f" in
  let g_slot = slot "cost_g" in
  let dead_slot = slot "cost_dead" in
  let f_cost =
    Cost_metrics.from_size (Code_size.of_int 7)
    |> Cost_metrics.notify_removed ~operation:Removed_operations.call
  in
  let g_cost =
    Cost_metrics.from_size (Code_size.of_int 11)
    |> Cost_metrics.notify_removed ~operation:Removed_operations.branch
  in
  let characteristics =
    Code_id.Map.of_list
      [ (f, Cost_metrics.{ cost_metrics = f_cost; function_slot_size = 2 });
        (g, Cost_metrics.{ cost_metrics = g_cost; function_slot_size = 3 }) ]
  in
  let live code_id : Function_declarations.code_id_in_function_declaration =
    Code_id { code_id; only_full_applications = true }
  in
  let deleted : Function_declarations.code_id_in_function_declaration =
    Deleted { function_slot_size = 3; dbg = Debuginfo.none }
  in
  let cost ~is_specialisation_site ?(value_slots = Value_slot.Map.empty) decls =
    let set =
      Set_of_closures.create ~is_specialisation_site
        ~synthetic_value_slots:Value_slot.Map.empty ~value_slots
        (Function_declarations.create (Function_slot.Lmap.of_list decls))
    in
    Cost_metrics.set_of_closures
      ~find_code_characteristics:(fun code_id ->
        Code_id.Map.find code_id characteristics)
      set
  in
  let check name expected actual =
    if not (Cost_metrics.equal expected actual)
    then
      fail "%s: expected %a, got %a" name Cost_metrics.print expected
        Cost_metrics.print actual
  in
  check "site charges both live bodies"
    (Cost_metrics.( + ) f_cost g_cost)
    (cost ~is_specialisation_site:true
       [f_slot, live f; g_slot, live g; dead_slot, deleted]);
  check "site does not charge deleted bodies" f_cost
    (cost ~is_specialisation_site:true
       [f_slot, live f; g_slot, deleted; dead_slot, deleted]);
  check "all-deleted site costs nothing" Cost_metrics.zero
    (cost ~is_specialisation_site:true [f_slot, deleted; g_slot, deleted]);
  let value_slot =
    Value_slot.create compilation_unit ~name:"captured"
      ~is_always_immediate:false Flambda_kind.value
  in
  let captured = Variable.create "captured" Flambda_kind.value in
  let value_slots =
    Value_slot.Map.singleton value_slot
      (Flambda2_term_basics.Simple.var captured)
  in
  let allocation_cost =
    Cost_metrics.from_size
      (Code_size.( + ) Code_size.alloc_size (Code_size.of_int 3))
  in
  check "ordinary closure charges allocation"
    (Cost_metrics.( + ) f_cost allocation_cost)
    (cost ~is_specialisation_site:false ~value_slots [f_slot, live f])

let () =
  if !failures > 0
  then (
    Format.eprintf "%d specialisation checks failed@." !failures;
    exit 1)
  else Format.printf "Specialisation checks passed@."
