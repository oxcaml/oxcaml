(* Check specialisation parsing, comparison, approximants and costs. *)

open Import

let base =
  {|let code size(1) f (x : val, y : val)
      specialised { x = sx; y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let site = closure specialisation_site f &toplevel
  synthetic { sx = 0; sy = 1 }
in
let $camlCompare = Block 0 () in
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

let unmark_site text =
  replace_once text ~pattern:"closure specialisation_site f" ~with_:"closure f"

let ordinary =
  unmark_site missing |> replace_once ~pattern:" synthetic {" ~with_:" with {"

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

let empty_site =
  replace_once missing ~pattern:" synthetic { sx = 0; sy = 1 }" ~with_:""

let empty = unmark_site empty_site

let empty_site_different_body =
  replace_once empty_site ~pattern:"cont k (x)" ~with_:"cont k (y)"

let malformed_site = mark_site ordinary

let duplicate_values = replace_once base ~pattern:"sy = 1" ~with_:"sy = 0"

let duplicate_values_reordered =
  replace_once duplicate_values ~pattern:"x = sx; y = sy"
    ~with_:"y = sy; x = sx"

let early_slots =
  {|let code size(1) before (unused : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  let site = closure specialisation_site f &my_alloc_region
    synthetic { sx = 0; sy = 0 }
  in
  cont k (0)
and code size(1) f (x : val, y : val)
      specialised { x = sx; y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let $camlCompare = Block 0 () in
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
  base
  |> replace_once ~pattern:"sx = 0; sy = 1" ~with_:"sx = $f; sy = $f"
  |> replace_once ~pattern:"let site ="
       ~with_:"let $f = closure f &toplevel with { cycle = $f } in\nlet site ="

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
    ~with_:"let v = %project_value_slot.[f].[sx] (site) in\nlet $camlCompare ="

let cyclic_renamed =
  cyclic
  |> replace_all ~pattern:"$f" ~with_:"$g"
  |> replace_all ~pattern:"sx" ~with_:"su"
  |> replace_all ~pattern:"sy =" ~with_:"sv ="
  |> replace_all ~pattern:"= sy" ~with_:"= sv"

let overlapping_slots =
  {|let code size(1) f (x : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
let code size(20) outer (x : val)
      my_closure &my_alloc_region my_depth -> k * e : val =
  let first = closure specialisation_site f &my_alloc_region
    synthetic { sa = 0; sb = 0 }
  in
  let second = closure specialisation_site f &my_alloc_region
    synthetic { sa = 0; sc = 0 }
  in
  (apply direct(f &my_alloc_region) first (x) -> after * e)
    where after (y : val) =
      apply direct(f &my_alloc_region) second (y) -> k * e
in
let $outer = closure outer &toplevel in
let $camlCompare = Block 0 ($outer) in
cont done ($camlCompare)
|}

let overlapping_slots_reordered =
  replace_once overlapping_slots ~pattern:"sa = 0; sb = 0"
    ~with_:"sb = 0; sa = 0"

let disjoint_slots =
  replace_once overlapping_slots ~pattern:"sa = 0; sc = 0"
    ~with_:"sd = 0; sc = 0"

let split_slots =
  {|let code size(1) f (x : val) specialised { x = sx }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
and code size(1) g (y : val) specialised { y = sy }
      my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (y)
in
let first = closure specialisation_site f &toplevel synthetic { sx = 0 }
and second = closure g &toplevel synthetic { sy = 1 }
in
let $camlCompare = Block 0 () in
cont done ($camlCompare)
|}

let combined_slots =
  split_slots
  |> replace_once ~pattern:"synthetic { sx = 0 }"
       ~with_:"synthetic { sx = 0; sy = 1 }"
  |> replace_once ~pattern:" synthetic { sy = 1 }" ~with_:""

let deleted_siblings =
  replace_once base
    ~pattern:
      "let site = closure specialisation_site f &toplevel\n\
      \  synthetic { sx = 0; sy = 1 }"
    ~with_:
      "let first = closure specialisation_site deleted size(2) @first &toplevel\n\
      \  synthetic { sx = 0; sy = 1 }\n\
       and middle = closure f @middle &toplevel\n\
       and last = closure deleted size(3) @last &toplevel"

let all_deleted =
  replace_once deleted_siblings ~pattern:"closure f @middle"
    ~with_:"closure deleted size(4) @middle"

let static_deleted_siblings =
  deleted_siblings
  |> replace_once ~pattern:"closure specialisation_site deleted"
       ~with_:"closure deleted"
  |> replace_once ~pattern:"  synthetic { sx = 0; sy = 1 }" ~with_:""
  |> replace_once ~pattern:"let first =" ~with_:"let $first ="
  |> replace_once ~pattern:"and middle =" ~with_:"and $middle ="
  |> replace_once ~pattern:"and last =" ~with_:"and $last ="

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
      | Error error ->
        Test_utils.dump_error error;
        Misc.fatal_errorf "Could not parse:@ %s" text)

let parse_fexpr text =
  with_text text ~f:(fun filename ->
      match Parse_flambda.parse_fexpr filename with
      | Ok unit -> unit
      | Error error ->
        Test_utils.dump_error error;
        Misc.fatal_errorf "Could not parse:@ %s" text)

type summary =
  { sites : int;
    specialised_params : int;
    synthetic_value_slots : int;
    declarations : Fexpr.fun_decl list
  }

let add_fun_decl summary (decl : Fexpr.fun_decl) =
  { summary with
    sites = (summary.sites + if decl.is_specialisation_site then 1 else 0);
    synthetic_value_slots =
      (summary.synthetic_value_slots
      +
      match decl.synthetic_value_slots with
      | None -> 0
      | Some slots -> List.length slots);
    declarations = decl :: summary.declarations
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
    { sites = 0;
      specialised_params = 0;
      synthetic_value_slots = 0;
      declarations = []
    }
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

let binding_in_recursive_handler text =
  let unit = parse text in
  let open Flambda in
  match Expr.descr (Flambda_unit.body unit) with
  | Let binding ->
    Let.pattern_match binding ~f:(fun bound_static ~body ->
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
                  (Let.defining_expr binding)
                  ~body:(jump use) ~free_names_of_body:Unknown
                |> Expr.create_let |> make_handler
              in
              (* The binding dominates its use, but [use] precedes [start] in
                 storage order. Its correspondence is discovered later. *)
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
    Misc.fatal_error "Expected a static binding"

let code_in_recursive_handler () =
  binding_in_recursive_handler
    {|let code size(1) g (x : val)
          my_closure &my_alloc_region my_depth -> k * e : val =
  cont k (x)
in
(apply direct(g &toplevel) (0) -> finish * error)
where finish (result : val) =
  let $camlCompare = Block 0 (result) in
  cont done ($camlCompare)
|}

let symbol_in_recursive_handler () =
  binding_in_recursive_handler
    {|let $captured = Block 0 (0) in
(let site = closure specialisation_site deleted size(2) @dead &toplevel
   synthetic { sx = $captured; sy = $captured }
 in
 cont finish (0))
where finish (result : val) =
  let $camlCompare = Block 0 (result) in
  cont done ($camlCompare)
|}

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
              (* The unknown correspondence for [g] must not skip the inner
                 handlers, where [h] is only discovered after its use. *)
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
      "dominator-scoped synthetic slot values", symbol_in_recursive_handler;
      "nested dominator-scoped code", code_in_nested_recursive_handlers ]

let () =
  different "missing specialised parameters" base missing;
  different "missing one specialised parameter" base missing_one;
  different "swapped specialised parameters" base swapped;
  different "ordinary versus synthetic slots" ordinary missing;
  equivalent "alpha-renamed parameters and slots" base renamed;
  equivalent "reordered annotation entries" base reordered;
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
    ordinary_function_slot_not_bijective;
  equivalent "overlapping equal-valued slots" overlapping_slots
    overlapping_slots_reordered;
  different "overlapping versus disjoint slots" overlapping_slots disjoint_slots;
  equivalent "split synthetic declarations" split_slots combined_slots;
  different "deleted slot size" deleted_siblings
    (replace_once deleted_siblings ~pattern:"deleted size(3)"
       ~with_:"deleted size(4)")

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
  check_approximant_both_directions "empty specialisation-site roundtrip"
    empty_site empty_site_different_body;
  check_approximant_both_directions "missing annotation" missing base;
  check_approximant_both_directions "missing one annotation" missing_one base;
  check_approximant_both_directions "swapped annotations" base swapped;
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
       ~with_:"cont k (y)");
  check_approximant_both_directions "overlapping slot approximant"
    overlapping_slots
    (replace_once overlapping_slots_reordered ~pattern:"cont k (x)"
       ~with_:"cont k (0)");
  check_approximant_both_directions "different overlap approximant"
    overlapping_slots disjoint_slots;
  check_approximant_both_directions "deleted sibling approximant"
    deleted_siblings
    (replace_once deleted_siblings ~pattern:"deleted size(3)"
       ~with_:"deleted size(4)")

let check_deleted_roundtrip name text =
  let original = parse text in
  let converted = Flambda_to_fexpr.conv original in
  let printed =
    Flambda2_ui.Flambda_colours.without_colours ~f:(fun () ->
        Format.asprintf "%a" Print_fexpr.flambda_unit converted)
  in
  let signature unit =
    (summary_of_unit unit).declarations
    |> List.rev_map (fun (decl : Fexpr.fun_decl) ->
        let slot =
          match decl.function_slot, decl.code_id with
          | Some slot, _ | None, Code_id slot -> slot.txt
          | None, Deleted _ ->
            Misc.fatal_error "Deleted declaration lost its function slot"
        in
        let size =
          match decl.code_id with
          | Code_id _ -> None
          | Deleted { function_slot_size; dbg } ->
            if Debuginfo.is_none dbg
            then fail "%s: deleted declaration lost its source location" name;
            Some function_slot_size
        in
        slot, size)
  in
  let expected = signature (parse_fexpr text) in
  let equal =
    List.equal (fun (slot1, size1) (slot2, size2) ->
        String.equal slot1 slot2 && Option.equal Int.equal size1 size2)
  in
  List.iter
    (fun unit ->
      if not (equal expected (signature unit))
      then fail "%s: changed function-slot order or sizes" name)
    [converted; parse_fexpr printed; Flambda_to_fexpr.conv (parse printed)];
  check_equivalent name ~left:text ~right:printed

let () =
  check_deleted_roundtrip "deleted siblings" deleted_siblings;
  check_deleted_roundtrip "entirely deleted site" all_deleted;
  check_deleted_roundtrip "static deleted siblings" static_deleted_siblings

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
  List.iter
    (fun value ->
      check_parse_error "duplicate split synthetic declarations"
        (replace_once split_slots ~pattern:"synthetic { sy = 1 }"
           ~with_:("synthetic { sy = 1; sx = " ^ value ^ " }"))
        ~message:"Synthetic value slot sx is defined more than once")
    ["0"; "1"];
  let ordinary_synthetic = unmark_site base in
  let mixed =
    ordinary_synthetic
    |> replace_once ~pattern:"synthetic { sx = 0; sy = 1 }"
         ~with_:"synthetic { sx = 0; sy = 1 } with { runtime = 2 }"
    |> replace_once ~pattern:"cont k (x)"
         ~with_:
           "let r = %project_value_slot.[f].[runtime] (my_closure) in\n\
           \  let result = %int_barith.add (x, r) in cont k (result)"
  in
  List.iter
    (fun (name, text) ->
      let printed =
        Flambda2_ui.Flambda_colours.without_colours ~f:(fun () ->
            Format.asprintf "%a" Print_fexpr.flambda_unit
              (Flambda_to_fexpr.conv (parse text)))
      in
      check_equivalent name ~left:text ~right:printed)
    [ "ordinary synthetic slots", ordinary_synthetic;
      "mixed runtime and synthetic slots", mixed;
      ( "static mixed slots",
        replace_once mixed ~pattern:"let site =" ~with_:"let $site =" ) ];
  check_parse_error "static specialisation site"
    (replace_once base ~pattern:"let site =" ~with_:"let $site =")
    ~message:"A specialisation site must be dynamically bound";
  check_parse_error "explicit static specialisation site"
    (base
    |> replace_once ~pattern:"let site =" ~with_:"let set_of_closures $site ="
    |> replace_once ~pattern:"synthetic { sx = 0; sy = 1 }"
         ~with_:"synthetic { sx = 0; sy = 1 } end")
    ~message:"A specialisation site must be dynamically bound";
  check_parse_error "empty static specialisation site"
    (replace_once empty_site ~pattern:"let site =" ~with_:"let $site =")
    ~message:"A specialisation site must be dynamically bound";
  check_parse_error "deleted function without a slot"
    (replace_once deleted_siblings ~pattern:" @first" ~with_:"")
    ~message:"A deleted function declaration must specify a function slot";
  check_parse_error "invalid deleted slot size"
    (replace_once deleted_siblings ~pattern:"deleted size(2)"
       ~with_:"deleted size(0)")
    ~message:"Deleted function slot size must be positive";
  check_parse_error "duplicate specialised parameter"
    duplicate_specialised_param
    ~message:"Specialised parameter x is given more than once";
  check_parse_error "locally allocated site" local_site
    ~message:"A specialisation site must have heap allocation mode";
  check_parse_error "explicit slot kind mismatch" mismatched_slot_kind
    ~message:"does not match kind";
  check_parse_error "synthetic slot projection" synthetic_projection
    ~message:"is used both as a synthetic and as an ordinary value slot"

(* Sites cost nothing; ordinary sets charge their bodies and allocation. *)
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
      Set_of_closures.create ~is_specialisation_site ~value_slots
        ~synthetic_value_slots:Value_slot.Map.empty
        (Function_declarations.create (Function_slot.Lmap.of_list decls))
    in
    Cost_metrics.set_of_closures
      ~find_code_characteristics:(fun code_id ->
        if is_specialisation_site
        then Misc.fatal_error "A site must not query generic body costs";
        Code_id.Map.find code_id characteristics)
      set
  in
  let check name expected actual =
    if not (Cost_metrics.equal expected actual)
    then
      fail "%s: expected %a, got %a" name Cost_metrics.print expected
        Cost_metrics.print actual
  in
  check "site costs nothing" Cost_metrics.zero
    (cost ~is_specialisation_site:true
       [f_slot, live f; g_slot, live g; dead_slot, deleted]);
  check "ordinary closed set charges its bodies"
    (Cost_metrics.( + )
       (Cost_metrics.( + ) f_cost g_cost)
       (Cost_metrics.from_size
          (Code_size.( + ) Code_size.alloc_size (Code_size.of_int 9))))
    (cost ~is_specialisation_site:false
       [f_slot, live f; g_slot, live g; dead_slot, deleted]);
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

(* Emission costs belong to fresh code bindings, not to their site or to the
   stored body metrics. Pending definitions can include dead/older versions. *)
let () =
  let module NO = Flambda2_nominal.Name_occurrences in
  let module NM = Flambda2_nominal.Name_mode in
  let module RSC = Rebuilt_static_const in
  let module LC = Lifted_constant in
  let module LCS = Lifted_constant_state in
  let unit =
    parse
      {|let code size(7) parent (x : val)
          my_closure &my_alloc_region my_depth -> k * e : val =
        apply direct(child &my_alloc_region) (x) -> k * e
      and code size(11) child (x : val)
          my_closure &my_alloc_region my_depth -> k * e : val =
        cont k (x)
      in
      let $camlCompare = Block 0 (0) in
      cont done ($camlCompare)
      |}
  in
  let codes =
    match Flambda.Expr.descr (Flambda_unit.body unit) with
    | Let binding -> (
      match Flambda.Let.defining_expr binding with
      | Static_consts group ->
        List.filter_map Flambda.Static_const_or_code.to_code
          (Flambda.Static_const_group.to_list group)
      | Simple _ | Prim _ | Set_of_closures _ | Rec_info _ ->
        Misc.fatal_error "Expected code definitions")
    | Let_cont _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
      Misc.fatal_error "Expected code definitions"
  in
  let find name =
    List.find
      (fun code -> String.equal (Code_id.name (Code.code_id code)) name)
      codes
  in
  let parent = find "parent" in
  let child = find "child" in
  let parent_id = Code.code_id parent in
  let child_id = Code.code_id child in
  (* Fexpr does not compute full body free names. Supply those of the direct
     call in [parent]; [child] has no free names. *)
  let parent =
    Code.with_params_and_body parent
      ~params_and_body:(Code.params_and_body parent)
      ~free_names_of_params_and_body:(NO.singleton_code_id child_id NM.normal)
      ~cost_metrics:(Code.cost_metrics parent)
  in
  let existing code =
    LC.create_code (Code.code_id code) (RSC.create_code' code)
  in
  let fresh code =
    LC.create_code (Code.code_id code)
      (RSC.charge_code_size (RSC.create_code' code))
  in
  let constants = LCS.singleton_list_of_constants [fresh parent; fresh child] in
  let roots = NO.singleton_code_id parent_id NM.normal in
  let check name expected constants roots =
    let actual = LCS.cost_metrics constants ~roots |> Cost_metrics.size in
    if not (Code_size.equal actual (Code_size.of_int expected))
    then
      fail "%s: expected size %d, got %a" name expected Code_size.print actual
  in
  let tracking =
    Oxcaml_flags.Flambda2.Inlining.speculative_inlining_track_lifted_constants
  in
  let previous = !tracking in
  Fun.protect
    ~finally:(fun () -> tracking := previous)
    (fun () ->
      List.iter
        (fun track ->
          tracking := track;
          check "nested generated bodies" 18 constants roots;
          check "duplicate pending definitions" 18
            (LCS.union constants constants)
            roots;
          check "dead parent, live child" 11 constants
            (NO.singleton_code_id child_id NM.normal);
          check "dead generated bodies" 0 constants NO.empty;
          if
            not
              (LCS.is_empty
                 (LCS.retain_reachable_for_speculation constants ~roots:NO.empty))
          then fail "Placement retained dependencies of an unused definition";
          let retained =
            LCS.retain_reachable_for_speculation constants ~roots
          in
          let placed_cost =
            LCS.fold retained ~init:Cost_metrics.zero ~f:(fun cost constant ->
                Cost_metrics.( + ) cost
                  (RSC.Group.cost_metrics_for_inlining
                     (LC.defining_exprs constant)))
          in
          if
            not
              (Code_size.equal
                 (Cost_metrics.size placed_cost)
                 (Code_size.of_int 18))
          then fail "Placement did not charge each retained body exactly once";
          check "phantom roots" 0 constants
            (NO.singleton_code_id parent_id NM.phantom);
          check "existing bodies are free" 0
            (LCS.singleton_list_of_constants [existing parent; existing child])
            roots;
          check "existing code reaches newly generated code" 11
            (LCS.singleton_list_of_constants [existing parent; fresh child])
            roots;
          let age_only =
            Code.with_params_and_body parent
              ~params_and_body:(Code.params_and_body child)
              ~free_names_of_params_and_body:NO.empty
              ~cost_metrics:(Code.cost_metrics parent)
            |> Code.with_newer_version_of (Some child_id)
          in
          check "age-only ancestor" 7
            (LCS.singleton_list_of_constants [fresh age_only; fresh child])
            roots)
        [false; true]);
  let function_slot =
    Function_slot.create
      (Current_unit.get_cu_exn ())
      ~name:"site" ~is_always_immediate:false Flambda2_kinds.Flambda_kind.value
  in
  let declarations =
    Function_declarations.create
      (Function_slot.Lmap.singleton function_slot
         (Function_declarations.Code_id
            { code_id = parent_id; only_full_applications = true }))
  in
  let slot =
    Value_slot.create ~is_synthetic:true
      (Current_unit.get_cu_exn ())
      ~name:"unused" ~is_always_immediate:false
      Flambda2_kinds.Flambda_kind.value
  in
  let unused = Variable.create "unused" Flambda2_kinds.Flambda_kind.value in
  let site =
    Set_of_closures.create ~is_specialisation_site:true
      ~value_slots:Value_slot.Map.empty
      ~synthetic_value_slots:
        (Value_slot.Map.singleton slot (Flambda2_term_basics.Simple.var unused))
      declarations
  in
  let alloc_mode =
    Flambda2_bound_identifiers.Alloc_mode.For_allocations.heap
      ~alloc_region:(Flambda_unit.toplevel_my_alloc_region unit)
  in
  let named = Flambda.Named.create_set_of_closures ~alloc_mode site in
  let site =
    Simplified_named.create_with_known_free_names
      ~machine_width:Target_system.Machine_width.Sixty_four named
      ~free_names:(Flambda.Named.free_names named)
      ~find_code_characteristics:(fun _ ->
        Misc.fatal_error "Site body cost lookup")
    |> Simplified_named.for_speculative_inlining
  in
  if NO.mem_var site.free_names unused
  then fail "Speculation retained a synthetic value";
  check "erased site does not root its declarations" 11 constants
    (NO.union site.free_names (NO.singleton_code_id child_id NM.normal))

let () =
  if !failures > 0
  then (
    Format.eprintf "%d specialisation checks failed@." !failures;
    exit 1)
  else Format.printf "Specialisation checks passed@."
