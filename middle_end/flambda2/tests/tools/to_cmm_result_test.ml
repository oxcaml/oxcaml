open Flambda2_identifiers

let failures = ref 0

let fail fmt =
  Format.kfprintf
    (fun ppf ->
      Format.fprintf ppf "@.";
      incr failures)
    Format.err_formatter ("FAIL " ^^ fmt)

(* Check local site pruning, infix references and exported sites. *)
let () =
  let module R = Flambda2_to_cmm.To_cmm_result in
  let module N = Flambda2_nominal.Name_occurrences in
  let compilation_unit =
    Compilation_unit.create Compilation_unit.Prefix.empty
      (Compilation_unit.Name.of_string "To_cmm_result_test")
  in
  Env.set_current_unit
    (Unit_info.make_dummy ~input_name:"to_cmm_result_test" compilation_unit);
  let module_symbol =
    Symbol.create compilation_unit
      (Linkage_name.of_string "camlTo_cmm_result_test")
  in
  let make_symbol name =
    Symbol.manufacture (Symbol.compilation_unit module_symbol) name
  in
  let empty = R.create ~module_symbol ~reachable_names:N.empty in
  let site_symbol = make_symbol "site" in
  let infix_symbol = make_symbol "site_infix" in
  let ordinary_symbol = make_symbol "ordinary" in
  let site = R.symbol empty site_symbol in
  let infix = R.symbol empty infix_symbol in
  let ordinary = R.symbol empty ordinary_symbol in
  let code = Cmm.global_symbol "callee_code" in
  let group first second =
    Cmm.
      [ Cint 6135n;
        Cdefine_symbol first;
        Csymbol_address code;
        Cint 1n;
        Cint 3321n;
        Cdefine_symbol second;
        Csymbol_address code;
        Cint 1n ]
  in
  let site_data = group site infix in
  let ordinary_data =
    Cmm.[Cint 3063n; Cdefine_symbol ordinary; Csymbol_address code; Cint 1n]
  in
  let res = R.add_specialisation_site_data empty site_data in
  let fundecl body : Cmm.fundecl =
    { fun_name = Cmm.global_symbol "test_function";
      fun_args = [];
      fun_body = body;
      fun_codegen_options = [];
      fun_poll = Lambda.Default_poll;
      fun_dbg = Debuginfo.none;
      fun_ret_type = Cmm.typ_val
    }
  in
  let reference symbol = Cmm.Cconst_symbol (symbol, Debuginfo.none) in
  let entry body = [Cmm.Cfunction (fundecl body)] in
  let module_data = (R.to_cmm empty ~extra_phrases:[]).data_items in
  let render phrases =
    List.map (Format.asprintf "%a" Printcmm.phrase) phrases
    |> List.sort String.compare
  in
  let symbol_names symbols =
    List.map (fun (symbol : Cmm.symbol) -> symbol.sym_name) symbols
    |> List.sort String.compare
  in
  let check name ?(extra_phrases = []) ?(gc_roots = []) res expected_data =
    let result = R.to_cmm res ~extra_phrases in
    let expected_data =
      module_data @ List.map (fun data -> Cmm.Cdata data) expected_data
    in
    if
      not
        (List.equal String.equal (render result.data_items)
           (render expected_data))
    then fail "%s: unexpected site data" name;
    if
      not
        (List.equal String.equal
           (symbol_names result.gc_roots)
           (symbol_names gc_roots))
    then fail "%s: unexpected GC roots" name
  in
  check "unreferenced local site" res [];
  check "function references site"
    (R.add_function res (fundecl (reference site)))
    [site_data];
  check "entry references infix label" res [site_data]
    ~extra_phrases:(entry (reference infix));
  check "helper data references site" res [site_data]
    ~extra_phrases:
      [Cmm.Cdata [Cmm.Cdefine_symbol ordinary; Cmm.Csymbol_address site]];
  check "invalid expression references site" res [site_data]
    ~extra_phrases:(entry (Cmm.Cinvalid { message = "test"; symbol = infix }));
  let phantom =
    Backend_var.With_provenance.create (Backend_var.create_local "phantom")
  in
  List.iter
    (fun defining_expr ->
      let body =
        Cmm.Cphantom_let
          (phantom, Some defining_expr, Cmm.Cconst_int (1, Debuginfo.none))
      in
      check "phantom references site" res [site_data]
        ~extra_phrases:(entry body))
    Cmm.
      [ Cphantom_const_symbol infix;
        Cphantom_read_symbol_field { sym = infix; field = 0 } ];
  List.iter
    (fun reference ->
      let data = [Cmm.Cdefine_symbol ordinary; reference] in
      check "ordinary data references infix label"
        (R.add_archive_data_items res data)
        [site_data; data])
    Cmm.[Csymbol_address infix; Csymbol_offset (infix, 8)];
  check "unreferenced ordinary closure unchanged"
    (R.add_archive_data_items res ordinary_data)
    [ordinary_data];
  let global_data = group { site with sym_global = Cmm.Global } infix in
  check "global site"
    (R.add_specialisation_site_data empty global_data)
    [global_data];
  let reachable_names =
    N.add_symbol N.empty site_symbol Flambda2_nominal.Name_mode.normal
  in
  let exported = R.create ~module_symbol ~reachable_names in
  let exported_data = group (R.symbol exported site_symbol) infix in
  check "CMX-reachable site"
    (R.add_specialisation_site_data exported exported_data)
    [exported_data];
  let rooted =
    R.add_gc_roots
      (R.add_archive_data_items res ordinary_data)
      [site_symbol; infix_symbol; ordinary_symbol]
  in
  check "site GC roots do not retain it" rooted [ordinary_data]
    ~gc_roots:[ordinary];
  check "retained closed site needs no GC root" rooted
    [site_data; ordinary_data] ~gc_roots:[ordinary]
    ~extra_phrases:(entry (reference infix))

let () =
  if !failures > 0
  then (
    Format.eprintf "%d Cmm site-data pruning checks failed@." !failures;
    exit 1)
  else Format.printf "Cmm site-data pruning checks passed@."
