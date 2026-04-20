open Import

let _ =
  try
    let file1 = Sys.argv.(1) in
    let file2 = Sys.argv.(2) in
    let unit1 = Test_utils.parse_flambda file1 in
    let unit2 = Test_utils.parse_flambda file2 in
<<<<<<< HEAD
    let modname1 =
      Parse_flambda.make_compilation_unit ~filename:file1 ~extension:".fl" ()
    in
    let unit_info = Unit_info.make_dummy ~input_name:file1 modname1 in
    Env.set_current_unit (Some unit_info);
||||||| f8c6716f8c
    let modname1 =
      Parse_flambda.make_compilation_unit ~filename:file1 ~extension:".fl" ()
    in
    let unit_info = Unit_info.make_dummy ~input_name:file1 modname1 in
    Env.set_unit_name (Some unit_info);
=======
    let unit_info = Parse_flambda.make_unit_info ~filename:file1 in
    Env.set_unit_name (Some unit_info);
>>>>>>> 5.2.0minus-31
    match Compare.flambda_units unit2 unit1 with
    | Equivalent -> ()
    | Different { approximant = unit2' } ->
      Format.printf "%a@." Print_fexpr.flambda_unit
        (Flambda_to_fexpr.conv unit2');
      exit 1
  with Test_utils.Failure -> exit 1
