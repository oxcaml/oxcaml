open Import

let _ =
  try
    let file1 = Sys.argv.(1) in
    let file2 = Sys.argv.(2) in
    let unit1 = Test_utils.parse_flambda file1 in
    let unit2 = Test_utils.parse_flambda file2 in
    let unit_info = Parse_flambda.make_unit_info ~filename:file1 in
    Env.set_unit_name (Some unit_info);
    match Compare.flambda_units unit2 unit1 with
    | Equivalent -> ()
    | Different { approximant = unit2' } ->
      Format.printf "%a@." Print_fexpr.flambda_unit
        (Flambda_to_fexpr.conv unit2');
      exit 1
  with Test_utils.Failure -> exit 1
