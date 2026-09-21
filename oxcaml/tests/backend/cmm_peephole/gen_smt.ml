let () =
  let ppf = Format.std_formatter in
  Cmm_peephole_engine.Rule.print_smt2_header ppf;
  List.iter (Cmm_peephole_engine.Rule.print_smt2 ppf) Cmm_peephole_rules.all;
  Format.pp_print_flush ppf ()
