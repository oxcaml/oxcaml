let byte = "byte"

let nat = "nat"

let jit = "jit"

let pr_output_rule ~mode basename =
  Printf.printf
    {|
(rule
 (action
  (with-accepted-exit-codes (or 0 1 125)
   (with-outputs-to %s.%s
    (run %stop %%{dep:%s.ml})))))
|}
    basename mode mode basename

let pr_diff_rule ~mode ~ref basename =
  Printf.printf {|
(rule
 (alias %stest)
 (action
  (diff %s.%s %s.%s)))
|} mode
    basename ref basename mode

let pr_alias () =
  Printf.printf
    {|
(alias
 (name runtest)
 (deps
  (alias %stest)
  (alias %stest)
  (alias %stest)))
|}
    byte nat jit

let pr_rules file =
  let basename = Filename.chop_extension file in
  pr_output_rule ~mode:byte basename;
  pr_output_rule ~mode:nat basename;
  pr_output_rule ~mode:jit basename;
  pr_diff_rule ~mode:byte ~ref:"expected" basename;
  pr_diff_rule ~mode:nat ~ref:byte basename;
  pr_diff_rule ~mode:jit ~ref:nat basename

let is_test_case filename = Filename.check_suffix filename ".ml"

let () =
  Sys.readdir "." |> Array.to_list |> List.sort String.compare
  |> List.filter is_test_case |> List.iter pr_rules;
  pr_alias ()
