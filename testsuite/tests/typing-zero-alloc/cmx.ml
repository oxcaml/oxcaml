(* TEST
   readonly_files = "cmx_lib.ml";
   flags = "-I +compiler-libs -I ocamlopt.opt";
   setup-ocamlopt.opt-build-env;
   module = "cmx_lib.ml";
   ocamlopt.opt;
   expect.opt;
*)

open Zero_alloc_info;;

#install_printer Allocation_sources.print;;

let read_cmx load_path prefix =
  let cmx, _ = Compilenv.read_unit_info (Load_path.find load_path) in
  let summaries = ref [] in
  iter cmx.ui_zero_alloc_info ~f:(fun name value ->
    if String.starts_with ~prefix name then
    summaries := (name, decode value) :: !summaries);
  !summaries
;;
[%%expect{|
val read_cmx : string -> string -> (string * Zero_alloc_info.components) list =
  <fun>
|}]

let _ =
  read_cmx
    "stdlib__List.cmx"
    "camlStdlib__List__iter_"
;;
[%%expect{|
- : (string * Zero_alloc_info.components) list =
  [("camlStdlib__List__iter_23_131_code",
    {nor =
      Top
       No_allocation_other_than_calling_arguments {argument #1 on normal paths};
     exn =
      Top
       No_allocation_other_than_calling_arguments {argument #1 on normal/exceptional paths};
     div =
      Top
       No_allocation_other_than_calling_arguments {argument #1 on normal/diverging paths}})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__catch_and_discard_exceptions_"
;;
[%%expect{|
- : (string * Zero_alloc_info.components) list =
[("camlCmx_lib__catch_and_discard_exceptions_0_1_code",
  {nor =
    Top
     No_allocation_other_than_calling_arguments {argument #1 on normal/exceptional paths};
   exn = Bot;
   div =
    Top
     No_allocation_other_than_calling_arguments {argument #1 on diverging paths}})]
|}]
