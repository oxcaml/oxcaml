(* TEST
   readonly_files = "cmx_lib.ml";
   flags = "-I +compiler-libs -I ocamlopt.opt";
   setup-ocamlopt.opt-build-env;
   module = "cmx_lib.ml";
   ocamlopt.opt;
   expect.opt;
*)

open Zero_alloc_utils;;

let read_cmx load_path prefix =
  let cmx, _ = Compilenv.read_unit_info (Load_path.find load_path) in
  let summaries = ref [] in
  Zero_alloc_info.iter cmx.ui_zero_alloc_info ~f:(fun name value ->
    if String.starts_with ~prefix name then
    summaries :=
      (name, Zero_alloc_info.decode value) :: !summaries);
  !summaries
;;
let print_allocation_sources =
  Allocation_sources.print Relevant_components_by_argument_index.print;;
[%%expect{|
val read_cmx :
  string ->
  string ->
  (string *
   Zero_alloc_utils.Relevant_components_by_argument_index.t
   Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
   Zero_alloc_utils.Components.t)
  list = <fun>
val print_allocation_sources :
  Format.formatter ->
  Zero_alloc_utils.Relevant_components_by_argument_index.t
  Zero_alloc_utils.Allocation_sources.t -> unit = <fun>
|}]

#install_printer print_allocation_sources;;

let _ =
  read_cmx
    "stdlib__List.cmx"
    "camlStdlib__List__iter_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlStdlib__List__iter_23_131_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #1 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/diverging paths}})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__catch_and_discard_exceptions_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__catch_and_discard_exceptions_0_4_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/exceptional paths};
   exn = Component.Bot;
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on diverging paths}})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__either_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__either_1_5_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal paths; argument #2 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on exceptional paths; argument #2 on exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on diverging paths; argument #2 on diverging paths}})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_rec_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__iter_rec_2_6_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #1 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/diverging paths}})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_loop_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__iter_loop_3_7_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #1 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/diverging paths}})]
|}]
