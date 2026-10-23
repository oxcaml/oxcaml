(* TEST
   readonly_files = "cmx_lib.ml";
   flags = "-g -I +compiler-libs -I ocamlopt.opt";
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
[("camlCmx_lib__catch_and_discard_exceptions_0_15_code",
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
[("camlCmx_lib__either_1_16_code",
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

(* Tail-recursive, *not* optimized into a loop: *)
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
[("camlCmx_lib__iter_rec_2_17_code",
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

(* Tail-recursive, optimized into a loop: *)
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
[("camlCmx_lib__iter_loop_3_18_code",
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

(* Call sites: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__apply_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__apply_4_19_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #1 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on diverging paths}})]
|}]

(* Call sites, flipping argument order: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__ylppa_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* Across compilation units: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_flipped_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* Mutual recursion: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__ping_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* Mutual recursion: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__pong_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* [caml_apply]: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__apply2_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__apply2_4_10_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #1 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on diverging paths}})]
|}]

(* [caml_apply]: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__ylppa2_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* Always exit with [raise Fail]: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__fail_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__fail_11_26_code",
  {Components.nor = Component.Bot; exn = Component.Safe; div = Component.Bot})]
|}]

(* Always diverge with [while true ...]: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__diverge_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__diverge_12_27_code",
  {Components.nor = Component.Bot; exn = Component.Bot; div = Component.Bot})]
|}]
(* CR-someday wsturgeon for wsturgeon: the above [div = Bot] is a known bug *)

(* Passing constant symbols to conditionally zero-alloc functions: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_fail_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* Passing constant symbols to conditionally zero-alloc functions: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_diverge_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]
