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
[("camlCmx_lib__catch_and_discard_exceptions_0_40_code",
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
[("camlCmx_lib__either_1_41_code",
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
[("camlCmx_lib__iter_rec_2_42_code",
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
[("camlCmx_lib__iter_loop_3_43_code",
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
[("camlCmx_lib__apply_4_44_code",
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
=
[("camlCmx_lib__ylppa_5_45_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #2 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #2 on exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #2 on diverging paths}})]
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
=
[("camlCmx_lib__iter_flipped_6_46_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #2 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #2 on normal/exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #2 on normal/diverging paths}})]
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
=
[("camlCmx_lib__ping_7_47_code",
  {Components.nor = Component.Bot;
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
    "camlCmx_lib__pong_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__pong_8_48_code",
  {Components.nor = Component.Bot;
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/diverging paths}})]
|}]

(* Mutual recursion through [apply]: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__aping_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__aping_9_49_code",
  {Components.nor = Component.Bot;
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
    "camlCmx_lib__apong_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__apong_10_50_code",
  {Components.nor = Component.Bot;
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #1 on normal/diverging paths}})]
|}]

(* CR wsturgeon for wsturgeon: fix [caml_apply] interaction (next PR) *)
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
[("camlCmx_lib__apply2_11_51_code",
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
=
[("camlCmx_lib__ylppa2_12_52_code",
  {Components.nor =
    Component.Top
     No_allocation_other_than_calling_arguments {argument #3 on normal paths};
   exn =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #3 on exceptional paths};
   div =
    Component.Top
     No_allocation_other_than_calling_arguments
      {argument #3 on diverging paths}})]
|}]
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__over_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__over2_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__partial_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__partial_15_55_code",
  {Components.nor = Component.Top Allocation_other_than_calling_arguments;
   exn = Component.Bot; div = Component.Bot})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__arity_1_expected_1_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__arity_1_expected_1_17_56_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__arity_2_expected_1_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__arity_2_expected_1_19_57_code",
  {Components.nor = Component.Top Allocation_other_than_calling_arguments;
   exn = Component.Safe; div = Component.Safe})]
|}]

let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__arity_2_expected_2_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__arity_2_expected_2_partial_23_59_code",
  {Components.nor = Component.Top Allocation_other_than_calling_arguments;
   exn = Component.Bot; div = Component.Bot});
 ("camlCmx_lib__arity_2_expected_2_21_58_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
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
[("camlCmx_lib__fail_26_60_code",
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
[("camlCmx_lib__diverge_27_61_code",
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
=
[("camlCmx_lib__iter_fail_28_62_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
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
=
[("camlCmx_lib__iter_diverge_29_63_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
|}]

(* Closure symbols, code symbols, and builtins are all distinct: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_ignore_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__iter_ignore_32_66_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
|}]
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_noop_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__iter_noop_34_67_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
|}]
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__iter_alloc_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
= []
|}]

(* Closure symbols to code symbols across compilation units: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__exists_some_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__exists_some_36_69_code",
  {Components.nor = Component.Safe; exn = Component.Safe;
   div = Component.Safe})]
|}]

(* Closure symbols to code symbols across compilation units in recursive
   functions: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__self_cross_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__self_cross_37_70_code",
  {Components.nor = Component.Bot; exn = Component.Safe;
   div = Component.Safe})]
|}]

(* Closure symbols to code symbols across compilation units in mutually
   recursive functions: *)
let _ =
  read_cmx
    "cmx_lib.cmx"
    "camlCmx_lib__cycle_a_"
;;
[%%expect{|
- : (string *
     Zero_alloc_utils.Relevant_components_by_argument_index.t
     Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
     Zero_alloc_utils.Components.t)
    list
=
[("camlCmx_lib__cycle_a_38_71_code",
  {Components.nor = Component.Bot; exn = Component.Safe;
   div = Component.Safe})]
|}]
