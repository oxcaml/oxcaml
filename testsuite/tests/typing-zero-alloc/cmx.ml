(* TEST
   readonly_files = "cmx_lib.ml";
   flags = "-g -I +compiler-libs -I ocamlopt.opt";
   setup-ocamlopt.opt-build-env;
   module = "cmx_lib.ml";
   ocamlopt.opt;
   expect.opt;
*)

open Zero_alloc_utils;;

type summary =
  Relevant_components_by_argument_index.t
  Allocation_sources.t
  Component.t
  Components.t
;;
[%%expect{|
type summary =
    Zero_alloc_utils.Relevant_components_by_argument_index.t
    Zero_alloc_utils.Allocation_sources.t Zero_alloc_utils.Component.t
    Zero_alloc_utils.Components.t
|}]

let read_cmx load_path prefix : summary =
  let cmx, _ = Compilenv.read_unit_info (Load_path.find load_path) in
  let summaries = ref [] in
  Zero_alloc_info.iter cmx.ui_zero_alloc_info ~f:(fun name value ->
    if String.starts_with ~prefix name then
    summaries :=
      Zero_alloc_info.decode value :: !summaries);
  match !summaries with
  | [summary] -> summary
  | [] -> failwith "no such summary found"
  | s1 :: s2 :: _ -> failwith "multiple such summaries found"
;;
[%%expect{|
val read_cmx : string -> string -> summary = <fun>
|}]

let read_cmx_lib prefix : summary =
  "camlCmx_lib__" ^ prefix ^ "_"
  |> read_cmx "cmx_lib.cmx"
;;
[%%expect{|
val read_cmx_lib : string -> summary = <fun>
|}]

let print_allocation_sources =
  Allocation_sources.print Relevant_components_by_argument_index.print
;;

[%%expect{|
val print_allocation_sources :
  Format.formatter ->
  Zero_alloc_utils.Relevant_components_by_argument_index.t
  Zero_alloc_utils.Allocation_sources.t -> unit = <fun>
|}]

#install_printer print_allocation_sources;;

read_cmx "stdlib__List.cmx" "camlStdlib__List__iter_"
[%%expect{|
- : summary =
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
    {argument #1 on normal/diverging paths}}
|}];;

read_cmx_lib "catch_and_discard_exceptions"
[%%expect{|
- : summary =
{Components.nor =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/exceptional paths};
 exn = Component.Bot;
 div =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on diverging paths}}
|}];;

read_cmx_lib "either"
[%%expect{|
- : summary =
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
    {argument #1 on diverging paths; argument #2 on diverging paths}}
|}];;

(* Tail-recursive, *not* optimized into a loop: *)
read_cmx_lib "iter_rec"
[%%expect{|
- : summary =
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
    {argument #1 on normal/diverging paths}}
|}];;

(* Tail-recursive, optimized into a loop: *)
read_cmx_lib "iter_loop"
[%%expect{|
- : summary =
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
    {argument #1 on normal/diverging paths}}
|}];;

(* Call sites: *)
read_cmx_lib "apply"
[%%expect{|
- : summary =
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
    {argument #1 on diverging paths}}
|}];;

(* Call sites, flipping argument order: *)
read_cmx_lib "ylppa"
[%%expect{|
- : summary =
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
    {argument #2 on diverging paths}}
|}];;

(* Across compilation units: *)
read_cmx_lib "iter_flipped"
[%%expect{|
- : summary =
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
    {argument #2 on normal/diverging paths}}
|}];;

(* Mutual recursion: *)
read_cmx_lib "ping"
[%%expect{|
- : summary =
{Components.nor = Component.Bot;
 exn =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/exceptional paths};
 div =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/diverging paths}}
|}];;
read_cmx_lib "pong"
[%%expect{|
- : summary =
{Components.nor = Component.Bot;
 exn =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/exceptional paths};
 div =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/diverging paths}}
|}];;

(* Mutual recursion through [apply]: *)
read_cmx_lib "aping"
[%%expect{|
- : summary =
{Components.nor = Component.Bot;
 exn =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/exceptional paths};
 div =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/diverging paths}}
|}];;
read_cmx_lib "apong"
[%%expect{|
- : summary =
{Components.nor = Component.Bot;
 exn =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/exceptional paths};
 div =
  Component.Top
   No_allocation_other_than_calling_arguments
    {argument #1 on normal/diverging paths}}
|}];;

(* CR wsturgeon for wsturgeon: fix [caml_apply] interaction (next PR) *)
(* [caml_apply]: *)
read_cmx_lib "apply2"
[%%expect{|
- : summary =
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
    {argument #1 on diverging paths}}
|}];;
read_cmx_lib "ylppa2"
[%%expect{|
- : summary =
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
    {argument #3 on diverging paths}}
|}];;
read_cmx_lib "over"
[%%expect{|
Exception: Failure "no such summary found".
|}];;
read_cmx_lib "over2"
[%%expect{|
Exception: Failure "no such summary found".
|}];;
read_cmx_lib "partial"
[%%expect{|
- : summary =
{Components.nor = Component.Top Allocation_other_than_calling_arguments;
 exn = Component.Bot; div = Component.Bot}
|}];;

read_cmx_lib "arity_1_expected_1"
[%%expect{|
- : summary =
{Components.nor = Component.Safe; exn = Component.Safe; div = Component.Safe}
|}];;

read_cmx_lib "arity_2_expected_1"
[%%expect{|
- : summary =
{Components.nor = Component.Top Allocation_other_than_calling_arguments;
 exn = Component.Safe; div = Component.Safe}
|}];;

read_cmx_lib "arity_2_expected_2"
[%%expect{|
Exception: Failure "multiple such summaries found".
|}];;
read_cmx_lib "arity_2_expected_2_partial"
[%%expect{|
- : summary =
{Components.nor = Component.Top Allocation_other_than_calling_arguments;
 exn = Component.Bot; div = Component.Bot}
|}];;

(* Always exit with [raise Fail]: *)
read_cmx_lib "fail"
[%%expect{|
- : summary =
{Components.nor = Component.Bot; exn = Component.Safe; div = Component.Bot}
|}];;

(* Always diverge with [while true ...]: *)
read_cmx_lib "diverge"
[%%expect{|
- : summary =
{Components.nor = Component.Bot; exn = Component.Bot; div = Component.Bot}
|}];;
(* CR-someday wsturgeon for wsturgeon: the above [div = Bot] is a known bug *)

(* Passing constant symbols to conditionally zero-alloc functions: *)
read_cmx_lib "iter_fail"
[%%expect{|
- : summary =
{Components.nor = Component.Safe; exn = Component.Safe; div = Component.Safe}
|}];;

(* Passing constant symbols to conditionally zero-alloc functions: *)
read_cmx_lib "iter_diverge"
[%%expect{|
- : summary =
{Components.nor = Component.Safe; exn = Component.Safe; div = Component.Safe}
|}];;

(* Closure symbols, code symbols, and builtins are all distinct: *)
read_cmx_lib "iter_ignore"
[%%expect{|
- : summary =
{Components.nor = Component.Safe; exn = Component.Safe; div = Component.Safe}
|}];;
read_cmx_lib "iter_noop"
[%%expect{|
- : summary =
{Components.nor = Component.Safe; exn = Component.Safe; div = Component.Safe}
|}];;
read_cmx_lib "iter_alloc"
[%%expect{|
Exception: Failure "no such summary found".
|}];;

(* Closure symbols to code symbols across compilation units: *)
read_cmx_lib "exists_some"
[%%expect{|
- : summary =
{Components.nor = Component.Safe; exn = Component.Safe; div = Component.Safe}
|}];;

(* Closure symbols to code symbols across compilation units in recursive
   functions: *)
read_cmx_lib "self_cross"
[%%expect{|
- : summary =
{Components.nor = Component.Bot; exn = Component.Safe; div = Component.Safe}
|}];;

(* Closure symbols to code symbols across compilation units in mutually
   recursive functions: *)
read_cmx_lib "cycle_a"
[%%expect{|
- : summary =
{Components.nor = Component.Bot; exn = Component.Safe; div = Component.Safe}
|}];;
