(* TEST
   readonly_files = "cmx_lib.ml";
   flags = "-I +compiler-libs -I ocamlopt.opt";
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

read_cmx "cmx_lib.cmx" "camlCmx_lib__catch_and_discard_exceptions_"
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

read_cmx "cmx_lib.cmx" "camlCmx_lib__either_"
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

(* CR wsturgeon: fix (work for the next PR) *)
read_cmx "cmx_lib.cmx" "camlCmx_lib__iter_rec_"
[%%expect{|
Exception: Failure "no such summary found".
|}];;

read_cmx "cmx_lib.cmx" "camlCmx_lib__iter_loop_"
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

read_cmx "cmx_lib.cmx" "camlCmx_lib__apply_"
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

(* CR wsturgeon: fix (work for the next PR) *)
read_cmx "cmx_lib.cmx" "camlCmx_lib__ylppa_"
[%%expect{|
Exception: Failure "no such summary found".
|}];;
