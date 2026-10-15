(* TEST
   flags = "-I +compiler-libs";
   expect.opt;
*)

open Zero_alloc_info;;

#install_printer Allocation_sources.print;;

let zero_alloc_info =
  let cmx, _ = Compilenv.read_unit_info (Load_path.find "stdlib__List.cmx") in
  let summaries = ref [] in
  iter cmx.ui_zero_alloc_info ~f:(fun name value ->
    if String.starts_with ~prefix:"camlStdlib__List__iter_" name then
    summaries := (name, decode value) :: !summaries);
  !summaries
;;
[%%expect{|
val zero_alloc_info : (string * Zero_alloc_info.components) list =
  [("camlStdlib__List__iter_23_131_code",
    {nor =
      Top
       {arguments_that_may_allocate = {argument #1 on normal paths};
        anything_else_may_allocate = false};
     exn =
      Top
       {arguments_that_may_allocate = {argument #1 on normal/exceptional paths};
        anything_else_may_allocate = false};
     div =
      Top
       {arguments_that_may_allocate = {argument #1 on normal/diverging paths};
        anything_else_may_allocate = false}})]
|}]
