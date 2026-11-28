(* TEST
 codegen_variant = "+sc";
 variant_available;
 toplevel.opt;
*)

(* #remove_directory removes what #directory added, also for a directory
   that is named inside the library root and that the toplevel of
   another variant therefore resolves in the tree of its variant.  The
   directives are called as functions for the sake of the directory, whose
   name depends on the installation. *)
#directory "+compiler-libs";;
let listed directory =
  List.mem
    (Codegen_variant.in_selected_root directory)
    (Load_path.get_path_list ());;
let listed_after directive =
  let unix = Filename.concat Config.standard_library "unix" in
  directive unix;
  listed unix;;
listed_after Opttopdirs.dir_directory;;
listed_after Opttopdirs.dir_remove_directory;;
