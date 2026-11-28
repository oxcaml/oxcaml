(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*       Fabrice Le Fessant, EPI Gallium, INRIA Paris-Rocquencourt        *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let auto_include find_in_dir fn =
  if !Clflags.no_auto_include_otherlibs || !Clflags.no_std_include then
    raise Not_found
  else
    let alert = Location.auto_include_alert in
    Load_path.auto_include_otherlibs alert find_in_dir fn

(* [Codegen_variant.expand_directory] (used by [init_path] below and the
   other resolution points for caller-requested '+'-directories) rejects
   directories that the selected variant's library tree does not provide
   while the library root does; [Codegen_variant] sits in utils and cannot
   register the printer itself. *)
let report_variant_directory_error ppf
      ({ directory; variant; variant_root }
       : Codegen_variant.directory_error) =
  Format_doc.fprintf ppf
    "@[<hov>'%s' does not exist in the library root selected for codegen \
     variant '%s'@ (%s);@ to use a build of it that matches this variant,@ \
     pass its absolute path instead@]"
    directory variant variant_root

let () =
  Location.register_error_of_exn (function
    | Codegen_variant.Directory_not_provided_by_variant err ->
      Some (Location.error_of_printer_file report_variant_directory_error err)
    | _ -> None)

(* A name is looked for as [Load_path.find] does it, with the name as
   written, and the file that is found is then mapped: so no name is
   searched for under a tree that the search goes through already, and a
   name with a directory part that the search finds inside the library
   root, from a directory above it, is the selected variant's, as when it
   is named by its path.  The one exception is a file that the invocation
   wrote in a directory of the library root that the selected variant's
   tree stands for: the search would look for it in that tree, so it is
   used as the file that it is.  Under the base variant, and for every
   file in a directory that is not replaced, this is [Load_path.find]. *)
let find_input name =
  if Codegen_variant.is_product_in_replaced_directory name
  then if Sys.file_exists name then name else raise Not_found
  else if Filename.is_implicit name
  then Codegen_variant.in_selected_root (Load_path.find name)
  else
    (* A counterpart exists, and is an implicit name when the root is one:
       it is not to be searched for. *)
    let file = Codegen_variant.in_selected_root name in
    if String.equal file name then Load_path.find name else file

(* Initialize the search path.
   [dir] (default: the current directory)
   is always searched first  unless -nocwd is specified,
   then the directories specified with the -I option (in command line order),
   then the standard library directory (unless the -nostdlib option is given),
   then the directories specified with the -H option (in command line order).
 *)

let init_path ?(auto_include=auto_include) ?(dir="") () =
  let visible =
    if !Clflags.use_threads then
      { Clflags.path = "+threads"; cmx_guaranteed = true }
      :: !Clflags.include_dirs
    else
      !Clflags.include_dirs
  in
  let visible =
    List.concat
      [!Compenv.last_include_dirs;
       visible;
       List.map
         (fun path : Clflags.include_dir ->
            { path; cmx_guaranteed = false })
         (* Config.flexdll_dirs is either [] or ["+flexdll"]: don't include a
            reference to the Standard Library when -nostdlib was specified.
            Nobody asked for this directory, so it is expanded here without
            the check [Codegen_variant.expand_directory] applies below to
            the caller-requested ones (which leaves it alone once
            expanded). *)
         (if !Clflags.no_std_include then []
          else
            List.map Codegen_variant.expand_directory_unchecked
              Config.flexdll_dirs);
       !Compenv.first_include_dirs]
  in
  let visible =
    List.map (fun (e : Clflags.include_dir) : Clflags.include_dir ->
      { path = Codegen_variant.expand_directory e.path;
        cmx_guaranteed = e.cmx_guaranteed })
      visible
  in
  let visible =
    (if !Clflags.no_cwd then []
     else
       (* The current directory is a directory of the load path like the
          others: inside the library root, it stands for its
          counterpart in the selected variant's tree, as [-I .] does.  So
          every file that is looked up by name comes from one tree, whoever
          looks it up: an interface, a .cmx file, an input of the link. *)
       [{ Clflags.path = Codegen_variant.in_selected_root dir;
          cmx_guaranteed = false }])
    @ List.rev_append visible
        (List.map
           (fun path -> { Clflags.path; cmx_guaranteed = true })
           (Clflags.std_include_dir ()))
  in
  let hidden =
    List.rev_map (fun (e : Clflags.include_dir) : Clflags.include_dir ->
      { path = Codegen_variant.expand_directory e.path;
        cmx_guaranteed = e.cmx_guaranteed })
      !Clflags.hidden_include_dirs
  in
  Load_path.init ~auto_include ~visible ~hidden;
  Env.reset_cache ~preserve_persistent_env:false

(* Read any [-parameters] flags. Important to do this before [initial_env ()]
   because someone might [-open] a parameterised module *)
let init_parameters () =
  let param_names = !Clflags.parameters in
  List.iter
    (fun param_name ->
        let param = Global_module.Parameter_name.of_string param_name in
        Env.register_parameter param
    )
    param_names

(* Return the initial environment in which compilation proceeds. *)

(* Note: do not do init_path() in initial_env, this breaks
   toplevel initialization (PR#8227) *)

let initial_env () =
  Ident.reinit();
  Types.Uid.reinit();
  Shape.Rec_var_ident.reinit();
  let initially_opened_module =
    if !Clflags.nopervasives then
      None
    else
      Some "Stdlib"
  in
  (* An interface inside the library root is the selected variant's,
     like the directory of -H that it is in; this is done for each
     compilation, since the selection can differ between files. *)
  let open_implicit_args =
    List.rev_map
      (fun (arg : Clflags.open_arg) : Clflags.open_arg ->
        match arg with
        | Open _ -> arg
        | Open_cmi { path; cmx_guaranteed } ->
          Open_cmi
            { path = Codegen_variant.in_selected_root path; cmx_guaranteed })
      !Clflags.open_args
  in
  Typemod.initial_env
    ~loc:(Location.in_file "command line")
    ~initially_opened_module
    ~open_implicit_args

let directory_exists dir =
  Sys.file_exists dir && Sys.is_directory dir

let rec make_directory dir =
  if directory_exists dir then () else
    begin
      make_directory (Filename.dirname dir);
      (try
        Sys.mkdir dir 0o777
      with (Sys_error _) as se ->
        if not (directory_exists dir) then raise se)
    end

let with_ppf_file ~file_prefix ~file_extension f =
  let with_ch ch =
    let ppf = Format.formatter_of_out_channel ch in
    ppf,
    (fun () ->
       Format.pp_print_flush ppf ();
       close_out ch)
  in
  let ppf_dump, finally =
    match !Clflags.dump_dir with
    | None -> with_ch (open_out (file_prefix ^ file_extension))
    | Some d ->
      let () = make_directory Filename.(dirname @@ concat d @@ file_prefix) in
      let _, ch =
        Filename.open_temp_file ~temp_dir:d (file_prefix ^ ".") file_extension
      in
      with_ch ch
  in
  Misc.try_finally (fun () -> f ppf_dump) ~always:finally

let with_ppf_dump ?stdout ~file_prefix f =
  match !Clflags.dump_into_file with
  | false ->
    let formatter =
      if Option.is_some stdout then Format.std_formatter else Format.err_formatter in
    Misc.try_finally (fun () -> f formatter)
  | true -> with_ppf_file ~file_prefix ~file_extension:".dump" f

let get_profile_file_prefix ~expected_suffix ~default_name =
  match !Clflags.profile_output_name with
  | None -> default_name
  | Some filename ->
    if Filename.check_suffix filename expected_suffix then
      Filename.chop_suffix filename expected_suffix
    else
      Compenv.fatal
        (Printf.sprintf
          "Profile output filename must have %s extension, got: %s"
          expected_suffix filename)
