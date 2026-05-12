(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type entry =
  { code_id : Code_id.t;
    dbg : Debuginfo.t;
    size : Code_size.t
  }

let entries : entry list ref = ref []

let record ~code_id ~dbg ~size = entries := { code_id; dbg; size } :: !entries

let rec mkdir_p dir =
  if Sys.file_exists dir
  then ()
  else
    let parent = Filename.dirname dir in
    if not (String.equal parent dir) then mkdir_p parent;
    try Sys.mkdir dir 0o777 with Sys_error _ when Sys.file_exists dir -> ()

let output_filename ~prefixname =
  let basename =
    Filename.basename prefixname ^ ".ml.functor-sizes.corrected"
  in
  match Flambda_features.dump_functor_sizes_dir () with
  | None -> prefixname ^ ".ml.functor-sizes.corrected"
  | Some dir ->
    let subdir =
      match !Clflags.directory with
      | None | Some "" -> ""
      | Some s -> s
    in
    let full_dir =
      if String.equal subdir "" then dir else Filename.concat dir subdir
    in
    mkdir_p full_dir;
    Filename.concat full_dir basename

let output_then_forget ~prefixname =
  let collected = List.rev !entries in
  entries := [];
  match collected with
  | [] -> ()
  | _ :: _ ->
    let filename = output_filename ~prefixname in
    let out_channel = open_out filename in
    Misc.try_finally
      ~always:(fun () -> close_out_noerr out_channel)
      (fun () ->
        let fmt = Format.formatter_of_out_channel out_channel in
        List.iter
          (fun { code_id; dbg; size } ->
            Format.fprintf fmt "%d\t%a\t%a@\n" (Code_size.to_int size)
              Code_id.print code_id Debuginfo.print_compact dbg)
          collected;
        Format.pp_print_flush fmt ())
