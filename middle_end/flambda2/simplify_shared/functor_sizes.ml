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

let record ~code_id ~dbg ~size =
  entries := { code_id; dbg; size } :: !entries

let output_then_forget ~prefixname =
  let collected = List.rev !entries in
  entries := [];
  match collected with
  | [] -> ()
  | _ :: _ ->
    let filename = prefixname ^ ".ml.functor-sizes.corrected" in
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
