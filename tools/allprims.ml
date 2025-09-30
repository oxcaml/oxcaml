(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  James Rayman, Jane Street, New York                   *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let () =
  List.iter print_endline Translprim.indexing_primitive_names;
  List.iter print_endline Translprim.array_vec_primitive_names;
  List.iter
    (fun op ->
      let s = Scalar.Operation.With_percent_prefix.to_string op in
      print_endline s)
    Scalar.Operation.all
;;
