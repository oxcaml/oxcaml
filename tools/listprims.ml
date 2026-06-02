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

let extract_primitives structure =
  let primitives = ref [] in
  let iterator =
    { Ast_iterator.default_iterator with
      pat =
        (fun _self pat ->
          match pat.ppat_desc with
          | Ppat_constant (Pconst_string (s, _, _))
            when String.length s > 0 && s.[0] = '%' ->
              primitives := s :: !primitives
          | _ -> ())
    }
  in
  iterator.structure iterator structure;
  !primitives
;;

let () =
  if Array.length Sys.argv < 2
  then (Printf.eprintf "Usage: %s <translprim.ml>\n" Sys.argv.(0); exit 1);

  let translprim_file = Sys.argv.(1) in
  let ic = open_in translprim_file in
  let lexbuf = Lexing.from_channel ic in
  Location.init lexbuf translprim_file;
  let structure = Parse.implementation lexbuf in
  close_in ic;

  let all_prims =
    extract_primitives structure
    @ Translprim.indexing_primitive_names
    @ Translprim.array_vec_primitive_names
    @ List.map
        Scalar.Operation.With_percent_prefix.to_string Scalar.Operation.all
    |> List.sort_uniq String.compare
  in
  List.iter print_endline all_prims
;;
