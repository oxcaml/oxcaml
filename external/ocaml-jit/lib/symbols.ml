(* Copyright (c) 2021 Nathan Rebours <nathan.p.rebours@gmail.com>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

open Import

type t = Address.t String.Map.t

let empty = String.Map.empty

let strict_union t t' =
  String.Map.union t t' ~f:(fun symbol_name _ _ ->
      failwithf "Symbol %s defined in several sections" symbol_name)

(* Identifies whether a symbol is a generic OCaml function
   that is generated on the fly for a phrase when required.
   Those symbols can appear several time in a toplevel session
   if the right conditions are met. *)
let is_gen_fun symbol_name =
  String.starts_with ~prefix:"caml_apply" symbol_name
  || String.starts_with ~prefix:"caml_curry" symbol_name
  || String.starts_with ~prefix:"caml_send" symbol_name
  || String.starts_with ~prefix:"caml_tuplify" symbol_name


let aggregate ~current ~new_symbols =
  (* TODO: We should most likely handle local symbols properly.
     For the moment we just special-case the local symbol used to
     identify the caml_call_gc glue code; other local symbols should
     be properly mangled and unique per compilation unit.  When
     encountering this symbol we always pick the most recent address for it,
     which will cause older definitions to be ignored. *)
  String.Map.union current new_symbols ~f:(fun symbol_name _old new_ ->
    if is_gen_fun symbol_name
       || String.equal symbol_name ".Lcaml_call_gc_"
       || String.equal symbol_name ".Lcaml_call_gc_sse_"
       || String.equal symbol_name ".Lcaml_call_gc_avx_"
       || String.equal symbol_name ".Lcaml_call_gc_avx512_"
       || String.starts_with ~prefix:".Lcaml_apply" symbol_name
       || String.starts_with ~prefix:".Lcaml_curry" symbol_name
       || String.starts_with ~prefix:".Lcaml_send" symbol_name
       || String.starts_with ~prefix:".Lcaml_tuplify" symbol_name
    then Some new_
    else failwithf "Multiple occurrences of the symbol %s" symbol_name)

let from_binary_section { address; value = binary_section } =
  let symbol_tbl = X86_binary_emitter.labels binary_section in
  Misc.Stdlib.String.Tbl.fold
    (fun name symbol acc ->
      match (symbol.X86_binary_emitter.sy_pos, name) with
      | None, _ -> failwithf "Symbol %s has no offset" name
      | Some _, ("caml_absf_mask" | "caml_negf_mask" |
                 "caml_absf32_mask" | "caml_negf32_mask") -> acc
      | Some offset, _ ->
          String.Map.add ~key:name ~data:(Address.add_int address offset) acc)
    symbol_tbl
    String.Map.empty

let find t name =
  match String.Map.find_opt name t with
  | Some addr -> Some addr
  | None -> Externals.dlsym name

let dprint t =
  Printf.printf "------ Symbols -----\n%!";
  String.Map.iter
    ~f:(fun ~key ~data ->
      Printf.printf "%s: %Lx\n%!" key (Address.to_int64 data))
    t
