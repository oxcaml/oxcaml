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

type empty

type filled

type _ t = {
  index_map : int String.Map.t;
  content : Address.t array;
  name : string;
  entry_size : int;
  write_entry : Buffer.t -> Address.t -> unit;
}

let from_binary_section (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    ~name ~entry_size ~is_relevant_reloc ~write_entry (section : a) =
  let relocs = E.Assembled_section.relocations section in
  let _, index_map =
    List.fold_left relocs ~init:(0, String.Map.empty)
      ~f:(fun (index, map) reloc ->
        if is_relevant_reloc reloc then
          let target = E.Relocation.target_symbol reloc in
          let label = Symbols.target_to_string target in
          if String.Map.mem label map then (index, map)
          else (index + 1, String.Map.add ~key:label ~data:index map)
        else (index, map))
  in
  { index_map; content = [||]; name; entry_size; write_entry }

let in_memory_size t = String.Map.cardinal t.index_map * t.entry_size

let fill symbols t =
  let size = String.Map.cardinal t.index_map in
  let content = Array.make size Address.placeholder in
  String.Map.iter t.index_map ~f:(fun ~key:symbol_name ~data:index ->
      match Symbols.find symbols symbol_name with
      | Some addr ->
        (match Sys.getenv_opt "OCAML_JIT_DEBUG" with
        | Some ("true" | "1") ->
          Printf.eprintf "%s entry[%d] = %s -> %Lx\n%!"
            t.name index symbol_name (Address.to_int64 addr)
        | _ -> ());
        content.(index) <- addr
      | None ->
          failwithf "Symbol %s refered to by the %s is unknown" symbol_name
            t.name);
  { t with content }

let content t =
  let size = in_memory_size t in
  if size = 0 then ""
  else
    let buf = Buffer.create size in
    Array.iter t.content ~f:(t.write_entry buf);
    Buffer.contents buf

let symbol_offset t symbol =
  let open Option.Op in
  let+ index = String.Map.find_opt symbol t.index_map in
  index * t.entry_size

let symbol_address { address; value = t } symbol =
  let open Option.Op in
  let+ offset = symbol_offset t symbol in
  Address.add_int address offset
