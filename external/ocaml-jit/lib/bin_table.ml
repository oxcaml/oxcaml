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

module type S = sig
  type _ t

  val from_binary_section : X86_binary_emitter.buffer -> empty t
  (** Creates a table with all the entries needed for the given binary section.
      The table will have the right size, i.e. one entry per corresponding relative
      relocations in the given section. You need to use [fill] to write the actual
      addresses of the pointed symbols. *)

  val fill : Symbols.t -> empty t -> filled t
  (** Fills the table with the absolute addresses of the symbols it holds *)

  val in_memory_size : _ t -> int
  (** Returns the size (in bytes) the table will take up in the memory *)

  val content : filled t -> string
  (** Returns the table in binary form, as it should be written at the end of the text section *)

  val symbol_address : _ t addressed -> string -> Address.t option
  (** [symbol_address table symbol] returns the absolute address at which the address
      or jump for [symbol] is stored within the given table. *)
end

module type IN = sig
  val name : string

  val entry_size : int

  val entry_from_relocation : Relocation.t -> string option

  val write_entry : Buffer.t -> Address.t -> unit
end

module Make (X : IN) : S = struct
  type _ t = { index_map : int String.Map.t; content : Address.t array }

  let from_binary_section binary_section =
    let raw_relocations = X86_binary_emitter.relocations binary_section in
    let relocations =
      List.filter_map ~f:Relocation.from_x86_relocation raw_relocations
    in
    let _, index_map =
      List.fold_left relocations ~init:(0, String.Map.empty)
        ~f:(fun (index, map) reloc ->
          match X.entry_from_relocation reloc with
          | Some label when String.Map.mem label map -> (index, map)
          | Some label -> (index + 1, String.Map.add ~key:label ~data:index map)
          | None -> (index, map))
    in
    { index_map; content = [||] }

  let in_memory_size t = String.Map.cardinal t.index_map * X.entry_size

  let fill symbols t =
    let size = String.Map.cardinal t.index_map in
    let content = Array.make size Address.placeholder in
    String.Map.iter t.index_map ~f:(fun ~key:symbol_name ~data:index ->
        match Symbols.find symbols symbol_name with
        | Some addr -> content.(index) <- addr
        | None ->
            failwithf "Symbol %s refered to by the %s is unknown" symbol_name
              X.name);
    { t with content }

  let content t =
    let size = in_memory_size t in
    if size = 0 then ""
    else
      let buf = Buffer.create size in
      Array.iter t.content ~f:(X.write_entry buf);
      Buffer.contents buf

  let symbol_offset t symbol =
    let open Option.Op in
    let+ index = String.Map.find_opt symbol t.index_map in
    index * X.entry_size

  let symbol_address { address; value = t } symbol =
    let open Option.Op in
    let+ offset = symbol_offset t symbol in
    Address.add_int address offset
end
