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

type need_reloc = Bin_table.empty

type relocated = Bin_table.filled

let name = ".text"

(* Generic text section that works with any Binary_emitter_intf.S *)
type ('section, 'reloc_state) t = {
  binary_section : 'section;
  got : 'reloc_state Bin_table.t;
  plt : 'reloc_state Bin_table.t;
}

let from_binary_section (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    (section : a) : (a, need_reloc) t =
  let got =
    Bin_table.from_binary_section
      (module E) ~name:"GOT" ~entry_size:Address.size
      ~is_relevant_reloc:E.Relocation.is_got_reloc
      ~write_entry:Address.emit section
  in
  let plt =
    Bin_table.from_binary_section
      (module E) ~name:"PLT" ~entry_size:E.Plt.entry_size
      ~is_relevant_reloc:E.Relocation.is_plt_reloc
      ~write_entry:(fun buf addr -> E.Plt.write_entry buf (Address.to_int64 addr))
      section
  in
  { binary_section = section; got; plt }

let in_memory_size (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    (t : (a, _) t) =
  let section_size = E.Assembled_section.size t.binary_section in
  (* Round up to 8-byte alignment for GOT entries *)
  let aligned_section_size = (section_size + 7) land (lnot 7) in
  aligned_section_size
  + Bin_table.in_memory_size t.got
  + Bin_table.in_memory_size t.plt

let relocate (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    ~symbols (t : (a, need_reloc) t addressed) =
  let open Result.Op in
  let section_size = E.Assembled_section.size t.value.binary_section in
  (* Round up to 8-byte alignment for GOT entries (required for LDR x) *)
  let aligned_section_size = (section_size + 7) land (lnot 7) in
  let got_address = Address.add_int t.address aligned_section_size in
  let got = Bin_table.fill symbols t.value.got in
  let plt_address =
    Address.add_int got_address (Bin_table.in_memory_size got)
  in
  let plt = Bin_table.fill symbols t.value.plt in
  let got_lookup =
    Some (fun sym -> Bin_table.symbol_address { address = got_address; value = got } sym)
  in
  let plt_lookup =
    Some (fun sym -> Bin_table.symbol_address { address = plt_address; value = plt } sym)
  in
  let+ () =
    Relocate.all (module E) ~symbols
      ~got_lookup ~plt_lookup
      ~section_name:name
      { address = t.address; value = t.value.binary_section }
  in
  let value = { t.value with got; plt } in
  { t with value }

let content (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    (t : (a, relocated) t) =
  let section_content = E.Assembled_section.contents t.binary_section in
  let section_size = String.length section_content in
  (* Add padding bytes to align GOT to 8 bytes *)
  let aligned_section_size = (section_size + 7) land (lnot 7) in
  let padding_size = aligned_section_size - section_size in
  let padding = String.make padding_size '\x00' in
  section_content ^ padding
  ^ Bin_table.content t.got
  ^ Bin_table.content t.plt

let symbols (type a r)
    (module E : Binary_emitter_intf.S
      with type Assembled_section.t = a
       and type Relocation.t = r)
    { address; value = t } =
  Symbols.from_binary_section (module E) { address; value = t.binary_section }
