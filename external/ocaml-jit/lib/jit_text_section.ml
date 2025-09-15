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

type 'a t = {
  binary_section : X86_binary_emitter.buffer;
  got : 'a Jit_got.t;
  plt : 'a Jit_plt.t;
}

let from_binary_section binary_section =
  let got = Jit_got.from_binary_section binary_section in
  let plt = Jit_plt.from_binary_section binary_section in
  { binary_section; got; plt }

let in_memory_size { binary_section; got; plt } =
  let section_size = X86_binary_emitter.size binary_section in
  let got_size = Jit_got.in_memory_size got in
  let plt_size = Jit_plt.in_memory_size plt in
  section_size + got_size + plt_size

let relocate ~symbols (t : need_reloc t addressed) =
  let open Result.Op in
  let got_address =
    Address.add_int t.address (X86_binary_emitter.size t.value.binary_section)
  in
  let got = Jit_got.fill symbols t.value.got in
  let plt_address = Address.add_int got_address (Jit_got.in_memory_size got) in
  let plt = Jit_plt.fill symbols t.value.plt in
  let+ () =
    Relocate.all_text ~symbols
      ~got:{ address = got_address; value = got }
      ~plt:{ address = plt_address; value = plt }
      { address = t.address; value = t.value.binary_section }
  in
  let value = { t.value with got; plt } in
  { t with value }

let content t =
  X86_binary_emitter.contents t.binary_section
  ^ Jit_got.content t.got ^ Jit_plt.content t.plt

let symbols { address; value = t } =
  Symbols.from_binary_section { address; value = t.binary_section }
