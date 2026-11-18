(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
include Numbers.Int

let none = -1

let entry_label = 1

let init_label = 99

let label_counter = ref init_label

let set_label l =
  if l < !label_counter
  then
    Misc.fatal_errorf "Cannot set label counter to %d, it must be >= %d" l
      !label_counter;
  label_counter := l

let cur_label () = !label_counter

(* Labels that exist only for the benefit of DWARF are numbered from here, so
   that they are distinct from the others. *)
(* CR-someday mshinwell: give DWARF-only labels an abstract type of their own
   rather than a range of numbers. A range boundary of [Compute_ranges] is
   either an existing label or a DWARF-only one, so this means a sum type
   through [Compute_ranges], its DWARF clients, [Linear.Llabel_for_dwarf],
   branch relaxation and the emitters, and a second namespace of labels in
   [Asm_label]. *)
let init_dwarf_label = 1 lsl 40

let dwarf_label_counter = ref init_dwarf_label

let new_label () =
  incr label_counter;
  if !label_counter >= init_dwarf_label
  then Misc.fatal_error "Label.new_label: too many labels";
  !label_counter

let new_dwarf_label () =
  incr dwarf_label_counter;
  !dwarf_label_counter

let set_dwarf_label l =
  if l < !dwarf_label_counter
  then
    Misc.fatal_errorf "Cannot set DWARF label counter to %d, it must be >= %d" l
      !dwarf_label_counter;
  dwarf_label_counter := l

let cur_dwarf_label () = !dwarf_label_counter

let reset () =
  label_counter := init_label;
  dwarf_label_counter := init_dwarf_label

let to_int t = t

let of_int_unsafe t = t

let to_string = Int.to_string

let format fmt t = Format.fprintf fmt "%d" t
