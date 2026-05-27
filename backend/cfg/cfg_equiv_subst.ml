(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2026 Jane Street Group LLC                                       *
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

type t =
  { labels : Label.t Label.Tbl.t;
    symbols : Cmm.symbol Cmm.Symbol_tbl.t;
    func_symbols : string Misc.Stdlib.String.Tbl.t
  }

let make () =
  { labels = Label.Tbl.create 16;
    symbols = Cmm.Symbol_tbl.create 16;
    func_symbols = Misc.Stdlib.String.Tbl.create 16
  }

let add_label subst from_lbl to_lbl =
  match Label.Tbl.find_opt subst.labels from_lbl with
  | None -> Label.Tbl.add subst.labels from_lbl to_lbl
  | Some existing ->
    if not (Label.equal existing to_lbl)
    then
      Misc.fatal_errorf
        "Cfg_equiv_subst.add_label: conflicting binding for %a (was %a, now %a)"
        Label.format from_lbl Label.format existing Label.format to_lbl

let add_symbol subst from_sym to_sym =
  match Cmm.Symbol_tbl.find_opt subst.symbols from_sym with
  | None -> Cmm.Symbol_tbl.add subst.symbols from_sym to_sym
  | Some existing ->
    if not (Cmm.equal_symbol existing to_sym)
    then
      Misc.fatal_errorf
        "Cfg_equiv_subst.add_symbol: conflicting binding for %s (was %s, now \
         %s)"
        from_sym.Cmm.sym_name existing.Cmm.sym_name to_sym.Cmm.sym_name

let add_func_symbol subst from_name to_name =
  match Misc.Stdlib.String.Tbl.find_opt subst.func_symbols from_name with
  | None -> Misc.Stdlib.String.Tbl.add subst.func_symbols from_name to_name
  | Some existing ->
    if not (String.equal existing to_name)
    then
      Misc.fatal_errorf
        "Cfg_equiv_subst.add_func_symbol: conflicting binding for %s (was %s, \
         now %s)"
        from_name existing to_name

let subst_label subst lbl =
  match Label.Tbl.find_opt subst.labels lbl with
  | Some lbl' -> lbl'
  | None ->
    Misc.fatal_errorf
      "Cfg_equiv_subst.subst_label: no substitution for label %a" Label.format
      lbl

let subst_symbol subst sym =
  match Cmm.Symbol_tbl.find_opt subst.symbols sym with
  | Some sym' -> sym'
  | None -> sym

let subst_func_symbol subst name =
  match Misc.Stdlib.String.Tbl.find_opt subst.func_symbols name with
  | Some name' -> name'
  | None -> name
