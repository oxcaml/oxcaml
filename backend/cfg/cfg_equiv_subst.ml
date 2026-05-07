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
