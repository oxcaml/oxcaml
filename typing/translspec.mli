(******************************************************************************
 *                                  OxCaml                                    *
 *                          Simon Spies, Jane Street                          *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(** [spectree ~bound e] checks that [e] is in the specification language.
    [bound] contains the identifiers bound outside of [e] that are local to the
    specification (e.g., the parameters of a law). Every other identifier is a
    global. Raises [Error] on a construct outside the language. *)
val spectree :
  bound:Ident.Set.t -> Typedtree.expression -> Typedtree.spec_expression

(** [typedtree e] is the inverse of [spectree]. It reconstructs the typedtree
    expression that [e] was produced from. *)
val typedtree : Typedtree.spec_expression -> Typedtree.expression

(** [spec e] strips [e] of its types. Sort variables are defaulted. *)
val spec : Typedtree.spec_expression -> Spec.expression

type error

exception Error of Location.t * error

val report_error : error Format_doc.printer
