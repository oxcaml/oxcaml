(******************************************************************************
 *                                  OxCaml                                    *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025--2026 Jane Street Group LLC                             *
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

(** Evaluate a quoted OCaml expression at runtime. *)
val eval : 'a expr @ once -> 'a eval

module Injector : sig
  (** A store of values injected into quotes for evaluation in the context
      of the current program execution; see [inject].  An injector
      corresponds to a compilation unit synthesised at runtime, whose module
      block holds the injected values. *)
  type t
end

(** [inject injector x] returns an expression denoting the value [x], which
    can be spliced into quotes evaluated by the [eval_with_injector] call
    that supplied [injector].  This works for values of types that cannot be
    universally injected (refs, closures, file descriptors, ...) because the
    resulting expressions are only meaningful within the current program
    execution.

    CR metaprogramming: nothing yet prevents such expressions from escaping
    the [eval_with_injector] callback (e.g. by being printed and compiled
    separately, or evaluated under a different injector), which is undefined
    behaviour.  This will be enforced with modes. *)
external inject : Injector.t -> 'a eval -> 'a expr = "%inject"

(** As [eval], but supplying an injector so that the expression being
    evaluated can contain injected values. *)
val eval_with_injector :
  (Injector.t -> 'a expr @ once) @ local once -> 'a eval
