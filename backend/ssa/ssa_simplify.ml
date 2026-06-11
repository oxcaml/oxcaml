(******************************************************************************
 *                                  OxCaml                                    *
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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-40-41-42"]

open Ssa.Export
open Ssa_reducer

(* Ssa_of_cmm wraps every two-target branch condition in a comparison to zero.
   When the condition is itself a comparison, that wrapper is a redundant nested
   comparison we fold away as the [Op]s are emitted: [(cmp) <> 0] is [cmp], and
   [(cmp) = 0] is the negated comparison. The folded result flows to the
   [Switch] index through the reducer's op map, so usually no terminator
   rewriting is needed. When the comparison cannot be flipped into a single op
   (e.g. an [and 1] parity test), we instead fold [(cmp) = 0] at the [Switch] by
   swapping its two targets. Folding lets Cfg_of_ssa fuse the inner comparison
   into the branch test instead of materializing a bit. *)
module Simplify_conditions (C : Context) = struct
  open! C
  include Default (C)

  (* The negation of a comparison op, when it is itself a single op. *)
  let negate_comparison (op : Ssa.op) : Ssa.op option =
    match[@warning "-fragile-match"] op with
    | Intop (Icomp c) -> Some (Intop (Icomp (Cmm.negate_integer_comparison c)))
    | Intop_imm (Icomp c, n) ->
      Some (Intop_imm (Icomp (Cmm.negate_integer_comparison c), n))
    | Floatop (w, Icompf c) ->
      Some (Floatop (w, Icompf (Cmm.negate_float_comparison c)))
    | _ -> None

  (* Whether a value is a comparison result, hence already 0/1. *)
  let is_comparison (v : out Value.t) =
    match[@warning "-fragile-match"] v with
    | Res
        ( { op =
              ( Intop (Icomp _)
              | Intop_imm (Icomp _, _)
              | Floatop (_, Icompf _)
              | Intop_imm (Iand, 1) );
            _
          },
          0 ) ->
      true
    | _ -> false

  let emit_op () ctx c ~op ~dbg ~typ ~(args : out Value.t array) =
    match[@warning "-fragile-match"] op, args with
    | Operation.Intop_imm (Icomp Cne, 0), [| arg |] when is_comparison arg ->
      Emitted_replacement [| arg |]
    | ( Operation.Intop_imm (Icomp Ceq, 0),
        [| Res ({ op = inner; args = inner_args; _ }, 0) |] ) -> (
      match negate_comparison inner with
      | Some negated ->
        Emitted_replacement (Cursor.emit_op ctx c negated dbg typ inner_args)
      | None -> For_next_reducer)
    | _ -> For_next_reducer

  (* Fallback for [(cmp) = 0] feeding a two-target switch when [cmp] could not
     be flipped (so [emit_op] left the comparison in place): swap the targets
     and use [cmp] directly as the index. *)
  let finish_block () ctx c ~dbg (t : out Terminator.t) =
    match[@warning "-fragile-match"] t with
    | Switch
        { index = Res ({ op = Intop_imm (Icomp Ceq, 0); args = [| arg |]; _ }, 0);
          targets = [| ifnot; ifso |]
        }
      when is_comparison arg ->
      Cursor.finish_block ctx c ~dbg
        (Switch { index = arg; targets = [| ifso; ifnot |] });
      Emitted_replacement ()
    | _ -> For_next_reducer
end

module Runner = Make_run (Simplify_conditions)

let run ssa = Runner.run ssa
