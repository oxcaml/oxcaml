(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Leo White, Jane Street, London                     *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Effect reflection.

    After Leo White's "Locality and Effect Reflection"
    ({{:https://github.com/lpw25/effect-reflection}lpw25/effect-reflection}),
    with the value-level functors of that library replaced by
    module-dependent functions (the [modular_explicits] extension).

    For an algebraic signature [O], [reify] and [reflect] witness the
    isomorphism between free-monad terms ['a Term(O).t] and
    handler-passing functions [Handler(O).t @ local -> 'a]. Handlers are
    passed at mode [local], so a computation cannot leak the ability to
    perform operations outside the dynamic extent of its handler. *)

(** The type of algebraic signatures: ['a O.t] is the type of operations
    returning ['a]. *)
module type Op = sig
  type 'a t
end

(** Terms of the free monad over a signature. *)
module Term (O : Op) : sig
  type 'a t =
    | Return : 'a @@ global -> 'a t
    | Op : 'r O.t @@ global * ('r -> 'a t) -> 'a t
end

(** A handler for the operations of a signature, valid within a scope. *)
module Handler (O : Op) : sig
  type t
end

(** [reify (module O) f] runs [f] with a handler for [O], reifying the
    operations [f] performs (and its final result) as a term of the free
    monad over [O]. *)
val reify : (module O : Op) -> (Handler(O).t @ local -> 'a) -> 'a Term(O).t

(** [perform (module O) op h] performs the operation [op] using the
    handler [h]. *)
val perform : (module O : Op) -> 'a O.t -> Handler(O).t @ local -> 'a

(** [reflect (module O) t h] interprets the term [t] by performing each
    of its operations using the handler [h]. Inverse to [reify]. *)
val reflect : (module O : Op) -> 'a Term(O).t -> Handler(O).t @ local -> 'a

(** The sum of two signatures. *)
module Sum (L : Op) (R : Op) : sig
  type 'a t =
    | Left : 'a L.t -> 'a t
    | Right : 'a R.t -> 'a t
end

(** [outl (module L) (module R) h] restricts a handler for [Sum(L)(R)]
    to a handler for [L]. *)
val outl :
  (module L : Op) -> (module R : Op) ->
  Handler(Sum(L)(R)).t @ local -> Handler(L).t @ local

(** [outr (module L) (module R) h] restricts a handler for [Sum(L)(R)]
    to a handler for [R]. *)
val outr :
  (module L : Op) -> (module R : Op) ->
  Handler(Sum(L)(R)).t @ local -> Handler(R).t @ local

(** Signatures parameterized by an extra type: [('a, 'e) O.t] is the
    type of operations returning ['a], at parameter ['e]. *)
module type Op1 = sig
  type ('a, 'e) t
end

(** Terms of the free monad over a parameterized signature. *)
module Term1 (O : Op1) : sig
  type ('a, 'e) t =
    | Return : 'a @@ global -> ('a, 'e) t
    | Op : ('r, 'e) O.t @@ global * ('r -> ('a, 'e) t) -> ('a, 'e) t
end

(** A handler for the operations of a parameterized signature. *)
module Handler1 (O : Op1) : sig
  type 'e t
end

(** [reify1] is [reify] for parameterized signatures. *)
val reify1 :
  (module O : Op1) ->
  ('e Handler1(O).t @ local -> 'a) -> ('a, 'e) Term1(O).t

(** [perform1] is [perform] for parameterized signatures. *)
val perform1 :
  (module O : Op1) -> ('a, 'e) O.t -> 'e Handler1(O).t @ local -> 'a

(** [reflect1] is [reflect] for parameterized signatures. *)
val reflect1 :
  (module O : Op1) ->
  ('a, 'e) Term1(O).t -> 'e Handler1(O).t @ local -> 'a
