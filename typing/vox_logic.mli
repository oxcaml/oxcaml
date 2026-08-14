(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Jules Jacobs, Jane Street                             *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The vox logic: a standalone, quantifier-free term language and the
    obligations handed to solver backends (see {!Vox_backend}).

    Nothing here knows about refinement types.  Translation from refinements
    resolves every OCaml symbol and produces a closed {!Signature.t}; a
    backend never consults the typing environment. *)

module Sort : sig
  type t =
    | Bool
    | Int
        (** Mathematical, unbounded integers ([Bigint]). *)
    | Bitvec of int
        (** OCaml [int] is [Bitvec 63].  This is why
            [x >= 0 |- x + 1 >= 0] is {e false} (it wraps at [max_int]) and
            why [abs] in the naive form does not verify ([abs min_int] is
            negative).  A reader expecting mathematical integers here will
            conclude the solver is broken; it is the arithmetic that wraps. *)
    | Uninterpreted of string
        (** An abstract type behind a signature: values are opaque and only
            the declared function symbols relate them. *)
    | Datatype of string
        (** A ground instance produced by {!Signature.instantiate}. *)

  val equal : t -> t -> bool

  (** A short name for use inside mangled instance names: [Bool], [Int],
      [Bv63], or the sort's own name. *)
  val key : t -> string
end

(** Interpreted operators.  These are SMT-LIB operators, not OCaml ones:
    encoding OCaml semantics (division by zero, shift ranges, ...) is the
    translation's job, composing these primitives. *)
module Op : sig
  type t =
    (* Bool *)
    | Not
    | And
    | Or
    | Implies
    (* any sort *)
    | Eq
    | Distinct
    (* Int: mathematical.  [Div]/[Mod] are SMT-LIB euclidean division. *)
    | Neg
    | Add
    | Sub
    | Mul
    | Div
    | Mod
    | Lt
    | Le
    | Gt
    | Ge
    (* Bitvec: two's complement.  Comparisons and division are signed. *)
    | Bv_neg
    | Bv_add
    | Bv_sub
    | Bv_mul
    | Bv_sdiv
    | Bv_srem
    | Bv_not
    | Bv_and
    | Bv_or
    | Bv_xor
    | Bv_shl
    | Bv_lshr
    | Bv_ashr
    | Bv_slt
    | Bv_sle
    | Bv_sgt
    | Bv_sge
end

module Literal : sig
  type t =
    | Bool of bool
    | Int of string
        (** Decimal digits with an optional leading [-]; unbounded. *)
    | Bitvec of { width : int; value : int64 }
        (** The low [width] bits of [value], two's complement;
            [1 <= width <= 64]. *)

  (** An OCaml [int] constant: [Bitvec { width = 63; value }]. *)
  val ocaml_int : int -> t
end

module Term : sig
  (** Variables carry no sort at the occurrence: the signature declares
      them, so two occurrences of one variable cannot disagree and
      ill-sortedness of a variable is unrepresentable rather than checked. *)
  type t =
    | Var of string
    | Const of Literal.t
    | App of Op.t * t list
    | Call of string * t list
        (** An uninterpreted function symbol from the signature. *)
    | Ite of t * t * t
    | Construct of string * t list
    | Select of string * int * t
        (** [Select (constructor, i, t)] projects field [i] (0-based) of
            [constructor] out of [t].  Applied to a value built by another
            constructor it is underspecified, an arbitrary but consistent
            value rather than an error: [head Nil] is some fixed [Int]. *)
    | Test of string * t
        (** [Test (constructor, t)]: whether [t] was built by
            [constructor]. *)
end

module Origin : sig
  (** Why a hypothesis holds; diagnostic payload for unused-hypothesis
      reporting. *)
  type t =
    { label : string
    ; location : Location.t
    }
end

(** Parametric datatype declarations, before instantiation.  The translation
    builds these from OCaml type declarations; records and tuples are
    single-constructor datatypes. *)
module Datatype : sig
  (** Field types.  [Arrow] is representable only so that instantiation can
      reject it: SMT datatypes cannot store functions. *)
  type ty =
    | Bool
    | Int
    | Bitvec of int
    | Uninterpreted of string
    | Param of string
    | Apply of string * ty list
    | Arrow of ty * ty

  type constructor =
    { constructor_name : string
    ; fields : (string * ty) list  (** selector name and field type *)
    }

  type decl =
    { decl_name : string
    ; params : string list
    ; constructors : constructor list
    }
end

module Signature : sig
  (** A ground constructor: selector names and field sorts. *)
  type constructor =
    { constructor_name : string
    ; fields : (string * Sort.t) list
    }

  (** A ground datatype instance.  The renderer groups mutually recursive
      instances into one [declare-datatypes] itself; declaration order here
      does not matter. *)
  type datatype =
    { datatype_name : string
    ; constructors : constructor list
    }

  (** Everything a backend needs to interpret an obligation's terms; sorts,
      symbols and datatype declarations.  Closed: there is no environment
      behind it. *)
  type t =
    { sorts : string list  (** uninterpreted sorts, as [declare-sort] *)
    ; datatypes : datatype list
    ; variables : (string * Sort.t) list
    ; functions : (string * Sort.t list * Sort.t) list
          (** uninterpreted functions: name, argument sorts, result sort *)
    }

  val empty : t

  (** [instantiate decls roots] monomorphises: one ground datatype per
      instantiation reached from [roots], whose [Datatype.ty] arguments must
      not mention [Param].  Returns the ground instances and the sort of
      each root, in order.

      An instance of [t] at arguments [a1 ... an] is named [t<k1,...,kn>]
      (see {!Sort.key}), and its constructors and selectors are suffixed the
      same way; a nullary instantiation keeps its names unchanged.

      Declaration, constructor and selector names must be globally unique
      before mangling: instances of two declarations (or two constructors)
      whose suffixed names coincide are rejected, not merged.

      Rejected, as errors:
      - non-regular recursion ([t] used at different arguments inside its
        own definition).  The test is conservative, as in vox2: it also
        rejects some instantiation patterns whose reachable instance set is
        finite (e.g. a recursive use at constant arguments), not only the
        genuinely infinite ones;
      - function-valued fields;
      - two instantiations mangling to one instance name. *)
  val instantiate :
    Datatype.decl list ->
    (string * Datatype.ty list) list ->
    (datatype list * Sort.t list, string) result
end

module Obligation : sig
  type hypothesis =
    { id : int
          (** stable and non-negative; how a backend names hypotheses in an
              unsat core *)
    ; term : Term.t
    ; origin : Origin.t
    }

  (** The hypotheses entail the goal, or so the caller hopes. *)
  type t =
    { signature : Signature.t
    ; hypotheses : hypothesis list
    ; goal : Term.t
    ; location : Location.t
    }
end
