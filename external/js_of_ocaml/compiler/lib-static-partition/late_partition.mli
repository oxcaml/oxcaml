(** Split a globally optimized program with deferred roots into eager code and one
    program per root. *)

open Js_of_ocaml_compiler

module Report : sig
  type value =
    { id : int
    ; name : string option
    ; definition : string
    ; literal_digest : string option
    }

  type t =
    { captures : value list
    ; relocated : value list
    ; rounds : int
    ; globally_optimized_blocks : int
    ; eager_blocks : int
    ; lazy_blocks : int
    }

  val to_json : t -> Yojson.Safe.t
end

type t

val split_exn : Code.program -> t
(** Split a globally optimized program with a single deferred root before closure
    conversion. Only constants, closures, and immutable blocks can move from eager
    initialization into the lazy program. Other crossing values retain their eager
    initialization and cross in one shared block. Raises [Failure] on unsupported
    ownership rather than copying initialized values. *)

type many

val split_many_exn : Code.program -> many
(** Split all sibling roots in one ownership fixed point. Shared values keep one eager
    owner; each output imports only the captures it needs from the union bridge. *)

val eager_many : many -> Code.program

val lazy_roots : many -> (string * Code.program) list

val reports : many -> (string * Report.t) list

val eager : t -> Code.program

val lazy_ : t -> Code.program

val report : t -> Report.t

val bridge_name : string
(** The global under which eager code registers the values roots capture. *)
