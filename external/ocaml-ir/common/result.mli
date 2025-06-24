open Std
open Ocaml_ir_fiber

module Reason : sig
  type t = private
    | Compiler_exn of string
    | Ocaml_ir_comp_error of Error.t
    | Build_system_file_missing of string * Error.t
    | File_missing of string * Error.t option
    | Not_in_repo of Error.t
    | Unsupported_source_file_extension
    | Cant_use_dot_merlin_file of string
    | Unsupported_language of string
    | Problem_finding_library of string
    | Problem_finding_ocaml_ir of Error.t
    | Other_error of Error.t
    | No_ir_produced of Language.t
  [@@deriving sexp]

  include sig
    [@@@ocaml.warning "-32"]

    include Sexplib0.Sexpable.S with type t := t
  end
  [@@ocaml.doc "@inline"] [@@merlin.hide]

  val to_string : t -> string
end

type 'a t = private
  | Ok of 'a
  | Error of Reason.t
[@@deriving sexp]

include sig
  [@@@ocaml.warning "-32"]

  include Sexplib0.Sexpable.S1 with type 'a t := 'a t
end
[@@ocaml.doc "@inline"] [@@merlin.hide]

val error : Reason.t -> 'a t
val compiler_exn : string -> 'a t
val ocaml_ir_comp_error : Error.t -> 'a t
val build_system_file_missing : file:string -> Error.t -> 'a t
val file_missing : ?err:Error.t -> file:string -> unit -> 'a t
val not_in_repo : Error.t -> 'a t
val unsupported_source_file_extension : 'a t
val cant_use_dot_merlin_file : string -> 'a t
val unsupported_language : string -> 'a t
val problem_finding_library : string -> 'a t
val problem_finding_ocaml_ir : Error.t -> 'a t
val no_ir_produced : Language.t -> 'a t
val other_error : Error.t -> 'a t
val ok_exn : 'a t -> 'a

include Monad.S with type 'a t := 'a t

val bind_to_deferred : 'a t -> f:('a -> 'b t Fiber.t) -> 'b t Fiber.t
val value_ignore_error : default:'a -> 'a t -> 'a

module Fiber : sig
  val ok : 'a Fiber.t -> 'a t Fiber.t

  include Monad.S with type 'a t = 'a t Fiber.t
end
