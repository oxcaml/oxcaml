open Lambda

module Or_missing : sig
  type 'a t =
    | Present of 'a
    | Missing

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( |>> ) : 'a t -> ('a -> 'b) -> 'b t
  end
end

type template_id

type env

type closure =
  { clo_params : Slambdaident.t array;
    clo_body : slambda;
    clo_env : env
  }

type halves =
  { slv_comptime : value Or_missing.t;
    slv_runtime : lambda
  }

and value =
  | SLVhalves of halves
  | SLVlayout of layout
  | SLVrecord of value Or_missing.t array
  | SLVclosure of template_id

val print_value : Format.formatter -> value -> unit

val print_halves : Format.formatter -> halves -> unit

val print_or_missing : Format.formatter -> value Or_missing.t -> unit

val print_closure : Format.formatter -> closure -> unit

module Env : sig
  type t = env

  val empty : t

  val add : t -> Slambdaident.t -> value Or_missing.t -> t

  val add_present : t -> Slambdaident.t -> value -> t

  val find : t -> Slambdaident.t -> value Or_missing.t
end

module Templates : sig
  type id = template_id

  type templates

  type t

  val empty : unit -> t

  val empty_templates : unit -> templates

  val add :
    t -> cu:Compilation_unit.t -> name:Slambdaident.t option -> closure -> id

  val add_foreign_templates : t -> templates -> unit

  val instantiate :
    t ->
    id ->
    value array ->
    (closure -> value array -> lambda) ->
    value Or_missing.t

  val templates : t -> templates

  val instantiations : t -> (Ident.t * lambda) list

  (** The set of idents bound to template instantiations in [instantiations].
      These are the [Ident.t]s of the let-bindings that wrap a compilation
      unit's body after [Slambdaeval]; they are persistent idents whose names
      are the canonical per-instance linkage names used for cross-unit
      deduplication of monomorphized layout-polymorphic functions. *)
  val instantiation_idents : t -> Ident.Set.t

  val print_templates : Format.formatter -> templates -> unit
end
