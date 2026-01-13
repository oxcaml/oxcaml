type param = Fexpr.prim_param

type t = Fexpr.prim_op

type to_fl_env = Fexpr_to_flambda_commons.env

type of_fl_env = Flambda_to_fexpr_commons.Env.t

(** general 2-way translation between fexpr-related and flambda-related types *)
type ('p, 't, 'r) lens =
  { of_fl : of_fl_env -> 'p -> 't;
    to_fl : to_fl_env -> 't -> 'r
  }

(** reversible lens *)
type ('a, 'b) map_lens = ('a, 'b, 'a) lens

(** fexpr primitive parameter payload conversion *)
type 'p value_lens = ('p, string Fexpr.located) map_lens

(** parameters conversion *)
type 'p params_lens = ('p, param list) map_lens

(** Full primitive translation assuming prior pattern-matching *)
type 'p prim_lens = ('p, t, Simple.t list -> Flambda_primitive.t) lens

(** reconversion to fexpr *)
type 'p conv = of_fl_env -> 'p -> t

(** composable lens constructor *)
type 'p param_cons

(** pattern recognition constructor *)
type 'p case_cons

type 'p cons0 = to_fl_env -> 'p -> Flambda_primitive.nullary_primitive

type 'p cons1 = to_fl_env -> 'p -> Flambda_primitive.unary_primitive

type 'p cons2 = to_fl_env -> 'p -> Flambda_primitive.binary_primitive

type 'p cons3 = to_fl_env -> 'p -> Flambda_primitive.ternary_primitive

type 'p cons4 = to_fl_env -> 'p -> Flambda_primitive.quaternary_primitive

(** Final boxing to primitive from parameters values

    The variadic version also provides the actual number of arguments *)
type 'p consN = to_fl_env -> 'p -> int -> Flambda_primitive.variadic_primitive

val wrap_loc : 'a -> 'a Fexpr.located

val unwrap_loc : 'a Fexpr.located -> 'a

(** All the necessary smart constructors to describe the structure of a fexpr
    primitive and its conversion from/to flambda.

    A primitive is necessarily described with its arity with the {!val:nullary}
    to {!val:variadic} constructors. Its given the syntacic name of the
    primitive (with the [%]), and parameters. It returns the conversion function
    from the flambda side, and register internally the reverse conversion that
    can be looked-up with {!val:lookup_prim}.

    The complexity comes from the description of the parameters of the primitive
    variant itself, which can be arbitrary caml values.

    As {!type:param} shows, there is three kinds of primitive parameter:
    - Positional, written [.(value)]
    - Flag, written [.flag]
    - Labeled, written [.label(value)]

    Flags and labeled can appear in any order, at any positions. Positional and
    labeled are provided with {!type:value_lens} to convert their payload.

    The constructors provide composable {!type:param_cons} values. They allow
    the construction of more complex types, such as tuples, list, options or
    boolean. *)
module Describe : sig
  (** {2 Primitive descriptors} *)

  (** Leaves the primitive unimplemented, will raise error if encountered during
      conversion *)
  val todo : string -> 'p conv

  (** For {!type:Flambda_primitive.nullary_primitive}. Takes [~params]
      constructor and final primitive variant wraping function. *)
  val nullary : string -> params:'p param_cons -> 'p cons0 -> 'p conv

  (** For {!type:Flambda_primitive.unary_primitive}. Takes [~params] constructor
      and final primitive variant wraping function. *)
  val unary : string -> params:'p param_cons -> 'p cons1 -> 'p conv

  (** For {!type:Flambda_primitive.binary_primitive}. Takes [~params]
      constructor and final primitive variant wraping function. *)
  val binary : string -> params:'p param_cons -> 'p cons2 -> 'p conv

  (** For {!type:Flambda_primitive.ternary_primitive}. Takes [~params]
      constructor and final primitive variant wraping function. *)
  val ternary : string -> params:'p param_cons -> 'p cons3 -> 'p conv

  (** For {!type:Flambda_primitive.quaternary_primitive}. Takes [~params]
      constructor and final primitive variant wraping function. *)
  val quaternary : string -> params:'p param_cons -> 'p cons4 -> 'p conv

  (** For {!type:Flambda_primitive.variadic_primitive}. Takes [~params]
      constructor and final primitive variant wraping function. *)
  val variadic : string -> params:'p param_cons -> 'p consN -> 'p conv

  (** {2 Parameter descriptors} *)

  (** Same as {!val:todo} for specific parameter. Can be buried in some specific
      cases. *)
  val todop : string -> 'p param_cons

  (** Syntactic positional parameter *)
  val positional : 'a value_lens -> 'a param_cons

  (** Syntactic labeled parameter *)
  val labeled : string -> 'a value_lens -> 'a param_cons

  (** Syntactic flag parameter. Flags have no values and the only information
      they carry is their presence. {!val:flag} has little reason to appear by
      itself except for pattern description. It will mostly appear within
      {!val:constructor_flag}. *)
  val flag : string -> unit param_cons

  (** Same as {!val:flag} but always matches and tell if the flag has been
      matched or not. *)
  val bool_flag : string -> bool param_cons

  (** Takes an associative list of flags and corresponding value. With optionnal
      handling when encountering an unlisted value.

      Caution: Implementing types that way do not ensure exhaustivity
      unfortunately. The no match handler can be used to enforce it by matching
      on its argument. *)
  val constructor_flag :
    ?no_match_handler:('p -> param list) -> (string * 'p) list -> 'p param_cons

  (** State no parameters are expected. Only useful at toplevel. *)
  val no_param : unit param_cons

  (** Couple of constructions. *)
  val param2 : 'a param_cons -> 'b param_cons -> ('a * 'b) param_cons

  (** Triple of constructions. *)
  val param3 :
    'a param_cons -> 'b param_cons -> 'c param_cons -> ('a * 'b * 'c) param_cons

  (** Specify a default value for the underlying constructor, which will be used
      if there is no match. In conversion to fexpr, if the provided parameter
      equals the default, no parameter will be produced. [~eq] allows the
      optionnal definition of equality function, defaults to {!val:Stdlib.(=)}.
  *)
  val default :
    def:'p -> ?eq:('p -> 'p -> bool) -> 'p param_cons -> 'p param_cons

  (** Makes a construction optionnal. Always matches. *)
  val option : 'p param_cons -> 'p option param_cons

  (** Describes repetition of the given construction. *)
  val list : 'p param_cons list -> 'p list param_cons

  (** Custom transformation of parameter value. *)
  val map : ('b, 'a) map_lens -> 'a param_cons -> 'b param_cons

  (** Simple pattern-matching of parameters. Takes a list of case construction
      (see {!val:case}) and represents the first that matches in appearance
      order.

      Caution: exhaustivity is no enforced. And patterns may be fragile, take
      care to order them properly to avoid mismatch, especially with optionnal
      parameters. *)
  val either :
    ?no_match_handler:('p -> param list) -> 'p case_cons list -> 'p param_cons

  (** Describes a constructor pattern. Takes the construction we want to match.
      - [~box] allows mapping of the matched value, usually to wrap it in the
        type of the enclosing {!val:either}.
      - [~unbox] is the reverse but expect an optionnal return value, to tell if
        the value corresponds to the described case. This is the only place
        where exhaustivity can be enforced. *)
  val case :
    box:(to_fl_env -> 'c -> 'p) ->
    unbox:(of_fl_env -> 'p -> 'c option) ->
    'c param_cons ->
    'p case_cons

  (** Same as {!val:case}, but [~box] and [~unbox] are identity. *)
  val id_case : 'p param_cons -> 'p case_cons

  (** {2 Parameter payload descriptors}

      Can be extended here. Or defined elsewhere for specific values. *)

  (** Raw value *)
  val string : string Fexpr.located value_lens

  (** Parsed as integer. Fails if the string is not one. *)
  val int : int value_lens

  (** Same as {!val:string}. With some tweaks to the parser, we could call
      actual parsing start-points in it. *)
  val diy : string Fexpr.located value_lens

  (** Similar to {!val:constructor_flag} but for payload. Exhaustivity not
      enforced. *)
  val constructor_value : (string * 'p) list -> 'p value_lens
end

(** Fetch primitive conversion function from registered descriptions *)
val lookup_prim :
  t -> (to_fl_env -> t -> Simple.t list -> Flambda_primitive.t) option
