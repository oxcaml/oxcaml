type param = Fexpr.prim_param

type t = Fexpr.prim_op

type decode_env = Fexpr_to_flambda_commons.env

type encode_env = Flambda_to_fexpr_commons.Env.t

(** general 2-way translation between fexpr-related and flambda-related types *)
type ('p, 't, 'r) lens =
  { encode : encode_env -> 'p -> 't;
    decode : decode_env -> 't -> 'r
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
type 'p conv = encode_env -> 'p -> t

(** composable lens constructor *)
type 'p param_cons

(** pattern recognition constructor *)
type 'p case_cons

type 'p cons0 = decode_env -> 'p -> Flambda_primitive.nullary_primitive

type 'p cons1 = decode_env -> 'p -> Flambda_primitive.unary_primitive

type 'p cons2 = decode_env -> 'p -> Flambda_primitive.binary_primitive

type 'p cons3 = decode_env -> 'p -> Flambda_primitive.ternary_primitive

type 'p cons4 = decode_env -> 'p -> Flambda_primitive.quaternary_primitive

(** Final boxing to primitive from parameters values

    The variadic version also provides the actual number of arguments *)
type 'p consN = decode_env -> 'p -> int -> Flambda_primitive.variadic_primitive

val wrap_loc : 'a -> 'a Fexpr.located

val unwrap_loc : 'a Fexpr.located -> 'a

(** All the necessary smart constructors to describe the structure of a fexpr
    primitive and its conversion from/to flambda.

    A primitive is necessarily described with its arity with the {!val:nullary}
    to {!val:variadic} constructors. It is given the syntactic name of the
    primitive (with the [%]), and parameters. It returns the conversion function
    from the flambda side, and register internally the reverse conversion that
    can be looked-up with {!val:lookup_prim}.

    The complexity comes from the description of the parameters of the primitive
    variant itself, which can be arbitrary caml values.

    As {!type:param} shows, there is two kinds of primitive parameter:
    - Tuples, written [.[val1, ..., valN]]
    - Labeled, written [.label[val1, ..., valN]]. They can be without arguments
      [.label] to denote flags.

    Flags and labeled can appear in any order, at any positions. Both of them
    can be nested, the dot separator is only for toplevel parameters.

    The type {!type:value_lens} is used to describe conversions of custom types.
    Label, flags and inner values are all treated the same.

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

  (** Constructor from simple lens *)
  val value : 'a value_lens -> 'a param_cons

  (** Syntactic positional parameter *)
  val positional : 'a param_cons -> 'a param_cons

  (** Syntactic labeled parameter *)
  val labeled : string -> 'a param_cons -> 'a param_cons

  (** Syntactic flag parameter. Flags have no values and the only information
      they carry is their presence. {!val:flag} has little reason to appear by
      itself except for pattern description. It will mostly appear within
      {!val:constructor_flag}. *)
  val flag : string -> unit param_cons

  (** Same as {!val:flag} but always matches and tell if the flag has been
      matched or not. *)
  val bool_flag : string -> bool param_cons

  (** Takes an associative list of flags and corresponding value. With optional
      handling when encountering an unlisted value.

      Caution: Implementing types that way do not ensure exhaustivity
      unfortunately. The no match handler can be used to enforce it by matching
      on its argument. *)
  val constructor_flag :
    ?no_match_handler:('p -> unit) -> (string * 'p) list -> 'p param_cons

  (** State no parameters are expected. Only useful at toplevel. *)
  val param0 : unit param_cons

  (** Couple of constructions. *)
  val param2 : 'a param_cons -> 'b param_cons -> ('a * 'b) param_cons

  (** Triple of constructions. *)
  val param3 :
    'a param_cons -> 'b param_cons -> 'c param_cons -> ('a * 'b * 'c) param_cons

  (** Triple of constructions. *)
  val param4 :
    'a param_cons ->
    'b param_cons ->
    'c param_cons ->
    'd param_cons ->
    ('a * 'b * 'c * 'd) param_cons

  (** Triple of constructions. *)
  val param5 :
    'a param_cons ->
    'b param_cons ->
    'c param_cons ->
    'd param_cons ->
    'e param_cons ->
    ('a * 'b * 'c * 'd * 'e) param_cons

  (** Specify a default value for the underlying constructor, which will be used
      if there is no match. In conversion to fexpr, if the provided parameter
      equals the default, no parameter will be produced. [~eq] allows the
      optional definition of equality function, defaults to {!val:Stdlib.(=)}.
  *)
  val default :
    def:'p -> ?eq:('p -> 'p -> bool) -> 'p param_cons -> 'p param_cons

  (** Makes a construction optional. Always matches. *)
  val option : 'p param_cons -> 'p option param_cons

  (** Makes a list of a construction. Atomized to make the list explicit,
      expects its own level of bracket syntactically *)
  val list : 'p param_cons -> 'p list param_cons

  (** Custom transformation of parameter value.

      Labels inverted to be related to the argument. [maps x ~to_ ~from] read
      "maps [x] to return type" and "maps [x] from return type" *)
  val maps :
    to_:(encode_env -> 'b -> 'a) ->
    from:(decode_env -> 'a -> 'b) ->
    'a param_cons ->
    'b param_cons

  (** Pattern maching *)

  (** Using monadic notation, we define patterns using [let|] binding a
      [param_cons] and a boxing function, ending with a call to [return_either]
      with a full pattern matching on the outer type. This is then bound to the
      resulting [param_cons] with [let|=]. The order of declaration of pattern
      is the order in which they will be tried against. Example:

      {[
      let|= example =
          let| b = bool, (fun _ b -> if b then 1 else 0) in
          let| i = int, (fun _ -> Fun.id) in
          return_either
            (fun env p ->
               match p with
               | 0 -> b env false
               | 1 -> b env true
               | x -> i env x)
      ]}

      This is a [int param_cons] encoded to either a bool ["true" | "false"] or
      an int. *)

  type 'p encode_case = encode_env -> 'p -> param list

  type 'p build_either

  val return_either : (encode_env -> 'p -> param list) -> 'p build_either

  val ( let| ) :
    'case param_cons * (decode_env -> 'case -> 'p) ->
    ('case encode_case -> 'p build_either) ->
    'p build_either

  val ( let|= ) : 'p build_either -> ('p param_cons -> 'a) -> 'a

  val param2_case :
    decode:(decode_env -> 'c1 -> 'c2 -> 'p) ->
    'c1 param_cons ->
    'c2 param_cons ->
    ('c1 * 'c2) param_cons * (decode_env -> 'c1 * 'c2 -> 'p)

  val param3_case :
    decode:(decode_env -> 'c1 -> 'c2 -> 'c3 -> 'p) ->
    'c1 param_cons ->
    'c2 param_cons ->
    'c3 param_cons ->
    ('c1 * 'c2 * 'c3) param_cons * (decode_env -> 'c1 * 'c2 * 'c3 -> 'p)

  val param4_case :
    decode:(decode_env -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'p) ->
    'c1 param_cons ->
    'c2 param_cons ->
    'c3 param_cons ->
    'c4 param_cons ->
    ('c1 * 'c2 * 'c3 * 'c4) param_cons
    * (decode_env -> 'c1 * 'c2 * 'c3 * 'c4 -> 'p)

  val param5_case :
    decode:(decode_env -> 'c1 -> 'c2 -> 'c3 -> 'c4 -> 'c5 -> 'p) ->
    'c1 param_cons ->
    'c2 param_cons ->
    'c3 param_cons ->
    'c4 param_cons ->
    'c5 param_cons ->
    ('c1 * 'c2 * 'c3 * 'c4 * 'c5) param_cons
    * (decode_env -> 'c1 * 'c2 * 'c3 * 'c4 * 'c5 -> 'p)

  (** {2 Parameter payload descriptors}

      Can be extended here. Or defined elsewhere for specific values. *)

  (** Raw value *)
  val string : string Fexpr.located param_cons

  (** Parsed as integer. Fails if the string is not one. *)
  val int : int param_cons

  (** Parsed as boolean. Fails if the string is not one. *)
  val bool : bool param_cons

  (** Same as {!val:string}. With some tweaks to the parser, we could call
      actual parsing start-points in it. *)
  val diy : string Fexpr.located param_cons
end

(** Fetch primitive conversion function from registered descriptions *)
val lookup_prim :
  t -> (decode_env -> t -> Simple.t list -> Flambda_primitive.t) option
