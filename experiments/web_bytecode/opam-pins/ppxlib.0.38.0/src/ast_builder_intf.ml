open! Import

module type Loc = sig
  val loc : Location.t
end

module type Additional_helpers = sig
  type 'a with_loc

  val eint : (int -> expression) with_loc
  val echar : (char -> expression) with_loc
  val estring : (string -> expression) with_loc
  val efloat : (string -> expression) with_loc
  val eint32 : (int32 -> expression) with_loc
  val eint64 : (int64 -> expression) with_loc
  val enativeint : (nativeint -> expression) with_loc
  val ebool : (bool -> expression) with_loc
  val pint : (int -> pattern) with_loc
  val pchar : (char -> pattern) with_loc
  val pstring : (string -> pattern) with_loc
  val pfloat : (string -> pattern) with_loc
  val pint32 : (int32 -> pattern) with_loc
  val pint64 : (int64 -> pattern) with_loc
  val pnativeint : (nativeint -> pattern) with_loc
  val pbool : (bool -> pattern) with_loc
  val eunit : expression with_loc
  val punit : pattern with_loc

  val evar : (string -> expression) with_loc
  (** [evar id] produces a [Pexp_ident _] expression, it parses its input so you
      can pass any dot-separated identifier, for instance:
      [evar ~loc "Foo.bar"]. *)

  val pvar : (string -> pattern) with_loc

  val eapply : (expression -> expression list -> expression) with_loc
  (** Same as pexp_apply but without labels *)

  val eabstract : (pattern list -> expression -> expression) with_loc
  val esequence : (expression list -> expression) with_loc
  val ppat_tuple_opt : (pattern list -> pattern option) with_loc
  val pexp_tuple_opt : (expression list -> expression option) with_loc

  val pexp_fun :
    (arg_label -> expression option -> pattern -> expression -> expression)
    with_loc
  (** [pexp_fun] can be used to create function expressions. It will check if
      the function's body is itself a function expression and if so it will
      coalesce the arguments.

      For example, if we have [pexp_fun Nolabel None (var "x") f] and [f] is
      [fun y -> x + y] then the function expression returned will be
      [fun x y -> x + y] and not [fun x -> y -> x + y]. However, it will be more
      efficient to create maximum arity functions directly with
      {! pexp_function}. *)

  val pexp_function :
    (function_param list ->
    type_constraint option ->
    function_body ->
    expression)
    with_loc

  val pexp_function_cases : (Import.cases -> expression) with_loc
  (** [pexp_function_cases] builds an expression in the shape
      [function C1 -> E1 | ...]. *)

  val pconstruct : constructor_declaration -> pattern option -> pattern
  val econstruct : constructor_declaration -> expression option -> expression

  val elist_tail : (expression list -> expression -> expression) with_loc
  (** [elist_tail ~loc [expr1; expr2; expr3] expr_tail] produces the expression
      [expr1::expr2::expr3::expr_tail]. *)

  val elist : (expression list -> expression) with_loc
  (** [elist ~loc [expr1; expr2; expr3]] produces the list litteral expression
      [[expr1; expr2; expr3]]. *)

  val plist_tail : (pattern list -> pattern -> pattern) with_loc
  (** [plist_tail ~loc [pat1; pat2; pat3] pat_tail] produces the pattern
      [pat1::pat2::pat3::pat_tail]. *)

  val plist : (pattern list -> pattern) with_loc
  (** [plist ~loc [pat1; pat2; pat3]] produces the list pattern
      [[pat1; pat2; pat3]]. *)

  val value_binding :
    (pat:Import.pattern -> expr:Import.expression -> Import.value_binding)
    with_loc

  val pstr_value_list :
    loc:Location.t ->
    Asttypes.rec_flag ->
    value_binding list ->
    structure_item list
  (** [pstr_value_list ~loc rf vbs] = [pstr_value ~loc rf vbs] if [vbs <> []],
      [[]] otherwise. *)

  val nonrec_type_declaration :
    (name:string Loc.t ->
    params:(core_type * Asttypes.variance) list ->
    cstrs:(core_type * core_type * Location.t) list ->
    kind:type_kind ->
    private_:Asttypes.private_flag ->
    manifest:core_type option ->
    type_declaration)
    with_loc
  [@@deprecated
    "[since 2016-10] use Nonrecursive on the P(str|sig)_type instead"]

  val unapplied_type_constr_conv :
    (Longident.t Loc.t -> f:(string -> string) -> expression) with_loc
  (** [unapplied_type_constr_conv] is the standard way to map identifiers to
      conversion fonctions, for preprocessor that creates values that follow the
      structure of types. More precisely,
      [path_conv path (sprintf "sexp_of_%s")] is:

      - sexp_of_t if path is "t"
      - A.B.sexp_of_foo if path is "A.B.foo"
      - A.B.sexp_of_f__foo (module A1) (module A2) if path is
        "A.B.F(A1)(A2).foo" [type_constr_conv] also applies it to a list of
        expression, which both prevents the compiler from allocating useless
        closures, and almost always what is needed, since type constructors are
        always applied. *)

  val type_constr_conv :
    (Longident.t Loc.t -> f:(string -> string) -> expression list -> expression)
    with_loc

  val eta_reduce : expression -> expression option
  (** Tries to simplify [fun v1 v2 .. -> f v1 v2 ..] into [f]. Only works when
      [f] is a path, not an arbitrary expression as that would change the
      meaning of the code. This can be used either for cleaning up the generated
      code, or to reduce allocation if [f] is a local variable (the compiler
      won't optimize the allocation of the closure).

      Eta-reduction can change the types/behavior in some corner cases that are
      unlikely to show up in generated code:

      - if [f] has optional arguments, eta-expanding [f] can drop them
      - because labels commute, it can change the type of an expression: $ let f
        ~x y = x + y let f2 = fun x -> add x;; val f : x:int -> int -> int =
        <fun> val f2 : int -> x:int -> int = <fun> In fact, if [f] does side
        effects before receiving all its arguments, and if the eta-expansion is
        partially applied, eta-reducing could change behavior.

      [eta_reduce_if_possible_and_nonrec] is meant for the case where the
      resulting expression is going to be bound in a potentially recursive
      let-binding, where we have to keep the eta-expansion when [rec_flag] is
      [Recursive] to avoid a compile error. *)

  val eta_reduce_if_possible : expression -> expression

  val eta_reduce_if_possible_and_nonrec :
    expression -> rec_flag:rec_flag -> expression

  (** {2:future-asts Compat functions for future AST nodes}

      The functions in this section provide a safe interface to generate AST
      nodes that cannot be represented with Ppxlib's own AST but are available
      with more recent versions of the compiler.

      Note that producing such nodes will make the generated code incompatible
      with compilers older than the feature you are trying to represent. Those
      nodes also won't play nicely with the driver's default source output or if
      printed as source using [Ppxlib.Pprintast]. You can use the
      --use-compiler-pp flag of the driver to use your current compiler's AST to
      source printers. *)

  val ppat_effect : (pattern -> pattern -> pattern) with_loc
  (** Returns an encoded effect pattern as introduced in OCaml 5.3 *)

  val ptyp_labeled_tuple :
    ((string option * core_type) list -> core_type) with_loc
  (** Returns an encoded labeled tuple type as introduced in OCaml 5.4. *)

  val pexp_labeled_tuple :
    ((string option * expression) list -> expression) with_loc
  (** Returns an encoded labeled tuple expression as introduced in OCaml 5.4. *)

  val ppat_labeled_tuple :
    ((string option * pattern) list -> closed_flag -> pattern) with_loc
  (** Returns an encoded labeled tuple pattern as introduced in OCaml 5.4. *)
end

module type Located = sig
  type 'a with_loc
  type 'a t = 'a Loc.t

  val loc : _ t -> Location.t
  val mk : ('a -> 'a t) with_loc
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_lident : string t -> Longident.t t
  val lident : (string -> Longident.t t) with_loc
end

type 'a without_location = 'a
type 'a with_location = loc:Location.t -> 'a

module type S = sig
  module Located : Located with type 'a with_loc := 'a without_location
  include Ast_builder_generated.Intf_located
  include Additional_helpers with type 'a with_loc := 'a without_location
end
