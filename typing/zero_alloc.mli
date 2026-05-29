type check = Builtin_attributes.zero_alloc_check =
  { strict: bool;
    opt: bool;
    arity: int;
    loc: Location.t;
    custom_error_msg : string option;
  }

type assume = Builtin_attributes.zero_alloc_assume =
  { strict: bool;
    never_returns_normally: bool;
    never_raises: bool;
    arity: int;
    loc: Location.t;
  }

type const = Builtin_attributes.zero_alloc_attribute =
  | Default_zero_alloc
  | Ignore_assert_all
  | Check of check
  | Assume of assume

type check_context =
  | Signature
  (* zero_alloc requirements come from a module signature *)
  | Fun_param
  (* mainly used when type-checking applications;
     zero_alloc requirements come from a higher-order function being applied to
     another function *)
  | Type_constraint
  (* zero_alloc requirements compared against a type constraint *)
  | Default
  (* all other contexts *)

(* This type represents whether or not a function will be checked for
   zero-alloc-ness, and with what configuration (strict, opt, etc). It can be a
   variable which will be filled in when the module the function is in is
   compared against its signature, allowing to infer zero-alloc checks. *)
type t

(* [default] corresponds to [Default_zero_alloc], meaning no check will be
   done. *)
val default : t

(* [ignore_assert_all] corresponds to [Ignore_assert_all], meaning no check will be
   done even if [Clflags.zero_alloc_assert] is set to "all" or "all_opt". *)
val ignore_assert_all : t

val create_const : const -> t

(* [create_var loc n] creates a variable. [loc] is the location of the function
   you are creating a variable for and [n] is its syntactic arity of the
   function the variable is being created for. *)
val create_var : Location.t -> int -> t

(* In the case [t] is a variable, [get t] returns its current contents as a
   [const] and has no effect. *)
val get : t -> const

(* True iff [t] is structurally [Const Default_zero_alloc].
   Unlike [get t = Default_zero_alloc], this returns [false] for a [Var] whose
   [desc] happens to be [None]; the [Var] still carries information that's worth
   preserving for later mutation. *)
val is_default_const : t -> bool

(* For types.ml's backtracking mechanism. *)
type change
val set_change_log : (change -> unit) -> unit
val undo_change : change -> unit

(* These are the errors that may be raised by [sub_exn] below. *)
type error
val error_is_arity_mismatch : error -> bool
val print_error : Format_doc.formatter -> error -> unit

(* An [error] indicating that two zero_alloc views should agree but don't:
   one carries an annotation while the other does not. *)
val one_missing : error

(* [sub ~context t1 t2] checks whether the zero_alloc check t1 is stronger than
   the zero_alloc check t2. It returns [Ok ()] if so, and [Error e] if not.
   The exact error being reported depends on the context. If [t1] is a variable,
   it may be set to make the relation hold. *)
val sub : context:check_context -> t -> t -> (unit, error) Result.t

(* [check_payload_to_string ?apparent_arity c] returns the keyword portion of a
   zero_alloc check attribute, e.g. [" strict opt arity 2"]. If
   [~apparent_arity] equals the check's arity, the arity suffix is omitted. *)
val check_payload_to_string : ?apparent_arity:int -> check -> string

(* [check_option_equal ~context c1 c2] checks whether two optional [check]
   values are identical. Returns an error if one is present and the other is
   absent, or if both are present but differ. *)
val check_option_equal :
  context:check_context -> check option -> check option ->
  (unit, error) Result.t

val debug_printer : Format.formatter -> t -> unit
