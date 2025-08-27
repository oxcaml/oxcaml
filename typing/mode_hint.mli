open Allowance

(** Hint for a constant bound. See [Mode.Report.print_const] for what each non-trivial constructor means. *)
type 'd const =
  | Nil : ('l * 'r) const  (** The constant bound is not explained. *)
  | Min_comonadic : ('l * disallowed) pos const
      (** Used for [min] on the LHS of [submode] for comonadic axes. *)
  | Max_comonadic : (disallowed * 'r) pos const
      (** Used for [max] on the RHS of [submode] for comonadic axes. *)
  | Min_monadic : ('l * disallowed) neg const
      (** Similiar to [Min_comonadic] but for monadic axes. *)
  | Max_monadic : (disallowed * 'r) neg const
      (** Similiar to [Max_comonadic] but for monadic axes. *)
  | Lazy_allocated_on_heap : (disallowed * 'r) pos const
  | Class_legacy_monadic : ('l * disallowed) neg const
  | Class_legacy_comonadic : ('l * disallowed) pos const
  | Tailcall_function : (disallowed * 'r) pos const
  | Tailcall_argument : (disallowed * 'r) pos const
  | Mutable_read : (disallowed * 'r) neg const
  | Mutable_write : (disallowed * 'r) neg const
  | Lazy_forced : (disallowed * 'r) neg const
  | Function_return : (disallowed * 'r) pos const
  | Stack_expression : ('l * disallowed) pos const
  | Module_allocated_on_heap : (disallowed * 'r) pos const
  constraint 'd = _ * _
[@@ocaml.warning "-62"]

(** A description of what type of item is being closed over *)
type lock_item =
  | Value
  | Module
  | Class
  | Constructor

(** A description of what type of closure is closing a value *)
type closure_context =
  | Function
  | Functor
  | Lazy

(** Details of an item being closed by a context *)
type closure_details =
  { closure_context : closure_context;
    value_loc : Location.t;  (** Location of the value being closed over *)
    value_lid : Longident.t;  (** Identifier for the value being closed over *)
    value_item : lock_item  (** The item type of the value being closed over *)
  }

(** Hint for a morphism on bounds. See [Mode.Report.print_morph] for what each non-trivial
    constructor means. *)
type 'd morph =
  | Gap : ('l * 'r) morph  (** The morphism is not explained.  *)
  | Skip : ('l * 'r) morph
      (** The morphism doesn't change the bound and should be skipped in printing. *)
  | Close_over : closure_details -> ('l * disallowed) morph
  | Is_closed_by : closure_details -> (disallowed * 'r) morph
  | Captured_by_partial_application : (disallowed * 'r) morph
  | Adj_captured_by_partial_application : ('l * disallowed) morph
  | Crossing : ('l * 'r) morph
  constraint 'd = _ * _
[@@ocaml.warning "-62"]
