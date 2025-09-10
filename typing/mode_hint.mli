open Allowance

type ('d0, 'd1) polarity =
  | Monadic : ('l * 'r, 'r * 'l) polarity
  | Comonadic : ('l * 'r, 'l * 'r) polarity
  constraint 'd0 = _ * _ constraint 'd1 = _ * _
[@@warning "-62"]

type mutable_part =
  | Record_field of string
  | Array_elements

(** Hint for a constant bound. See [Mode.Report.print_const] for what each non-trivial constructor means. *)
type 'd const =
  | Unknown : ('l * 'r) const  (** The constant bound is not explained. *)
  | Lazy_allocated_on_heap : (disallowed * 'r) pos const
  | Class_legacy_monadic : ('l * disallowed) neg const
  | Class_legacy_comonadic : ('l * disallowed) pos const
  | Tailcall_function : (disallowed * 'r) pos const
  | Tailcall_argument : (disallowed * 'r) pos const
  | Mutable_read : mutable_part -> (disallowed * 'r) neg const
  | Mutable_write : mutable_part -> (disallowed * 'r) neg const
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
  | Unknown : ('l * 'r) morph  (** The morphism is not explained. *)
  | Unknown_non_rigid : ('l * 'r) morph
      (** Similiar to [Unknown], but in the special case where the morph doesn't change the
    bound, it can be skipped. *)
  (* CR-soon zqian: usages of [Unknown_non_rigid] should be replaced with
     corresponding proper hints *)
  | Skip : ('l * 'r) morph
      (** The morphism doesn't change the bound and should be skipped in printing. *)
  | Close_over : closure_details -> ('l * disallowed) morph
  | Is_closed_by : closure_details -> (disallowed * 'r) morph
  | Captured_by_partial_application : (disallowed * 'r) morph
  | Adj_captured_by_partial_application : ('l * disallowed) morph
  | Crossing : ('l * 'r) morph
  | Argument_to_parameter : ('l * disallowed, 'd) polarity -> 'd morph
  | Parameter_to_argument : (disallowed * 'r, 'd) polarity -> 'd morph
  constraint 'd = _ * _
[@@ocaml.warning "-62"]
