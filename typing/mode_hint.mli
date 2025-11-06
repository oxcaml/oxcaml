open Allowance

(** A description of what type of item is being closed over *)
type lock_item =
  | Value
  | Module
  | Class
  | Constructor

type ident =
  { category : lock_item;
    lid : Longident.t
        (** Sometimes we want the ident to represent [M.x] but the loc can only
        point to [M]. This field would store [M.x]. *)
  }

(** Description of pinpoints to accompany the location. The constructors are not
mutually exclusive - some might be more precise than others *)
type pinpoint_desc =
  | Unknown
  | Ident of ident  (** An identifier *)
  | Function  (** A function definition *)
  | Functor  (** A functor definition *)
  | Lazy  (** A lazy expression *)
  | Allocation  (** An allocation *)
  | Expression  (** An arbitrary expression *)

(** A pinpoint is a location in the source code, accompanied by additional description *)
type pinpoint = Location.t * pinpoint_desc

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

type ('d0, 'd1) polarity =
  | Monadic : ('l * 'r, 'r * 'l) polarity
  | Comonadic : ('l * 'r, 'l * 'r) polarity
  constraint 'd0 = _ * _ constraint 'd1 = _ * _
[@@warning "-62"]

type closure_details =
  { closure : pinpoint;
    closed : pinpoint
  }

type containing =
  | Tuple
  | Record of string
  | Array
  | Constructor of string
(* CR-soon zqian: add the relation between structure and items *)

type contains =
  { containing : containing;
    contained : pinpoint
  }

type is_contained_by =
  { containing : containing;
    container : Location.t
  }

type allocation_desc =
  | Unknown
  | Optional_argument
  | Function_coercion
  | Float_projection

type allocation = allocation_desc Location.loc

(** Hint for morphisms. When acompanied by a destination [pinpoint], [morph]
   gives a source [pinpoint] and explains the relation between them. See
   [Mode.Report.print_morph] for what each non-trivial constructor means. *)
type 'd morph =
  | Unknown : ('l * 'r) morph  (** The morphism is not explained. *)
  | Unknown_non_rigid : ('l * 'r) morph
      (** Similiar to [Unknown], but in the special case where the morph doesn't change the
    bound, it can be skipped. *)
  (* CR-soon zqian: usages of [Unknown_non_rigid] should be replaced with
     corresponding proper hints *)
  | Skip : ('l * 'r) morph
      (** The morphism doesn't change the bound and should be skipped in printing. *)
  | Close_over :
      ('d, 'l * disallowed) polarity * closure_details
      -> ('l * disallowed) morph
  | Is_closed_by :
      ('d, disallowed * 'r) polarity * closure_details
      -> (disallowed * 'r) morph
  (* CR-soon zqian: currently [Close_over] and [Is_closed_by] both store both
     the source and destination pinpoints. Once we make [pinpoint] mandatory for
     submode calls, each constructor only needs to store the info of its source
     pinpoint. *)
  | Captured_by_partial_application : (disallowed * 'r) morph
  | Adj_captured_by_partial_application : ('l * disallowed) morph
  | Crossing : ('l * 'r) morph
  | Allocation_r : allocation -> (disallowed * 'r) morph
  | Allocation_l : allocation -> ('l * disallowed) morph
  | Contains_l : ('l * disallowed, 'd) polarity * contains -> 'd morph
  | Is_contained_by : ('l * 'r, 'd) polarity * is_contained_by -> 'd morph
  | Contains_r : (disallowed * 'r, 'd) polarity * contains -> 'd morph
  constraint 'd = _ * _
[@@ocaml.warning "-62"]
