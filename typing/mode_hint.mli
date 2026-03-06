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

type structure_item = lock_item * Ident.t

(** Pinpoint's description to accompany pinpoint's location. It's about the
    syntax rather than the value. For example, we distinguish:
    - between a module identifer and a module definition
    - between a structure definition and a functor definition
    - BUT NOT between two module identifiers where one is a functor and the
      other is a structure.

    The constructors are not mutually exclusive - some might be more precise
    than others. *)
type pinpoint_desc =
  | Unknown
  | Ident of ident  (** An identifier *)
  | Function  (** A function definition *)
  | Module  (** A module definition *)
  | Functor  (** A functor definition *)
  | Structure  (** A structure definition *)
  | Lazy  (** A lazy expression *)
  | Allocation  (** An allocation *)
  | Expression  (** An arbitrary expression *)
  | Class  (** A class declaration *)
  | Object  (** An object declaration *)
  | Loop  (** A loop *)
  | Letop  (** A let op expression *)
  | Cases_result  (** The result of cases *)
  | Pattern  (** A pattern *)
  | Structure_item of structure_item
      (** an item in a structure being pointed at *)

(** A pinpoint is a location in the source code, accompanied by additional
    description *)
type pinpoint = Location.t * pinpoint_desc

type mutable_part =
  | Record_field of string
  | Array_elements

type always_dynamic =
  | Application
  | Try_with

type legacy =
  | Compilation_unit
  | Toplevel
  | Class

(* CR-soon zqian: add loop and function body to [region_desc] *)
type region_desc = Borrow

type region = Location.t * region_desc

type ('d0, 'd1) polarity =
  | Monadic : ('l * 'r, 'r * 'l) polarity
  | Comonadic : ('l * 'r, 'l * 'r) polarity
  constraint 'd0 = _ * _ constraint 'd1 = _ * _
[@@warning "-62"]

(* CR-soon zqian: add the const hint for "min on the LHS", and one for "max on
the RHS". They are similiar to the [Skip] morph hint and should raise when being
printed. *)

(** Hint for a constant bound. See [Mode.Report.print_const] for what each
    non-trivial constructor means. *)
type 'd const =
  | Unknown : ('l * 'r) const  (** The constant bound is not explained. *)
  | Lazy_allocated_on_heap : (disallowed * 'r) pos const
  | Legacy : legacy -> ('l * 'r) const
  | Tailcall_function : (disallowed * 'r) pos const
  | Tailcall_argument : (disallowed * 'r) pos const
  | Mutable_read : mutable_part -> (disallowed * 'r) neg const
  | Mutable_write : mutable_part -> (disallowed * 'r) neg const
  | Lazy_forced : (disallowed * 'r) neg const
  | Function_return : (disallowed * 'r) pos const
  | Stack_expression : ('l * disallowed) pos const
  | Module_allocated_on_heap : (disallowed * 'r) pos const
  | Module_must_be_many : (disallowed * 'r) pos const
  | Module_must_be_aliased : ('l * disallowed) neg const
  | Always_dynamic : always_dynamic -> ('l * disallowed) neg const
  | Branching : ('l * disallowed) neg const
  | Is_used_in : pinpoint -> (disallowed * 'r) const
      (** A variant of [Is_closed_by] where the closure mode is constant.
          INVARIANT: The [pinpoint] cannot be [Unknown]. *)
  | Borrowed : Location.t * ('l * 'r, 'd) polarity -> 'd const
  | Escape_region : region -> (disallowed * 'r) const
  constraint 'd = _ * _
[@@ocaml.warning "-62"]

type closure_details =
  { closure : pinpoint;
    closed : pinpoint
  }

(* CR-someday zqian: Put [Modality.Const.t] here, once the dependency circle is
   resolved. To fix that, we can move [Modality.Const] to in front of [Hint],
   while [Modality] stays in place. *)
type modality = Modality

type containing =
  | Tuple
  | Record of string * modality
  | Array of modality
  | Constructor of string * modality
  | Structure of structure_item * modality
(* Some structure items (such as classes) don't have modalities. We gloss over
     for simplicity. *)

type contains =
  { containing : containing;
    contained : pinpoint
  }

type is_contained_by =
  { containing : containing;
    container : pinpoint
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
  | Skip : ('l * 'r) morph
      (** The morphism doesn't change the bound and should be skipped in
          printing. *)
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
    (* CR-someday zqian: add [Tail_of_region] which connects the mode of region
       to the mode of the region's tail *)
  constraint 'd = _ * _
[@@ocaml.warning "-62"]
