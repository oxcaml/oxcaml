(** Shared infrastructure for ANSI 256-colour pretty-printing. Front-ends such
    as [Cfg_colours] and [Flambda_colours] define domain-specific directives
    on top of {!push}; all directives push or pop a state on a single global
    stack. *)

(** A colour directive. Can be passed as an argument to [Format.printf] and
    friends using the "%t" specifier. Each directive (besides [pop]) acts by
    pushing a new state onto a stack, allowing the previous state to be
    restored using [pop]. *)
type directive = Format.formatter -> unit

(** Undo the most recent directive, restoring the previous state. Raises a
    fatal error if the stack is empty. *)
val pop : directive

(** Push a new colour state onto the stack. Each of [fg] and [bg] that is not
    specified is inherited from the current state. *)
val push : ?fg:int -> ?bg:int -> directive

(** Push a copy of the current state onto the stack. Useful when setting a
    colour conditionally so that a following [pop] will always be matched. *)
val none : directive

(** Run [f] with colour output globally disabled, restoring the previous
    setting when [f] returns (or raises). *)
val without_colours : f:(unit -> 'a) -> 'a
