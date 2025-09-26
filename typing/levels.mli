(* All the following wrapper functions revert to the original level,
   even in case of exception. *)
val with_local_level : ?post:('a -> unit) -> (unit -> 'a) -> 'a
(* [with_local_level (fun () -> cmd) ~post] evaluates [cmd] at a
   raised level.
   If given, [post] is applied to the result, at the original level.
   It is expected to contain only level related post-processing. *)

val with_local_level_if : bool -> (unit -> 'a) -> post:('a -> unit) -> 'a
(* Same as [with_local_level], but only raise the level conditionally.
   [post] also is only called if the level is raised. *)

val with_local_level_iter : (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
(* Variant of [with_local_level], where [post] is iterated on the
   returned list. *)

val with_local_level_iter_if :
  bool -> (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
(* Conditional variant of [with_local_level_iter] *)

val with_level : level:int -> (unit -> 'a) -> 'a
(* [with_level ~level (fun () -> cmd)] evaluates [cmd] with
   [current_level] set to [level] *)

val with_level_if : bool -> level:int -> (unit -> 'a) -> 'a
(* Conditional variant of [with_level] *)

val with_local_level_if_principal : (unit -> 'a) -> post:('a -> unit) -> 'a

val with_local_level_iter_if_principal :
  (unit -> 'a * 'b list) -> post:('b -> unit) -> 'a
(* Applications of [with_local_level_if] and [with_local_level_iter_if]
   to [!Clflags.principal] *)

val with_local_level_for_class : ?post:('a -> unit) -> (unit -> 'a) -> 'a
(* Variant of [with_local_level], where the current level is raised but
   the nongen level is not touched *)

val with_raised_nongen_level : (unit -> 'a) -> 'a
(* Variant of [with_local_level],
   raises the nongen level to the current level *)

val reset_global_level : unit -> unit
(* Reset the global level before typing an expression *)

val increase_global_level : unit -> int

val restore_global_level : int -> unit
(* This pair of functions is only used in Typetexp *)

val create_scope : unit -> int

val get_current_level : unit -> int

val get_global_level : unit -> int

val get_nongen_level : unit -> int

val update_current_level : int -> unit

(* For use with ocamldebug *)
type global_state

val global_state : global_state

val print_global_state : Format.formatter -> global_state -> unit
