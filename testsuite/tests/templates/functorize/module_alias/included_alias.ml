(* Parameters: P *)

(* Workaround for the issue exposed by [pure_alias.ml]: wrapping the alias
   in [include] inside a [struct] forces a body-level use of [Message],
   which records [Message] with [Exact] precision in this cmi.  The
   functorizer can then transitively bundle [Message] (no
   [Pruned_Message] substitution). *)
module Message = struct
  include Message
end
