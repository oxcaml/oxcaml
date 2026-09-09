(* Parameters: P *)

(* Real alias to a parameterized module: compiled with -no-alias-deps so
   Message has Approximate precision in cmi_globals, but the functorizer
   must still track it as a concrete dependency. *)
module Message = Message

let greeting (p : P.t) : Message.t = Message.hello p
