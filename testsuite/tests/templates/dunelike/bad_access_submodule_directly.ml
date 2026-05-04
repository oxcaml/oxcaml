(* This is compiled as an external user with -nocwd, after fancy__Flourish.cmi
   has been copied to cwd.  The mangled name is inaccessible because -nocwd
   removes cwd from the load path. *)

(* Be naughty and try to directly access a submodule in [Fancy] *)
type t = Fancy__Flourish.t
