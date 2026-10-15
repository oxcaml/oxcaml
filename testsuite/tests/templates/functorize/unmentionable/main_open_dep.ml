(* The "open" gap: [open]ing the bundle instance must not expose the
   [Unmentionable] transitive deps by name.  Opening a functor application
   goes through [Env.add_signature], which must preserve visibility so that
   [Foo__A] stays unmentionable. *)

open Bundle_foo_lib.Make (P_int) ()

let () = print_endline (Foo__A.hello (P_int.create ()))
