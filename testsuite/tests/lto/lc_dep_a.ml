(* One set of closures with two functions. [unused] is exposed from this
   module but no module in the program uses it, so a whole-program Reaper
   solve may delete it along with [z], the value slot only it captures. That
   changes the layout of the set, so any code in dependent modules that
   projects the surviving slots (e.g. after inlining [used]) must agree with
   the layout of this module's rebuilt form. *)

let base = Sys.opaque_identity 0

let x = Sys.opaque_identity 11

let y = Sys.opaque_identity 22

let z = Sys.opaque_identity 33

let rec used n = if n < base then x + y else x - y [@@inline always]

and unused n = if n > base then y + z else used (n - 1)
