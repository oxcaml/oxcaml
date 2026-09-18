(* TEST
   expect;
*)

type ('a : any) t

external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]

external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]

[%%expect {|
type ('a : any) t
external read : ('a : any mod external_). 'a t -> 'a = "%peek"
  [@@layout_poly]
external write : ('a : any mod external_). 'a t -> 'a -> unit = "%poke"
  [@@layout_poly]
|}]

type v : value_or_null mod external_

(* CR dkalinichenko: this used to test that peek and poke is forbidden
   for non-immediate values, but currently, [any mod external] rules them out.
   We will have non-immediate external values once externality is a modal axis. *)

let bad_read p : v = read p
[%%expect {|
type v : value_or_null mod external_
>> Fatal error: Blambda_of_lambda: (peek
tagged_immediate) is not supported in bytecode
Uncaught exception: Misc.Fatal_error

|}]

let bad_write p (v : v) = write p v
[%%expect {|
>> Fatal error: Blambda_of_lambda: (poke
tagged_immediate) is not supported in bytecode
Uncaught exception: Misc.Fatal_error

|}]
