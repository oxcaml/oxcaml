module F = Stdlib_upstream_compatible.Float_u
module I32 = Stdlib_upstream_compatible.Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u
module N = Stdlib_upstream_compatible.Nativeint_u

type product = #(int * F.t * string * I64.t)

val raise_any : ('a : any). unit -> 'a

val forward : ('a : any). (unit -> 'a) -> unit -> 'a

val apply1 : ('a : any). (unit -> 'a) -> 'a

val choose : ('a : any). bool -> (unit -> 'a) -> (unit -> 'a) -> unit -> 'a

type forwarder =
  { run : ('a : any). (unit -> 'a) -> unit -> 'a
  }

val stored : forwarder

val make : string -> forwarder

module type FORWARDER = sig
  val run : ('a : any). (unit -> 'a) -> unit -> 'a

  val fail : ('a : any). unit -> 'a
end

module Direct : FORWARDER

module Make (X : FORWARDER) : FORWARDER

module Made : FORWARDER
