module F = Stdlib_upstream_compatible.Float_u
module I32 = Stdlib_upstream_compatible.Int32_u
module I64 = Stdlib_upstream_compatible.Int64_u
module N = Stdlib_upstream_compatible.Nativeint_u

type product = #(int * F.t * string * I64.t)

let[@inline never] raise_any : type (a : any). unit -> a =
  fun () -> assert false

let[@inline never] forward : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> f ()

let[@inline never] apply1 : type (a : any). (unit -> a) -> a = fun f -> f ()

let[@inline never] choose
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> unit -> a =
  fun b f g () -> if Sys.opaque_identity b then f () else g ()

type forwarder =
  { run : ('a : any). (unit -> 'a) -> unit -> 'a
  }

let stored = { run = forward }

let[@inline never] make name =
  let name = Sys.opaque_identity name in
  { run =
      (fun f () ->
        ignore (Sys.opaque_identity name);
        forward f ())
  }

module type FORWARDER = sig
  val run : ('a : any). (unit -> 'a) -> unit -> 'a

  val fail : ('a : any). unit -> 'a
end

module Direct : FORWARDER = struct
  let run f () = forward f ()

  let fail () = raise_any ()
end

module Make (X : FORWARDER) : FORWARDER = struct
  let run f () = X.run f ()

  let fail () = X.fail ()
end

module Made = Make (Direct)
