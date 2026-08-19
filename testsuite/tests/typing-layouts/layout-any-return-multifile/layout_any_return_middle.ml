module P = Layout_any_return_provider

let[@inline never] forward_imported
    : type (a : any). (unit -> a) -> unit -> a =
  fun f () -> P.forward f ()

let[@inline never] choose_imported
    : type (a : any). bool -> (unit -> a) -> (unit -> a) -> unit -> a =
  fun b f g () -> P.choose b f g ()

let raise_imported : type (a : any). unit -> a =
  fun () -> P.raise_any ()

let stored_imported = P.stored

module From_signature : P.FORWARDER = struct
  let run f () = P.Direct.run f ()

  let fail () = P.Direct.fail ()
end

module From_functor = P.Make (From_signature)

let made_imported = P.make "middle"
