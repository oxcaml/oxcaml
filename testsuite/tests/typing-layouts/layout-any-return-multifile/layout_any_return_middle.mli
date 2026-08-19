module P = Layout_any_return_provider

val forward_imported : ('a : any). (unit -> 'a) -> unit -> 'a

val choose_imported :
  ('a : any). bool -> (unit -> 'a) -> (unit -> 'a) -> unit -> 'a

val raise_imported : ('a : any). unit -> 'a

val stored_imported : P.forwarder

val made_imported : P.forwarder

module From_signature : P.FORWARDER

module From_functor : P.FORWARDER
