type t = A [@@deriving_inline foo]

include struct
  [@@@ocaml.warning "-60"]

  let _ = fun (_ : t) -> ()

  module Foo = struct end

  let _ =
    ();
    ();
    [%foo]
end [@@ocaml.doc "@inline"]

[@@@inline.end]

type u = B [@@deriving_inline nested_jkind]

let _ = fun (_ : u) -> ()

type nested : value non_null non_float

[@@@inline.end]
