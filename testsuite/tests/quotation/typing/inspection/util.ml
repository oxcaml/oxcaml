#syntax quotations on

include (struct
  let test ?(eval = true) e =
    let e = Obj.magic_many e in
    print_endline (Quote.string_of_expr e); print_newline ();
    if eval then [%eval: unit] e else ()
end : sig
  val test : ?eval:bool -> <[unit]> expr @ once -> unit
end)
