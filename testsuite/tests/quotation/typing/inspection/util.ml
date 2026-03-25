#syntax quotations on

include (struct
  let test ?(eval = true) (e : <[unit]> expr) =
    let e = Obj.magic_many e in
    print_endline (Quote.string_of_expr e); print_newline ();
    if eval then Eval.eval e else ()
end : sig
  val test : ?eval:bool -> <[unit]> expr @ once -> unit
end)
