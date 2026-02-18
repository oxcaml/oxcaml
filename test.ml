#syntax quotations on

open (struct
  let eval a = Obj.magic a
end : sig
  val eval : 'a expr -> 'a eval
end)

let f (x : <[ $('a) eval ]> expr) : 'a eval eval = eval x
