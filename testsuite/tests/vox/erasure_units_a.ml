(* An erased parameter named [x] here must not affect other units that
   happen to reuse the same ident name and stamp (the erased-ident table is
   per unit). *)
let f (x : int @ erased) = 0
let use () = f (erased_ 1)
