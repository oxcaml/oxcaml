exception E

let[@inline never] raise_e () = raise E

(* Like Base's [Error.raise]: a non-tail call, so the caller stays in
   backtraces *)
let[@inline always] raise_nontail () = raise_e () [@nontail]

let[@inline] value_exn = function Some x -> x | None -> raise_nontail ()
