(* There are several allocating expressions that enclose the cursor *)

type t =
  | T of { global_ t : t }
  | Not_t of string option

let f x = exclave_ T { t = Not_t (Some x) }
                                    (* ^ *)

let f x = exclave_ (Some (Some x))
                            (* ^ *)
