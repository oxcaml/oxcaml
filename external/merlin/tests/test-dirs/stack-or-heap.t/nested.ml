(* There are several allocating expressions that enclose the cursor *)

type t =
  | T of { t : t @@ global }
  | Not_t of string option

let f x = exclave_ T { t = Not_t (Some x) }
                                    (* ^ *)

let f x = exclave_ (Some (Some x))
                            (* ^ *)
