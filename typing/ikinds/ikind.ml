(*-------------------------------------------------------------------------*)
(* Types shared by ikind algorithms                                        *)
(*-------------------------------------------------------------------------*)

module Rigid_name = struct
  type unknown_id = int

  type t =
    | Atom of
        { constr : Path.t;
          arg_index : int
        }
    | Param of int
    | Unknown of unknown_id

  let compare a b =
    if a == b
    then 0
    else
      match a, b with
      | Atom a1, Atom a2 ->
        let h = Path.compare a1.constr a2.constr in
        if h != 0 then h else Int.compare a1.arg_index a2.arg_index
      | Param x, Param y -> Int.compare x y
      | Atom _, Param _ -> -1
      | Param _, Atom _ -> 1
      | Unknown x, Unknown y -> Int.compare x y
      | Unknown _, _ -> 1
      | _, Unknown _ -> -1

  let to_string = function
    | Atom { constr; arg_index } ->
      Printf.sprintf "%s.%d" (Format.asprintf "%a" Path.print constr) arg_index
    | Param i -> Printf.sprintf "param[%d]" i
    | Unknown id -> Printf.sprintf "unknown[%d]" id

  let atomic constr arg_index = Atom { constr; arg_index }

  let param i = Param i

  let fresh_unknown =
    let next = ref 0 in
    fun () ->
      let id = !next in
      incr next;
      Unknown id
end

module Ldd = struct
  module Name = Rigid_name

  include Ldd.Make (Rigid_name)
end
