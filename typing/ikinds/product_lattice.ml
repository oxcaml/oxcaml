module type SHAPE = sig
  val axis_sizes : int array (* n_i >= 1 *)
end

(* Simple array-based product lattice for debugging/prototyping. *)
module Make (S : SHAPE) = struct
  type t = int array

  let axis_sizes = Array.copy S.axis_sizes

  let num_axes = Array.length axis_sizes

  let bot = Array.make num_axes 0

  let top = Array.init num_axes (fun i -> axis_sizes.(i) - 1)

  let join (a : t) (b : t) : t = Array.map2 max a b

  let meet (a : t) (b : t) : t = Array.map2 min a b

  let leq (a : t) (b : t) : bool = Array.for_all2 (fun ai bi -> ai <= bi) a b

  let equal (a : t) (b : t) : bool = Array.for_all2 ( = ) a b

  let hash (a : t) = Hashtbl.hash (Array.to_list a)

  (* Axis-wise residual: zero out an axis if b's level >= a's level *)
  let co_sub (a : t) (b : t) : t =
    Array.map2 (fun ai bi -> if bi >= ai then 0 else ai) a b

  let get_axis (v : t) ~axis:i : int = v.(i)

  let set_axis (v : t) ~axis:i ~level:lev : t =
    if lev < 0 || lev >= axis_sizes.(i)
    then invalid_arg "set_axis: level out of range";
    let a = Array.copy v in
    a.(i) <- lev;
    a

  let encode ~levels : t =
    if Array.length levels <> num_axes then invalid_arg "encode: wrong arity";
    Array.mapi
      (fun i lev ->
        if lev < 0 || lev >= axis_sizes.(i)
        then invalid_arg "encode: level out of range";
        lev)
      levels

  let decode (v : t) : int array = Array.copy v

  let find_non_bot_axis (v : t) : int option =
    let rec loop i =
      if i >= num_axes then None else if v.(i) > 0 then Some i else loop (i + 1)
    in
    loop 0

  let pp (v : t) : string =
    let parts =
      v |> Array.to_list |> List.map string_of_int |> String.concat ","
    in
    Printf.sprintf "[%s]" parts

  let to_string = pp
end
