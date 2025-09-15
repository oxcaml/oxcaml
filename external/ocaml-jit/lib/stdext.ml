module Map = struct
  module type S = sig
    include MoreLabels.Map.S

    val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

    val show : (Format.formatter -> 'a -> unit) -> 'a t -> string
  end

  module type Key = sig
    include MoreLabels.Map.OrderedType

    val pp : Format.formatter -> t -> unit

    val show : t -> string
  end

  module Make (Key : Key) : S with type key = Key.t = struct
    include MoreLabels.Map.Make (Key)

    let pp_bindings pp_a fmt = function
      | [] -> Format.fprintf fmt "[]"
      | [ (k, a) ] -> Format.fprintf fmt "[(%a, %a)]" Key.pp k pp_a a
      | (k, a) :: tl ->
        Format.fprintf fmt "[ (%a, %a)@ " Key.pp k pp_a a;
        List.iter
          (fun (k, a) -> Format.fprintf fmt "; (%a, %a)@ " Key.pp k pp_a a)
          tl;
        Format.fprintf fmt "]"

    let pp pp_a fmt t = pp_bindings pp_a fmt (bindings t)

    let show pp_a t = Format.asprintf "%a" (pp pp_a) t
  end
end

module Option = struct
  include Stdlib.Option

  module Op = struct
    let ( let* ) o f = match o with None -> None | Some x -> f x

    let ( let+ ) o f = match o with None -> None | Some x -> Some (f x)
  end
end

module Result = struct
  include Stdlib.Result

  module Op = struct
    let ( let* ) r f = match r with Error e -> Error e | Ok x -> f x

    let ( let+ ) r f = match r with Error e -> Error e | Ok x -> Ok (f x)
  end

  module List = struct
    let map_all ~f l =
      let rec aux acc l =
        match (acc, l) with
        | Error err_list, [] -> Error (List.rev err_list)
        | Ok mapped, [] -> Ok (List.rev mapped)
        | _, hd :: tl ->
          let acc =
            match (f hd, acc) with
            | Error err, Error err_list -> Error (err :: err_list)
            | Error err, _ -> Error [ err ]
            | Ok _, Error _ -> acc
            | Ok x, Ok mapped -> Ok (x :: mapped)
          in
          aux acc tl
      in
      aux (Ok []) l

    let iter_all ~f l =
      let rec aux acc l =
        match (acc, l) with
        | Error err_list, [] -> Error (List.rev err_list)
        | Ok _, [] -> acc
        | _, hd :: tl ->
          let acc =
            match (f hd, acc) with
            | Error err, Error err_list -> Error (err :: err_list)
            | Error err, _ -> Error [ err ]
            | Ok (), _ -> acc
          in
          aux acc tl
      in
      aux (Ok ()) l
  end
end

module String = struct
  module String = Stdlib.String
  include StringLabels

  module Map = Map.Make (struct
      type t = string

      let compare = String.compare

      let pp fmt t = Format.fprintf fmt "%S" t

      let show t = Format.asprintf "%a" pp t
    end)

  let starts_with ~prefix str =
    let str_len = String.length str in
    let prefix_len = String.length prefix in
    if str_len < prefix_len then false
    else
      let rec aux curr =
        if curr = prefix_len then true
        else prefix.[curr] = str.[curr] && aux (curr + 1)
      in
      aux 0
end
