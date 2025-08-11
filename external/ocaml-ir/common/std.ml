open Sexplib.Std
include StdLabels

module Make_map_sexp (Map : Map.S) = struct
  let map_of_sexp key_of_sexp data_of_sexp = function
    | Sexplib.Sexp.List nodes ->
      let l =
        List.map
          ~f:
            (function
             | Sexplib.Sexp.List [ key; data ] -> key_of_sexp key, data_of_sexp data
             | sexp ->
               Sexplib.Conv.of_sexp_error "map_of_sexp: List [key; data] needed" sexp)
          nodes
      in
      l |> List.to_seq |> Map.of_seq
    | sexp -> Sexplib.Conv.of_sexp_error "map_of_sexp: list needed" sexp
  ;;

  let sexp_of_map sexp_of_key sexp_of_data m =
    let l =
      m
      |> Map.to_seq
      |> Seq.map (fun (key, data) ->
           Sexplib.Sexp.List [ sexp_of_key key; sexp_of_data data ])
      |> List.of_seq
    in
    Sexplib.Sexp.List l
  ;;
end

module Error = struct
  type t = string [@@deriving sexp]

  include struct
    let _ = fun (_ : t) -> ()
    let t_of_sexp = (string_of_sexp : Sexplib0.Sexp.t -> t)
    let _ = t_of_sexp
    let sexp_of_t = (sexp_of_string : t -> Sexplib0.Sexp.t)
    let _ = sexp_of_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  exception Exn of string

  let raise t = raise (Exn t)
  let of_exn exn = Printexc.to_string exn
  let to_string_hum t = t
  let of_string t = t
  let createf format = Printf.sprintf format
end

module Monad = struct
  module type S = Std_intf.Monad.S

  module Make (Arg : Std_intf.Monad.Arg) = struct
    include Arg

    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map t ~f

    module Let_syntax = struct
      module Let_syntax = struct
        include Arg

        let both a b = bind ~f:(fun a -> map ~f:(fun b -> a, b) b) a

        module Open_on_rhs = struct end
      end
    end
  end
end

module Option = struct
  include Option

  include Monad.Make (struct
    include Option

    let return = some
    let bind o ~f = bind o f
    let map o ~f = map f o
  end)

  let value_exn ?(here : Lexing.position option) ?message x =
    match x with
    | Some l -> l
    | None ->
      (match here, message with
       | Some here, Some message ->
         Error.createf "File %s on line %d: %s" here.pos_fname here.pos_lnum message
       | None, Some message -> Error.createf "%s" message
       | Some here, None ->
         Error.createf "File %s on line %d" here.pos_fname here.pos_lnum
       | None, None -> Error.createf "")
      |> Error.raise
  ;;
end

let compare_option f a b = Option.compare f a b

module Int = struct
  include Int

  module Map = struct
    module T = Map.Make (Int)
    include Make_map_sexp (T)
    include T

    let t_of_sexp f s = map_of_sexp Sexplib.Std.int_of_sexp f s
    let sexp_of_t f t = sexp_of_map Sexplib.Std.sexp_of_int f t

    let of_alist_multi l =
      List.fold_left l ~init:empty ~f:(fun acc (k, v) ->
        match find_opt k acc with
        | Some v' -> add k (v :: v') acc
        | None -> add k [ v ] acc)
    ;;

    let to_alist t = to_seq t |> List.of_seq
  end
end

module String = struct
  include String

  module Map = struct
    module T = Map.Make (String)
    include Make_map_sexp (T)
    include T

    let t_of_sexp f s = map_of_sexp Sexplib.Std.string_of_sexp f s
    let sexp_of_t f t = sexp_of_map Sexplib.Std.sexp_of_string f t
  end

  let split_lines s =
    let lines = String.split_on_char ~sep:'\n' s in
    List.rev lines
    |> List.fold_left ~init:(true, []) ~f:(fun (should_remove_empty_lines, acc) x ->
         match x with
         | "" when should_remove_empty_lines -> true, acc
         | x -> false, x :: acc)
    |> snd
  ;;

  let is_substring ~substring s =
    let found = ref false in
    let len = String.length substring in
    for pos = 0 to String.length s - len do
      if String.equal (String.sub ~pos ~len s) substring then found := true
    done;
    !found
  ;;

  let split_at_indices str ~indices =
    let indices = List.sort indices ~cmp:Int.compare @ [ String.length str ] in
    List.fold_left indices ~init:([], 0) ~f:(fun (accum, index_begin) index_end ->
      let accum =
        String.sub str ~pos:index_begin ~len:(index_end - index_begin) :: accum
      in
      accum, index_end)
    |> fst
    |> List.rev
  ;;

  let starts_with ~prefix s =
    let len_s = length s
    and len_pre = length prefix in
    let rec aux i =
      if i = len_pre
      then true
      else if unsafe_get s i <> unsafe_get prefix i
      then false
      else aux (i + 1)
    in
    len_s >= len_pre && aux 0
  ;;
end

module List = struct
  include List

  let remove_consecutive_duplicates list ~equal =
    let rec loop to_keep accum = function
      | [] -> to_keep :: accum
      | hd :: tl ->
        if equal hd to_keep then loop to_keep accum tl else loop hd (to_keep :: accum) tl
    in
    match list with
    | [] -> []
    | hd :: tl -> rev (loop hd [] tl)
  ;;

  let filter_mapi l ~f =
    let rev_filter_mapi l ~f =
      let rec loop i l accum =
        match l with
        | [] -> accum
        | hd :: tl ->
          (match f i hd with
           | Some x -> loop (i + 1) tl (x :: accum)
           | None -> loop (i + 1) tl accum)
      in
      loop 0 l []
    in
    rev (rev_filter_mapi l ~f)
  ;;

  let intersperse t ~sep =
    match t with
    | [] -> []
    | x :: xs -> x :: fold_right xs ~init:[] ~f:(fun y acc -> sep :: y :: acc)
  ;;
end

module Array = struct
  include Array

  module Binary_search = struct
    let last_less_than_or_equal_to a ~compare ~key =
      let rec linear_search_last_satisfying low high =
        if high < low
        then None
        else if compare key a.(high) >= 0
        then Some high
        else linear_search_last_satisfying low (high - 1)
      in
      let rec find_range_near_last_satisfying low high =
        if high - low < 8
        then linear_search_last_satisfying low high
        else (
          let mid = low + ((high - low) / 2) in
          let elt = a.(mid) in
          if compare key elt < 0
          then find_range_near_last_satisfying low (mid - 1)
          else find_range_near_last_satisfying mid high)
      in
      find_range_near_last_satisfying 0 (Array.length a - 1)
    ;;
  end
end
