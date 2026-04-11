module String = struct
  include String

  let is_prefix t ~prefix =
    let rec is_prefix_from t ~prefix ~pos ~len =
      pos >= len
      || Char.equal (String.get t pos) (String.get prefix pos)
         && is_prefix_from t ~prefix ~pos:(pos + 1) ~len
    in
    String.length t >= String.length prefix
    && is_prefix_from t ~prefix ~pos:0 ~len:(String.length prefix)
end

module Option = struct
  include Option

  module Op = struct
    let ( let* ) = Option.bind
    let ( let+ ) o f = Option.map f o
  end

  module List = struct
    let map ~f l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | hd :: tl -> (
            match f hd with None -> None | Some x -> aux (x :: acc) tl)
      in
      aux [] l
  end
end

module List = struct
  include List

  let without_first l ~pred =
    let rec aux seen = function
      | [] -> None
      | hd :: tl when pred hd -> Some (hd, List.rev_append seen tl)
      | hd :: tl -> aux (hd :: seen) tl
    in
    aux [] l

  let fold_left_map ~acc ~f l =
    let rec aux (acc, mapped) = function
      | [] -> (acc, List.rev mapped)
      | hd :: tl ->
          let acc, hd' = f ~acc hd in
          aux (acc, hd' :: mapped) tl
    in
    aux (acc, []) l
end
