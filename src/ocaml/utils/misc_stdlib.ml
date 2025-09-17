(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

let pp_parens_if condition printer ppf arg =
  Format.fprintf ppf "%s%a%s"
    (if condition then "(" else "")
    printer arg
    (if condition then ")" else "")

let pp_nested_list ~nested ~pp_element ~pp_sep ppf arg =
  Format.fprintf ppf "@[%a@]"
    (pp_parens_if nested
       (Format.pp_print_list ~pp_sep (pp_element ~nested:true)))
    arg

let to_string_of_print print =
  let to_string t =
    (* Implemented similarly to [Format.asprintf] *)
    let buf = Buffer.create 32 in
    let ppf = Format.formatter_of_buffer buf in
    Format.pp_set_margin ppf Int.max_int;
    print ppf t;
    Format.pp_print_flush ppf ();
    Buffer.contents buf
  in
  to_string

module List = struct
  include List (* for references to Stdlib.List later in this file *)

  let is_empty = function
    | [] -> true
    | _ :: _ -> false

  let map_option f l =
    let rec aux l acc =
      match l with
      | [] -> Some (List.rev acc)
      | x :: xs ->
        match f x with
        | None -> None
        | Some x -> aux xs (x :: acc)
    in
    aux l []

  let map3 f =
    let rec loop acc as_ bs cs = match as_, bs, cs with
      | [], [], [] -> List.rev acc
      | a :: as_, b :: bs, c :: cs -> loop (f a b c :: acc) as_ bs cs
      | _ -> invalid_arg "map3"
    in
    loop []

  let some_if_all_elements_are_some l =
      let rec aux acc l =
        match l with
        | [] -> Some (List.rev acc)
        | None :: _ -> None
        | Some h :: t -> aux (h :: acc) t
      in
      aux [] l

  let rec iter_until_error ~f l =
      match l with
      | [] -> Ok ()
      | x :: xs ->
        match f x with
        | Ok () -> iter_until_error ~f xs
        | Error _ as e -> e

  let [@inline] merge_fold ~cmp ~left_only ~right_only ~both ~init t1 t2 =
      let rec loop acc t1 t2 =
        match t1, t2 with
        | [], [] -> acc
        | a :: t1', [] -> loop (left_only acc a) t1' []
        | [], b :: t2' -> loop (right_only acc b) [] t2'
        | a :: t1', b :: t2' ->
            match cmp a b with
            | 0 -> loop (both acc a b) t1' t2'
            | c when c < 0 -> loop (left_only acc a) t1' t2
            | _ -> loop (right_only acc b) t1 t2'
      in
      loop init t1 t2

  let [@inline] merge_iter ~cmp ~left_only ~right_only ~both t1 t2 =
      merge_fold t1 t2 ~cmp
        ~init:()
        ~left_only:(fun () a -> left_only a)
        ~right_only:(fun () b -> right_only b)
        ~both:(fun () a b -> both a b)

  let fold_left_map2 f accu l1 l2 =
      let rec aux f accu res l1 l2 =
        match l1, l2 with
        | [], [] -> accu, List.rev res
        | a1 :: l1, a2 :: l2 ->
          let accu', r = f accu a1 a2 in
          aux f accu' (r :: res) l1 l2
        | _, _ -> invalid_arg "fold_left_map2"
      in
      aux f accu [] l1 l2
end

module Option = struct
  type 'a t = 'a option

  let first_some a b = match a with
    | Some _ -> a
    | None -> b ()

  let print print_contents ppf t =
    match t with
    | None -> Format.pp_print_string ppf "None"
    | Some contents ->
      Format.fprintf ppf "@[(Some@ %a)@]" print_contents contents
end

module Array = struct
  let exists2 p a1 a2 =
    let n = Array.length a1 in
    if Array.length a2 <> n then invalid_arg "Misc.Stdlib.Array.exists2";
    let rec loop i =
      if i = n then false
      else if p (Array.unsafe_get a1 i) (Array.unsafe_get a2 i) then true
      else loop (succ i) in
    loop 0

  let fold_left2 f x a1 a2 =
    if Array.length a1 <> Array.length a2
    then invalid_arg "Misc.Stdlib.Array.fold_left2";
    let r = ref x in
    for i = 0 to Array.length a1 - 1 do
      r := f !r (Array.unsafe_get a1 i) (Array.unsafe_get a2 i)
    done;
    !r

  let for_alli p a =
    let n = Array.length a in
    let rec loop i =
      if i = n then true
      else if p i (Array.unsafe_get a i) then loop (succ i)
      else false in
    loop 0

  let all_somes a =
    try
      Some (Array.map (function None -> raise_notrace Exit | Some x -> x) a)
    with
    | Exit -> None

  let equal eq_elt l1 l2 =
    (* Basically inlines [Array.for_all2] to avoid the [raise] *)
    let n = Array.length l1 in
    Int.equal n (Array.length l2) &&
    let rec loop i =
      if Int.equal i n then
        true
      else if eq_elt (Array.unsafe_get l1 i) (Array.unsafe_get l2 i) then
        loop (succ i)
      else
        false
    in
    loop 0

  let compare compare arr1 arr2 =
    let len1 = Array.length arr1 in
    let len2 = Array.length arr2 in
    if len1 <> len2 then
      Int.compare len1 len2
    else
      let rec loop i =
        if i >= len1 then 0
        else
          let cmp = compare arr1.(i) arr2.(i) in
          if cmp <> 0 then cmp else loop (i + 1)
      in
      loop 0

  let map_sharing f a =
    let same = ref true in
    let f' x =
      let x' = f x in
      if x != x' then
        same := false;
      x'
    in
    let a' = (Array.map [@inlined hint]) f' a in
    if !same then a else a'

  let of_list_map f = function
    | [] -> [| |]
    | hd :: tl ->
      let a = Array.make (1 + List.length tl) (f hd) in
      List.iteri (fun i x -> Array.unsafe_set a (i+1) (f x)) tl;
      a
end

module String = struct
  include String
  module Set = Set.Make(String)
  module Map = Map.Make(String)
  module Tbl = Hashtbl.Make(struct
    include String
    let hash = Hashtbl.hash
  end)

  let for_all f t =
    let len = String.length t in
    let rec loop i =
      i = len || (f t.[i] && loop (i + 1))
    in
    loop 0

  let print ppf t =
    Format.pp_print_string ppf t

  let begins_with ?(from = 0) str ~prefix =
    let rec helper idx =
      if idx < 0 then true
      else
        String.get str (from + idx) = String.get prefix idx && helper (idx-1)
    in
    let n = String.length str in
    let m = String.length prefix in
    if n >= from + m then helper (m-1) else false

  let split_on_string str ~split_on =
    let n = String.length str in
    let m = String.length split_on in
    let rec helper acc last_idx idx =
      if idx = n then
        let cur = String.sub str last_idx (idx - last_idx) in
        List.rev (cur :: acc)
      else if begins_with ~from:idx str ~prefix:split_on then
        let cur = String.sub str last_idx (idx - last_idx) in
        helper (cur :: acc) (idx + m) (idx + m)
      else
        helper acc last_idx (idx + 1)
    in
    helper [] 0 0

  let split_on_chars str ~split_on:chars =
    let rec helper chars_left s acc =
      match chars_left with
      | [] -> s :: acc
      | c :: cs ->
        List.fold_right (helper cs) (String.split_on_char c s) acc
    in
    helper chars str []

  let split_last_exn str ~split_on =
    let n = String.length str in
    let ridx = String.rindex str split_on in
    String.sub str 0 ridx, String.sub str (ridx + 1) (n - ridx - 1)

  let starts_with ~prefix s =
    let len_s = length s
    and len_pre = length prefix in
    let rec aux i =
      if i = len_pre then true
      else if unsafe_get s i <> unsafe_get prefix i then false
      else aux (i + 1)
    in len_s >= len_pre && aux 0

  let ends_with ~suffix s =
    let len_s = length s
    and len_suf = length suffix in
    let diff = len_s - len_suf in
    let rec aux i =
      if i = len_suf then true
      else if unsafe_get s (diff + i) <> unsafe_get suffix i then false
      else aux (i + 1)
    in diff >= 0 && aux 0
end

module Int = struct
  include Int
  let min (a : int) (b : int) = min a b
  let max (a : int) (b : int) = max a b
end

module Monad = struct
  module type Basic2 = sig
    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t

    val return : 'a -> ('a, _) t
  end

  module type Basic = sig
    type 'a t
    include Basic2 with type ('a, _) t := 'a t
  end

  module type S2 = sig
    type ('a, 'e) t

    val bind : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val (>>=) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
    val return : 'a -> ('a, _) t
    val map : ('a -> 'b) -> ('a, 'e) t -> ('b, 'e) t
    val join : (('a, 'e) t, 'e) t -> ('a, 'e) t
    val both : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
    val ignore_m : (_, 'e) t -> (unit, 'e) t
    val all : ('a, 'e) t list -> ('a list, 'e) t
    val all_unit : (unit, 'e) t list -> (unit, 'e) t

    module Syntax : sig
      val (let+) : ('a, 'e) t -> ('a -> 'b) -> ('b, 'e) t
      val (and+) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
      val (let*) : ('a, 'e) t -> ('a -> ('b, 'e) t) -> ('b, 'e) t
      val (and*) : ('a, 'e) t -> ('b, 'e) t -> ('a * 'b, 'e) t
    end
  end

  module type S = sig
    type 'a t
    include S2 with type ('a, _) t := 'a t
  end

  module[@inline] Make2 (X : Basic2) = struct
    include X

    let[@inline] ( >>= ) t f = bind t f

    let map f m =
      bind m (fun a -> return (f a))

    let join m = bind m Fun.id

    let both t1 t2 = t1 >>= fun t1 -> t2 >>= fun t2 -> return (t1, t2)

    let ignore_m m = bind m (fun _ -> return ())

    let all ms =
      let rec loop acc = function
        | [] -> return (List.rev acc)
        | m :: ms -> bind m (fun a -> loop (a :: acc) ms)
      in
      loop [] ms

    let rec all_unit = function
      | [] -> return ()
      | m :: ms -> bind m (fun _ -> all_unit ms)

    module Syntax = struct
      let[@inline] (let+) t f = map f t
      let[@inline] (and+) a b = both a b
      let[@inline] (let*) t f = bind t f
      let[@inline] (and*) a b = (and+) a b
    end
  end

  module[@inline] Make (X : Basic) = struct
    include Make2(struct
        include X
        type ('a, _) t = 'a X.t
      end)

    type nonrec 'a t = 'a X.t
  end

  module Identity = Make(struct
      type 'a t = 'a
      let[@inline] bind x f = f x
      let[@inline] return x = x
    end)

  module Option = Make(struct
      include Stdlib.Option
      let return = some
    end)

  module Result = Make2(struct
      include Stdlib.Result
      let return = ok
    end)
end

let format_as_unboxed_literal s =
  if String.starts_with ~prefix:"-" s
  then "-#" ^ (String.sub s 1 (String.length s - 1))
  else "#" ^ s

module Le_result = struct
  type t =
    | Equal
    | Less
    | Not_le

  let combine sr1 sr2 =
    match sr1, sr2 with
    | Equal, Equal -> Equal
    | Equal, Less | Less, Equal | Less, Less -> Less
    | Not_le, _ | _, Not_le -> Not_le

  let combine_list ts = List.fold_left combine Equal ts

  let is_le = function
    | Equal -> true
    | Less -> true
    | Not_le -> false

  let is_equal = function
    | Equal -> true
    | Less | Not_le -> false

  let less_or_equal ~le a b =
    match le a b, le b a with
    | true, true -> Equal
    | true, false -> Less
    | false, _ -> Not_le

  let equal ~le a b = le a b && le b a
end

(*********************************************)
(* Fancy types *)

type (_, _) eq = Refl : ('a, 'a) eq

module type T1 = sig
  type 'a t
end

module type T2 = sig
  type ('a, 'b) t
end

module Nonempty_list = struct
  type nonrec 'a t = ( :: ) of 'a * 'a list

  let to_list (x :: xs) : _ list = x :: xs

  let of_list_opt : _ list -> _ t option = function
    | [] -> None
    | (x :: xs)-> Some (x :: xs)

  let map f (x :: xs) = f x :: List.map f xs

  let pp_print ?pp_sep f ppf t =
    Format.pp_print_list ?pp_sep f ppf (to_list t)

  let (@) (x :: xs) (y :: ys) =
    x :: List.(xs @ (y :: ys))
end

module Maybe_bounded = struct
  type t =
    | Unbounded
    | Bounded of { mutable bound: int }

  let decr = function
    | Unbounded -> ()
    | Bounded r when r.bound > 0 -> r.bound <- r.bound - 1
    | Bounded _ -> ()

  let incr = function
    | Unbounded -> ()
    | Bounded r ->
      if Int.equal r.bound Int.max_int
      then
        let msg = Format.asprintf "incr called with max_int (%d)" Int.max_int in
        raise (Invalid_argument msg)
      else
        r.bound <- r.bound + 1

  let is_depleted = function
    | Unbounded -> false
    | Bounded r -> r.bound <= 0

  let is_in_bounds n t =
    if n < 0 then false
    else
      match t with
      | Unbounded -> true
      | Bounded r -> n < r.bound

  let is_out_of_bounds n t =
    if n < 0 then true
    else
      match t with
      | Unbounded -> false
      | Bounded r -> n >= r.bound

  let of_int n = if n < 0 then Bounded { bound = 0 } else Bounded { bound = n }

  let of_option = function
    | None -> Unbounded
    | Some n -> of_int n
end
