(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The following is a "big endian" implementation. *)

type key = int

module Builtins = struct
  external select_value :
    'a. bool -> ('a[@local_opt]) -> ('a[@local_opt]) -> ('a[@local_opt])
    = "caml_csel_value"
  [@@noalloc] [@@no_effects] [@@no_coeffects] [@@builtin]
end

external int_clz : int -> (int[@untagged])
  = "caml_int_clz_tagged_to_tagged" "caml_int_clz_tagged_to_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

type pbit = int

let[@inline always] zero_bit i bit = i land bit = 0

(* Most significant 1 bit *)
let highest_bit x = 1 lsl (62 - int_clz x)

(* Highest bit at which [prefix0] and [prefix1] differ *)
let branching_bit prefix0 prefix1 = highest_bit (prefix0 lxor prefix1)

(* Keep only the bits strictly higher than [i] *)
let mask i bit = i land -(bit lsl 1)

(* Does [i] match [prefix], whose length is [bit]? In other words, does [i]
   match [prefix] at every position strictly higher than [bit]? *)
let match_prefix i prefix bit = mask i bit = prefix

let[@inline always] pbit_bit p = p land -p

let[@inline always] pbit_prefix p = p lxor pbit_bit p

let[@inline always] make_pbit prefix bit = prefix lor bit

let[@inline always] higher bit0 bit1 = bit0 lsr 1 > bit1 lsr 1

let[@inline always] match_pbit_with_bit i p bit = i lxor p land -(bit lsl 1) = 0

let[@inline always] match_pbit i p =
  let bit = pbit_bit p in
  match_pbit_with_bit i p bit

(* Provides a total ordering over [(prefix, bit)] pairs. Not otherwise
   specified. (Only useful for implementing [compare], which is similarly
   loosely specified.) *)
let compare_prefix prefix0 bit0 prefix1 bit1 =
  (* Signed comparison is fine here, so long as it's a total ordering *)
  let c = compare bit0 bit1 in
  if c = 0 then compare prefix0 prefix1 else c

let compare_pbit p0 p1 =
  compare_prefix (pbit_prefix p0) (pbit_bit p0) (pbit_prefix p1) (pbit_bit p1)

let includes_pbit p0 p1 =
  let bit0 = pbit_bit p0 in
  let bit1 = pbit_bit p1 in
  higher bit0 bit1 && match_pbit_with_bit p1 p0 bit0

module Set = struct
  type elt = key

  type t =
    | Empty
    | Leaf of elt
    | Branch of pbit * t * t

  let[@inline always] branch pbit t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | (Leaf _ | Branch _), (Leaf _ | Branch _) -> Branch (pbit, t0, t1)

  let[@inline always] branch_non_empty pbit t0 t1 = Branch (pbit, t0, t1)

  let[@inline always] is_empty t = t == Empty

  let empty = Empty

  let[@inline always] singleton i = Leaf i

  let join i0 t0 i1 t1 =
    let bit = branching_bit i0 i1 in
    let pbit = make_pbit (mask i0 bit) bit in
    if zero_bit i0 bit then branch pbit t0 t1 else branch pbit t1 t0

  let rec mem i t =
    match t with
    | Empty -> false
    | Leaf j -> j = i
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      let x = i lxor pbit in
      if x land -(bit lsl 1) = 0
      then mem i (Builtins.select_value (x land bit <> 0) t0 t1)
      else false

  let rec add i t =
    match t with
    | Empty -> Leaf i
    | Leaf j -> if i = j then t else join i (Leaf i) j t
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      let x = i lxor pbit in
      if x land -(bit lsl 1) = 0
      then
        if x land bit <> 0
        then
          let t0' = add i t0 in
          if t0' == t0 then t else branch_non_empty pbit t0' t1
        else
          let t1' = add i t1 in
          if t1' == t1 then t else branch_non_empty pbit t0 t1'
      else join i (Leaf i) pbit t

  let rec remove i t =
    match t with
    | Empty -> Empty
    | Leaf j -> if i = j then Empty else t
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then
          let t0' = remove i t0 in
          if t0' == t0 then t else branch pbit t0' t1
        else
          let t1' = remove i t1 in
          if t1' == t1 then t else branch pbit t0 t1'
      else t

  let rec union t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | Leaf i0, Leaf i1 -> if i0 = i1 then t0 else join i0 t0 i1 t1
    | Leaf i, Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit_with_bit i pbit bit
      then
        if zero_bit i bit
        then
          let t10' = union t0 t10 in
          if t10' == t10 then t1 else branch_non_empty pbit t10' t11
        else
          let t11' = union t0 t11 in
          if t11' == t11 then t1 else branch_non_empty pbit t10 t11'
      else join i t0 pbit t1
    | Branch (pbit, t00, t01), Leaf i ->
      let bit = pbit_bit pbit in
      if match_pbit_with_bit i pbit bit
      then
        if zero_bit i bit
        then
          let t00' = union t00 t1 in
          if t00' == t00 then t0 else branch_non_empty pbit t00' t01
        else
          let t01' = union t01 t1 in
          if t01' == t01 then t0 else branch_non_empty pbit t00 t01'
      else join pbit t0 i t1
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then
        let t00' = union t00 t10 in
        let t01' = union t01 t11 in
        if t00' == t00 && t01' == t01
        then t0
        else if t00' == t10 && t01' == t11
        then t1
        else branch_non_empty pbit0 t00' t01'
      else if includes_pbit pbit0 pbit1
      then
        if zero_bit pbit1 bit0
        then
          let t00' = union t00 t1 in
          if t00' == t00 then t0 else branch_non_empty pbit0 t00' t01
        else
          let t01' = union t01 t1 in
          if t01' == t01 then t0 else branch_non_empty pbit0 t00 t01'
      else if includes_pbit pbit1 pbit0
      then
        if zero_bit pbit0 bit1
        then
          let t10' = union t0 t10 in
          if t10' == t10 then t1 else branch_non_empty pbit1 t10' t11
        else
          let t11' = union t0 t11 in
          if t11' == t11 then t1 else branch_non_empty pbit1 t10 t11'
      else join pbit0 t0 pbit1 t1

  let union_sharing = union

  let union_shared = union

  let rec subset t0 t1 =
    if t0 == t1
    then true
    else
      match t0, t1 with
      | Empty, _ -> true
      | _, Empty -> false
      | Branch _, Leaf _ -> false
      | Leaf i, _ -> mem i t1
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let bit1 = pbit_bit pbit1 in
        if pbit0 = pbit1
        then subset t00 t10 && subset t01 t11
        else if includes_pbit pbit1 pbit0
        then if zero_bit pbit0 bit1 then subset t0 t10 else subset t0 t11
        else false

  let rec inter t0 t1 =
    if t0 == t1
    then t0
    else
      match t0, t1 with
      | Empty, _ | _, Empty -> Empty
      | Leaf i, _ -> if mem i t1 then t0 else Empty
      | _, Leaf i -> if mem i t0 then t1 else Empty
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let bit0 = pbit_bit pbit0 in
        let bit1 = pbit_bit pbit1 in
        if pbit0 = pbit1
        then
          let t00' = inter t00 t10 in
          let t01' = inter t01 t11 in
          if t00' == t00 && t01' == t01
          then t0
          else if t00' == t10 && t01' == t11
          then t1
          else branch pbit0 t00' t01'
        else if includes_pbit pbit0 pbit1
        then if zero_bit pbit1 bit0 then inter t00 t1 else inter t01 t1
        else if includes_pbit pbit1 pbit0
        then if zero_bit pbit0 bit1 then inter t0 t10 else inter t0 t11
        else Empty

  let rec inter_domain_is_non_empty t0 t1 =
    if t0 == t1
    then not (is_empty t0)
    else
      match t0, t1 with
      | Empty, _ | _, Empty -> false
      | Leaf i, _ -> mem i t1
      | _, Leaf i -> mem i t0
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let bit0 = pbit_bit pbit0 in
        let bit1 = pbit_bit pbit1 in
        if pbit0 = pbit1
        then
          inter_domain_is_non_empty t00 t10 || inter_domain_is_non_empty t01 t11
        else if includes_pbit pbit0 pbit1
        then
          if zero_bit pbit1 bit0
          then inter_domain_is_non_empty t00 t1
          else inter_domain_is_non_empty t01 t1
        else if includes_pbit pbit1 pbit0
        then
          if zero_bit pbit0 bit1
          then inter_domain_is_non_empty t0 t10
          else inter_domain_is_non_empty t0 t11
        else false

  let disjoint t0 t1 = not (inter_domain_is_non_empty t0 t1)

  let rec diff t0 t1 =
    if t0 == t1
    then Empty
    else
      match t0, t1 with
      | Empty, _ -> Empty
      | _, Empty -> t0
      | Leaf i0, Leaf i1 -> if i0 = i1 then Empty else t0
      | Leaf i, Branch (pbit, t10, t11) ->
        let bit = pbit_bit pbit in
        if match_pbit_with_bit i pbit bit
        then if zero_bit i bit then diff t0 t10 else diff t0 t11
        else t0
      | Branch (pbit, t00, t01), Leaf i ->
        let bit = pbit_bit pbit in
        if match_pbit_with_bit i pbit bit
        then
          if zero_bit i bit
          then
            let t00' = diff t00 t1 in
            if t00' == t00 then t0 else branch pbit t00' t01
          else
            let t01' = diff t01 t1 in
            if t01' == t01 then t0 else branch pbit t00 t01'
        else t0
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let bit0 = pbit_bit pbit0 in
        let bit1 = pbit_bit pbit1 in
        if pbit0 = pbit1
        then
          let t00' = diff t00 t10 in
          let t01' = diff t01 t11 in
          if t00' == t00 && t01' == t01 then t0 else branch pbit0 t00' t01'
        else if includes_pbit pbit0 pbit1
        then
          if zero_bit pbit1 bit0
          then
            let t00' = diff t00 t1 in
            if t00' == t00 then t0 else branch pbit0 t00' t01
          else
            let t01' = diff t01 t1 in
            if t01' == t01 then t0 else branch pbit0 t00 t01'
        else if includes_pbit pbit1 pbit0
        then if zero_bit pbit0 bit1 then diff t0 t10 else diff t0 t11
        else t0

  let diff_sharing = diff

  let diff_shared = diff

  let rec cardinal t =
    match t with
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_, t0, t1) -> cardinal t0 + cardinal t1

  let rec unsigned_iter f t =
    match t with
    | Empty -> ()
    | Leaf key -> f key
    | Branch (_, t0, t1) ->
      unsigned_iter f t0;
      unsigned_iter f t1

  let iter f t =
    match t with
    | Empty -> ()
    | Leaf key -> f key
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then (
        unsigned_iter f t1;
        unsigned_iter f t0)
      else (
        unsigned_iter f t0;
        unsigned_iter f t1)

  let rec unsigned_fold f t acc =
    match t with
    | Empty -> acc
    | Leaf key -> f key acc
    | Branch (_, t0, t1) -> unsigned_fold f t1 (unsigned_fold f t0 acc)

  let fold f t acc =
    match t with
    | Empty -> acc
    | Leaf key -> f key acc
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then unsigned_fold f t0 (unsigned_fold f t1 acc)
      else unsigned_fold f t1 (unsigned_fold f t0 acc)

  let rec unsigned_for_all p t =
    match t with
    | Empty -> true
    | Leaf key -> p key
    | Branch (_, t0, t1) -> unsigned_for_all p t0 && unsigned_for_all p t1

  let for_all p t =
    match t with
    | Empty -> true
    | Leaf key -> p key
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then unsigned_for_all p t1 && unsigned_for_all p t0
      else unsigned_for_all p t0 && unsigned_for_all p t1

  let rec unsigned_exists p t =
    match t with
    | Empty -> false
    | Leaf key -> p key
    | Branch (_, t0, t1) -> unsigned_exists p t0 || unsigned_exists p t1

  let exists p t =
    match t with
    | Empty -> false
    | Leaf key -> p key
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then unsigned_exists p t1 || unsigned_exists p t0
      else unsigned_exists p t0 || unsigned_exists p t1

  let rec filter p t =
    match t with
    | Empty -> t
    | Leaf i -> if p i then t else Empty
    | Branch (pbit, t0, t1) ->
      let t0' = filter p t0 in
      let t1' = filter p t1 in
      if t0' == t0 && t1' == t1 then t else branch pbit t0' t1'

  let rec partition p t =
    match t with
    | Empty -> Empty, Empty
    | Leaf i -> if p i then t, Empty else Empty, t
    | Branch (pbit, t0, t1) ->
      let yes0, no0 = partition p t0 in
      let yes1, no1 = partition p t1 in
      branch pbit yes0 yes1, branch pbit no0 no1

  let rec union_list ts =
    match ts with [] -> empty | t :: ts -> union t (union_list ts)

  let filter_map f t =
    let rec loop f acc = function
      | Empty -> acc
      | Leaf i -> ( match f i with None -> acc | Some j -> add j acc)
      | Branch (_, t0, t1) -> loop f (loop f acc t0) t1
    in
    loop f Empty t

  let rec unsigned_min_elt t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf key -> key
    | Branch (_, t0, _) -> unsigned_min_elt t0

  let min_elt t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf key -> key
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      unsigned_min_elt (if bit < 0 then t1 else t0)

  let[@inline always] min_elt_opt t =
    match t with Empty -> None | Leaf _ | Branch _ -> Some (min_elt t)

  let rec unsigned_max_elt t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf key -> key
    | Branch (_, _, t1) -> unsigned_max_elt t1

  let max_elt t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf key -> key
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      unsigned_max_elt (if bit < 0 then t0 else t1)

  let[@inline always] max_elt_opt t =
    match t with Empty -> None | Leaf _ | Branch _ -> Some (max_elt t)

  let rec choose t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf key -> key
    | Branch (_, t0, _) -> choose t0

  let[@inline always] choose_opt t =
    match t with Empty -> None | Leaf _ | Branch _ -> Some (choose t)

  let rec equal t0 t1 =
    if t0 == t1
    then true
    else
      match t0, t1 with
      | Empty, Empty -> assert false
      | Leaf i, Leaf j -> i = j
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        pbit0 = pbit1 && equal t00 t10 && equal t01 t11
      | (Empty | Leaf _ | Branch _), _ -> false

  let rec compare t0 t1 =
    if t0 == t1
    then 0
    else
      match t0, t1 with
      | Empty, Empty -> assert false
      | Leaf i, Leaf j -> Int.compare i j
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let c = compare_pbit pbit0 pbit1 in
        if c = 0
        then
          let c = compare t00 t10 in
          if c = 0 then compare t01 t11 else c
        else c
      | Empty, Leaf _ -> 1
      | Empty, Branch _ -> 1
      | Leaf _, Branch _ -> 1
      | Leaf _, Empty -> -1
      | Branch _, Empty -> -1
      | Branch _, Leaf _ -> -1

  let same_sign_split i t =
    let rec loop t =
      match t with
      | Empty -> Empty, false, Empty
      | Leaf j ->
        if i = j
        then Empty, true, Empty
        else if j < i
        then t, false, Empty
        else Empty, false, t
      | Branch (pbit, t0, t1) ->
        let bit = pbit_bit pbit in
        if match_pbit_with_bit i pbit bit
        then
          if zero_bit i bit
          then
            let lt, mem, gt = loop t0 in
            lt, mem, branch pbit gt t1
          else
            let lt, mem, gt = loop t1 in
            branch pbit t0 lt, mem, gt
        else if i < pbit_prefix pbit
        then Empty, false, t
        else t, false, Empty
    in
    loop t

  let split i t =
    match t with
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then
        if i < 0
        then
          let lt, mem, gt = same_sign_split i t1 in
          lt, mem, branch pbit t0 gt
        else
          let lt, mem, gt = same_sign_split i t0 in
          branch pbit lt t1, mem, gt
      else same_sign_split i t
    | Empty | Leaf _ -> same_sign_split i t

  let find elt t = if mem elt t then elt else raise_notrace Not_found

  let to_list t =
    let rec loop acc t =
      match t with
      | Empty -> acc
      | Leaf i -> i :: acc
      | Branch (_, t0, t1) -> loop (loop acc t1) t0
    in
    match t with
    | Empty -> []
    | Leaf i -> [i]
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0 then loop (loop [] t0) t1 else loop (loop [] t1) t0

  let elements = to_list

  let to_seq t =
    let rec aux acc () =
      match acc with
      | [] -> Seq.Nil
      | t0 :: rest -> (
        match t0 with
        | Empty -> aux rest ()
        | Leaf key -> Seq.Cons (key, aux rest)
        | Branch (_, t1, t2) -> aux (t1 :: t2 :: rest) ())
    in
    fun () ->
      match t with
      | Empty -> Seq.Nil
      | Leaf key -> Seq.Cons (key, aux [])
      | Branch (pbit, t0, t1) ->
        let bit = pbit_bit pbit in
        if bit < 0 then aux [t1; t0] () else aux [t0; t1] ()

  let of_list l = List.fold_left (fun set elt -> add elt set) empty l

  let map f t = fold (fun elt acc -> add (f elt) acc) t empty

  let get_singleton t =
    match t with Empty | Branch _ -> None | Leaf elt -> Some elt

  let valid t =
    let rec check_deep prefix bit t =
      match t with
      | Empty -> false
      | Leaf i -> (bit = 0 && prefix = i) || match_prefix i prefix bit
      | Branch (pbit, t0, t1) ->
        let bit' = pbit_bit pbit in
        let prefix' = pbit_prefix pbit in
        let prefix0 = prefix' in
        let prefix1 = prefix' lor bit' in
        let bit0 = bit' lsr 1 in
        bit' <> 0
        && pbit = make_pbit prefix' bit'
        && (bit = bit' || higher bit bit')
        && match_prefix prefix' prefix bit
        && check_deep prefix0 bit0 t0 && check_deep prefix1 bit0 t1
    in
    is_empty t || check_deep 0 min_int t
end

module Map = struct
  type key = int

  type +'a t =
    | Empty
    | Leaf of key * 'a
    | Branch of pbit * 'a t * 'a t

  let[@inline always] branch pbit t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | (Leaf _ | Branch _), (Leaf _ | Branch _) -> Branch (pbit, t0, t1)

  let[@inline always] branch_non_empty pbit t0 t1 = Branch (pbit, t0, t1)

  let[@inline always] is_empty t = t == Empty

  let empty = Empty

  let[@inline always] singleton i d = Leaf (i, d)

  let rec mem i t =
    match t with
    | Empty -> false
    | Leaf (j, _) -> j = i
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      let x = i lxor pbit in
      if x land -(bit lsl 1) = 0
      then mem i (Builtins.select_value (x land bit <> 0) t0 t1)
      else false

  let rec find i t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf (j, d) -> if j = i then d else raise_notrace Not_found
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      let x = i lxor pbit in
      if x land -(bit lsl 1) = 0
      then find i (Builtins.select_value (x land bit <> 0) t0 t1)
      else raise_notrace Not_found

  let[@inline always] find_opt key t =
    match find key t with exception Not_found -> None | datum -> Some datum

  let join i0 t0 i1 t1 =
    let bit = branching_bit i0 i1 in
    let pbit = make_pbit (mask i0 bit) bit in
    if zero_bit i0 bit then branch pbit t0 t1 else branch pbit t1 t0

  let rec add i d t =
    match t with
    | Empty -> Leaf (i, d)
    | Leaf (j, d') ->
      if i = j
      then if d == d' then t else Leaf (i, d)
      else join i (Leaf (i, d)) j t
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      let x = i lxor pbit in
      if x land -(bit lsl 1) = 0
      then
        if x land bit <> 0
        then
          let t0' = add i d t0 in
          if t0' == t0 then t else branch_non_empty pbit t0' t1
        else
          let t1' = add i d t1 in
          if t1' == t1 then t else branch_non_empty pbit t0 t1'
      else join i (Leaf (i, d)) pbit t

  let rec replace key f t =
    match t with
    | Empty -> Empty
    | Leaf (key', datum) ->
      if key = key'
      then
        let datum' = f datum in
        if datum' == datum then t else Leaf (key, datum')
      else t
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if match_pbit key pbit
      then
        if zero_bit key bit
        then
          let t0' = replace key f t0 in
          if t0' == t0 then t else branch_non_empty pbit t0' t1
        else
          let t1' = replace key f t1 in
          if t1' == t1 then t else branch_non_empty pbit t0 t1'
      else t

  let rec update key f t =
    match t with
    | Empty -> (
      match f None with None -> t | Some datum -> Leaf (key, datum))
    | Leaf (key', datum) -> (
      if key = key'
      then
        match f (Some datum) with
        | None -> Empty
        | Some datum' -> if datum' == datum then t else Leaf (key, datum')
      else
        match f None with
        | None -> t
        | Some datum' -> join key (Leaf (key, datum')) key' t)
    | Branch (pbit, t0, t1) -> (
      let bit = pbit_bit pbit in
      if match_pbit key pbit
      then
        if zero_bit key bit
        then
          let t0' = update key f t0 in
          if t0' == t0 then t else branch pbit t0' t1
        else
          let t1' = update key f t1 in
          if t1' == t1 then t else branch pbit t0 t1'
      else
        match f None with
        | None -> t
        | Some datum' -> join key (Leaf (key, datum')) pbit t)

  let rec remove i t =
    match t with
    | Empty -> Empty
    | Leaf (j, _) -> if i = j then Empty else t
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then
          let t0' = remove i t0 in
          if t0' == t0 then t else branch pbit t0' t1
        else
          let t1' = remove i t1 in
          if t1' == t1 then t else branch pbit t0 t1'
      else t

  let[@inline always] leaf_or_empty key = function
    | None -> Empty
    | Some datum -> Leaf (key, datum)

  let rec merge_left f t0 =
    match t0 with
    | Empty -> Empty
    | Leaf (i, d) -> leaf_or_empty i (f i (Some d) None)
    | Branch (pbit, t00, t01) ->
      let t01' = merge_left f t01 in
      let t00' = merge_left f t00 in
      branch pbit t00' t01'

  let rec merge_right f t1 =
    match t1 with
    | Empty -> Empty
    | Leaf (i, d) -> leaf_or_empty i (f i None (Some d))
    | Branch (pbit, t10, t11) ->
      let t11' = merge_right f t11 in
      let t10' = merge_right f t10 in
      branch pbit t10' t11'

  let rec merge f t0 t1 =
    match t0, t1 with
    | Empty, _ -> merge_right f t1
    | _, Empty -> merge_left f t0
    | Leaf (i0, d0), Leaf (i1, d1) ->
      if i0 = i1
      then leaf_or_empty i0 (f i0 (Some d0) (Some d1))
      else join i0 (merge_left f t0) i1 (merge_right f t1)
    | Leaf (i, _), Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then
          let t11' = merge_right f t11 in
          let t10' = merge f t0 t10 in
          branch pbit t10' t11'
        else
          let t11' = merge f t0 t11 in
          let t10' = merge_right f t10 in
          branch pbit t10' t11'
      else join i (merge_left f t0) pbit (merge_right f t1)
    | Branch (pbit, t00, t01), Leaf (i, _) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then
          let t01' = merge_left f t01 in
          let t00' = merge f t00 t1 in
          branch pbit t00' t01'
        else
          let t01' = merge f t01 t1 in
          let t00' = merge_left f t00 in
          branch pbit t00' t01'
      else join pbit (merge_left f t0) i (merge_right f t1)
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then
        let t01' = merge f t01 t11 in
        let t00' = merge f t00 t10 in
        branch pbit0 t00' t01'
      else if includes_pbit pbit0 pbit1
      then
        if zero_bit pbit1 bit0
        then
          let t01' = merge_left f t01 in
          let t00' = merge f t00 t1 in
          branch pbit0 t00' t01'
        else
          let t01' = merge f t01 t1 in
          let t00' = merge_left f t00 in
          branch pbit0 t00' t01'
      else if includes_pbit pbit1 pbit0
      then
        if zero_bit pbit0 bit1
        then
          let t11' = merge_right f t11 in
          let t10' = merge f t0 t10 in
          branch pbit1 t10' t11'
        else
          let t11' = merge f t0 t11 in
          let t10' = merge_right f t10 in
          branch pbit1 t10' t11'
      else join pbit0 (merge_left f t0) pbit1 (merge_right f t1)

  let rec union_total f t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | Leaf (i0, d0), Leaf (i1, d1) ->
      if i0 = i1
      then
        let d = f i0 d0 d1 in
        if d == d0 then t0 else if d == d1 then t1 else Leaf (i0, d)
      else join i0 t0 i1 t1
    | Leaf (i, _), Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit_with_bit i pbit bit
      then
        if zero_bit i bit
        then
          let t10' = union_total f t0 t10 in
          if t10' == t10 then t1 else branch_non_empty pbit t10' t11
        else
          let t11' = union_total f t0 t11 in
          if t11' == t11 then t1 else branch_non_empty pbit t10 t11'
      else join i t0 pbit t1
    | Branch (pbit, t00, t01), Leaf (i, _) ->
      let bit = pbit_bit pbit in
      if match_pbit_with_bit i pbit bit
      then
        if zero_bit i bit
        then
          let t00' = union_total f t00 t1 in
          if t00' == t00 then t0 else branch_non_empty pbit t00' t01
        else
          let t01' = union_total f t01 t1 in
          if t01' == t01 then t0 else branch_non_empty pbit t00 t01'
      else join pbit t0 i t1
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then
        let t00' = union_total f t00 t10 in
        let t01' = union_total f t01 t11 in
        if t00' == t00 && t01' == t01
        then t0
        else if t00' == t10 && t01' == t11
        then t1
        else branch_non_empty pbit0 t00' t01'
      else
        let pbit0_includes_pbit1 =
          higher bit0 bit1 && match_pbit_with_bit pbit1 pbit0 bit0
        in
        if pbit0_includes_pbit1
        then
          if zero_bit pbit1 bit0
          then
            let t00' = union_total f t00 t1 in
            if t00' == t00 then t0 else branch_non_empty pbit0 t00' t01
          else
            let t01' = union_total f t01 t1 in
            if t01' == t01 then t0 else branch_non_empty pbit0 t00 t01'
        else if higher bit1 bit0 && match_pbit_with_bit pbit0 pbit1 bit1
        then
          if zero_bit pbit0 bit1
          then
            let t10' = union_total f t0 t10 in
            if t10' == t10 then t1 else branch_non_empty pbit1 t10' t11
          else
            let t11' = union_total f t0 t11 in
            if t11' == t11 then t1 else branch_non_empty pbit1 t10 t11'
        else join pbit0 t0 pbit1 t1

  let union_total_shared = union_total

  let[@inline always] union_left_biased t0 t1 =
    union_total (fun _ left _right -> left) t0 t1

  let[@inline always] union_right_biased t0 t1 =
    union_total (fun _ _left right -> right) t0 t1

  let rec union f t0 t1 =
    match t0, t1 with
    | Empty, _ -> t1
    | _, Empty -> t0
    | Leaf (i0, d0), Leaf (i1, d1) ->
      if i0 = i1
      then
        match f i0 d0 d1 with
        | None -> Empty
        | Some d -> if d == d0 then t0 else if d == d1 then t1 else Leaf (i0, d)
      else join i0 t0 i1 t1
    | Leaf (i, _), Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit_with_bit i pbit bit
      then
        if zero_bit i bit
        then
          let t10' = union f t0 t10 in
          if t10' == t10 then t1 else branch pbit t10' t11
        else
          let t11' = union f t0 t11 in
          if t11' == t11 then t1 else branch pbit t10 t11'
      else join i t0 pbit t1
    | Branch (pbit, t00, t01), Leaf (i, _) ->
      let bit = pbit_bit pbit in
      if match_pbit_with_bit i pbit bit
      then
        if zero_bit i bit
        then
          let t00' = union f t00 t1 in
          if t00' == t00 then t0 else branch pbit t00' t01
        else
          let t01' = union f t01 t1 in
          if t01' == t01 then t0 else branch pbit t00 t01'
      else join pbit t0 i t1
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then
        let t00' = union f t00 t10 in
        let t01' = union f t01 t11 in
        if t00' == t00 && t01' == t01
        then t0
        else if t00' == t10 && t01' == t11
        then t1
        else branch pbit0 t00' t01'
      else
        let pbit0_includes_pbit1 =
          higher bit0 bit1 && match_pbit_with_bit pbit1 pbit0 bit0
        in
        if pbit0_includes_pbit1
        then
          if zero_bit pbit1 bit0
          then
            let t00' = union f t00 t1 in
            if t00' == t00 then t0 else branch pbit0 t00' t01
          else
            let t01' = union f t01 t1 in
            if t01' == t01 then t0 else branch pbit0 t00 t01'
        else if higher bit1 bit0 && match_pbit_with_bit pbit0 pbit1 bit1
        then
          if zero_bit pbit0 bit1
          then
            let t10' = union f t0 t10 in
            if t10' == t10 then t1 else branch pbit1 t10' t11
          else
            let t11' = union f t0 t11 in
            if t11' == t11 then t1 else branch pbit1 t10 t11'
        else join pbit0 t0 pbit1 t1

  let union_sharing = union

  let union_shared = union

  let rec update_many_right f t1 =
    match t1 with
    | Empty -> Empty
    | Leaf (k, d1) -> leaf_or_empty k (f k None d1)
    | Branch (pbit, t10, t11) ->
      let t11' = update_many_right f t11 in
      let t10' = update_many_right f t10 in
      branch pbit t10' t11'

  let rec update_many f t0 t1 =
    match t0, t1 with
    | _, Empty -> t0
    | Empty, _ -> update_many_right f t1
    | Leaf (i0, d0), Leaf (i1, d1) ->
      if i0 = i1
      then leaf_or_empty i0 (f i0 (Some d0) d1)
      else join i0 t0 i1 (update_many_right f t1)
    | Leaf (i, _), Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then
          let t11' = update_many_right f t11 in
          let t10' = update_many f t0 t10 in
          branch pbit t10' t11'
        else
          let t11' = update_many f t0 t11 in
          let t10' = update_many_right f t10 in
          branch pbit t10' t11'
      else join i t0 pbit (update_many_right f t1)
    | Branch _, Leaf (_, _) ->
      merge
        (fun key left right ->
          match left, right with
          | Some d0, Some d1 -> f key (Some d0) d1
          | Some d0, None -> Some d0
          | None, Some d1 -> f key None d1
          | None, None -> None)
        t0 t1
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then
        let t01' = update_many f t01 t11 in
        let t00' = update_many f t00 t10 in
        branch pbit0 t00' t01'
      else if includes_pbit pbit0 pbit1
      then
        if zero_bit pbit1 bit0
        then
          let t00' = update_many f t00 t1 in
          branch pbit0 t00' t01
        else
          let t01' = update_many f t01 t1 in
          branch pbit0 t00 t01'
      else if includes_pbit pbit1 pbit0
      then
        if zero_bit pbit0 bit1
        then
          let t11' = update_many_right f t11 in
          let t10' = update_many f t0 t10 in
          branch pbit1 t10' t11'
        else
          let t11' = update_many f t0 t11 in
          let t10' = update_many_right f t10 in
          branch pbit1 t10' t11'
      else join pbit0 t0 pbit1 (update_many_right f t1)

  let rec diff f t0 t1 =
    match t0, t1 with
    | Empty, _ -> Empty
    | _, Empty -> t0
    | Leaf (i0, d0), Leaf (i1, d1) ->
      if i0 = i1 then leaf_or_empty i0 (f i0 d0 d1) else t0
    | Leaf (i, _), Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then if zero_bit i bit then diff f t0 t10 else diff f t0 t11
      else t0
    | Branch (pbit, t00, t01), Leaf (i, _d1) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then branch pbit (diff f t00 t1) t01
        else branch pbit t00 (diff f t01 t1)
      else t0
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then branch pbit0 (diff f t00 t10) (diff f t01 t11)
      else if includes_pbit pbit0 pbit1
      then
        if zero_bit pbit1 bit0
        then branch pbit0 (diff f t00 t1) t01
        else branch pbit0 t00 (diff f t01 t1)
      else if includes_pbit pbit1 pbit0
      then if zero_bit pbit0 bit1 then diff f t0 t10 else diff f t0 t11
      else t0

  let rec diff_sharing f t0 t1 =
    match t0, t1 with
    | Empty, _ -> Empty
    | _, Empty -> t0
    | Leaf (i0, d0), Leaf (i1, d1) ->
      if i0 = i1
      then
        match f i0 d0 d1 with
        | None -> Empty
        | Some d' -> if d' == d0 then t0 else Leaf (i0, d')
      else t0
    | Leaf (i, _), Branch (pbit, t10, t11) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit then diff_sharing f t0 t10 else diff_sharing f t0 t11
      else t0
    | Branch (pbit, t00, t01), Leaf (i, _d1) ->
      let bit = pbit_bit pbit in
      if match_pbit i pbit
      then
        if zero_bit i bit
        then
          let t00' = diff_sharing f t00 t1 in
          if t00' == t00 then t0 else branch pbit t00' t01
        else
          let t01' = diff_sharing f t01 t1 in
          if t01' == t01 then t0 else branch pbit t00 t01'
      else t0
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then
        let t00' = diff_sharing f t00 t10 in
        let t01' = diff_sharing f t01 t11 in
        if t00' == t00 && t01' == t01 then t0 else branch pbit0 t00' t01'
      else if includes_pbit pbit0 pbit1
      then
        if zero_bit pbit1 bit0
        then
          let t00' = diff_sharing f t00 t1 in
          if t00' == t00 then t0 else branch pbit0 t00' t01
        else
          let t01' = diff_sharing f t01 t1 in
          if t01' == t01 then t0 else branch pbit0 t00 t01'
      else if includes_pbit pbit1 pbit0
      then
        if zero_bit pbit0 bit1
        then diff_sharing f t0 t10
        else diff_sharing f t0 t11
      else t0

  let diff_shared f t0 t1 = if t0 == t1 then Empty else diff_sharing f t0 t1

  let rec inter f t0 t1 =
    match t0, t1 with
    | Empty, _ | _, Empty -> Empty
    | Leaf (i, d0), _ -> (
      match find i t1 with
      | exception Not_found -> Empty
      | d1 -> Leaf (i, f i d0 d1))
    | _, Leaf (i, d1) -> (
      match find i t0 with
      | exception Not_found -> Empty
      | d0 -> Leaf (i, f i d0 d1))
    | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
      let bit0 = pbit_bit pbit0 in
      let bit1 = pbit_bit pbit1 in
      if pbit0 = pbit1
      then branch pbit0 (inter f t00 t10) (inter f t01 t11)
      else if includes_pbit pbit0 pbit1
      then if zero_bit pbit1 bit0 then inter f t00 t1 else inter f t01 t1
      else if includes_pbit pbit1 pbit0
      then if zero_bit pbit0 bit1 then inter f t0 t10 else inter f t0 t11
      else Empty

  let rec inter_domain_is_non_empty t0 t1 =
    if t0 == t1
    then not (is_empty t0)
    else
      match t0, t1 with
      | Empty, _ | _, Empty -> false
      | Leaf (i, _), _ -> mem i t1
      | _, Leaf (i, _) -> mem i t0
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let bit0 = pbit_bit pbit0 in
        let bit1 = pbit_bit pbit1 in
        if pbit0 = pbit1
        then
          inter_domain_is_non_empty t00 t10 || inter_domain_is_non_empty t01 t11
        else if includes_pbit pbit0 pbit1
        then
          if zero_bit pbit1 bit0
          then inter_domain_is_non_empty t00 t1
          else inter_domain_is_non_empty t01 t1
        else if includes_pbit pbit1 pbit0
        then
          if zero_bit pbit0 bit1
          then inter_domain_is_non_empty t0 t10
          else inter_domain_is_non_empty t0 t11
        else false

  let diff_domains t0 t1 = diff (fun _ _ _ -> None) t0 t1

  let rec cardinal t =
    match t with
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_, t0, t1) -> cardinal t0 + cardinal t1

  let rec unsigned_iter f t =
    match t with
    | Empty -> ()
    | Leaf (key, d) -> f key d
    | Branch (_, t0, t1) ->
      unsigned_iter f t0;
      unsigned_iter f t1

  let iter f t =
    match t with
    | Empty -> ()
    | Leaf (key, d) -> f key d
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then (
        unsigned_iter f t1;
        unsigned_iter f t0)
      else (
        unsigned_iter f t0;
        unsigned_iter f t1)

  let rec unsigned_fold f t acc =
    match t with
    | Empty -> acc
    | Leaf (key, d) -> f key d acc
    | Branch (_, t0, t1) -> unsigned_fold f t1 (unsigned_fold f t0 acc)

  let fold f t acc =
    match t with
    | Empty -> acc
    | Leaf (key, d) -> f key d acc
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then unsigned_fold f t0 (unsigned_fold f t1 acc)
      else unsigned_fold f t1 (unsigned_fold f t0 acc)

  let rec unsigned_for_all p t =
    match t with
    | Empty -> true
    | Leaf (key, d) -> p key d
    | Branch (_, t0, t1) -> unsigned_for_all p t0 && unsigned_for_all p t1

  let for_all p t =
    match t with
    | Empty -> true
    | Leaf (key, d) -> p key d
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then unsigned_for_all p t1 && unsigned_for_all p t0
      else unsigned_for_all p t0 && unsigned_for_all p t1

  let rec unsigned_exists p t =
    match t with
    | Empty -> false
    | Leaf (key, d) -> p key d
    | Branch (_, t0, t1) -> unsigned_exists p t0 || unsigned_exists p t1

  let exists p t =
    match t with
    | Empty -> false
    | Leaf (key, d) -> p key d
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then unsigned_exists p t1 || unsigned_exists p t0
      else unsigned_exists p t0 || unsigned_exists p t1

  let rec filter p t =
    match t with
    | Empty -> t
    | Leaf (i, d) -> if p i d then t else Empty
    | Branch (pbit, t0, t1) ->
      let t1' = filter p t1 in
      let t0' = filter p t0 in
      if t0' == t0 && t1' == t1 then t else branch pbit t0' t1'

  let rec filter_map f t =
    match t with
    | Empty -> Empty
    | Leaf (k, d) -> (
      match f k d with None -> Empty | Some d' -> Leaf (k, d'))
    | Branch (pbit, t0, t1) ->
      let t1' = filter_map f t1 in
      let t0' = filter_map f t0 in
      branch pbit t0' t1'

  let rec filter_map_sharing f t =
    match t with
    | Empty -> t
    | Leaf (k, d) -> (
      match f k d with
      | None -> Empty
      | Some d' when d == d' -> t
      | Some d' -> Leaf (k, d'))
    | Branch (pbit, t0, t1) ->
      let t0' = filter_map_sharing f t0 in
      if t0' == t0
      then
        let t1' = filter_map_sharing f t1 in
        if t1' == t1 then t else branch pbit t0 t1'
      else
        let t1' = filter_map_sharing f t1 in
        branch pbit t0' t1'

  let rec partition p t =
    match t with
    | Empty -> Empty, Empty
    | Leaf (i, d) -> if p i d then t, Empty else Empty, t
    | Branch (pbit, t0, t1) ->
      let yes0, no0 = partition p t0 in
      let yes1, no1 = partition p t1 in
      branch pbit yes0 yes1, branch pbit no0 no1

  let rec choose t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf (key, d) -> key, d
    | Branch (_, t0, _) -> choose t0

  let[@inline always] choose_opt t =
    match t with Empty -> None | Leaf _ | Branch _ -> Some (choose t)

  let rec unsigned_min_binding t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf (key, d) -> key, d
    | Branch (_, t0, _) -> unsigned_min_binding t0

  let min_binding t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf (key, d) -> key, d
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      unsigned_min_binding (if bit < 0 then t1 else t0)

  let[@inline always] min_binding_opt t =
    match t with Empty -> None | Leaf _ | Branch _ -> Some (min_binding t)

  let rec unsigned_max_binding t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf (key, d) -> key, d
    | Branch (_, _, t1) -> unsigned_max_binding t1

  let max_binding t =
    match t with
    | Empty -> raise_notrace Not_found
    | Leaf (key, d) -> key, d
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      unsigned_max_binding (if bit < 0 then t0 else t1)

  let[@inline always] max_binding_opt t =
    match t with Empty -> None | Leaf _ | Branch _ -> Some (max_binding t)

  let rec equal f t0 t1 =
    if t0 == t1
    then true
    else
      match t0, t1 with
      | Empty, Empty -> assert false
      | Leaf (i, d0), Leaf (j, d1) -> i = j && f d0 d1
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        pbit0 = pbit1 && equal f t00 t10 && equal f t01 t11
      | (Empty | Leaf _ | Branch _), _ -> false

  let rec compare f t0 t1 =
    if t0 == t1
    then 0
    else
      match t0, t1 with
      | Empty, Empty -> assert false
      | Leaf (i, d0), Leaf (j, d1) ->
        let c = Int.compare i j in
        if c <> 0 then c else f d0 d1
      | Branch (pbit0, t00, t01), Branch (pbit1, t10, t11) ->
        let c = compare_pbit pbit0 pbit1 in
        if c = 0
        then
          let c = compare f t00 t10 in
          if c = 0 then compare f t01 t11 else c
        else c
      | Empty, Leaf _ -> 1
      | Empty, Branch _ -> 1
      | Leaf _, Branch _ -> 1
      | Leaf _, Empty -> -1
      | Branch _, Empty -> -1
      | Branch _, Leaf _ -> -1

  let same_sign_split key t =
    let rec loop t =
      match t with
      | Empty -> Empty, None, Empty
      | Leaf (j, d) ->
        if key = j
        then Empty, Some d, Empty
        else if j < key
        then t, None, Empty
        else Empty, None, t
      | Branch (pbit, t0, t1) ->
        let bit = pbit_bit pbit in
        if match_pbit key pbit
        then
          if zero_bit key bit
          then
            let lt, mem, gt = loop t0 in
            lt, mem, branch pbit gt t1
          else
            let lt, mem, gt = loop t1 in
            branch pbit t0 lt, mem, gt
        else if key < pbit_prefix pbit
        then Empty, None, t
        else t, None, Empty
    in
    loop t

  let split key t =
    match t with
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0
      then
        if key < 0
        then
          let lt, mem, gt = same_sign_split key t1 in
          lt, mem, branch pbit t0 gt
        else
          let lt, mem, gt = same_sign_split key t0 in
          branch pbit lt t1, mem, gt
      else same_sign_split key t
    | Empty | Leaf _ -> same_sign_split key t

  let bindings t =
    let rec loop acc t =
      match t with
      | Empty -> acc
      | Leaf (i, d) -> (i, d) :: acc
      | Branch (_, t0, t1) -> loop (loop acc t1) t0
    in
    match t with
    | Empty -> []
    | Leaf (i, d) -> [i, d]
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0 then loop (loop [] t0) t1 else loop (loop [] t1) t0

  let rec map f t =
    match t with
    | Empty -> Empty
    | Leaf (k, datum) -> Leaf (k, f datum)
    | Branch (pbit, t0, t1) ->
      let t1' = map f t1 in
      let t0' = map f t0 in
      branch_non_empty pbit t0' t1'

  let rec map_sharing f t =
    match t with
    | Empty -> t
    | Leaf (k, v) ->
      let v' = f v in
      if v == v' then t else Leaf (k, v')
    | Branch (pbit, t0, t1) ->
      let t0' = map_sharing f t0 in
      let t1' = map_sharing f t1 in
      if t0' == t0 && t1' == t1 then t else branch_non_empty pbit t0' t1'

  let rec mapi f t =
    match t with
    | Empty -> Empty
    | Leaf (key, datum) -> Leaf (key, f key datum)
    | Branch (pbit, t0, t1) ->
      let t1' = mapi f t1 in
      let t0' = mapi f t0 in
      branch_non_empty pbit t0' t1'

  type 'a iterator =
    | Done
    | Next of (key * 'a) * 'a t list

  let rec iterator0 t rest =
    match t with
    | Empty -> Done
    | Leaf (k, d) -> Next ((k, d), rest)
    | Branch (_, t0, t1) -> iterator0 t0 (t1 :: rest)

  let iterator t =
    match t with
    | Empty -> Done
    | Leaf (k, d) -> Next ((k, d), [])
    | Branch (pbit, t0, t1) ->
      let bit = pbit_bit pbit in
      if bit < 0 then iterator0 t1 [t0] else iterator0 t0 [t1]

  let[@inline always] current it =
    match it with Done -> None | Next (b, _) -> Some b

  let[@inline always] advance it =
    match it with
    | Done | Next (_, []) -> Done
    | Next (_, t :: rest) -> iterator0 t rest

  let rec seek0 k t rest =
    match t, rest with
    | Leaf (i, d), _ when k <= i -> Next ((i, d), rest)
    | Branch (pbit, t0, t1), _ when match_pbit k pbit ->
      let bit = pbit_bit pbit in
      if zero_bit k bit then seek0 k t0 (t1 :: rest) else seek0 k t1 rest
    | Branch (pbit, t0, t1), _ when k <= pbit_prefix pbit ->
      iterator0 t0 (t1 :: rest)
    | (Empty | Leaf _ | Branch _), [] -> Done
    | (Empty | Leaf _ | Branch _), t' :: rest' -> seek0 k t' rest'

  let seek it k =
    match it with
    | Done -> Done
    | Next ((key, _), rest) -> (
      if k <= key
      then it
      else match rest with [] -> Done | t' :: rest' -> seek0 k t' rest')

  let to_seq t =
    let rec aux acc () =
      match acc with
      | [] -> Seq.Nil
      | t0 :: rest -> (
        match t0 with
        | Empty -> aux rest ()
        | Leaf (key, value) -> Seq.Cons ((key, value), aux rest)
        | Branch (_, t1, t2) -> aux (t1 :: t2 :: rest) ())
    in
    fun () ->
      match t with
      | Empty -> Seq.Nil
      | Leaf (key, value) -> Seq.Cons ((key, value), aux [])
      | Branch (pbit, t0, t1) ->
        let bit = pbit_bit pbit in
        if bit < 0 then aux [t1; t0] () else aux [t0; t1] ()

  let of_list l = List.fold_left (fun map (k, d) -> add k d map) empty l

  let[@inline always] get_singleton t =
    match t with
    | Empty | Branch _ -> None
    | Leaf (key, datum) -> Some (key, datum)

  let[@inline always] disjoint_union ?eq ~print t1 t2 =
    if t1 == t2
    then t1
    else
      let fail key =
        Misc.fatal_errorf
          "Patricia_tree.disjoint_union: key %a is in intersection" print key
      in
      union
        (fun key datum1 datum2 ->
          match eq with
          | None -> fail key
          | Some eq -> if eq datum1 datum2 then Some datum1 else fail key)
        t1 t2

  (* CR-someday lmaurer: This should be doable as a fast map operation if we
     generalize [Ops.map] by letting the returned tree be built by a different
     [Tree] module *)
  let map_keys f t = fold (fun i d acc -> add (f i) d acc) t empty

  let rec keys : 'a t -> Set.t = function
    | Empty -> Set.Empty
    | Leaf (key, _value) -> Set.Leaf key
    | Branch (pbit, t0, t1) -> Set.Branch (pbit, keys t0, keys t1)

  let data t = List.map snd (bindings t)

  let rec of_set f : Set.t -> 'a t = function
    | Set.Empty -> Empty
    | Set.Leaf key -> Leaf (key, f key)
    | Set.Branch (pbit, t0, t1) -> Branch (pbit, of_set f t0, of_set f t1)

  let valid t =
    let rec check_deep prefix bit t =
      match t with
      | Empty -> false
      | Leaf (i, _) -> (bit = 0 && prefix = i) || match_prefix i prefix bit
      | Branch (pbit, t0, t1) ->
        let bit' = pbit_bit pbit in
        let prefix' = pbit_prefix pbit in
        let prefix0 = prefix' in
        let prefix1 = prefix' lor bit' in
        let bit0 = bit' lsr 1 in
        bit' <> 0
        && pbit = make_pbit prefix' bit'
        && (bit = bit' || higher bit bit')
        && match_prefix prefix' prefix bit
        && check_deep prefix0 bit0 t0 && check_deep prefix1 bit0 t1
    in
    is_empty t || check_deep 0 min_int t
end

module Raw_set = Set
module Raw_map = Map

type set = Set.t

type +!'a map = 'a Map.t

module Make (X : sig
  val print : Format.formatter -> key -> unit
end) =
struct
  module Set = struct
    include Raw_set
    module Elt = X

    let [@ocamlformat "disable"] print ppf s =
      let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" Elt.print e) s in
      Format.fprintf ppf "@[<1>{@[<1>%a@ @]}@]" elts s

    let to_string s = Format.asprintf "%a" print s
  end

  module Map = struct
    include Raw_map
    module Key = X

    type nonrec key = key

    module Set = Set

    let [@ocamlformat "disable"] print_debug print_datum ppf t =
      let rec pp ppf t =
        match t with
        | Empty -> Format.pp_print_string ppf "()"
        | Leaf (k, v) -> Format.fprintf ppf "@[<hv 1>(%x@ %a)@]" k print_datum v
        | Branch (pbit, l, r) ->
          Format.fprintf ppf "@[<hv 1>(branch@ %x@ %a@ %a)@]" pbit pp l pp r
      in
      pp ppf t

    let disjoint_union ?eq ?print t1 t2 =
      ignore print;
      Raw_map.disjoint_union ~print:Key.print ?eq t1 t2

    let [@ocamlformat "disable"] print print_datum ppf t =
      if is_empty t then
        Format.fprintf ppf "{}"
      else
        Format.fprintf ppf "@[<hov 1>{%a}@]"
          (Format.pp_print_list ~pp_sep:Format.pp_print_space
             (fun ppf (key, datum) ->
                Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
                  Key.print key print_datum datum))
          (bindings t)
  end
end
[@@inline always]
