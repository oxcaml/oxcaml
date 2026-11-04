(** val assoc_default : 'a1 -> int -> (int * 'a1) list -> 'a1 **)

let assoc_default dflt id l =
  match List.assoc_opt id l with Some c -> c | None -> dflt

(** val count_occurrences : ('a1 -> bool) -> 'a1 list -> int **)

let count_occurrences same l =
  List.fold_left (fun n x -> if same x then n + 1 else n) 0 l

(** val sort_uniq : int list -> int list **)

let sort_uniq = List.sort_uniq Int.compare

(** val length_at_most : int -> 'a1 list -> bool **)

let rec length_at_most limit = function
  | [] -> 0 <= limit
  | _ :: rest -> if 0 < limit then length_at_most (limit - 1) rest else false

(** val map_opt : ('a1 -> 'a2 option) -> 'a1 list -> 'a2 list option **)

let rec map_opt f = function
  | [] -> Some []
  | x :: l0 -> (
    match f x with
    | Some y -> (
      match map_opt f l0 with Some l1 -> Some (y :: l1) | None -> None)
    | None -> None)

(** val filter_map_opt : ('a1 -> 'a2 option option) -> 'a1 list -> 'a2 list
    option **)

let rec filter_map_opt f = function
  | [] -> Some []
  | x :: l0 -> (
    match f x with
    | Some y -> (
      match filter_map_opt f l0 with
      | Some l1 -> Some (match y with Some y0 -> y0 :: l1 | None -> l1)
      | None -> None)
    | None -> None)

(** val nth_int : 'a1 list -> int -> 'a1 option **)

let rec nth_int l i =
  match l with
  | [] -> None
  | x :: l0 -> if Int.equal i 0 then Some x else nth_int l0 (i - 1)

(** val same_length : 'a1 list -> 'a2 list -> bool **)

let rec same_length l1 l2 =
  match l1 with
  | [] -> ( match l2 with [] -> true | _ :: _ -> false)
  | _ :: l3 -> ( match l2 with [] -> false | _ :: l4 -> same_length l3 l4)
