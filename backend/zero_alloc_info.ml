[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module String = Misc.Stdlib.String

module Call_outcomes = struct
  type t = int

  let normal_return = 1

  let exceptional_return = 2

  let diverging = 4

  let all = 7
end

module Allocation_sources = struct
  type t =
    | Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments of
        Call_outcomes.t Numbers.Int.Map.t

  let unknown = Allocation_other_than_calling_arguments

  let argument ~lowered_index ~outcomes =
    No_allocation_other_than_calling_arguments
      (Numbers.Int.Map.singleton lowered_index outcomes)

  let is_empty : t -> bool = function
    | Allocation_other_than_calling_arguments -> false
    | No_allocation_other_than_calling_arguments arguments_called ->
      Numbers.Int.Map.is_empty arguments_called

  let compare t1 t2 = match t1, t2 with
    | ( Allocation_other_than_calling_arguments,
        Allocation_other_than_calling_arguments ) ->
      0
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      Numbers.Int.Map.compare Int.compare a1 a2
    | ( Allocation_other_than_calling_arguments,
        No_allocation_other_than_calling_arguments _ ) ->
      1
    | ( No_allocation_other_than_calling_arguments _,
        Allocation_other_than_calling_arguments ) ->
      -1

  let join t1 t2 = match t1, t2 with
    | Allocation_other_than_calling_arguments, _
    | _, Allocation_other_than_calling_arguments
      -> Allocation_other_than_calling_arguments
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      No_allocation_other_than_calling_arguments
        (Numbers.Int.Map.union (fun _ lhs rhs -> Some (lhs lor rhs)) a1 a2)

  let lessequal t1 t2 = compare (join t1 t2) t2 = 0

  let rename_arguments t ~f =
    match t with
    | Allocation_other_than_calling_arguments ->
      Allocation_other_than_calling_arguments
    | No_allocation_other_than_calling_arguments arguments_called ->
      Numbers.Int.Map.fold
        (fun callee_argument outcomes renamed ->
          match f callee_argument with
          | None -> Allocation_other_than_calling_arguments
          | Some caller_argument ->
            join renamed (argument ~lowered_index:caller_argument ~outcomes))
        arguments_called
        (No_allocation_other_than_calling_arguments Numbers.Int.Map.empty)

  let meet t1 t2 = match t1, t2 with
    | ( Allocation_other_than_calling_arguments,
        Allocation_other_than_calling_arguments ) ->
      Allocation_other_than_calling_arguments
    | ( No_allocation_other_than_calling_arguments a1,
        No_allocation_other_than_calling_arguments a2 ) ->
      No_allocation_other_than_calling_arguments
        ( Numbers.Int.Map.merge
            ( fun _ lhs rhs ->
                match lhs, rhs with
                | Some lhs, Some rhs ->
                  let outcomes = lhs land rhs in
                  if outcomes = 0 then None else Some outcomes
                | None, _ | _, None -> None )
            a1 a2 )
    | ( No_allocation_other_than_calling_arguments arguments_called,
        Allocation_other_than_calling_arguments )
    | ( Allocation_other_than_calling_arguments,
        No_allocation_other_than_calling_arguments arguments_called )
      -> No_allocation_other_than_calling_arguments arguments_called

  let print ppf t =
    let print_paths ppf (argument, outcomes) =
      let paths = [] in
      let paths =
        if outcomes land Call_outcomes.diverging <> 0
        then "diverging" :: paths
        else paths
      in
      let paths =
        if outcomes land Call_outcomes.exceptional_return <> 0
        then "exceptional" :: paths
        else paths
      in
      let paths =
        if outcomes land Call_outcomes.normal_return <> 0
        then "normal" :: paths
        else paths
      in
      Format.fprintf ppf "argument #%d on %a paths" (argument + 1)
        ( Format.pp_print_list
            ~pp_sep:(fun ppf () -> Format.fprintf ppf "/")
            Format.pp_print_string )
        paths
    in
    match t with
    | Allocation_other_than_calling_arguments ->
      Format.fprintf ppf "Allocation_other_than_calling_arguments"
    | No_allocation_other_than_calling_arguments arguments_called ->
      Format.fprintf ppf
        "No_allocation_other_than_calling_arguments {@[<hov>%a@]}"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
           print_paths)
        (Numbers.Int.Map.bindings arguments_called)
end

type component =
  | Top of Allocation_sources.t
  | Safe
  | Bot

type components =
  { nor : component;
    exn : component;
    div : component
  }

type value =
  { components : int;
    arguments_that_may_allocate : (int * int) list
  }

type t = { mutable zero_alloc : value String.Map.t }

let create () = { zero_alloc = String.Map.empty }

let reset t = t.zero_alloc <- String.Map.empty

let merge src ~into:dst =
  let join key _ _ = Misc.fatal_errorf "Unexpected merge %s" key in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc

(* Tag 0: [Top Allocation_other_than_calling_arguments]
 * Tag 1: [Safe]
 * Tag 2: [Bot]
 * Tag 3: [Top (No_allocation_other_than_calling_arguments ...)] *)
let encode_component index component (components, calls) =
  let with_tag tag = components lor (tag lsl (2 * index)) in
  match component with
  | Top Allocation_other_than_calling_arguments -> with_tag 0, calls
  | Safe -> with_tag 1, calls
  | Bot -> with_tag 2, calls
  | Top (No_allocation_other_than_calling_arguments arguments_called) ->
    let calls =
      Numbers.Int.Map.fold
        (fun argument outcomes calls ->
          Numbers.Int.Map.update argument
            (fun previous ->
              Some
                (Option.value previous ~default:0 lor (outcomes lsl (3 * index))))
            calls)
        arguments_called calls
    in
    with_tag 3, calls

let encode { nor; exn; div } =
  let components, calls =
    (0, Numbers.Int.Map.empty) |> encode_component 0 nor
    |> encode_component 1 exn |> encode_component 2 div
  in
  if components = 0
  then None
  else
    Some
      { components;
        arguments_that_may_allocate = Numbers.Int.Map.bindings calls
      }

let decode_component index value =
  match (value.components lsr (2 * index)) land 3 with
  | 0 -> Top Allocation_other_than_calling_arguments
  | 1 -> Safe
  | 2 -> Bot
  | 3 ->
    let arguments_called =
      List.fold_left
        (fun calls (argument, outcomes) ->
          let outcomes = (outcomes lsr (3 * index)) land Call_outcomes.all in
          if outcomes = 0
          then calls
          else Numbers.Int.Map.add argument outcomes calls)
        Numbers.Int.Map.empty value.arguments_that_may_allocate
    in
    Top (No_allocation_other_than_calling_arguments arguments_called)
  | n -> Misc.fatal_errorf "Zero_alloc_info cannot decode component %d" n

let decode value =
  if value.components = 0
  then Misc.fatal_error "Zero_alloc_info unexpected empty encoding";
  { nor = decode_component 0 value;
    exn = decode_component 1 value;
    div = decode_component 2 value
  }

let get_value (t : t) s = String.Map.find_opt s t.zero_alloc

let set_value (t : t) s (v : value) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    Some new_
  in
  t.zero_alloc <- String.Map.update s (f v) t.zero_alloc

let iter ~f t = String.Map.iter f t.zero_alloc

module Raw = struct
  type entries = (string * value) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let print (name, v) =
      Printf.printf "\t\t%s = %#x" name v.components;
      List.iter
        (fun (argument, outcomes) ->
          Printf.printf " arg#%d=%#x" (argument + 1) outcomes)
        v.arguments_that_may_allocate;
      print_newline ()
    in
    Printf.printf "Function summaries for static checks:\n";
    List.iter print t

  let print = function None -> () | Some t -> print t
end

let to_raw (t : t) : Raw.t =
  if String.Map.is_empty t.zero_alloc
  then None
  else Some (String.Map.bindings t.zero_alloc)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t -> { zero_alloc = Raw.entries_to_map t }
