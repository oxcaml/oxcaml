[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module String = Misc.Stdlib.String

module Call_outcomes = struct
  type t = int

  let normal_return = 1

  let exceptional_return = 2

  let diverging = 4

  let all = 7

  let is_subset ~sub ~super = sub land super = sub
end

module Tag = struct
  type t =
    | N
    | E
    | D

  let compare : t -> t -> int = fun t1 t2 -> Stdlib.compare t1 t2

  let print = function N -> "nor" | E -> "exn" | D -> "div"

  let all = [N; E; D]

  let outcome = function
    | N -> Call_outcomes.normal_return
    | E -> Call_outcomes.exceptional_return
    | D -> Call_outcomes.diverging

  let position = function N -> 0 | E -> 1 | D -> 2
end

module Call_outcomes_by_component = struct
  type t = int

  let empty = 0

  let add t name outcomes =
    t lor (outcomes lsl (3 * Tag.position name))

  let get t name =
    (t lsr (3 * Tag.position name)) land Call_outcomes.all
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
    arguments_that_may_allocate : (int * Call_outcomes_by_component.t) list
  }

type t =
  { mutable zero_alloc : value String.Map.t;
    mutable aliases : string String.Map.t
  }

let create () = { zero_alloc = String.Map.empty; aliases = String.Map.empty }

let reset t =
  t.zero_alloc <- String.Map.empty;
  t.aliases <- String.Map.empty

let merge src ~into:dst =
  let join key _ _ = Misc.fatal_errorf "Unexpected merge %s" key in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc;
  dst.aliases <- String.Map.union join dst.aliases src.aliases

(* Tag 0: [Top Allocation_other_than_calling_arguments]
 * Tag 1: [Safe]
 * Tag 2: [Bot]
 * Tag 3: [Top (No_allocation_other_than_calling_arguments ...)] *)
let encode_component name component (components, calls) =
  let with_tag tag =
    components lor (tag lsl (2 * Tag.position name))
  in
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
                (Call_outcomes_by_component.add
                   (Option.value previous
                      ~default:Call_outcomes_by_component.empty)
                   name outcomes))
            calls)
        arguments_called calls
    in
    with_tag 3, calls

let encode { nor; exn; div } =
  let components, calls =
    (0, Numbers.Int.Map.empty)
    |> encode_component N nor
    |> encode_component E exn
    |> encode_component D div
  in
  if components = 0
  then None
  else
    Some
      { components;
        arguments_that_may_allocate = Numbers.Int.Map.bindings calls
      }

let decode_component name value =
  match (value.components lsr (2 * Tag.position name)) land 3 with
  | 0 -> Top Allocation_other_than_calling_arguments
  | 1 -> Safe
  | 2 -> Bot
  | 3 ->
    let arguments_called =
      List.fold_left
        (fun calls (argument, outcomes_by_component) ->
          let outcomes =
            Call_outcomes_by_component.get outcomes_by_component name
          in
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
  { nor = decode_component N value;
    exn = decode_component E value;
    div = decode_component D value
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

let add_alias t ~symbol ~code_symbol =
  t.aliases <- String.Map.add symbol code_symbol t.aliases

let resolve_alias t symbol =
  Option.value (String.Map.find_opt symbol t.aliases) ~default:symbol

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
  let exported =
    String.Map.fold
      (fun symbol code_symbol exported ->
        match get_value t code_symbol with
        | Some value -> String.Map.add symbol value exported
        | None -> exported)
      t.aliases t.zero_alloc
  in
  if String.Map.is_empty exported
  then None
  else Some (String.Map.bindings exported)

let of_raw (t : Raw.t) : t =
  match t with
  | None -> create ()
  | Some t -> { zero_alloc = Raw.entries_to_map t; aliases = String.Map.empty }
