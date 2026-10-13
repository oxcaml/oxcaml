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
    { arguments_that_may_allocate : Call_outcomes.t Numbers.Int.Map.t;
      anything_else_may_allocate : bool
    }

  let unknown =
    { arguments_that_may_allocate = Numbers.Int.Map.empty;
      anything_else_may_allocate = true
    }

  let argument ~lowered_index ~outcomes =
    { arguments_that_may_allocate =
        Numbers.Int.Map.singleton lowered_index outcomes;
      anything_else_may_allocate = false
    }

  let is_empty t =
    (not t.anything_else_may_allocate)
    && Numbers.Int.Map.is_empty t.arguments_that_may_allocate

  let only_allocates_on_argument_outcomes t ~outcomes =
    (not t.anything_else_may_allocate)
    && Numbers.Int.Map.for_all
         (fun _ required -> required land outcomes = required)
         t.arguments_that_may_allocate

  let compare t1 t2 =
    let c =
      Bool.compare t1.anything_else_may_allocate t2.anything_else_may_allocate
    in
    if c <> 0
    then c
    else
      Numbers.Int.Map.compare Int.compare t1.arguments_that_may_allocate
        t2.arguments_that_may_allocate

  let lessequal t1 t2 =
    t2.anything_else_may_allocate
    || (not t1.anything_else_may_allocate)
       && Numbers.Int.Map.for_all
            (fun argument outcomes ->
              match
                Numbers.Int.Map.find_opt argument t2.arguments_that_may_allocate
              with
              | None -> false
              | Some other -> outcomes land other = outcomes)
            t1.arguments_that_may_allocate

  let join t1 t2 =
    { arguments_that_may_allocate =
        Numbers.Int.Map.union
          (fun _ left right -> Some (left lor right))
          t1.arguments_that_may_allocate t2.arguments_that_may_allocate;
      anything_else_may_allocate =
        t1.anything_else_may_allocate || t2.anything_else_may_allocate
    }

  let meet t1 t2 =
    if t1.anything_else_may_allocate
    then t2
    else if t2.anything_else_may_allocate
    then t1
    else
      { arguments_that_may_allocate =
          Numbers.Int.Map.merge
            (fun _ left right ->
              match left, right with
              | Some left, Some right ->
                let outcomes = left land right in
                if outcomes = 0 then None else Some outcomes
              | None, _ | _, None -> None)
            t1.arguments_that_may_allocate t2.arguments_that_may_allocate;
        anything_else_may_allocate = false
      }

  let print ppf t =
    let print_call ppf (argument, outcomes) =
      Format.fprintf ppf "#%d:%s%s%s" (argument + 1)
        (if outcomes land Call_outcomes.normal_return <> 0 then "N" else "")
        (if outcomes land Call_outcomes.exceptional_return <> 0 then "E" else "")
        (if outcomes land Call_outcomes.diverging <> 0 then "D" else "")
    in
    Format.fprintf ppf "{arguments=[%a]; anything_else=%b}"
      (Format.pp_print_list
         ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
         print_call)
      (Numbers.Int.Map.bindings t.arguments_that_may_allocate)
      t.anything_else_may_allocate
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

let encode_component index component (components, calls) =
  match component with
  | Top sources ->
    let components =
      if sources.anything_else_may_allocate
      then components
      else components lor (1 lsl (6 + index))
    in
    let calls =
      Numbers.Int.Map.fold
        (fun argument outcomes calls ->
          Numbers.Int.Map.update argument
            (fun previous ->
              Some
                (Option.value previous ~default:0 lor (outcomes lsl (3 * index))))
            calls)
        sources.arguments_that_may_allocate calls
    in
    components, calls
  | Safe -> components lor (1 lsl (2 * index)), calls
  | Bot -> components lor (2 lsl (2 * index)), calls

let encode { nor; exn; div } =
  let components, calls =
    (0, Numbers.Int.Map.empty) |> encode_component 0 nor
    |> encode_component 1 exn |> encode_component 2 div
  in
  if components = 0 && Numbers.Int.Map.is_empty calls
  then None
  else
    Some
      { components;
        arguments_that_may_allocate = Numbers.Int.Map.bindings calls
      }

let decode_component index value =
  match (value.components lsr (2 * index)) land 3 with
  | 0 ->
    let arguments_that_may_allocate =
      List.fold_left
        (fun calls (argument, outcomes) ->
          let outcomes = (outcomes lsr (3 * index)) land Call_outcomes.all in
          if outcomes = 0
          then calls
          else Numbers.Int.Map.add argument outcomes calls)
        Numbers.Int.Map.empty value.arguments_that_may_allocate
    in
    Top
      { arguments_that_may_allocate;
        anything_else_may_allocate =
          value.components land (1 lsl (6 + index)) = 0
      }
  | 1 -> Safe
  | 2 -> Bot
  | n -> Misc.fatal_errorf "Zero_alloc_info cannot decode component %d" n

let decode value =
  if value.components = 0 && List.is_empty value.arguments_that_may_allocate
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
