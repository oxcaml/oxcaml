[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
module String = Misc.Stdlib.String

type t = { mutable zero_alloc : int String.Map.t }

let create () = { zero_alloc = String.Map.empty }

let reset t = t.zero_alloc <- String.Map.empty

let merge src ~into:dst =
  let join key b1 b2 =
    Misc.fatal_errorf "Unexpected merge %s %d %d" key b1 b2
  in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc

type value = int

type component =
  | Top of { every_allocation_comes_from_calling_an_argument : bool }
  | Safe
  | Bot

type components =
  { nor : component;
    exn : component;
    div : component
  }

let encode_component index = function
  | Top { every_allocation_comes_from_calling_an_argument } ->
    if every_allocation_comes_from_calling_an_argument then 1 lsl (6 + index) else 0
  | Safe -> 1 lsl (2 * index)
  | Bot -> 2 lsl (2 * index)

let encode { nor; exn; div } =
  let value =
    encode_component 0 nor lor encode_component 1 exn lor encode_component 2 div
  in
  if value = 0 then None else Some value

let decode_component index value =
  match (value lsr (2 * index)) land 3 with
  | 0 ->
    Top
      { every_allocation_comes_from_calling_an_argument = value land (1 lsl (6 + index)) <> 0
      }
  | 1 -> Safe
  | 2 -> Bot
  | n -> Misc.fatal_errorf "Zero_alloc_info cannot decode component %d" n

let decode value =
  if value = 0 then Misc.fatal_error "Zero_alloc_info unexpected 0 encoding";
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
  type entries = (string * int) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let print (name, v) = Printf.printf "\t\t%s = %#x\n" name v in
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
