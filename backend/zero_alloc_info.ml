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

module Component_value = struct
  type t = int

  let of_int i =
    if i lsr 2 = 0
    then i
    else
      Misc.fatal_error
        "[Zero_alloc_info.Component_value.of_int]: value outside range 0-3"

  let to_int i = i
end

module Value = struct
  (* CR-someday wsturgeon for wsturgeon: nonzero by refinement? *)
  type t = int

  let encode
      ~(encode_component : 'a -> Component_value.t)
      (components : 'a Zero_alloc_utils.Components.t) : t option =
    let c =
      (encode_component components.nor)
      lor (encode_component components.exn lsl 2)
      lor (encode_component components.div lsl 4)
    in
    if c = 0 then None else Some c

  let decode
      ~(decode_component : Component_value.t -> 'a)
      (i : t) : 'a Zero_alloc_utils.Components.t =
    if i land 0b111111 = 0
    then
      Misc.fatal_error "Zero_alloc_checker unexpected 0 encoding"
    else
      { nor = decode_component (i land 3);
        exn = decode_component ((i lsr 2) land 3);
        div = decode_component ((i lsr 4) land 3)
      }
end

let get_value (t : t) s = String.Map.find_opt s t.zero_alloc

let set_value (t : t) s (v : Value.t) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    Some new_
  in
  t.zero_alloc <- String.Map.update s (f v) t.zero_alloc

let iter ~f t = String.Map.iter f t.zero_alloc

module Raw = struct
  type entries = (string * int) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let print t =
    let print (name, v) = Printf.printf "\t\t%s = %#x\n" name v in
    (* CR gyorsh: move encode/decode here somehow for noalloc *)
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
