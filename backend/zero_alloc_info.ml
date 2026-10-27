[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
open Zero_alloc_utils
module String = Misc.Stdlib.String

module Encoded = struct
  let pack ~bits_per_component (components : int Components.t) =
    components.nor
    lor (components.exn lsl bits_per_component)
    lor (components.div lsl (2 * bits_per_component))

  let unpack ~bits_per_component packed : int Components.t =
    let mask = (1 lsl bits_per_component) - 1 in
    { nor = packed land mask;
      exn = (packed lsr bits_per_component) land mask;
      div = (packed lsr (2 * bits_per_component)) land mask
    }

  module Component = struct
    type t = int list Allocation_sources.t Component.t

    let pack_relevant_components (relevant : bool Components.t) =
      pack ~bits_per_component:1
        (Components.map relevant ~f:(fun _tag -> Bool.to_int))

    let unpack_relevant_components packed : bool Components.t =
      Components.map (unpack ~bits_per_component:1 packed) ~f:(fun _tag bit ->
          bit <> 0)

    let relevant_components_bits = 3

    let pack_callback
        ({ arity; relevant_components } :
          Relevant_components_by_argument_index.callback) =
      pack_relevant_components relevant_components
      lor (arity lsl relevant_components_bits)

    let unpack_callback packed : Relevant_components_by_argument_index.callback
        =
      { arity = packed lsr relevant_components_bits;
        relevant_components =
          unpack_relevant_components
            (packed land ((1 lsl relevant_components_bits) - 1))
      }

    let encode_arguments
        (relevant_components_by_argument_index :
          Relevant_components_by_argument_index.t) =
      match
        (* Find the greatest key, i.e. the index of the last callback: *)
        Numbers.Int.Map.max_binding_opt relevant_components_by_argument_index
      with
      | None -> []
      | Some (last_callback_argument_index, _) ->
        List.init (last_callback_argument_index + 1) (fun argument_index ->
            match
              Numbers.Int.Map.find_opt argument_index
                relevant_components_by_argument_index
            with
            | None -> 0
            | Some callback -> pack_callback callback)

    let decode_arguments arguments : Relevant_components_by_argument_index.t =
      arguments
      |> List.mapi (fun i bits -> i, bits)
      |> List.filter (fun (_, bits) -> bits <> 0)
      |> List.map (fun (i, bits) -> i, unpack_callback bits)
      |> Numbers.Int.Map.of_list

    let encode = Component.map ~f:(Allocation_sources.map ~f:encode_arguments)

    let decode = Component.map ~f:(Allocation_sources.map ~f:decode_arguments)

    let compare =
      Component.compare
        ~compare_top:
          (Allocation_sources.compare
             ~compare_args_only:(List.compare Int.compare))
  end

  module Components = struct
    type t =
      | Unconditional of int
      | Conditional of Component.t Components.t

    let unconditional_tag : Component.t -> int option = function
      | Bot -> Some 0
      | Safe -> Some 1
      | Top (No_allocation_other_than_calling_arguments _) -> None
      | Top Allocation_other_than_calling_arguments -> Some 3

    let component_of_unconditional_tag : int -> Component.t = function
      | 0 -> Bot
      | 1 -> Safe
      | 3 -> Top Allocation_other_than_calling_arguments
      | tag ->
        Misc.fatal_errorf
          "[Zero_alloc_info.Encoded.Components.decode]: invalid tag %d" tag

    let encode ~(encode_component : 'a -> Component.t)
        (components : 'a Components.t) : t =
      let encoded : Component.t Components.t =
        Components.map components ~f:(fun _tag component ->
            encode_component component)
      in
      match
        ( unconditional_tag encoded.nor,
          unconditional_tag encoded.exn,
          unconditional_tag encoded.div )
      with
      | Some nor, Some exn, Some div ->
        Unconditional (pack ~bits_per_component:2 { nor; exn; div })
      | _, _, _ -> Conditional encoded

    let decode ~(decode_component : Component.t -> 'a) (t : t) :
        'a Zero_alloc_utils.Components.t =
      let encoded : Component.t Components.t =
        match t with
        | Unconditional tags ->
          Components.map (unpack ~bits_per_component:2 tags) ~f:(fun _tag ->
              component_of_unconditional_tag)
        | Conditional encoded -> encoded
      in
      Components.map encoded ~f:(fun _tag component ->
          decode_component component)

    let all_top : t =
      Zero_alloc_utils.(
        Components.create (fun _ignore_tag ->
            Component.Top
              Allocation_sources.Allocation_other_than_calling_arguments))
      |> encode ~encode_component:Component.encode
    [@@ocaml.warning "-44"]

    let ( = ) (t1 : t) (t2 : t) =
      match t1, t2 with
      | Unconditional tags1, Unconditional tags2 -> Int.equal tags1 tags2
      | Conditional encoded1, Conditional encoded2 ->
        Int.equal (Components.compare Component.compare encoded1 encoded2) 0
      | Unconditional _, Conditional _ | Conditional _, Unconditional _ -> false
  end
end

let encode components =
  Encoded.Components.encode ~encode_component:Encoded.Component.encode
    components

let decode =
  Encoded.Components.decode ~decode_component:Encoded.Component.decode

type t = { mutable zero_alloc : Encoded.Components.t String.Map.t }

let create () = { zero_alloc = String.Map.empty }

let reset t = t.zero_alloc <- String.Map.empty

let merge src ~into:dst =
  let join key _ _ = Misc.fatal_errorf "Unexpected merge %s" key in
  dst.zero_alloc <- String.Map.union join dst.zero_alloc src.zero_alloc

let get (t : t) s = String.Map.find_opt s t.zero_alloc

let set (t : t) s (v : Encoded.Components.t) =
  let f new_ old =
    if not (Option.is_none old)
    then Misc.fatal_errorf "Value of %s is already set" s;
    Some new_
  in
  t.zero_alloc <- String.Map.update s (f v) t.zero_alloc

let iter ~f t = String.Map.iter f t.zero_alloc

module Raw = struct
  type entries = (string * Encoded.Components.t) list

  type t = entries option

  let entries_to_map (e : entries) =
    List.fold_left (fun acc (k, v) -> String.Map.add k v acc) String.Map.empty e

  let bits ~width n =
    String.init width (fun i ->
        if n land (1 lsl (width - 1 - i)) <> 0 then '1' else '0')

  let describe_component : Encoded.Component.t -> string = function
    | Bot -> "bot"
    | Safe -> "safe"
    | Top Allocation_other_than_calling_arguments -> "top"
    | Top (No_allocation_other_than_calling_arguments arguments) ->
      let describe_argument packed =
        Printf.sprintf "%s@%d"
          (bits ~width:Encoded.Component.relevant_components_bits packed)
          (packed lsr Encoded.Component.relevant_components_bits)
      in
      "[" ^ String.concat "; " (List.map describe_argument arguments) ^ "]"

  let print t =
    let print (name, (components : Encoded.Components.t)) =
      match components with
      | Unconditional tags -> Printf.printf "\t\t%s = %#x\n" name tags
      | Conditional { nor; exn; div } ->
        Printf.printf "\t\t%s = nor=%s exn=%s div=%s\n" name
          (describe_component nor) (describe_component exn)
          (describe_component div)
    in
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
