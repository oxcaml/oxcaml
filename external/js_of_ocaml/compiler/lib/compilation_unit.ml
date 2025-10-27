(* CR-soon jvanburen: it's horrifying (and unsafe) to have this not shared with oxcaml. it
   needs to match what the compiler generates (it should just be a string) *)

type global_module_name = private
  { head : string
  ; args : Obj.t
  }
[@@ocaml.warning "-unused-field"]

type full = private
  | With_prefix of
      { name : string
      ; for_pack_prefix : Obj.t
      }
  | Global of global_module_name

type t = Obj.t

let name_as_string t =
  let tag = Obj.tag t in
  assert (tag < 2 || tag = Obj.string_tag);
  if tag = Obj.string_tag
  then Sys.opaque_identity (Obj.obj t : string)
  else
    let full = Sys.opaque_identity (Obj.obj t : full) in
    match full with
    | With_prefix { name; _ } -> name
    | Global { head; _ } -> head
