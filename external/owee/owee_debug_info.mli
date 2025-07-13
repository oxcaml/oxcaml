open Owee_buf

type value =
    Int of Int64.t
  | RelAddress of Int64.t
  | String of string
  | Present

type t = {
  tag: Owee_tag.t;
  abbrev : u128;
  offset : s128;
  attributes: value Owee_attribute.Map.t;
  children: t list
}


val comp_dir : t -> string option
val debug_line_offset : t -> u64 option

(* CR-someday poechsel: [read_all] could return a Seq.t and read the DIEs
   incrementally. *)
val read_all
  : with_children:bool
  -> abbrevs:Owee_debug_abbrev.t
  -> Owee_elf.section array
  -> Owee_buf.t
  -> t list

val read_single_cu :
  abbrevs:Owee_debug_abbrev.t
  -> pointers_to_other_sections:Owee_debug_line.pointers_to_other_sections option
  -> Owee_buf.cursor -> t option
