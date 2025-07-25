open Owee_buf

let read_length t ~is_64bit =
  if is_64bit then Int64.to_int (Read.u64 t) else Read.u32 t

let read_addr t ~address_size =
  if address_size = 8 then  (Read.u64 t) else Int64.of_int (Read.u32 t)

type value =
    Int of Int64.t
  | RelAddress of Int64.t
  | String of string
  | Present

module AttrMap = Owee_attribute.Map

type cu_config = {
  pointers_to_other_sections: Owee_debug_line.pointers_to_other_sections option;
  is_64bit: bool;
  address_size: int;
  die_base_offset: int;
  abbrevs : Owee_debug_abbrev.t;
  debug_abbrev_offset: int;
}

let read_string ~config form t =
  let extract_from = function
    | None -> None
    | Some buf ->
      let offset = if config.is_64bit then Int64.to_int (Read.u64 t) else Read.u32 t in
      let cursor = cursor buf ~at:offset in
      Read.zero_string cursor ()
  in
  match form with
  | `line_strp ->
    Option.bind config.pointers_to_other_sections
      (fun p -> extract_from p.Owee_debug_line.debug_line_str)
  | `strp ->
    Option.bind config.pointers_to_other_sections
      (fun p -> extract_from p.Owee_debug_line.debug_str)
  | `string -> Read.zero_string t ()
  | _ -> None

let consume_attributes ~config attributes chunk =
  List.fold_left
    (fun acc Owee_debug_abbrev.{ name; form; _ } ->
       (* These forms are the most generic and common ones.
          Directories only need `line_strp, `strp or `string.  *)
       match form with
       | `ref1 ->
         let v = Read.u8 chunk in
         AttrMap.add name (RelAddress (Int64.of_int v)) acc
       | `ref2 ->
         let v = Read.u16 chunk in
         AttrMap.add name (RelAddress (Int64.of_int v)) acc
       | `ref4 ->
         let v = Read.u32 chunk in
         AttrMap.add name (RelAddress (Int64.of_int v)) acc
       | `ref_udata ->
         let v = Read.uleb128 chunk in
         AttrMap.add name (RelAddress (Int64.of_int v)) acc

       | `sec_offset ->
         let v = read_length ~is_64bit:config.is_64bit chunk in
         AttrMap.add name (RelAddress (Int64.of_int v)) acc

       | `data1 ->
         let v = Read.u8 chunk in
         AttrMap.add name (Int (Int64.of_int v)) acc
       | `data2 ->
         let v = Read.u16 chunk in
         AttrMap.add name (Int (Int64.of_int v)) acc
       | `data4 ->
         let v = Read.u32 chunk in
         AttrMap.add name (Int (Int64.of_int v)) acc
       | `udata ->
         let v = Read.uleb128 chunk in
         AttrMap.add name (Int (Int64.of_int v)) acc
       | `addr ->
         let v = read_addr ~address_size:config.address_size chunk in
         AttrMap.add name (Int v) acc
       | `flag_present ->
         AttrMap.add name Present acc

       | (`line_strp | `strp | `string) as form ->
         (match read_string ~config form chunk with
          | None -> invalid_format "Unterminated filename"
          | Some x ->
            AttrMap.add name (String x) acc
         )
       | _ ->
         Owee_form.skip form chunk ~address_size:config.address_size
           ~is_64bit:config.is_64bit;
         acc
    )
    AttrMap.empty
    attributes

type t = {
  tag: Owee_tag.t;
  abbrev : u128;
  offset : s128;
  attributes: value AttrMap.t;
  children: t list;
}

module TreeFromDwarf : sig
  (* Helper module to help reconstructing the hierarchy of dies from DWARF's
     representation. *)
  type die = t
  type t

  (* Create an empty hierarchy *)
  val empty : t

  (* Extract the root of the tree. Will fail if the tree is not well-reconstructed. *)
  val extract_root : t -> die

  (* In the following a "level" is a list of nodes that are at the same depth in the
     hierarchy.*)

  (* Add the die to the last level of the tree.
     Can be understood as adding a new child to the last node
     of the previous level.
  *)
  val add_die_to_last_level : die -> t -> t

  (* Collapse one level into its parent level.
     Can be understood as setting the child of the last node
     of the previous level to be the dies in the last level and
     to pop the last level.
  *)
  val collapse_one_level_exn : t -> t

  (* Repeats [add_die_to_last_level] until it's not possible anymore *)
  val collapse_all_levels : t -> t

  (* Open a new level *)
  val open_level : t -> t

end = struct
  type die = t

  (* We reconstruct the tree structure by keeping
     track of a stack of all DIEs in flight.
     The DIEs at level i+1 in the stack are
     the children of the last die at level i in the stack. *)
  module Level : sig
    type t
    val empty : t
    val add : t -> die -> t
    val extract_root : t -> die
    val collapse : t -> children:t -> t
  end
  =  struct
    type t = die list

    let empty = []

    let add t die = die :: t

    let extract_root t =
      match t with
      | [ root ] -> root
      | _ -> invalid_format "DIE t is weird"

    let collapse t ~children =
      match t with
      | parent::prevs ->
        { parent with children = List.rev children} :: prevs
      | _ ->
        invalid_format "DIE t is weird"
  end

  type nonrec t = Level.t list

  let empty = [ Level.empty ]

  let extract_root = function
    | [ level ] -> Level.extract_root level
    | _ -> invalid_format "Incorrect DIE tree -- multiple master dies"

  let add_die_to_last_level die t =
    match t with
    | hd :: tl -> Level.add hd die :: tl
    | _ -> invalid_format "DIE t is weird"

  let open_level t = Level.empty :: t

  let collapse_one_level t =
    match t with
    | children :: parents :: tl ->
      Some ((Level.collapse parents ~children)::tl)
    | _ -> None

  let collapse_one_level_exn t =
    match collapse_one_level t with
    | Some t -> t
    | None -> invalid_format "DIE tree is weird"

  let rec collapse_all_levels t =
    match collapse_one_level t with
    | Some t -> collapse_all_levels t
    | None -> t
end


let read_one_die_no_child ~config chunk =
  let die_offset = config.die_base_offset + chunk.position in

  let abbrev_code = Read.uleb128 chunk in
  if abbrev_code = 0 then None
  else
  match Owee_debug_abbrev.get
          ~offset:config.debug_abbrev_offset
          ~code:abbrev_code config.abbrevs
  with
  | None -> invalid_format "unknown abbrev entry"
  | Some { attributes; has_child;  tag; _ } ->
    let attributes =
      consume_attributes
        ~config
        attributes chunk
    in
    Some (has_child,
          { offset=die_offset; abbrev=abbrev_code;tag; attributes; children = [] })

(* DIEs have a tree structure.
   The preorder traversal of the tree is stored in DWARF information.
*)
let rec read_one_die ~with_children ~config tree chunk =
  if at_end chunk then
    (* Once we can't read any more dies we need to close the tree:
       we collapse all dies of the stack.  *)
    TreeFromDwarf.collapse_all_levels tree
  else
    match read_one_die_no_child ~config chunk with
    | None -> begin
        (* We read a null value, meaning that we need to close
           the current sequence of siblings. In practice that means
           collapsing the last level of the stack. *)
        let tree = TreeFromDwarf.collapse_one_level_exn tree in
        (* The next DIE might be a sibling of the previous DIE. *)
        read_one_die ~with_children:true ~config tree chunk
      end
    | Some (has_child, die) ->
      let tree = TreeFromDwarf.add_die_to_last_level die tree in
      if has_child then
        (* The DIE we just read has some children -- instantiate
           a new list of children. *)
        if with_children then
          read_one_die ~with_children:true ~config (TreeFromDwarf.open_level tree) chunk
        else tree
      else
        (* The next DIE will be a sibling of the current DIE. *)
        read_one_die ~with_children:true ~config tree chunk
let read_cu_header ~abbrevs ~pointers_to_other_sections cursor =
  let total_length = Read.u32 cursor in
  let is_64bit     = total_length = 0xFFFF_FFFF in
  let total_length =
    if is_64bit then Read.u64 cursor else Int64.of_int total_length
  in
  let die_base_offset = cursor.position in
  let chunk = sub cursor (Int64.to_int total_length) in
  let version = Read.u16 chunk in
  assert_format (version >= 2 && version <= 5)
    (Printf.sprintf "unknown .debug_info version: \
                     Got %d expected a version between 2 and 5"
       version);
  let address_size, debug_abbrev_offset =
    if version >= 5 then begin
      let unit_type = Read.u8 chunk in
      let address_size = Read.u8 chunk in
      let debug_abbrev_offset = read_length chunk ~is_64bit in
      begin
        match unit_type with
        | 0x01 (* DW_UT_compile *)
        | 0x03 (* DW_UT_partial *) -> ()
        | 0x04 (* DW_UT_skeleton *)
        | 0x05 (* DW_UT_split_compile *) ->
          let dwo_unit = Read.u64 chunk in
          ignore (dwo_unit : u64)
        | 0x02 (* DW_UT_type *)
        | 0x06 (* DW_UT_split_type *) ->
          let type_signature = Read.u64 chunk in
          let type_offset = read_length chunk ~is_64bit in
          ignore (type_signature : u64);
          ignore (type_offset : int)
        | 0x80 (* DW_UT_lo_user *)
        | 0xff (* DW_UT_hi_user *) -> ()
        | _ -> ()
      end;
      (address_size, debug_abbrev_offset)
    end
    else
      let debug_abbrev_offset = read_length chunk ~is_64bit in
      let address_size = Read.u8 chunk in
      (address_size, debug_abbrev_offset)
  in
  {
    pointers_to_other_sections;
    abbrevs;
    debug_abbrev_offset;
    is_64bit;
    address_size;
    die_base_offset
  }, chunk

let read_single_cu ~abbrevs ~pointers_to_other_sections cursor  =
  let config, chunk = read_cu_header ~abbrevs ~pointers_to_other_sections cursor in
  match read_one_die_no_child ~config chunk with
  | Some (_, c) -> Some c
  | None -> None

let read_compilation_unit ~with_children ~abbrevs ~pointers_to_other_sections cursor =
  let config, chunk = read_cu_header ~abbrevs ~pointers_to_other_sections cursor in
  let tree =
    read_one_die ~with_children ~config TreeFromDwarf.empty chunk
  in
  TreeFromDwarf.extract_root tree

let read ~with_children ~abbrevs ~pointers_to_other_sections cursor =
  if at_end cursor
  then None
  else Some (read_compilation_unit
               ~with_children
               ~pointers_to_other_sections
               ~abbrevs
               cursor)

let comp_dir {attributes; _} =
  AttrMap.find_opt Owee_attribute.Comp_dir attributes
  |> Option.map (function | String s -> s | _ -> assert false)

let debug_line_offset {attributes; _} =
  AttrMap.find_opt Owee_attribute.Stmt_list attributes
  |> Option.map (function | RelAddress s -> s | Int s -> s | _ -> assert false)

let read_all ~with_children ~abbrevs sections map =
  match Owee_elf.find_section sections ".debug_info" with
  | None -> []
  | Some section ->
    let body = Owee_buf.cursor (Owee_elf.section_body map section) in
    let pointers_to_other_sections =
      Some (Owee_elf.debug_line_pointers map sections)
    in
    let rec aux acc =
      match
        read
          ~with_children
          ~pointers_to_other_sections
          ~abbrevs
          body
      with
      | None -> acc
      | Some die -> aux (die :: acc)
    in
    aux []
