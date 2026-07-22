(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Minimal dependency-free XML reader, tuned to the regular shape of
   [x86-intel.xml] (the Intel Intrinsics Guide data). Same spirit as the
   hand-rolled CSV parser in [simdgen.ml]: it is not a general XML parser, but
   it handles elements, attributes, self-closing tags, text content, comments,
   the [<?xml?>] declaration, and the handful of entities that appear in the
   data. *)

type node =
  { tag : string;
    attrs : (string * string) list;
    children : node list;
    text : string
  }

let attr node name = List.assoc_opt name node.attrs

let children_named node tag =
  List.filter (fun child -> String.equal child.tag tag) node.children

let child_named node tag =
  match children_named node tag with child :: _ -> Some child | [] -> None

(* Decode the small set of XML entities present in the data. *)
let decode_entities s =
  let buf = Buffer.create (String.length s) in
  let n = String.length s in
  let i = ref 0 in
  while !i < n do
    if s.[!i] = '&'
    then (
      let semi = try String.index_from s !i ';' with Not_found -> !i in
      let name = String.sub s (!i + 1) (semi - !i - 1) in
      (match name with
      | "lt" -> Buffer.add_char buf '<'
      | "gt" -> Buffer.add_char buf '>'
      | "amp" -> Buffer.add_char buf '&'
      | "quot" -> Buffer.add_char buf '"'
      | "apos" -> Buffer.add_char buf '\''
      | other when String.length other > 1 && other.[0] = '#' ->
        let code =
          if other.[1] = 'x' || other.[1] = 'X'
          then
            int_of_string ("0x" ^ String.sub other 2 (String.length other - 2))
          else int_of_string (String.sub other 1 (String.length other - 1))
        in
        (* Only ASCII appears in this data. *)
        if code < 128 then Buffer.add_char buf (Char.chr code)
      | other ->
        Buffer.add_char buf '&';
        Buffer.add_string buf other;
        Buffer.add_char buf ';');
      i := semi + 1)
    else (
      Buffer.add_char buf s.[!i];
      incr i)
  done;
  Buffer.contents buf

(* Find the first occurrence of [pat] in [src] at or after [from]. Raises
   [Not_found] if absent. *)
let find_sub src pat from =
  let pl = String.length pat and sl = String.length src in
  let rec go i =
    if i + pl > sl
    then raise Not_found
    else if String.sub src i pl = pat
    then i
    else go (i + 1)
  in
  go from

type parser_state =
  { src : string;
    len : int;
    mutable pos : int
  }

let peek st = if st.pos < st.len then Some st.src.[st.pos] else None

let starts_with st prefix =
  let pl = String.length prefix in
  st.pos + pl <= st.len && String.sub st.src st.pos pl = prefix

let skip_ws st =
  while
    st.pos < st.len
    &&
    match st.src.[st.pos] with
    | ' ' | '\t' | '\n' | '\r' -> true
    | _ -> false
  do
    st.pos <- st.pos + 1
  done

let is_name_char c =
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | ':' | '.' -> true
  | _ -> false

let read_name st =
  let start = st.pos in
  while st.pos < st.len && is_name_char st.src.[st.pos] do
    st.pos <- st.pos + 1
  done;
  String.sub st.src start (st.pos - start)

(* At '<', already known not to be a comment/PI/decl. Parses one element and its
   subtree, returning the node. *)
let rec read_element st =
  assert (st.src.[st.pos] = '<');
  st.pos <- st.pos + 1;
  let tag = read_name st in
  let attrs = read_attrs st [] in
  skip_ws st;
  if starts_with st "/>"
  then (
    st.pos <- st.pos + 2;
    { tag; attrs; children = []; text = "" })
  else (
    (* consume '>' *)
    assert (st.src.[st.pos] = '>');
    st.pos <- st.pos + 1;
    let children, text = read_content st tag in
    { tag; attrs; children; text })

and read_attrs st acc =
  skip_ws st;
  match peek st with
  | Some c when is_name_char c ->
    let name = read_name st in
    skip_ws st;
    let value =
      if peek st = Some '='
      then (
        st.pos <- st.pos + 1;
        skip_ws st;
        let quote = st.src.[st.pos] in
        st.pos <- st.pos + 1;
        let start = st.pos in
        while st.pos < st.len && st.src.[st.pos] <> quote do
          st.pos <- st.pos + 1
        done;
        let raw = String.sub st.src start (st.pos - start) in
        st.pos <- st.pos + 1;
        decode_entities raw)
      else ""
    in
    read_attrs st ((name, value) :: acc)
  | _ -> List.rev acc

and read_content st tag =
  let text = Buffer.create 16 in
  let children = ref [] in
  let finished = ref false in
  while not !finished do
    if st.pos >= st.len
    then finished := true
    else if starts_with st "</"
    then (
      st.pos <- st.pos + 2;
      let close = read_name st in
      assert (String.equal close tag);
      skip_ws st;
      assert (st.src.[st.pos] = '>');
      st.pos <- st.pos + 1;
      finished := true)
    else if starts_with st "<!--"
    then
      let close = try find_sub st.src "-->" st.pos with Not_found -> st.len in
      st.pos <- close + 3
    else if starts_with st "<![CDATA["
    then (
      let close = try find_sub st.src "]]>" st.pos with Not_found -> st.len in
      Buffer.add_string text
        (String.sub st.src (st.pos + 9) (close - st.pos - 9));
      st.pos <- close + 3)
    else if peek st = Some '<'
    then children := read_element st :: !children
    else (
      Buffer.add_char text st.src.[st.pos];
      st.pos <- st.pos + 1)
  done;
  List.rev !children, String.trim (decode_entities (Buffer.contents text))

(* Skip a comment, processing instruction, or doctype at the top level. *)
let skip_prolog_item st =
  if starts_with st "<!--"
  then
    let close = try find_sub st.src "-->" st.pos with Not_found -> st.len in
    st.pos <- close + 3
  else if starts_with st "<?"
  then
    let close = try find_sub st.src "?>" st.pos with Not_found -> st.len in
    st.pos <- close + 2
  else if starts_with st "<!"
  then
    let close =
      try String.index_from st.src st.pos '>' with Not_found -> st.len
    in
    st.pos <- close + 1
  else ()

let parse (src : string) : node =
  let st = { src; len = String.length src; pos = 0 } in
  let rec find_root () =
    skip_ws st;
    if starts_with st "<!--" || starts_with st "<?" || starts_with st "<!"
    then (
      skip_prolog_item st;
      find_root ())
    else read_element st
  in
  find_root ()

let parse_file path =
  In_channel.with_open_bin path In_channel.input_all |> parse
