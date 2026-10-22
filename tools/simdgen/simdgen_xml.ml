(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2026 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Parses specifically [x86-intel.xml] (the Intel Intrinsics Guide data). *)

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

let error msg = failwith ("Simdgen_xml: " ^ msg)

let decode_entities s =
  let n = String.length s in
  let buf = Buffer.create n in
  let rec go i =
    if i < n
    then (
      if s.[i] <> '&'
      then (
        Buffer.add_char buf s.[i];
        go (i + 1))
      else
        let semi = String.index_from s i ';' in
        (match String.sub s (i + 1) (semi - i - 1) with
        | "lt" -> Buffer.add_char buf '<'
        | "gt" -> Buffer.add_char buf '>'
        | "amp" -> Buffer.add_char buf '&'
        | "quot" -> Buffer.add_char buf '"'
        | "apos" -> Buffer.add_char buf '\''
        | name when String.starts_with ~prefix:"#" name ->
          let code = String.sub name 1 (String.length name - 1) in
          Buffer.add_utf_8_uchar buf (Uchar.of_int (int_of_string code))
        | name -> error ("unknown entity " ^ name));
        go (semi + 1))
  in
  go 0;
  Buffer.contents buf

type parser_state =
  { src : string;
    mutable pos : int
  }

let peek st = st.src.[st.pos]

let advance st = st.pos <- st.pos + 1

let consume st c = if peek st = c then advance st else error "unexpected char"

let consume_string st s = String.iter (consume st) s

let skip_while st f =
  while st.pos < String.length st.src && f st.src.[st.pos] do
    advance st
  done

let read_while st f =
  let start = st.pos in
  skip_while st f;
  String.sub st.src start (st.pos - start)

let is_space = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false

let is_name_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '-' | ':' | '.' -> true
  | _ -> false

let read_name st =
  match read_while st is_name_char with "" -> error "expected name" | n -> n

(* Called just after the opening '<'. Comments cannot contain "--", so the first
   one starts the closing "-->". *)
let skip_comment st =
  consume_string st "!--";
  let rec find_dashes () =
    st.pos <- String.index_from st.src st.pos '-' + 1;
    if peek st <> '-' then find_dashes ()
  in
  find_dashes ();
  consume_string st "->"

let rec read_attrs st =
  skip_while st is_space;
  if is_name_char (peek st)
  then (
    let name = read_name st in
    skip_while st is_space;
    consume st '=';
    skip_while st is_space;
    consume st '"';
    let value = decode_entities (read_while st (fun c -> c <> '"')) in
    consume st '"';
    (name, value) :: read_attrs st)
  else []

(* Called just after the opening '<'. *)
let rec read_element st =
  let tag = read_name st in
  let attrs = read_attrs st in
  if peek st = '/'
  then (
    consume_string st "/>";
    { tag; attrs; children = []; text = "" })
  else (
    consume st '>';
    let children, text = read_content st tag in
    { tag; attrs; children; text })

and read_content st tag =
  let text = Buffer.create 16 in
  let rec loop children =
    Buffer.add_string text (read_while st (fun c -> c <> '<'));
    consume st '<';
    match peek st with
    | '/' ->
      advance st;
      if not (String.equal (read_name st) tag) then error "mismatched tag";
      skip_while st is_space;
      consume st '>';
      List.rev children
    | '!' ->
      skip_comment st;
      loop children
    | _ -> loop (read_element st :: children)
  in
  let children = loop [] in
  children, String.trim (decode_entities (Buffer.contents text))

let parse src =
  let st = { src; pos = 0 } in
  let rec read_root () =
    skip_while st is_space;
    consume st '<';
    if peek st = '!'
    then (
      skip_comment st;
      read_root ())
    else read_element st
  in
  let root = read_root () in
  skip_while st is_space;
  if st.pos <> String.length src then error "trailing content";
  root

let parse_file path =
  In_channel.with_open_bin path In_channel.input_all |> parse
