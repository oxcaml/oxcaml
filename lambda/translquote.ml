(******************************************************************************
 *                                  OxCaml                                    *
 *                      Andrej Ivaskovic, Jane Street                         *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

(* CR metaprogramming aivaskovic: This file has not been code reviewed *)

open Typedtree

module Lam = struct
  open Lambda

  (*
  let combinator modname field =
    lazy
      (let _, env = Lazy.force camlinternalQuote in
       let lid =
         match unflatten (String.split_on_char '.' modname) with
         | None -> Lident field
         | Some lid -> Ldot (lid, field)
       in
       match Env.find_value_by_name_lazy lid env with
       | p, _ -> transl_value_path Loc_unknown env p
       | exception Not_found ->
         fatal_error
           ("Primitive CamlinternalQuote." ^ modname ^ "." ^ field
          ^ " not found."))
  *)

  let string loc s = Lconst (Const_base (Const_string (s, loc, None)))
  (*
  let true_ = Lconst (Const_base (Const_int 1))

  let false_ = Lconst (Const_base (Const_int 0))

  let quote_bool b = if b then true_ else false_

  let none = Lconst (Const_base (Const_int 0))

  let some x =
    Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x], Loc_unknown)

  let option opt = match opt with None -> none | Some x -> some x

  let string_option loc s = option (Option.map (string loc) s)

  let nil = Lconst (Const_base (Const_int 0))

  let cons hd tl =
    Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [hd; tl], Loc_unknown)

  let hd l = Lprim (Pfield (0, Immediate, Reads_vary), [l], Loc_unknown)

  let tl l = Lprim (Pfield (1, Immediate, Reads_vary), [l], Loc_unknown)

  let rec mk_list list =
    match list with [] -> nil | hd :: tl -> cons hd (mk_list tl)

  let pair (x, y) =
    Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x; y], Loc_unknown)

  let triple (x, y, z) =
    Lprim (Pmakeblock (0, Immutable, None, alloc_heap), [x; y; z], Loc_unknown)

  let bind id def body =
    Llet
      ( Strict,
        Pvalue { raw_kind = Pgenval; nullable = Non_nullable },
        id,
        debug_uid_none,
        def,
        body )
  *)
end

(* Lifted from [Pprintast] *)
module Transl = struct
  open Asttypes
  open Format
  open Location
  open Longident
  open Typedtree

  let prefix_symbols = ['!'; '?'; '~']

  let infix_symbols =
    ['='; '<'; '>'; '@'; '^'; '|'; '&'; '+'; '-'; '*'; '/'; '$'; '%'; '#']

  (* type fixity = Infix| Prefix  *)
  let special_infix_strings =
    ["asr"; "land"; "lor"; "lsl"; "lsr"; "lxor"; "mod"; "or"; ":="; "!="; "::"]

  let letop s =
    String.length s > 3
    && s.[0] = 'l'
    && s.[1] = 'e'
    && s.[2] = 't'
    && List.mem s.[3] infix_symbols

  let andop s =
    String.length s > 3
    && s.[0] = 'a'
    && s.[1] = 'n'
    && s.[2] = 'd'
    && List.mem s.[3] infix_symbols

  (* determines if the string is an infix string.
     checks backwards, first allowing a renaming postfix ("_102") which
     may have resulted from Texp -> Texp -> Texp translation, then checking
     if all the characters in the beginning of the string are valid infix
     characters. *)
  let fixity_of_string = function
    | "" -> `Normal
    | s when List.mem s special_infix_strings -> `Infix s
    | s when List.mem s.[0] infix_symbols -> `Infix s
    | s when List.mem s.[0] prefix_symbols -> `Prefix s
    | s when s.[0] = '.' -> `Mixfix s
    | s when letop s -> `Letop s
    | s when andop s -> `Andop s
    | _ -> `Normal

  let view_fixity_of_exp = function
    | { exp_desc = Texp_ident (_, { txt = Lident l; _ }, _, _, _);
        exp_attributes = []
      } ->
      fixity_of_string l
    | _ -> `Normal

  let is_infix = function `Infix _ -> true | _ -> false

  let is_mixfix = function `Mixfix _ -> true | _ -> false

  let is_kwdop = function `Letop _ | `Andop _ -> true | _ -> false

  let first_is c str = str <> "" && str.[0] = c

  let last_is c str = str <> "" && str.[String.length str - 1] = c

  let first_is_in cs str = str <> "" && List.mem str.[0] cs

  (* which identifiers are in fact operators needing parentheses *)
  let needs_parens txt =
    let fix = fixity_of_string txt in
    is_infix fix || is_mixfix fix || is_kwdop fix
    || first_is_in prefix_symbols txt

  (* some infixes need spaces around parens to avoid clashes with comment
     syntax *)
  let needs_spaces txt = first_is '*' txt || last_is '*' txt

  (* Turn an arbitrary variable name into a valid OCaml identifier by adding \#
     in case it is a keyword, or parenthesis when it is an infix or prefix
     operator. *)
  let ident_of_name ppf txt =
    let format : (_, _, _) format =
      if Lexer.is_keyword txt
      then "\\#%s"
      else if not (needs_parens txt)
      then "%s"
      else if needs_spaces txt
      then "(@;%s@;)"
      else "(%s)"
    in
    fprintf ppf format txt

  let protect_longident ppf print_longident longprefix txt =
    if not (needs_parens txt)
    then fprintf ppf "%a.%a" print_longident longprefix ident_of_name txt
    else if needs_spaces txt
    then fprintf ppf "%a.(@;%s@;)" print_longident longprefix txt
    else fprintf ppf "%a.(%s)" print_longident longprefix txt

  let is_curry_attr attr =
    attr.Parsetree.attr_name.txt = Builtin_attributes.curry_attr_name

  let split_out_curry_attr attrs =
    let curry, non_curry = List.partition is_curry_attr attrs in
    let is_curry = match curry with [] -> false | _ :: _ -> true in
    is_curry, non_curry

  type space_formatter = (unit, Format.formatter, unit) format

  let override = function Override -> "!" | Fresh -> ""

  (* variance encoding: need to sync up with the [parser.mly] *)
  let type_variance = function
    | NoVariance -> ""
    | Covariant -> "+"
    | Contravariant -> "-"

  let type_injectivity = function NoInjectivity -> "" | Injective -> "!"

  type construct =
    [ `cons of expression list
    | `list of expression list
    | `nil
    | `normal
    | `simple of Longident.t
    | `tuple
    | `btrue
    | `bfalse ]

  let view_expr x =
    match x.exp_desc with
    | Texp_construct ({ txt = Lident "()"; _ }, _, _, _) -> `tuple
    | Texp_construct ({ txt = Lident "true"; _ }, _, _, _) -> `btrue
    | Texp_construct ({ txt = Lident "false"; _ }, _, _, _) -> `bfalse
    | Texp_construct ({ txt = Lident "[]"; _ }, _, _, _) -> `nil
    | Texp_construct ({ txt = Lident "::"; _ }, _, _ :: _, _) ->
      let rec loop exp acc =
        match exp with
        | { exp_desc = Texp_construct ({ txt = Lident "[]"; _ }, _, _, _);
            exp_attributes = []
          } ->
          List.rev acc, true
        | { exp_desc = Texp_construct ({ txt = Lident "::"; _ }, _, [e1; e2], _);
            exp_attributes = []
          } ->
          loop e2 (e1 :: acc)
        | e -> List.rev (e :: acc), false
      in
      let ls, b = loop x [] in
      if b then `list ls else `cons ls
    | Texp_construct (x, _, [], _) -> `simple x.txt
    | _ -> `normal

  let is_simple_construct : construct -> bool = function
    | `nil | `tuple | `list _ | `simple _ | `btrue | `bfalse -> true
    | `cons _ | `normal -> false

  let pp = fprintf

  type ctxt =
    { pipe : bool;
      semi : bool;
      ifthenelse : bool;
      functionrhs : bool
    }

  let reset_ctxt =
    { pipe = false; semi = false; ifthenelse = false; functionrhs = false }

  let under_pipe ctxt = { ctxt with pipe = true }

  let under_semi ctxt = { ctxt with semi = true }

  let under_ifthenelse ctxt = { ctxt with ifthenelse = true }

  let under_functionrhs ctxt = { ctxt with functionrhs = true }
  (*
let reset_semi ctxt = { ctxt with semi=false }
let reset_ifthenelse ctxt = { ctxt with ifthenelse=false }
let reset_pipe ctxt = { ctxt with pipe=false }
*)

  let list :
        'a.
        ?sep:space_formatter ->
        ?first:space_formatter ->
        ?last:space_formatter ->
        (Format.formatter -> 'a -> unit) ->
        Format.formatter ->
        'a list ->
        unit =
   fun ?sep ?first ?last fu f xs ->
    let first = match first with Some x -> x | None -> ("" : _ format6)
    and last = match last with Some x -> x | None -> ("" : _ format6)
    and sep = match sep with Some x -> x | None -> ("@ " : _ format6) in
    let aux f = function
      | [] -> ()
      | [x] -> fu f x
      | xs ->
        let rec loop f = function
          | [x] -> fu f x
          | x :: xs ->
            fu f x;
            pp f sep;
            loop f xs
          | _ -> assert false
        in
        pp f first;
        loop f xs;
        pp f last
    in
    aux f xs

  let option :
        'a.
        ?first:space_formatter ->
        ?last:space_formatter ->
        (Format.formatter -> 'a -> unit) ->
        Format.formatter ->
        'a option ->
        unit =
   fun ?first ?last fu f a ->
    let first = match first with Some x -> x | None -> ("" : _ format6)
    and last = match last with Some x -> x | None -> ("" : _ format6) in
    match a with
    | None -> ()
    | Some x ->
      pp f first;
      fu f x;
      pp f last

  let paren :
        'a.
        ?first:space_formatter ->
        ?last:space_formatter ->
        bool ->
        (Format.formatter -> 'a -> unit) ->
        Format.formatter ->
        'a ->
        unit =
   fun ?(first = ("" : _ format6)) ?(last = ("" : _ format6)) b fu f x ->
    if b
    then (
      pp f "(";
      pp f first;
      fu f x;
      pp f last;
      pp f ")")
    else fu f x

  let rec longident f = function
    | Lident s -> ident_of_name f s
    | Ldot (y, s) -> protect_longident f longident y s
    | Lapply (y, s) -> pp f "%a(%a)" longident y longident s

  let longident_loc f x = pp f "%a" longident x.txt

  let constant f = function
    | Const_char i -> pp f "%C" i
    | Const_untagged_char i -> pp f "#%C" i
    | Const_string (i, _, None) -> pp f "%S" i
    | Const_string (i, _, Some delim) -> pp f "{%s|%s|%s}" delim i delim
    | Const_int (i, None) -> paren (first_is '-' i) (fun f -> pp f "%s") f i
    | Const_int (i, Some m) ->
      paren (first_is '-' i) (fun f (i, m) -> pp f "%s%c" i m) f (i, m)
    | Const_float (i, None) -> paren (first_is '-' i) (fun f -> pp f "%s") f i
    | Const_float (i, Some m) ->
      paren (first_is '-' i) (fun f (i, m) -> pp f "%s%c" i m) f (i, m)
    | Const_unboxed_float (x, None) ->
      paren (first_is '-' x)
        (fun f -> pp f "%s")
        f
        (Misc.format_as_unboxed_literal x)
    | Const_unboxed_float (x, Some suffix) | Const_unboxed_integer (x, suffix)
      ->
      paren (first_is '-' x)
        (fun f (x, suffix) -> pp f "%s%c" x suffix)
        f
        (Misc.format_as_unboxed_literal x, suffix)

  (* trailing space*)
  let mutable_flag f = function Immutable -> () | Mutable -> pp f "mutable@;"

  let virtual_flag f = function Concrete -> () | Virtual -> pp f "virtual@;"

  (* trailing space added *)
  let rec_flag f rf =
    match rf with Nonrecursive -> () | Recursive -> pp f "rec "

  let nonrec_flag f rf =
    match rf with Nonrecursive -> pp f "nonrec " | Recursive -> ()

  let direction_flag f = function
    | Upto -> pp f "to@ "
    | Downto -> pp f "downto@ "

  let private_flag f = function Public -> () | Private -> pp f "private@ "

  let iter_loc f ctxt { txt; loc = _ } = f ctxt txt

  let constant_string f s = pp f "%S" s

  let tyvar_of_name s =
    if String.length s >= 2 && s.[1] = '\''
    then
      (* without the space, this would be parsed as
         a character literal *)
      "' " ^ s
    else if Lexer.is_keyword s
    then "'\\#" ^ s
    else if String.equal s "_"
    then s
    else "'" ^ s

  let tyvar ppf s = Format.fprintf ppf "%s" (tyvar_of_name s)

  let string_loc ppf x = fprintf ppf "%s" x.txt

  let string_quot f x = pp f "`%a" ident_of_name x

  (* legacy modes and modalities *)
  let legacy_mode f { txt = Mode s; _ } =
    let s =
      match s with
      | "local" -> "local_"
      | s -> Misc.fatal_errorf "Unrecognized mode %s - should not parse" s
    in
    pp_print_string f s

  let legacy_modes f m =
    pp_print_list ~pp_sep:(fun f () -> pp f " ") legacy_mode f m

  let optional_legacy_modes f m =
    match m with
    | [] -> ()
    | m ->
      legacy_modes f m;
      pp_print_space f ()

  let legacy_modality f m =
    let ({ txt; _ }) = (m : Parsetree.modality Location.loc) in
    let s =
      match txt with
      | Modality "global" -> "global_"
      | Modality s ->
        Misc.fatal_errorf "Unrecognized modality %s - should not parse" s
    in
    pp_print_string f s

  let legacy_modalities f m =
    pp_print_list ~pp_sep:(fun f () -> pp f " ") legacy_modality f m

  let optional_legacy_modalities f m =
    match m with
    | [] -> ()
    | m ->
      legacy_modalities f m;
      pp_print_space f ()

  (* new mode and modality syntax *)
  let mode f { txt = Mode s; _ } = pp_print_string f s

  let modes f m = pp_print_list ~pp_sep:(fun f () -> pp f " ") mode f m

  let optional_at_modes f m =
    match m with [] -> () | m -> pp f " %@ %a" modes m

  let modality f m =
    let { txt = Modality txt; _ } = m in
    pp_print_string f txt

  let modalities f m = pp_print_list ~pp_sep:(fun f () -> pp f " ") modality f m

  let optional_modalities ?(pre = fun _ () -> ()) ?(post = fun _ () -> ()) f m =
    match m with
    | [] -> ()
    | m ->
      pre f ();
      pp f "%a" modalities m;
      post f ()

  let optional_space_atat_modalities f m =
    let pre f () = Format.fprintf f "@ %@%@@ " in
    optional_modalities ~pre f m

  let optional_atat_modalities_newline f m =
    let pre f () = Format.fprintf f "%@%@@ " in
    optional_modalities ~pre ~post:pp_print_newline f m

  (** For a list of modes, we either print everything in old syntax (if they
  are purely old modes), or everything in new syntax. *)
  let print_modes_in_old_syntax =
    List.for_all (fun m ->
        let (Mode txt) = m.txt in
        match txt with "local" -> true | _ -> false)

  (** For a list of modalities, we either print all in old syntax (if they are
  purely old modalities), or all in new syntax. *)
  let print_modality_in_old_syntax =
    List.for_all (fun m ->
        let (Modality txt) = m.txt in
        match txt with "global" -> true | _ -> false)

  let modalities_type pty ctxt f pca =
    let m = pca.ca_modalities in
    if print_modality_in_old_syntax m
    then pp f "%a%a" optional_legacy_modalities m (pty ctxt) pca.ca_type
    else pp f "%a%a" (pty ctxt) pca.ca_type optional_space_atat_modalities m

  (* c ['a,'b] *)
  let rec core_type_with_optional_legacy_modes pty ctxt f (c, m) =
    match m with
    | [] -> pty ctxt f c
    | _ :: _ ->
      if print_modes_in_old_syntax m
      then pp f "%a%a" optional_legacy_modes m (core_type1 ctxt) c
      else pp f "%a%a" (core_type1 ctxt) c optional_at_modes m

  and type_with_label ctxt f (label, c, mode) =
    match label with
    | Nolabel ->
      core_type_with_optional_legacy_modes core_type1 ctxt f (c, mode)
      (* otherwise parenthesize *)
    | Labelled s | Position s ->
      pp f "%a:%a" ident_of_name s
        (core_type_with_optional_legacy_modes core_type1 ctxt)
        (c, mode)
    | Optional s ->
      pp f "?%a:%a" ident_of_name s
        (core_type_with_optional_legacy_modes core_type1 ctxt)
        (c, mode)

  and jkind_annotation ?(nested = false) ctxt f (k : Parsetree.jkind_annotation)
      =
    match k.pjkind_desc with
    | Pjk_default -> pp f "_"
    | Pjk_abbreviation s -> pp f "%s" s
    | Pjk_mod (t, modes) -> (
      match modes with
      | [] -> Misc.fatal_error "malformed jkind annotation"
      | _ :: _ ->
        Misc.pp_parens_if nested
          (fun f (t, modes) ->
            pp f "%a mod %a"
              (jkind_annotation ~nested:true ctxt)
              t
              (pp_print_list ~pp_sep:pp_print_space mode)
              modes)
          f (t, modes))
    | Pjk_with _ -> Misc.fatal_error "no jkind with in quotes"
    | Pjk_kind_of _ -> Misc.fatal_error "no jkind kind-of in quotes"
    | Pjk_product ts ->
      Misc.pp_parens_if nested
        (fun f ts ->
          pp f "@[%a@]"
            (list (jkind_annotation ~nested:true ctxt) ~sep:"@ & ")
            ts)
        f ts

  and tyvar_jkind tyvar f (str, jkind) =
    match jkind with
    | None -> tyvar f str
    | Some lay -> pp f "(%a : %a)" tyvar str (jkind_annotation reset_ctxt) lay

  and tyvar_loc_jkind tyvar f (str, jkind) = tyvar_jkind tyvar f (str.txt, jkind)

  and tyvar_loc_option_jkind f (str, jkind) =
    match jkind with
    | None -> tyvar_loc_option f str
    | Some jkind ->
      pp f "(%a : %a)" tyvar_loc_option str (jkind_annotation reset_ctxt) jkind

  and name_jkind f (name, jkind) =
    match jkind with
    | None -> ident_of_name f name
    | Some jkind ->
      pp f "(%a : %a)" ident_of_name name (jkind_annotation reset_ctxt) jkind

  and name_loc_jkind f (str, jkind) = name_jkind f (str.txt, jkind)

  and core_type ctxt f x =
    if x.ctyp_attributes <> []
    then
      pp f "((%a)%a)" (core_type ctxt)
        { x with ctyp_attributes = [] }
        (attributes ctxt) x.ctyp_attributes
    else
      match x.ctyp_desc with
      | Ttyp_arrow (l, ct1, ct2, m1, m2) ->
        pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
          (type_with_label ctxt) (l, ct1, m1) (return_type ctxt) (ct2, m2)
      | Ttyp_alias (ct, s, j) ->
        pp f "@[<2>%a@;as@;%a@]" (core_type1 ctxt) ct tyvar_loc_option_jkind
          (s, j)
      | Ttyp_poly ([], ct) -> core_type ctxt f ct
      | Ttyp_poly (sl, ct) ->
        pp f "@[<2>%a%a@]"
          (fun f l ->
            match l with
            | [] -> ()
            | _ -> pp f "%a@;.@;" (list (tyvar_loc_jkind tyvar) ~sep:"@;") l)
          sl (core_type ctxt) ct
      | Ttyp_of_kind jkind ->
        pp f "@[(type@ :@ %a)@]" (jkind_annotation reset_ctxt) jkind
      | _ -> pp f "@[<2>%a@]" (core_type1 ctxt) x

  and core_type1 ctxt f x =
    if x.ctyp_attributes <> []
    then core_type ctxt f x
    else
      match x.ctyp_desc with
      | Ttyp_var (s, jkind) -> (tyvar_jkind tyvar) f (s, jkind)
      | Ttyp_tuple tl ->
        pp f "(%a)" (list (labeled_core_type1 ctxt) ~sep:"@;*@;") tl
      | Ttyp_unboxed_tuple l -> core_type1_labeled_tuple ctxt f ~unboxed:true l
      | Ttyp_constr (li, l) ->
        pp f (* "%a%a@;" *) "%a%a"
          (fun f l ->
            match l with
            | [] -> ()
            | [x] -> pp f "%a@;" (core_type1 ctxt) x
            | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l)
          l longident_loc li
      | Ttyp_variant (l, closed, low) ->
        let first_is_inherit =
          match l with { rf_desc = Rinherit _ } :: _ -> true | _ -> false
        in
        let type_variant_helper f x =
          match x.rf_desc with
          | Ttag (l, _, ctl) ->
            pp f "@[<2>%a%a@;%a@]" (iter_loc string_quot) l
              (fun f l ->
                match l with
                | [] -> ()
                | _ -> pp f "@;of@;%a" (list (core_type ctxt) ~sep:"&") ctl)
              ctl (attributes ctxt) x.rf_attributes
          | Tinherit ct -> core_type ctxt f ct
        in
        pp f "@[<2>[%a%a]@]"
          (fun f l ->
            match l, closed with
            | [], Closed -> ()
            | [], Open -> pp f ">" (* Cf #7200: print [>] correctly *)
            | _ ->
              pp f "%s@;%a"
                (match closed, low with
                | Closed, None -> if first_is_inherit then " |" else ""
                | Closed, Some _ -> "<" (* FIXME desugar the syntax sugar*)
                | Open, _ -> ">")
                (list type_variant_helper ~sep:"@;<1 -2>| ")
                l)
          l
          (fun f low ->
            match low with
            | Some [] | None -> ()
            | Some xs -> pp f ">@ %a" (list string_quot) xs)
          low
      | Ttyp_object (l, o) ->
        let core_field_type f x =
          match x.pof_desc with
          | Otag (l, ct) ->
            (* Cf #7200 *)
            pp f "@[<hov2>%a: %a@ %a@ @]" ident_of_name l.txt (core_type ctxt)
              ct (attributes ctxt) x.pof_attributes
          | Oinherit ct -> pp f "@[<hov2>%a@ @]" (core_type ctxt) ct
        in
        let field_var f = function
          | Asttypes.Closed -> ()
          | Asttypes.Open -> (
            match l with [] -> pp f ".." | _ -> pp f " ;..")
        in
        pp f "@[<hov2><@ %a%a@ > @]"
          (list core_field_type ~sep:";")
          l field_var o (* Cf #7200 *)
      | Ttyp_class (li, l) ->
        (*FIXME*)
        pp f "@[<hov2>%a@;#%a@]"
          (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
          l longident_loc li
      | Ttyp_package (lid, cstrs) -> (
        let aux f (s, ct) =
          pp f "type %a@ =@ %a" longident_loc s (core_type ctxt) ct
        in
        match cstrs with
        | [] -> pp f "@[<hov2>(module@ %a)@]" longident_loc lid
        | _ ->
          pp f "@[<hov2>(module@ %a@ with@ %a)@]" longident_loc lid
            (list aux ~sep:"@ and@ ") cstrs)
      | Ttyp_open (li, ct) ->
        pp f "@[<hov2>%a.(%a)@]" longident_loc li (core_type ctxt) ct
      | Ttyp_quote t -> pp f "@[<hov2><[%a]>@]" (core_type ctxt) t
      | Ttyp_splice t -> pp f "@[<hov2>$(%a)@]" (core_type ctxt) t
      | Ttyp_arrow _ | Ttyp_alias _ | Ttyp_poly _ | Ttyp_of_kind _ ->
        paren true (core_type ctxt) f x

  and core_type2 ctxt f x =
    if x.ctyp_attributes <> []
    then core_type ctxt f x
    else
      match x.ctyp_desc with
      | Ttyp_poly (sl, ct) ->
        pp f "@[<2>%a%a@]"
          (fun f l ->
            match l with
            | [] -> ()
            | _ -> pp f "%a@;.@;" (list (tyvar_loc_jkind tyvar) ~sep:"@;") l)
          sl (core_type1 ctxt) ct
      | _ -> core_type1 ctxt f x

  and tyvar_option f = function None -> pp f "_" | Some name -> tyvar f name

  and tyvar_loc_option f str = tyvar_option f (Option.map Location.get_txt str)

  and core_type1_labeled_tuple ctxt f ~unboxed tl =
    pp f "%s(%a)"
      (if unboxed then "#" else "")
      (list (labeled_core_type1 ctxt) ~sep:"@;*@;")
      tl

  and labeled_core_type1 ctxt f (label, ty) =
    (match label with None -> () | Some s -> pp f "%s:" s);
    core_type1 ctxt f ty

  and return_type ctxt f (x, m) =
    let is_curry, ctyp_attributes = split_out_curry_attr x.ctyp_attributes in
    let x = { x with ctyp_attributes } in
    if is_curry
    then core_type_with_optional_legacy_modes core_type1 ctxt f (x, m)
    else core_type_with_optional_legacy_modes core_type ctxt f (x, m)

  and core_type_with_optional_modes ctxt f (ty, modes) =
    match modes with
    | [] -> core_type ctxt f ty
    | _ :: _ -> pp f "%a%a" (core_type2 ctxt) ty optional_at_modes modes

  (********************pattern********************)
  (* be cautious when use [pattern], [pattern1] is preferred *)
  and pattern ctxt f x =
    if x.pat_attributes <> []
    then
      pp f "((%a)%a)" (pattern ctxt)
        { x with pat_attributes = [] }
        (attributes ctxt) x.pat_attributes
    else
      match x.pat_desc with
      | Tpat_alias (p, s) ->
        pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p ident_of_name s.txt
      | _ -> pattern_or ctxt f x

  and pattern_or ctxt f x =
    let rec left_associative x acc =
      match x with
      | { pat_desc = Tpat_or (p1, p2); pat_attributes = [] } ->
        left_associative p1 (p2 :: acc)
      | x -> x :: acc
    in
    match left_associative x [] with
    | [] -> assert false
    | [x] -> pattern1 ctxt f x
    | orpats -> pp f "@[<hov0>%a@]" (list ~sep:"@ | " (pattern1 ctxt)) orpats

  and pattern1 ctxt (f : Format.formatter) (x : pattern) : unit =
    let rec pattern_list_helper f p =
      match p with
      | { pat_desc =
            Tpat_construct ({ txt = Lident "::"; _ }, Some ([], inner_pat));
          pat_attributes = []
        } -> (
        match inner_pat.pat_desc with
        | Tpat_tuple ([(None, pat1); (None, pat2)], Closed) ->
          pp f "%a::%a" (simple_pattern ctxt) pat1 pattern_list_helper
            pat2 (*RA*)
        | _ -> pattern1 ctxt f p)
      | _ -> pattern1 ctxt f p
    in
    if x.pat_attributes <> []
    then pattern ctxt f x
    else
      match x.pat_desc with
      | Tpat_variant (l, Some p) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name l (simple_pattern ctxt) p
      | Tpat_construct ({ txt = Lident ("()" | "[]" | "true" | "false"); _ }, _)
        ->
        simple_pattern ctxt f x
      | Tpat_construct (({ txt; _ } as li), po) -> (
        if (* FIXME The third field always false *)
           txt = Lident "::"
        then pp f "%a" pattern_list_helper x
        else
          match po with
          | Some ([], x) ->
            pp f "%a@;%a" longident_loc li (simple_pattern ctxt) x
          | Some (vl, x) ->
            pp f "%a@ (type %a)@;%a" longident_loc li
              (list ~sep:"@ " name_loc_jkind)
              vl (simple_pattern ctxt) x
          | None -> pp f "%a" longident_loc li)
      | _ -> simple_pattern ctxt f x

  and labeled_pattern1 ctxt (f : Format.formatter) (label, x) : unit =
    let simple_name =
      match x with
      | { pat_desc = Tpat_var { txt = s; _ }; pat_attributes = []; _ } -> Some s
      | _ -> None
    in
    match label, simple_name with
    | None, _ -> pattern1 ctxt f x
    | Some lbl, Some simple_name when String.equal simple_name lbl ->
      pp f "~%s" lbl
    | Some lbl, _ ->
      pp f "~%s:" lbl;
      pattern1 ctxt f x

  and simple_pattern ctxt (f : Format.formatter) (x : pattern) : unit =
    if x.pat_attributes <> []
    then pattern ctxt f x
    else
      match x.pat_desc with
      | Tpat_construct
          ({ txt = Lident (("()" | "[]" | "true" | "false") as x); _ }, None) ->
        pp f "%s" x
      | Tpat_any -> pp f "_"
      | Tpat_var { txt; _ } -> ident_of_name f txt
      | Tpat_array (mut, l) ->
        let punct = match mut with Mutable -> '|' | Immutable -> ':' in
        pp f "@[<2>[%c%a%c]@]" punct (list (pattern1 ctxt) ~sep:";") l punct
      | Tpat_unpack { txt = None } -> pp f "(module@ _)@ "
      | Tpat_unpack { txt = Some s } -> pp f "(module@ %s)@ " s
      | Tpat_type li -> pp f "#%a" longident_loc li
      | Tpat_record (l, closed) -> record_pattern ctxt f ~unboxed:false l closed
      | Tpat_record_unboxed_product (l, closed) ->
        record_pattern ctxt f ~unboxed:true l closed
      | Tpat_tuple (l, closed) ->
        labeled_tuple_pattern ctxt f ~unboxed:false l closed
      | Tpat_unboxed_tuple (l, closed) ->
        labeled_tuple_pattern ctxt f ~unboxed:true l closed
      | Tpat_constant c -> pp f "%a" constant c
      | Tpat_interval (c1, c2) -> pp f "%a..%a" constant c1 constant c2
      | Tpat_variant (l, None) -> pp f "`%a" ident_of_name l
      | Tpat_constraint (p, ct, _) ->
        pp f "@[<2>(%a@;:@;%a)@]" (pattern1 ctxt) p (core_type ctxt)
          (Option.get ct)
      | Tpat_lazy p -> pp f "@[<2>(lazy@;%a)@]" (simple_pattern ctxt) p
      | Tpat_exception p -> pp f "@[<2>exception@;%a@]" (pattern1 ctxt) p
      | Tpat_extension e -> extension ctxt f e
      | Tpat_open (lid, p) ->
        let with_paren =
          match p.pat_desc with
          | Tpat_array _ | Tpat_record _ | Tpat_record_unboxed_product _
          | Tpat_construct
              ({ txt = Lident ("()" | "[]" | "true" | "false"); _ }, None) ->
            false
          | _ -> true
        in
        pp f "@[<2>%a.%a @]" longident_loc lid
          (paren with_paren @@ pattern1 ctxt)
          p
      | _ -> paren true (pattern ctxt) f x

  and record_pattern ctxt f ~unboxed l closed =
    let longident_x_pattern f (li, p) =
      match li, p with
      | ( { txt = Lident s; _ },
          { pat_desc = Tpat_var { txt; _ }; pat_attributes = []; _ } )
        when s = txt ->
        pp f "@[<2>%a@]" longident_loc li
      | _ -> pp f "@[<2>%a@;=@;%a@]" longident_loc li (pattern1 ctxt) p
    in
    let hash = if unboxed then "#" else "" in
    match closed with
    | Closed ->
      pp f "@[<2>%s{@;%a@;}@]" hash (list longident_x_pattern ~sep:";@;") l
    | Open ->
      pp f "@[<2>%s{@;%a;_}@]" hash (list longident_x_pattern ~sep:";@;") l

  and labeled_tuple_pattern ctxt f ~unboxed l closed =
    let closed_flag ppf = function Closed -> () | Open -> pp ppf ",@;.." in
    pp f "@[<1>%s(%a%a)@]"
      (if unboxed then "#" else "")
      (list ~sep:",@;" (labeled_pattern1 ctxt))
      l closed_flag closed

  (** for special treatment of modes in labeled expressions *)
  and pattern2 ctxt f p =
    match p.pat_desc with
    | Tpat_constraint (p, ct, m) -> (
      match ct, print_modes_in_old_syntax m with
      | Some ct, true ->
        pp f "@[<2>%a%a@;:@;%a@]" optional_legacy_modes m (simple_pattern ctxt)
          p (core_type ctxt) ct
      | Some ct, false ->
        pp f "@[<2>%a@;:@;%a@]" (simple_pattern ctxt) p
          (core_type_with_optional_modes ctxt)
          (ct, m)
      | None, true ->
        pp f "@[<2>%a%a@]" optional_legacy_modes m (simple_pattern ctxt) p
      | None, false ->
        pp f "@[<2>%a%a@]" (simple_pattern ctxt) p optional_at_modes m)
    | _ -> pattern1 ctxt f p

  (** for special treatment of modes in labeled expressions *)
  and simple_pattern1 ctxt f p =
    match p.pat_desc with
    | Tpat_constraint _ -> pp f "(%a)" (pattern2 ctxt) p
    | _ -> simple_pattern ctxt f p

  and label_exp ctxt f (l, opt, p) =
    match l with
    | Nolabel ->
      (* single case pattern parens needed here *)
      pp f "%a" (simple_pattern1 ctxt) p
    | Optional rest -> (
      match p with
      | { pat_desc = Tpat_var { txt; _ }; pat_attributes = [] } when txt = rest
        -> (
        match opt with
        | Some o -> pp f "?(%a=@;%a)" ident_of_name rest (expression ctxt) o
        | None -> pp f "?%a" ident_of_name rest)
      | _ -> (
        match opt with
        | Some o ->
          pp f "?%a:(%a=@;%a)@;" ident_of_name rest (pattern2 ctxt) p
            (expression ctxt) o
        | None -> pp f "?%a:%a@;" ident_of_name rest (simple_pattern1 ctxt) p))
    | Labelled l | Position l -> (
      match p with
      | { pat_desc = Tpat_var { txt; _ }; pat_attributes = [] } when txt = l ->
        pp f "~%a" ident_of_name l
      | _ -> pp f "~%a:%a" ident_of_name l (simple_pattern1 ctxt) p)

  and sugar_expr ctxt f e =
    if e.exp_attributes <> []
    then false
    else
      match e.exp_desc with
      | Texp_apply
          ( { exp_desc = Texp_ident { txt = id; _ }; exp_attributes = []; _ },
            args )
        when List.for_all (fun (lab, _) -> lab = Nolabel) args -> (
        let print_indexop a path_prefix assign left sep right print_index
            indices rem_args =
          let print_path ppf = function
            | None -> ()
            | Some m -> pp ppf ".%a" longident m
          in
          match assign, rem_args with
          | false, [] ->
            pp f "@[%a%a%s%a%s@]" (simple_expr ctxt) a print_path path_prefix
              left (list ~sep print_index) indices right;
            true
          | true, [v] ->
            pp f "@[%a%a%s%a%s@ <-@;<1 2>%a@]" (simple_expr ctxt) a print_path
              path_prefix left (list ~sep print_index) indices right
              (simple_expr ctxt) v;
            true
          | _ -> false
        in
        match id, List.map snd args with
        | Lident "!", [e] ->
          pp f "@[<hov>!%a@]" (simple_expr ctxt) e;
          true
        | Ldot (path, (("get" | "set") as func)), a :: other_args -> (
          let assign = func = "set" in
          let print = print_indexop a None assign in
          match path, other_args with
          | Lident "Array", i :: rest ->
            print ".(" "" ")" (expression ctxt) [i] rest
          | Lident "String", i :: rest ->
            print ".[" "" "]" (expression ctxt) [i] rest
          | Ldot (Lident "Bigarray", "Array1"), i1 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1] rest
          | Ldot (Lident "Bigarray", "Array2"), i1 :: i2 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1; i2] rest
          | Ldot (Lident "Bigarray", "Array3"), i1 :: i2 :: i3 :: rest ->
            print ".{" "," "}" (simple_expr ctxt) [i1; i2; i3] rest
          | ( Ldot (Lident "Bigarray", "Genarray"),
              { exp_desc = Texp_array (_, indexes); exp_attributes = [] }
              :: rest ) ->
            print ".{" "," "}" (simple_expr ctxt) indexes rest
          | _ -> false)
        | (Lident s | Ldot (_, s)), a :: i :: rest when first_is '.' s ->
          (* extract operator:
             assignment operators end with [right_bracket ^ "<-"],
             access operators end with [right_bracket] directly
          *)
          let multi_indices = String.contains s ';' in
          let i =
            match i.exp_desc with
            | Texp_array (_, l) when multi_indices -> l
            | _ -> [i]
          in
          let assign = last_is '-' s in
          let kind =
            (* extract the right end bracket *)
            let n = String.length s in
            if assign then s.[n - 3] else s.[n - 1]
          in
          let left, right =
            match kind with
            | ')' -> '(', ")"
            | ']' -> '[', "]"
            | '}' -> '{', "}"
            | _ -> assert false
          in
          let path_prefix = match id with Ldot (m, _) -> Some m | _ -> None in
          let left = String.sub s 0 (1 + String.index s left) in
          print_indexop a path_prefix assign left ";" right
            (if multi_indices then expression ctxt else simple_expr ctxt)
            i rest
        | _ -> false)
      | _ -> false

  and expression ctxt f x =
    if x.exp_attributes <> []
    then
      pp f "((%a)@,%a)" (expression ctxt)
        { x with exp_attributes = [] }
        (attributes ctxt) x.exp_attributes
    else
      match x.exp_desc with
      | Texp_function _ | Texp_match _ | Texp_try _ | Texp_sequence _
      | Texp_ifthenelse _ | Texp_sequence _
        when ctxt.ifthenelse ->
        paren true (expression reset_ctxt) f x
      | Texp_let _ | Texp_letmodule _ | Texp_open _ | Texp_letexception _
      | Texp_letop _
        when ctxt.semi ->
        paren true (expression reset_ctxt) f x
      | Texp_function (params, constraint_, body) -> (
        match params, constraint_ with
        (* Omit [fun] if there are no params. *)
        | [], { ret_type_constraint = None; ret_mode_annotations = []; _ } ->
          (* If function cases are a direct body of a function,
             the function node should be wrapped in parens so
             it doesn't become part of the enclosing function. *)
          let should_paren =
            match body with
            | Tfunction_cases _ -> ctxt.functionrhs
            | Tfunction_body _ -> false
          in
          let ctxt' = if should_paren then reset_ctxt else ctxt in
          pp f "@[<2>%a@]" (paren should_paren (function_body ctxt')) body
        | [], constraint_ ->
          pp f "@[<2>(%a%a)@]" (function_body ctxt) body
            (function_constraint ctxt) constraint_
        | _ :: _, _ ->
          pp f "@[<2>fun@;%t@]" (fun f ->
              function_params_then_body ctxt f params constraint_ body
                ~delimiter:"->"))
      | Texp_match (e, l) ->
        pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]" (expression reset_ctxt)
          e (case_list ctxt) l
      | Texp_try (e, l) ->
        pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
          (* "try@;@[<2>%a@]@\nwith@\n%a"*)
          (expression reset_ctxt)
          e (case_list ctxt) l
      | Texp_let (mf, rf, l, e) ->
        (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
           (*no indentation here, a new line*) *)
        (*   rec_flag rf *)
        (*   mutable_flag mf *)
        pp f "@[<2>%a in@;<1 -2>%a@]" (bindings reset_ctxt) (mf, rf, l)
          (expression ctxt) e
      | Texp_apply
          ( { exp_desc = Texp_extension ({ txt = "extension.exclave" }, PStr [])
            },
            [(Nolabel, sbody)] ) ->
        pp f "@[<2>exclave_ %a@]" (expression ctxt) sbody
      | Texp_apply (e, l) -> (
        if not (sugar_expr ctxt f x)
        then
          match view_fixity_of_exp e with
          | `Infix s -> (
            match l with
            | [((Nolabel, _) as arg1); ((Nolabel, _) as arg2)] ->
              (* FIXME associativity label_x_expression_param *)
              pp f "@[<2>%a@;%s@;%a@]"
                (label_x_expression_param reset_ctxt)
                arg1 s
                (label_x_expression_param ctxt)
                arg2
            | _ ->
              pp f "@[<2>%a %a@]" (simple_expr ctxt) e
                (list (label_x_expression_param ctxt))
                l)
          | `Prefix s -> (
            let s =
              if List.mem s ["~+"; "~-"; "~+."; "~-."]
                 &&
                 match l with
                 (* See #7200: avoid turning (~- 1) into (- 1) which is
                    parsed as an int literal *)
                 | [(_, { exp_desc = Texp_constant _ })] -> false
                 | _ -> true
              then String.sub s 1 (String.length s - 1)
              else s
            in
            match l with
            | [(Nolabel, x)] -> pp f "@[<2>%s@;%a@]" s (simple_expr ctxt) x
            | _ ->
              pp f "@[<2>%a %a@]" (simple_expr ctxt) e
                (list (label_x_expression_param ctxt))
                l)
          | _ ->
            pp f "@[<hov2>%a@]"
              (fun f (e, l) ->
                pp f "%a@ %a" (expression2 ctxt) e
                  (list (label_x_expression_param reset_ctxt))
                  l)
                (* reset here only because [function,match,try,sequence]
                   are lower priority *)
              (e, l))
      | Texp_construct (li, Some eo)
        when not (is_simple_construct (view_expr x)) -> (
        (* Not efficient FIXME*)
        match view_expr x with
        | `cons ls -> list (simple_expr ctxt) f ls ~sep:"@;::@;"
        | `normal -> pp f "@[<2>%a@;%a@]" longident_loc li (simple_expr ctxt) eo
        | _ -> assert false)
      | Texp_setfield (e1, li, e2) ->
        pp f "@[<2>%a.%a@ <-@ %a@]" (simple_expr ctxt) e1 longident_loc li
          (simple_expr ctxt) e2
      | Texp_ifthenelse (e1, e2, eo) ->
        (* @;@[<2>else@ %a@]@] *)
        let fmt : (_, _, _) format =
          "@[<hv0>@[<2>if@ %a@]@;@[<2>then@ %a@]%a@]"
        in
        let expression_under_ifthenelse = expression (under_ifthenelse ctxt) in
        pp f fmt expression_under_ifthenelse e1 expression_under_ifthenelse e2
          (fun f eo ->
            match eo with
            | Some x ->
              pp f "@;@[<2>else@;%a@]" (expression (under_semi ctxt)) x
            | None -> () (* pp f "()" *))
          eo
      | Texp_sequence _ ->
        let rec sequence_helper acc = function
          | { exp_desc = Texp_sequence (e1, e2); exp_attributes = [] } ->
            sequence_helper (e1 :: acc) e2
          | v -> List.rev (v :: acc)
        in
        let lst = sequence_helper [] x in
        pp f "@[<hv>%a@]" (list (expression (under_semi ctxt)) ~sep:";@;") lst
      | Texp_new li -> pp f "@[<hov2>new@ %a@]" longident_loc li
      | Texp_override l ->
        (* FIXME *)
        let string_x_expression f (s, e) =
          pp f "@[<hov2>%a@ =@ %a@]" ident_of_name s.txt (expression ctxt) e
        in
        pp f "@[<hov2>{<%a>}@]" (list string_x_expression ~sep:";") l
      | Texp_letmodule (s, me, e) ->
        pp f "@[<hov2>let@ module@ %s@ =@ %a@ in@ %a@]"
          (Option.value s.txt ~default:"_")
          (module_expr reset_ctxt) me (expression ctxt) e
      | Texp_letexception (cd, e) ->
        pp f "@[<hov2>let@ exception@ %a@ in@ %a@]"
          (extension_constructor ctxt)
          cd (expression ctxt) e
      | Texp_assert e -> pp f "@[<hov2>assert@ %a@]" (simple_expr ctxt) e
      | Texp_lazy e -> pp f "@[<hov2>lazy@ %a@]" (simple_expr ctxt) e
      (* Texp_poly: impossible but we should print it anyway, rather than
         assert false *)
      | Texp_open (o, e) ->
        pp f "@[<2>let open%s %a in@;%a@]"
          (override o.popen_override)
          (module_expr ctxt) o.popen_expr (expression ctxt) e
      | Texp_variant (l, Some eo) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name l (simple_expr ctxt) eo
      | Texp_letop { let_; ands; body } ->
        pp f "@[<2>@[<v>%a@,%a@] in@;<1 -2>%a@]" (binding_op ctxt) let_
          (list ~sep:"@," (binding_op ctxt))
          ands (expression ctxt) body
      | Texp_extension e -> extension ctxt f e
      | Texp_unreachable -> pp f "."
      | Texp_overwrite (e1, e2) ->
        (* Similar to the case of [Texp_stack] *)
        pp f "@[<hov2>overwrite_@ %a@ with@ %a@]" (expression2 reset_ctxt) e1
          (expression2 reset_ctxt) e2
      | Texp_quotation e -> pp f "@[<hov2><[%a]>@]" (expression ctxt) e
      | Texp_antiquotation e -> pp f "@[$%a@]" (simple_expr ctxt) e
      | Texp_hole _ -> pp f "_"
      | _ -> expression1 ctxt f x

  and expression1 ctxt f x =
    if x.exp_attributes <> []
    then expression ctxt f x
    else
      match x.exp_desc with
      | Texp_object cs -> pp f "%a" (class_structure ctxt) cs
      | _ -> expression2 ctxt f x
  (* used in [Texp_apply] *)

  and expression2 ctxt f x =
    if x.exp_attributes <> []
    then expression ctxt f x
    else
      match x.exp_desc with
      | Texp_field (e, li) ->
        pp f "@[<hov2>%a.%a@]" (simple_expr ctxt) e longident_loc li
      | Texp_unboxed_field (e, li) ->
        pp f "@[<hov2>%a.#%a@]" (simple_expr ctxt) e longident_loc li
      | Texp_send (e, s) ->
        pp f "@[<hov2>%a#%a@]" (simple_expr ctxt) e ident_of_name s.txt
      | _ -> simple_expr ctxt f x

  and simple_expr ctxt f x =
    if x.exp_attributes <> []
    then expression ctxt f x
    else
      match x.exp_desc with
      | Texp_construct _ when is_simple_construct (view_expr x) -> (
        match view_expr x with
        | `nil -> pp f "[]"
        | `tuple -> pp f "()"
        | `btrue -> pp f "true"
        | `bfalse -> pp f "false"
        | `list xs ->
          pp f "@[<hv0>[%a]@]"
            (list (expression (under_semi ctxt)) ~sep:";@;")
            xs
        | `simple x -> longident f x
        | _ -> assert false)
      | Texp_ident li -> longident_loc f li
      (* (match view_fixity_of_exp x with *)
      (* |`Normal -> longident_loc f li *)
      (* | `Prefix _ | `Infix _ -> pp f "( %a )" longident_loc li) *)
      | Texp_constant c -> constant f c
      | Texp_pack me -> pp f "(module@;%a)" (module_expr ctxt) me
      | Texp_tuple l -> labeled_tuple_expr ctxt f ~unboxed:false l
      | Texp_unboxed_tuple l -> labeled_tuple_expr ctxt f ~unboxed:true l
      | Texp_constraint (e, ct, m) -> (
        match ct, print_modes_in_old_syntax m with
        | None, true -> pp f "(%a %a)" legacy_modes m (expression ctxt) e
        | None, false ->
          pp f "(%a : _%a)" (expression ctxt) e optional_at_modes m
        | Some ct, _ ->
          pp f "(%a : %a)" (expression ctxt) e
            (core_type_with_optional_modes ctxt)
            (ct, m))
      | Texp_coerce (e, cto1, ct) ->
        pp f "(%a%a :> %a)" (expression ctxt) e
          (option (core_type ctxt) ~first:" : " ~last:" ")
          cto1 (* no sep hint*)
          (core_type ctxt) ct
      | Texp_variant (l, None) -> pp f "`%a" ident_of_name l
      | Texp_record (l, eo) -> record_expr ctxt f ~unboxed:false l eo
      | Texp_record_unboxed_product (l, eo) ->
        record_expr ctxt f ~unboxed:true l eo
      | Texp_array (mut, l) ->
        let punct = match mut with Immutable -> ':' | Mutable -> '|' in
        pp f "@[<0>@[<2>[%c%a%c]@]@]" punct
          (list (simple_expr (under_semi ctxt)) ~sep:";")
          l punct
      | Texp_idx (ba, uas) ->
        pp f "(%a%a)" (block_access ctxt) ba (list unboxed_access ~sep:"") uas
      | Texp_comprehension comp -> comprehension_expr ctxt f comp
      | Texp_while (e1, e2) ->
        let fmt : (_, _, _) format = "@[<2>while@;%a@;do@;%a@;done@]" in
        pp f fmt (expression ctxt) e1 (expression ctxt) e2
      | Texp_for (s, e1, e2, df, e3) ->
        let fmt : (_, _, _) format =
          "@[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]"
        in
        let expression = expression ctxt in
        pp f fmt (pattern ctxt) s expression e1 direction_flag df expression e2
          expression e3
      | _ -> paren true (expression ctxt) f x

  and attributes ctxt f l = List.iter (attribute ctxt f) l

  and item_attributes ctxt f l = List.iter (item_attribute ctxt f) l

  and attribute ctxt f a =
    pp f "@[<2>[@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

  and item_attribute ctxt f a =
    pp f "@[<2>[@@@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

  and floating_attribute ctxt f a =
    pp f "@[<2>[@@@@@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

  and extension ctxt f (s, e) = pp f "@[<2>[%%%s@ %a]@]" s.txt (payload ctxt) e

  and exception_declaration ctxt f x =
    pp f "@[<hov2>exception@ %a@]%a"
      (extension_constructor ctxt)
      x.tyexn_constructor (item_attributes ctxt) x.tyexn_attributes

  and kind_abbrev ctxt f name jkind =
    pp f "@[<hov2>kind_abbrev_@ %a@ =@ %a@]" string_loc name
      (jkind_annotation ctxt) jkind

  and pp_print_params_then_equals ctxt f x =
    if x.exp_attributes <> []
    then pp f "=@;%a" (expression ctxt) x
    else
      match x.exp_desc with
      | Texp_function (params, constraint_, body) ->
        function_params_then_body ctxt f params constraint_ body ~delimiter:"="
      | _ -> pp_print_exp_newtype ctxt "=" f x

  and poly_type ctxt core_type f (vars, typ) =
    pp f "type@;%a.@;%a"
      (list ~sep:"@;" (tyvar_loc_jkind pp_print_string))
      vars (core_type ctxt) typ

  and poly_type_with_optional_modes ctxt f (vars, typ, modes) =
    match modes with
    | [] -> poly_type ctxt core_type f (vars, typ)
    | _ :: _ ->
      pp f "%a%a"
        (poly_type ctxt core_type1)
        (vars, typ) optional_at_modes modes

  (* transform [f = fun g h -> ..] to [f g h = ... ] could be improved *)
  and binding ctxt f { vb_pat = p; vb_expr = x; _ } =
    (* .vb_attributes have already been printed by the caller, #bindings *)
    match ct with
    | Some (Pvc_constraint { locally_abstract_univars = []; typ }) ->
      pp f "%a@;:@;%a@;=@;%a" (simple_pattern ctxt) p
        (core_type_with_optional_modes ctxt)
        (typ, modes) (expression ctxt) x
    | Some (Pvc_constraint { locally_abstract_univars = vars; typ }) ->
      pp f "%a@;: %a@;=@;%a" (simple_pattern ctxt) p
        (poly_type_with_optional_modes ctxt)
        (List.map (fun x -> x, None) vars, typ, modes)
        (expression ctxt) x
    | Some (Pvc_coercion { ground = None; coercion }) ->
      pp f "%a@;:>@;%a@;=@;%a" (simple_pattern ctxt) p (core_type ctxt) coercion
        (expression ctxt) x
    | Some (Pvc_coercion { ground = Some ground; coercion }) ->
      pp f "%a@;:%a@;:>@;%a@;=@;%a" (simple_pattern ctxt) p (core_type ctxt)
        ground (core_type ctxt) coercion (expression ctxt) x
    | None -> (
      (* CR layouts 1.5: We just need to check for [is_desugared_gadt] because
         the parser hasn't been upgraded to parse [let x : type a. ... = ...] as
         [Pvb_constraint] as it has been upstream. Once we move to the 5.2
         parsetree encoding of type annotations.
      *)
      let tyvars_str tyvars = List.map (fun v -> v.txt) tyvars in
      let tyvars_jkind_str tyvars =
        List.map (fun (v, _jkind) -> v.txt) tyvars
      in
      let is_desugared_gadt p e =
        let gadt_pattern =
          match p with
          | { pat_desc =
                Tpat_constraint
                  ( ({ pat_desc = Tpat_var _ } as pat),
                    Some { ctyp_desc = Ttyp_poly (args_tyvars, rt) },
                    _ );
              pat_attributes = []
            } ->
            Some (pat, args_tyvars, rt)
          | _ -> None
        in
        let rec gadt_exp tyvars e =
          match e with
          (* no need to handle jkind annotations here; the extracted variables
             don't get printed -- they're just used to decide how to print *)
          | { exp_desc = Texp_newtype (tyvar, _jkind, e); exp_attributes = [] }
            ->
            gadt_exp (tyvar :: tyvars) e
          | { exp_desc = Texp_constraint (e, Some ct, _); exp_attributes = [] }
            ->
            Some (List.rev tyvars, e, ct)
          | _ -> None
        in
        let gadt_exp = gadt_exp [] e in
        match gadt_pattern, gadt_exp with
        | Some (p, pt_tyvars, pt_ct), Some (e_tyvars, e, e_ct)
          when tyvars_jkind_str pt_tyvars = tyvars_str e_tyvars ->
          let ety = Ast_helper.Typ.varify_constructors e_tyvars e_ct in
          if ety = pt_ct then Some (p, pt_tyvars, e_ct, e) else None
        | _ -> None
      in
      match is_desugared_gadt p x with
      | Some (p, (_ :: _ as tyvars), ct, e) ->
        pp f "%a@;: %a@;=@;%a" (simple_pattern ctxt) p
          (poly_type_with_optional_modes ctxt)
          (tyvars, ct, modes) (expression ctxt) e
      | _ -> (
        match p with
        | { pat_desc = Tpat_var _; pat_attributes = [] } -> (
          match modes with
          | [] ->
            pp f "%a@ %a" (simple_pattern ctxt) p
              (pp_print_params_then_equals ctxt)
              x
          | _ ->
            pp f "(%a%a)@ %a" (simple_pattern ctxt) p optional_at_modes modes
              (pp_print_params_then_equals ctxt)
              x)
        | _ ->
          pp f "%a%a@;=@;%a" (pattern ctxt) p optional_at_modes modes
            (expression ctxt) x))

  (* [in] is not printed *)
  and bindings ctxt f (mf, rf, l) =
    let binding kwd mf rf f x =
      (* The other modes are printed inside [binding] *)
      let legacy, x =
        if print_modes_in_old_syntax x.vb_modes
        then x.vb_modes, { x with vb_modes = [] }
        else [], x
      in
      pp f "@[<2>%s %a%a%a%a@]%a" kwd mutable_flag mf rec_flag rf
        optional_legacy_modes legacy (binding ctxt) x (item_attributes ctxt)
        x.vb_attributes
    in
    match l with
    | [] -> ()
    | [x] -> binding "let" mf rf f x
    | x :: xs ->
      pp f "@[<v>%a@,%a@]" (binding "let" mf rf) x
        (list ~sep:"@," (binding "and" Immutable Nonrecursive))
        xs

  and binding_op ctxt f x =
    match x.pbop_pat, x.pbop_exp with
    | ( { pat_desc = Tpat_var { txt = pvar; _ }; pat_attributes = []; _ },
        { exp_desc = Texp_ident { txt = Lident evar; _ };
          exp_attributes = [];
          _
        } )
      when pvar = evar ->
      pp f "@[<2>%s %s@]" x.pbop_op.txt evar
    | pat, exp ->
      pp f "@[<2>%s %a@;=@;%a@]" x.pbop_op.txt (pattern ctxt) pat
        (expression ctxt) exp

  (* Don't just use [core_type] because we do not want parens around params
     with jkind annotations *)
  and core_type_param f ct =
    match ct.ctyp_desc with
    | Ttyp_any None -> pp f "_"
    | Ttyp_any (Some jk) -> pp f "_ : %a" (jkind_annotation reset_ctxt) jk
    | Ttyp_var (s, None) -> tyvar f s
    | Ttyp_var (s, Some jk) ->
      pp f "%a : %a" tyvar s (jkind_annotation reset_ctxt) jk
    | _ -> Misc.fatal_error "unexpected type in core_type_param"

  and type_param f (ct, (a, b)) =
    pp f "%s%s%a" (type_variance a) (type_injectivity b) core_type_param ct

  and type_params f = function
    | [] -> ()
    (* Normally, one param doesn't get parentheses, but it does when there is
       a jkind annotation. *)
    | [(({ ctyp_desc = Ttyp_any (Some _) | Ttyp_var (_, Some _) }, _) as param)]
      ->
      pp f "(%a) " type_param param
    | l -> pp f "%a " (list type_param ~first:"(" ~last:")" ~sep:",@;") l

  and case_list ctxt f l : unit =
    let aux f { c_lhs; c_guard; c_rhs } =
      pp f "@;| @[<2>%a%a@;->@;%a@]" (pattern ctxt) c_lhs
        (option (expression ctxt) ~first:"@;when@;")
        c_guard
        (expression (under_pipe ctxt))
        c_rhs
    in
    list aux f l ~sep:""

  and label_x_expression_param ctxt f (l, e) =
    let simple_name =
      match e with
      | { exp_desc = Texp_ident { txt = Lident l; _ }; exp_attributes = [] } ->
        Some l
      | _ -> None
    in
    match l with
    | Nolabel -> expression2 ctxt f e (* level 2*)
    | Optional str ->
      if Some str = simple_name
      then pp f "?%a" ident_of_name str
      else pp f "?%a:%a" ident_of_name str (simple_expr ctxt) e
    | Labelled lbl | Position lbl ->
      if Some lbl = simple_name
      then pp f "~%a" ident_of_name lbl
      else pp f "~%a:%a" ident_of_name lbl (simple_expr ctxt) e

  and tuple_component ctxt f (l, e) =
    let simple_name =
      match e with
      | { exp_desc = Texp_ident { txt = Lident l; _ }; exp_attributes = [] } ->
        Some l
      | _ -> None
    in
    match simple_name, l with
    (* Labeled component can be represented with pun *)
    | Some simple_name, Some lbl when String.equal simple_name lbl ->
      pp f "~%s" lbl
    (* Labeled component general case *)
    | _, Some lbl -> pp f "~%s:%a" lbl (simple_expr ctxt) e
    (* Unlabeled component *)
    | _, None -> expression2 ctxt f e (* level 2*)

  and block_access ctxt f = function
    | Baccess_field li -> pp f ".%a" longident_loc li
    | Baccess_array (mut, index_kind, index) ->
      let dotop = match mut with Mutable -> "." | Immutable -> ".:" in
      let suffix =
        match index_kind with
        | Index_int -> ""
        | Index_unboxed_int64 -> "L"
        | Index_unboxed_int32 -> "l"
        | Index_unboxed_int16 -> "S"
        | Index_unboxed_int8 -> "s"
        | Index_unboxed_nativeint -> "n"
      in
      pp f "%s%s(%a)" dotop suffix (expression ctxt) index
    | Baccess_block (mut, index) ->
      let s = match mut with Mutable -> "idx_mut" | Immutable -> "idx_imm" in
      pp f ".%s(%a)" s (expression ctxt) index

  and unboxed_access f = function
    | Uaccess_unboxed_field li -> pp f ".#%a" longident_loc li

  and comprehension_expr ctxt f cexp =
    let punct, comp =
      match cexp with
      | Tcomp_list_comprehension comp -> "", comp
      | Tcomp_array_comprehension (amut, comp) ->
        let punct = match amut with Mutable -> "|" | Immutable -> ":" in
        punct, comp
    in
    comprehension ctxt f ~open_:("[" ^ punct) ~close:(punct ^ "]") comp

  and comprehension ctxt f ~open_ ~close cexp =
    let { comp_body = body; comp_clauses = clauses } = cexp in
    pp f "@[<hv0>@[<hv2>%s%a@ @[<hv2>%a@]%s@]@]" open_ (expression ctxt) body
      (list ~sep:"@ " (comprehension_clause ctxt))
      clauses close

  and comprehension_clause ctxt f x =
    match x with
    | Texp_comp_for bindings ->
      pp f "@[for %a@]"
        (list ~sep:"@]@ @[and " (comprehension_binding ctxt))
        bindings
    | Texp_comp_when cond -> pp f "@[when %a@]" (expression ctxt) cond

  and comprehension_binding ctxt f x =
    let { comp_cb_iterator = iterator;
          comp_cb_pattern = pat;
          comp_cb_attributes = attrs
        } =
      x
    in
    pp f "%a%a %a" (attributes ctxt) attrs (pattern ctxt) pat
      (comprehension_iterator ctxt)
      iterator

  and comprehension_iterator ctxt f x =
    match x with
    | Tcomp_range { start; stop; direction } ->
      pp f "=@ %a %a%a" (expression ctxt) start direction_flag direction
        (expression ctxt) stop
    | Tcomp_in seq -> pp f "in %a" (expression ctxt) seq

  and function_param ctxt f pparam_desc =
    match pparam_desc with
    | Tparam_val (a, b, c) -> label_exp ctxt f (a, b, c)
    | Tparam_newtype (ty, None) -> pp f "(type %a)" ident_of_name ty.txt
    | Tparam_newtype (ty, Some annot) ->
      pp f "(type %a : %a)" ident_of_name ty.txt (jkind_annotation ctxt) annot

  and function_body ctxt f x =
    match x with
    | Tfunction_body body -> expression ctxt f body
    | Tfunction_cases (cases, _, attrs) ->
      pp f "@[<hv>function%a%a@]" (item_attributes ctxt) attrs (case_list ctxt)
        cases

  and function_constraint ctxt f x =
    (* We don't print [mode_annotations], which describes the whole function and goes on the
       [let] binding. *)
    (* Enable warning 9 to ensure that the record pattern doesn't miss any field.
  *)
    match[@ocaml.warning "+9"] x with
    | { ret_type_constraint = Some (Pconstraint ty); ret_mode_annotations; _ }
      ->
      pp f "@;:@;%a@;"
        (core_type_with_optional_modes ctxt)
        (ty, ret_mode_annotations)
    | { ret_type_constraint = Some (Pcoerce (ty1, ty2)); _ } ->
      pp f "@;%a:>@;%a"
        (option ~first:":@;" (core_type ctxt))
        ty1 (core_type ctxt) ty2
    | { ret_type_constraint = None; ret_mode_annotations; _ } ->
      pp f "%a" optional_at_modes ret_mode_annotations

  and function_params_then_body ctxt f params constraint_ body ~delimiter =
    let pp_params f =
      match params with
      | [] -> ()
      | _ :: _ -> pp f "%a@;" (list (function_param ctxt) ~sep:"@ ") params
    in
    pp f "%t%a%s@;%a" pp_params (function_constraint ctxt) constraint_ delimiter
      (function_body (under_functionrhs ctxt))
      body

  and labeled_tuple_expr ctxt f ~unboxed x =
    pp f "@[<hov2>%s(%a)@]"
      (if unboxed then "#" else "")
      (list (tuple_component ctxt) ~sep:",@;")
      x

  and record_expr ctxt f ~unboxed l eo =
    let longident_x_expression f (li, e) =
      match e with
      | { exp_desc = Texp_ident { txt; _ }; exp_attributes = []; _ }
        when li.txt = txt ->
        pp f "@[<hov2>%a@]" longident_loc li
      | _ -> pp f "@[<hov2>%a@;=@;%a@]" longident_loc li (simple_expr ctxt) e
    in
    let hash = if unboxed then "#" else "" in
    pp f "@[<hv0>@[<hv2>%s{@;%a%a@]@;}@]" (* "@[<hov2>%s{%a%a}@]" *) hash
      (option ~last:" with@;" (simple_expr ctxt))
      eo
      (list longident_x_expression ~sep:";@;")
      l

  (******************************************************************************)
  (* All exported functions must be defined or redefined below here and wrapped in
     [export_printer] in order to ensure they are invariant with respecto which
     language extensions are enabled. *)

  let Language_extension.For_pprintast.{ print_with_maximal_extensions } =
    Language_extension.For_pprintast.make_printer_exporter ()

  let print_reset_with_maximal_extensions f =
    print_with_maximal_extensions (f reset_ctxt)

  let expression f x = pp f "@[%a@]" (expression reset_ctxt) x

  let expression = print_with_maximal_extensions expression

  let string_of_expression x =
    ignore (flush_str_formatter ());
    let f = str_formatter in
    expression f x;
    flush_str_formatter ()

  let longident = print_with_maximal_extensions longident

  let core_type = print_reset_with_maximal_extensions core_type

  let pattern = print_reset_with_maximal_extensions pattern

  let binding = print_reset_with_maximal_extensions binding

  let type_declaration = print_reset_with_maximal_extensions type_declaration

  let jkind_annotation = print_reset_with_maximal_extensions jkind_annotation
end

(* Translation *)

let transl_quote (_transl_meta : Typedtree.expression -> Lambda.lambda)
    (_exp : Typedtree.expression) loc =
  Lam.string loc "Hello, world!"
