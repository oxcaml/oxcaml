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
    | `single of expression
    | `multiple
    | `simple of Longident.t
    | `tuple
    | `btrue
    | `bfalse ]

  let view_expr x =
    match x.exp_desc with
    | Texp_construct ({ txt = Lident "()"; _ }, _, [], _) -> `tuple
    | Texp_construct ({ txt = Lident "true"; _ }, _, [], _) -> `btrue
    | Texp_construct ({ txt = Lident "false"; _ }, _, [], _) -> `bfalse
    | Texp_construct ({ txt = Lident "[]"; _ }, _, [], _) -> `nil
    | Texp_construct ({ txt = Lident "::"; _ }, _, [_; _], _) ->
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
    | Texp_construct (_, _, [a], _) -> `single a
    | _ -> `multiple

  let is_simple_construct : construct -> bool = function
    | `nil | `tuple | `list _ | `simple _ | `btrue | `bfalse -> true
    | `cons _ | `single _ | `multiple -> false

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
    | Const_int i ->
      let i = Int.to_string i in
      paren (first_is '-' i) (fun f -> pp f "%s") f i
    | Const_float i -> paren (first_is '-' i) (fun f -> pp f "%s") f i
    | _ -> Misc.fatal_error "Translquote.constant"

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

  let drop_pat_sort : 'a * 'b * Jkind.sort -> 'a * 'b = fun (x, y, _) -> x, y

  let with_no_label : 'a -> string option * 'a = fun x -> None, x

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
  let legacy_mode f (Parsetree.Mode s) =
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
  let mode f (Parsetree.Mode s) = pp_print_string f s

  let modes f m = pp_print_list ~pp_sep:(fun f () -> pp f " ") mode f m

  let optional_at_modes f m =
    match m with [] -> () | m -> pp f " %@ %a" modes m

  let modality f { txt = Parsetree.Modality m; loc = _ } = pp_print_string f m

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
    List.for_all (function Parsetree.Mode "local" -> true | _ -> false)

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
    | Pjk_mod (t, ms) -> (
      match ms with
      | [] -> Misc.fatal_error "malformed jkind annotation"
      | _ :: _ ->
        Misc.pp_parens_if nested
          (fun f (t, ms) ->
            pp f "%a mod %a" (jkind_annotation ~nested:true ctxt) t modes ms)
          f
          (t, List.map Location.get_txt ms))
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
      (* CR jbachurski: Why does Ttyp_arrow not have mode annotations on it? *)
      | Ttyp_arrow (l, ct1, ct2) ->
        pp f "@[<2>%a@;->@;%a@]" (* FIXME remove parens later *)
          (type_with_label ctxt) (l, ct1, []) (return_type ctxt) (ct2, [])
      | Ttyp_alias (ct, s, j) ->
        pp f "@[<2>%a@;as@;%a@]" (core_type1 ctxt) ct tyvar_loc_option_jkind
          (s, j)
      | Ttyp_poly ([], ct) -> core_type ctxt f ct
      | Ttyp_poly (sl, ct) ->
        pp f "@[<2>%a%a@]"
          (fun f l ->
            match l with
            | [] -> ()
            | _ -> pp f "%a@;.@;" (list (tyvar_jkind tyvar) ~sep:"@;") l)
          sl (core_type ctxt) ct
      | Ttyp_of_kind jkind ->
        pp f "@[(type@ :@ %a)@]" (jkind_annotation reset_ctxt) jkind
      | _ -> pp f "@[<2>%a@]" (core_type1 ctxt) x

  and core_type1 ctxt f x =
    if x.ctyp_attributes <> []
    then core_type ctxt f x
    else
      match x.ctyp_desc with
      | Ttyp_var (None, jkind) -> (tyvar_jkind tyvar) f ("_", jkind)
      | Ttyp_var (Some s, jkind) -> (tyvar_jkind tyvar) f (s, jkind)
      | Ttyp_tuple tl ->
        pp f "(%a)" (list (labeled_core_type1 ctxt) ~sep:"@;*@;") tl
      | Ttyp_unboxed_tuple l -> core_type1_labeled_tuple ctxt f ~unboxed:true l
      | Ttyp_constr (_, li, l) ->
        pp f (* "%a%a@;" *) "%a%a"
          (fun f l ->
            match l with
            | [] -> ()
            | [x] -> pp f "%a@;" (core_type1 ctxt) x
            | _ -> list ~first:"(" ~last:")@;" (core_type ctxt) ~sep:",@;" f l)
          l longident_loc li
      | Ttyp_variant (l, closed, low) ->
        let first_is_inherit =
          match l with { rf_desc = Tinherit _ } :: _ -> true | _ -> false
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
          match x.of_desc with
          | OTtag (l, ct) ->
            (* Cf #7200 *)
            pp f "@[<hov2>%a: %a@ %a@ @]" ident_of_name l.txt (core_type ctxt)
              ct (attributes ctxt) x.of_attributes
          | OTinherit ct -> pp f "@[<hov2>%a@ @]" (core_type ctxt) ct
        in
        let field_var f = function
          | Asttypes.Closed -> ()
          | Asttypes.Open -> (
            match l with [] -> pp f ".." | _ -> pp f " ;..")
        in
        pp f "@[<hov2><@ %a%a@ > @]"
          (list core_field_type ~sep:";")
          l field_var o (* Cf #7200 *)
      | Ttyp_class (_, li, l) ->
        (*FIXME*)
        pp f "@[<hov2>%a@;#%a@]"
          (list (core_type ctxt) ~sep:"," ~first:"(" ~last:")")
          l longident_loc li
      | Ttyp_package { pack_txt = lid; pack_fields = cstrs } -> (
        let aux f (s, ct) =
          pp f "type %a@ =@ %a" longident_loc s (core_type ctxt) ct
        in
        match cstrs with
        | [] -> pp f "@[<hov2>(module@ %a)@]" longident_loc lid
        | _ ->
          pp f "@[<hov2>(module@ %a@ with@ %a)@]" longident_loc lid
            (list aux ~sep:"@ and@ ") cstrs)
      | Ttyp_open (_, li, ct) ->
        pp f "@[<hov2>%a.(%a)@]" longident_loc li (core_type ctxt) ct
      | Ttyp_quote t -> pp f "@[<hov2><[%a]>@]" (core_type ctxt) t
      | Ttyp_splice t -> pp f "@[<hov2>$(%a)@]" (core_type ctxt) t
      | Ttyp_arrow _ | Ttyp_alias _ | Ttyp_poly _ | Ttyp_of_kind _ ->
        paren true (core_type ctxt) f x
      | Ttyp_call_pos -> Misc.fatal_error "Transl.core_type1: Ttyp_call_pos"

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
            | _ -> pp f "%a@;.@;" (list (tyvar_jkind tyvar) ~sep:"@;") l)
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
      | Tpat_alias (p, _, s, _, _, _, _) ->
        pp f "@[<2>%a@;as@;%a@]" (pattern ctxt) p ident_of_name s.txt
      | _ -> pattern_or ctxt f x

  and pattern_or ctxt f x =
    let rec left_associative x acc =
      match x with
      | { pat_desc = Tpat_or (p1, p2, _); pat_attributes = [] } ->
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
            Tpat_construct ({ txt = Lident "::"; _ }, _, _, Some ([], inner_pat));
          pat_attributes = []
        } -> (
        match inner_pat.pat_desc with
        | Tpat_tuple [(None, pat1); (None, pat2)] ->
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
      | Tpat_construct
          ({ txt = Lident ("()" | "[]" | "true" | "false"); _ }, _, _, _) ->
        simple_pattern ctxt f x
      | Tpat_construct (({ txt; _ } as li), po, _, _) -> (
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
    assert (x.pat_extra = []);
    if x.pat_attributes <> []
    then pattern ctxt f x
    else
      match x.pat_desc with
      | Tpat_construct
          ({ txt = Lident (("()" | "[]" | "true" | "false") as x); _ }, None) ->
        pp f "%s" x
      | Tpat_any -> pp f "_"
      | Tpat_var (_, { txt; _ }, _, _, _) -> ident_of_name f txt
      | Tpat_array (mut, _, l) ->
        let punct = match mut with Mutable _ -> '|' | Immutable -> ':' in
        pp f "@[<2>[%c%a%c]@]" punct (list (pattern1 ctxt) ~sep:";") l punct
      | Tpat_record (l, closed) -> record_pattern ctxt f ~unboxed:false l closed
      | Tpat_record_unboxed_product (l, closed) ->
        record_pattern ctxt f ~unboxed:true l closed
      | Tpat_tuple l -> labeled_tuple_pattern ctxt f ~unboxed:false l Closed
      | Tpat_unboxed_tuple l ->
        labeled_tuple_pattern ctxt f ~unboxed:true (List.map drop_pat_sort l)
          Closed
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
    let closed_flag f = function Closed -> () | Open -> pp f ",@;.." in
    pp f "@[<1>%s(%a%a)@]"
      (if unboxed then "#" else "")
      (list ~sep:",@;" (labeled_pattern1 ctxt))
      l closed_flag closed

  (* CR jbachurski: pattern2 for modes *)

  (** for special treatment of modes in labeled expressions *)
  and simple_pattern1 ctxt f p =
    match p.pat_desc with
    | Tpat_constraint _ -> pp f "(%a)" (pattern1 ctxt) p
    | _ -> simple_pattern ctxt f p

  and trivial_pattern _ctxt f (p : Parsetree.pattern) =
    assert (p.ppat_attributes = []);
    match p.ppat_desc with
    | Ppat_any -> pp f "_"
    | Ppat_var { txt; _ } -> ident_of_name f txt
    | _ -> Misc.fatal_error "Translquote.trivial_pattern"

  (* CR jbachurski: cut [sugar_expr] *)

  and expression ctxt f x =
    assert (x.exp_extra = []);
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
      | Texp_function { params; body } -> (
        match params with
        (* Omit [fun] if there are no params. *)
        | [] ->
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
        | _ :: _ ->
          pp f "@[<2>fun@;%t@]" (fun f ->
              function_params_then_body ctxt f params body ~delimiter:"->"))
      | Texp_match (e, _, l, _) ->
        pp f "@[<hv0>@[<hv0>@[<2>match %a@]@ with@]%a@]" (expression reset_ctxt)
          e (case_list ctxt) l
      | Texp_try (e, l) ->
        pp f "@[<0>@[<hv2>try@ %a@]@ @[<0>with%a@]@]"
          (* "try@;@[<2>%a@]@\nwith@\n%a"*)
          (expression reset_ctxt)
          e (case_list ctxt) l
      | Texp_let (rf, l, e) ->
        (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
           (*no indentation here, a new line*) *)
        (*   rec_flag rf *)
        (*   mutable_flag mf *)
        pp f "@[<2>%a in@;<1 -2>%a@]" (bindings reset_ctxt) (Immutable, rf, l)
          (expression ctxt) e
      | Texp_letmutable (l, e) ->
        (* pp f "@[<2>let %a%a in@;<1 -2>%a@]"
           (*no indentation here, a new line*) *)
        (*   rec_flag rf *)
        (*   mutable_flag mf *)
        pp f "@[<2>%a in@;<1 -2>%a@]" (bindings reset_ctxt)
          (Mutable, Nonrecursive, [l])
          (expression ctxt) e
      | Texp_apply (e, l, _, _, _) -> (
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
               | [(_, Arg ({ exp_desc = Texp_constant _ }, _))] -> false
               | _ -> true
            then String.sub s 1 (String.length s - 1)
            else s
          in
          match l with
          | [(Nolabel, Arg (x, _))] ->
            pp f "@[<2>%s@;%a@]" s (simple_expr ctxt) x
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
      | Texp_construct (li, _, eo, _)
        when not (is_simple_construct (view_expr x)) -> (
        (* Not efficient FIXME*)
        match view_expr x with
        | `cons ls -> list (simple_expr ctxt) f ls ~sep:"@;::@;"
        | `single eo ->
          pp f "@[<2>%a@;%a@]" longident_loc li (simple_expr ctxt) eo
        | `multiple ->
          pp f "@[<2>%a@;%a@]" longident_loc li
            (labeled_tuple_expr ctxt ~unboxed:false)
            (List.map with_no_label eo)
        | _ -> assert false)
      | Texp_setfield (e1, _, li, _, e2) ->
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
          | { exp_desc = Texp_sequence (e1, _, e2); exp_attributes = [] } ->
            sequence_helper (e1 :: acc) e2
          | v -> List.rev (v :: acc)
        in
        let lst = sequence_helper [] x in
        pp f "@[<hv>%a@]" (list (expression (under_semi ctxt)) ~sep:";@;") lst
      | Texp_new (_, li, _, _) -> pp f "@[<hov2>new@ %a@]" longident_loc li
      | Texp_override (_, l) ->
        (* FIXME *)
        let string_x_expression f (_, s, e) =
          pp f "@[<hov2>%a@ =@ %a@]" ident_of_name s.txt (expression ctxt) e
        in
        pp f "@[<hov2>{<%a>}@]" (list string_x_expression ~sep:";") l
      | Texp_letmodule _ -> Misc.fatal_error "Translquote: Texp_letmodule"
      | Texp_letexception _ -> Misc.fatal_error "Translquote: Texp_letexception"
      | Texp_assert (e, _) -> pp f "@[<hov2>assert@ %a@]" (simple_expr ctxt) e
      | Texp_lazy e -> pp f "@[<hov2>lazy@ %a@]" (simple_expr ctxt) e
      | Texp_open _ -> Misc.fatal_error "Translquote: Texp_letexception"
      | Texp_variant (l, Some (eo, _)) ->
        pp f "@[<2>`%a@;%a@]" ident_of_name l (simple_expr ctxt) eo
      | Texp_letop { let_; ands; body } ->
        pp f "@[<2>@[<v>%a@,%a@] in@;<1 -2>%a@]" (binding_op ctxt) let_
          (list ~sep:"@," (binding_op ctxt))
          ands (expression ctxt) body
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
    if x.exp_attributes <> [] then expression ctxt f x else expression2 ctxt f x
  (* used in [Texp_apply] *)

  and expression2 ctxt f x =
    if x.exp_attributes <> []
    then expression ctxt f x
    else
      match x.exp_desc with
      | Texp_field (e, _, li, _, _, _) ->
        pp f "@[<hov2>%a.%a@]" (simple_expr ctxt) e longident_loc li
      | Texp_unboxed_field (e, _, li, _, _) ->
        pp f "@[<hov2>%a.#%a@]" (simple_expr ctxt) e longident_loc li
      | Texp_send _ -> Misc.fatal_error "Translquote.expression2: Texp_send"
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
      | Texp_ident (_, li, _, _, _) -> longident_loc f li
      (* (match view_fixity_of_exp x with *)
      (* |`Normal -> longident_loc f li *)
      (* | `Prefix _ | `Infix _ -> pp f "( %a )" longident_loc li) *)
      | Texp_constant c -> constant f c
      | Texp_pack _ -> Misc.fatal_error "Translquote.simple_expr: Texp_pack"
      (* CR jbachurski: mode? *)
      | Texp_tuple (l, _) -> labeled_tuple_expr ctxt f ~unboxed:false l
      | Texp_unboxed_tuple l ->
        labeled_tuple_expr ctxt f ~unboxed:true (List.map drop_pat_sort l)
      | Texp_variant (l, None) -> pp f "`%a" ident_of_name l
      (* CR jbachurski: mode? *)
      | Texp_record
          { fields = l;
            representation = _;
            extended_expression = eo;
            alloc_mode = _
          } ->
        record_expr ctxt f ~unboxed:false l (Option.map (fun (e, _, _) -> e) eo)
      (* CR jbachurski: mode? *)
      | Texp_record_unboxed_product
          { fields = l; representation = _; extended_expression = eo } ->
        record_expr ctxt f ~unboxed:true l (Option.map (fun (e, _) -> e) eo)
      | Texp_array (mut, _, l, _) ->
        let punct = match mut with Immutable -> ':' | Mutable _ -> '|' in
        pp f "@[<0>@[<2>[%c%a%c]@]@]" punct
          (list (simple_expr (under_semi ctxt)) ~sep:";")
          l punct
      | Texp_idx (ba, uas) ->
        pp f "(%a%a)" (block_access ctxt) ba (list unboxed_access ~sep:"") uas
      | (Texp_list_comprehension _ | Texp_array_comprehension _) as comp ->
        comprehension_expr ctxt f comp
      | Texp_while { wh_cond = e1; wh_body = e2; wh_body_sort = _ } ->
        let fmt : (_, _, _) format = "@[<2>while@;%a@;do@;%a@;done@]" in
        pp f fmt (expression ctxt) e1 (expression ctxt) e2
      | Texp_for
          { for_pat = s;
            for_from = e1;
            for_to = e2;
            for_dir = df;
            for_body = e3;
            for_debug_uid = _;
            for_body_sort = _
          } ->
        let fmt : (_, _, _) format =
          "@[<hv0>@[<hv2>@[<2>for %a =@;%a@;%a%a@;do@]@;%a@]@;done@]"
        in
        let expression = expression ctxt in
        pp f fmt (pattern ctxt) s expression e1 direction_flag df expression e2
          expression e3
      | _ -> paren true (expression ctxt) f x

  and attributes ctxt f l = List.iter (attribute ctxt f) l

  and item_attributes ctxt f l = List.iter (item_attribute ctxt f) l

  and attribute ctxt f (a : attribute) =
    pp f "@[<2>[@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

  and item_attribute ctxt f (a : attribute) =
    pp f "@[<2>[@@@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

  and floating_attribute ctxt f (a : attribute) =
    pp f "@[<2>[@@@@@@%s@ %a]@]" a.attr_name.txt (payload ctxt) a.attr_payload

  and extension ctxt f (s, e) = pp f "@[<2>[%%%s@ %a]@]" s.txt (payload ctxt) e

  and kind_abbrev ctxt f name jkind =
    pp f "@[<hov2>kind_abbrev_@ %a@ =@ %a@]" string_loc name
      (jkind_annotation ctxt) jkind

  and payload ctxt f : Parsetree.payload -> unit = function
    | PStr [{ pstr_desc = Pstr_eval (e, attrs) }] ->
      pp f "@[<2>%a@]%a" Pprintast.expression e (item_attributes ctxt) attrs
    | PStr _ -> Misc.fatal_error "Translquote.payload: PStr"
    | PTyp x ->
      pp f ":@ ";
      Pprintast.core_type f x
    | PSig _ -> Misc.fatal_error "Translquote.payload: PSig"
    | PPat (x, None) ->
      pp f "?@ ";
      Pprintast.pattern f x
    | PPat (x, Some e) ->
      pp f "?@ ";
      Pprintast.pattern f x;
      pp f " when ";
      Pprintast.expression f e

  and pp_print_params_then_equals ctxt f x =
    if x.exp_attributes <> []
    then pp f "=@;%a" (expression ctxt) x
    else
      match x.exp_desc with
      | Texp_function { params; body; _ } ->
        function_params_then_body ctxt f params body ~delimiter:"="
      | _ ->
        Misc.fatal_error
          "Translquote.pp_print_params_then_equals: case for \
           pp_print_pexp_newtype"

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
    (* CR jbachurski: modes? *)
    (* CR jbachurski: printing GADTs *)
    match p with
    | { pat_desc = Tpat_var _; pat_attributes = [] } ->
      pp f "%a@ %a" (simple_pattern ctxt) p (pp_print_params_then_equals ctxt) x
    | _ -> pp f "%a@;=@;%a" (pattern ctxt) p (expression ctxt) x

  (* [in] is not printed *)
  and bindings ctxt f (mf, rf, l) =
    let binding kwd mf rf f x =
      (* CR jbachurski: No modes *)
      pp f "@[<2>%s %a%a%a@]%a" kwd mutable_flag mf rec_flag rf (binding ctxt) x
        (item_attributes ctxt) x.vb_attributes
    in
    match l with
    | [] -> ()
    | [x] -> binding "let" mf rf f x
    | x :: xs ->
      pp f "@[<v>%a@,%a@]" (binding "let" mf rf) x
        (list ~sep:"@," (binding "and" Immutable Nonrecursive))
        xs

  and binding_op ctxt f
      { bop_op_name = { txt = name; loc = _ }; bop_exp = exp; _ } =
    pp f "@[<2>%s %a@;=@;%a@]" name (pattern ctxt) pat (expression ctxt) exp

  (* Don't just use [core_type] because we do not want parens around params
     with jkind annotations *)
  and core_type_param f ct =
    match ct.ctyp_desc with
    | Ttyp_var (None, None) -> pp f "_"
    | Ttyp_var (None, Some jk) -> pp f "_ : %a" (jkind_annotation reset_ctxt) jk
    | Ttyp_var (Some s, None) -> tyvar f s
    | Ttyp_var (Some s, Some jk) ->
      pp f "%a : %a" tyvar s (jkind_annotation reset_ctxt) jk
    | _ -> Misc.fatal_error "unexpected type in core_type_param"

  and type_param f (ct, (a, b)) =
    pp f "%s%s%a" (type_variance a) (type_injectivity b) core_type_param ct

  and type_params f = function
    | [] -> ()
    (* Normally, one param doesn't get parentheses, but it does when there is
       a jkind annotation. *)
    | [(({ ctyp_desc = Ttyp_var (_, Some _) }, _) as param)] ->
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

  and label_x_expression_param ctxt f (l, a) =
    match a with
    | Arg (e, _) -> (
      let simple_name =
        match e with
        | { exp_desc = Texp_ident (_, { txt = Lident l; _ }, _, _, _);
            exp_attributes = []
          } ->
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
        else pp f "~%a:%a" ident_of_name lbl (simple_expr ctxt) e)
    | Omitted _ -> Misc.fatal_error "Translquote.label_x_expression_param"

  and tuple_component ctxt f (l, e) =
    let simple_name =
      match e with
      | { exp_desc = Texp_ident (_, { txt = Lident l; _ }, _, _, _);
          exp_attributes = []
        } ->
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
    | Baccess_field (li, _) -> pp f ".%a" longident_loc li
    | Baccess_array { mut; index_kind; index; _ } ->
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
    | Uaccess_unboxed_field (li, _) -> pp f ".#%a" longident_loc li

  and comprehension_expr ctxt f cexp =
    let punct, comp =
      match cexp with
      | Texp_list_comprehension comp -> "", comp
      | Texp_array_comprehension (amut, _, comp) ->
        let punct = match amut with Mutable _ -> "|" | Immutable -> ":" in
        punct, comp
      | _ -> Misc.fatal_error "Translquote.comprehension_expr"
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
    let { comp_cb_iterator = iterator; comp_cb_attributes = attrs } = x in
    pp f "%a%a" (attributes ctxt) attrs (comprehension_iterator ctxt) iterator

  and comprehension_iterator ctxt f x =
    match x with
    | Texp_comp_range { pattern = pat; start; stop; direction } ->
      pp f "%a =@ %a %a%a" (trivial_pattern ctxt) pat (expression ctxt) start
        direction_flag direction (expression ctxt) stop
    | Texp_comp_in { pattern = pat; sequence } ->
      pp f "%a in %a" (pattern ctxt) pat (expression ctxt) sequence

  and function_body ctxt f x =
    match x with
    | Tfunction_body body -> expression ctxt f body
    | Tfunction_cases
        { fc_cases = cases; fc_attributes = attrs; fc_exp_extra; _ } ->
      assert (fc_exp_extra = None);
      pp f "@[<hv>function%a%a@]" (item_attributes ctxt) attrs (case_list ctxt)
        cases

  and function_params_then_body ctxt f params body ~delimiter =
    let pp_params f =
      match params with
      | [] -> ()
      | _ :: _ -> pp f "%a@;" (list (function_param ctxt) ~sep:"@ ") params
    in
    pp f "%t%a%s@;%a" pp_params delimiter
      (function_body (under_functionrhs ctxt))
      body

  and labeled_tuple_expr ctxt f ~unboxed x =
    pp f "@[<hov2>%s(%a)@]"
      (if unboxed then "#" else "")
      (list (tuple_component ctxt) ~sep:",@;")
      x

  and record_expr ctxt f ~unboxed l eo =
    let longident_x_expression f (_, e) =
      match e with
      | Overridden (li, e) ->
        pp f "@[<hov2>%a@;=@;%a@]" longident_loc li (simple_expr ctxt) e
      | Kept _ -> ()
    in
    let hash = if unboxed then "#" else "" in
    pp f "@[<hv0>@[<hv2>%s{@;%a%a@]@;}@]" (* "@[<hov2>%s{%a%a}@]" *) hash
      (option ~last:" with@;" (simple_expr ctxt))
      eo
      (list longident_x_expression ~sep:";@;")
      (Array.to_list l)

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

  let jkind_annotation = print_reset_with_maximal_extensions jkind_annotation
end

(* Translation *)

let transl_quote (_transl_meta : Typedtree.expression -> Lambda.lambda)
    (_exp : Typedtree.expression) loc =
  Lam.string loc "Hello, world!"
