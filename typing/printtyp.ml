(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*  Xavier Leroy and Jerome Vouillon, projet Cristal, INRIA Rocquencourt  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Printing functions *)

open Misc
open Ctype
open Format
open Longident
open Path
open Asttypes
open Types
open Mode
open Btype
open Outcometree

module String = Misc.Stdlib.String
module Int = Misc.Stdlib.Int
module Sig_component_kind = Shape.Sig_component_kind
module Style = Misc.Style

(* Note [When to print jkind annotations]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   Jkind annotations are only occasionally necessary to write
   (compilation can often infer jkinds), so when should we print
   them? This Note addresses all the cases.

   Case (C1). The jkind on a type declaration, like
   [type 'a t : <<this one>> = ...].

   We print the jkind when it cannot be inferred from the rest of what is
   printed. Specifically, we print the user-written jkind in any of these
   cases:

   (C1.1) The type declaration is abstract, has no manifest (i.e.,
   it's written without any [=]-signs), and the annotation is not equivalent to value.

   In this case, there is no way to know the jkind without the annotation.

   (C1.2) The type has unsafe mode crossings. In this case, the jkind is overridden by the
   user rather than being inferred from the definition.

   Case (C2). The jkind on a type parameter to a type, like
   [type ('a : <<this one>>) t = ...].

   This jkind is printed if both of the following are true:

   (C2.1) The jkind is something other than the default [value].
   (* CR layouts reisenberg: update when the default changes *)

   (C2.2) The variable has no constraints on it. (If there is a constraint,
   the constraint determines the jkind, so printing the jkind is
   redundant.)

   We *could*, in theory, print this only when it cannot be inferred.
   But this amounts to repeating inference. The heuristic also runs into
   trouble when considering the possibility of a recursive type. So, in
   order to keep the pretty-printer simple, we just always print the
   (non-default) annotation.

   Another design possibility is to pass in verbosity level as some kind
   of flag.

   Case (C3). The jkind on a universal type variable, like
   [val f : ('a : <<this one>>). 'a -> 'a].

   We should print this jkind annotation whenever it is neither the
   default [value] nor an unfilled sort variable. (But see (X1) below.)
   (* CR layouts reisenberg: update when the default changes *)
   This is a challenge, though, because the type in a [val] does not
   explicitly quantify its free variables. So we must collect the free
   variables, look to see whether any have interesting jkinds, and
   print the whole set of variables if any of them do. This is all
   implemented in [extract_qtvs], used also in a number of other places
   we do quantification (e.g. gadt-syntax constructors).

   Exception (X1). When we are still in the process of inferring a type,
   there may be an unfilled sort variable. Here is an example:

   {[
      module M : sig
        val f : int -> bool -> char
      end = struct
        let f true _ = ()
      end
   ]}

   The problem is that [f]'s first parameter is conflicted between being
   [int] and [bool]. But the second parameter in the [struct] will have
   type ['a : <<sort variable>>]. We generally do not want to print this,
   however, and so we don't -- except when [-verbose-types] is set.

   We imagine that merlin, when run verbosely, will set [-verbose-types].
   This will allow an informative type to be printed for e.g. [let f x = x],
   which can work with any sort.
*)

(* Print a long identifier *)
let longident = Pprintast.longident

let () = Env.print_longident := longident

(* Print an identifier avoiding name collisions *)

module Out_name = struct
  let create x = { printed_name = x }
  let print x = x.printed_name
  let set out_name x = out_name.printed_name <- x
end

(** Some identifiers may require hiding when printing *)
type bound_ident = { hide:bool; ident:Ident.t }

(* printing environment for path shortening and naming *)
let printing_env = ref Env.empty

(* When printing, it is important to only observe the
   current printing environment, without reading any new
   cmi present on the file system *)
let in_printing_env f = Env.without_cmis f !printing_env

let human_unique n id = Printf.sprintf "%s/%d" (Ident.name id) n

 type namespace = Sig_component_kind.t =
    | Value
    | Type
    | Constructor
    | Label
    | Unboxed_label
    | Module
    | Module_type
    | Extension_constructor
    | Class
    | Class_type


module Namespace = struct

  let id = function
    | Type -> 0
    | Module -> 1
    | Module_type -> 2
    | Class -> 3
    | Class_type -> 4
    | Extension_constructor | Value | Constructor | Label -> 5
    | Unboxed_label -> 6
     (* we do not handle those component *)

  let size = 1 + id Unboxed_label


  let pp ppf x =
    Format.pp_print_string ppf (Shape.Sig_component_kind.to_string x)

  (** The two functions below should never access the filesystem,
      and thus use {!in_printing_env} rather than directly
      accessing the printing environment *)
  let lookup =
    let to_lookup f lid = fst @@ in_printing_env (f (Lident lid)) in
    function
    | Some Type -> to_lookup Env.find_type_by_name
    | Some Module -> to_lookup Env.find_module_by_name
    | Some Module_type -> to_lookup Env.find_modtype_by_name
    | Some Class -> to_lookup Env.find_class_by_name
    | Some Class_type -> to_lookup Env.find_cltype_by_name
    | None
    | Some(Value|Extension_constructor|Constructor|Label|Unboxed_label) ->
         fun _ -> raise Not_found

  let location namespace id =
    let path = Path.Pident id in
    try Some (
        match namespace with
        | Some Type -> (in_printing_env @@ Env.find_type path).type_loc
        | Some Module -> (in_printing_env @@ Env.find_module path).md_loc
        | Some Module_type -> (in_printing_env @@ Env.find_modtype path).mtd_loc
        | Some Class -> (in_printing_env @@ Env.find_class path).cty_loc
        | Some Class_type -> (in_printing_env @@ Env.find_cltype path).clty_loc
        | Some (Extension_constructor|Value|Constructor|Label|Unboxed_label)
        | None ->
            Location.none
      ) with Not_found -> None

  let best_class_namespace = function
    | Papply _ | Pdot _ -> Some Module
    | Pextra_ty _ -> assert false (* Only in type path *)
    | Pident c ->
        match location (Some Class) c with
        | Some _ -> Some Class
        | None -> Some Class_type

end

(** {2 Conflicts printing}
    Conflicts arise when multiple items are attributed the same name,
    the following module stores the global conflict references and
    provides the printing functions for explaining the source of
    the conflicts.
*)
module Conflicts = struct
  module M = String.Map
  type explanation =
    { kind: namespace; name:string; root_name:string; location:Location.t}
  let explanations = ref M.empty
  let collect_explanation namespace n id =
    let name = human_unique n id in
    let root_name = Ident.name id in
    if not (M.mem name !explanations) then
      match Namespace.location (Some namespace) id with
      | None -> ()
      | Some location ->
          let explanation = { kind = namespace; location; name; root_name } in
          explanations := M.add name explanation !explanations

  let pp_explanation ppf r=
    Format.fprintf ppf "@[<v 2>%a:@,Definition of %s %a@]"
      Location.print_loc r.location (Sig_component_kind.to_string r.kind)
      Style.inline_code r.name

  let print_located_explanations ppf l =
    Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp_explanation) l

  let reset () = explanations := M.empty
  let list_explanations () =
    let c = !explanations in
    reset ();
    c |> M.bindings |> List.map snd |> List.sort Stdlib.compare


  let print_toplevel_hint ppf l =
    let conj ppf () = Format.fprintf ppf " and@ " in
    let pp_namespace_plural ppf n = Format.fprintf ppf "%as" Namespace.pp n in
    let root_names = List.map (fun r -> r.kind, r.root_name) l in
    let unique_root_names = List.sort_uniq Stdlib.compare root_names in
    let submsgs = Array.make Namespace.size [] in
    let () = List.iter (fun (n,_ as x) ->
        submsgs.(Namespace.id n) <- x :: submsgs.(Namespace.id n)
      )  unique_root_names in
    let pp_submsg ppf names =
      match names with
      | [] -> ()
      | [namespace, a] ->
          Format.fprintf ppf
        "@ \
         @[<2>@{<hint>Hint@}: The %a %a has been defined multiple times@ \
         in@ this@ toplevel@ session.@ \
         Some toplevel values still refer to@ old@ versions@ of@ this@ %a.\
         @ Did you try to redefine them?@]"
        Namespace.pp namespace
        Style.inline_code a Namespace.pp namespace
      | (namespace, _) :: _ :: _ ->
      Format.fprintf ppf
        "@ \
         @[<2>@{<hint>Hint@}: The %a %a have been defined multiple times@ \
         in@ this@ toplevel@ session.@ \
         Some toplevel values still refer to@ old@ versions@ of@ those@ %a.\
         @ Did you try to redefine them?@]"
        pp_namespace_plural namespace
        Format.(pp_print_list ~pp_sep:conj Style.inline_code)
        (List.map snd names)
        pp_namespace_plural namespace in
    Array.iter (pp_submsg ppf) submsgs

  let print_explanations ppf =
    let ltop, l =
      (* isolate toplevel locations, since they are too imprecise *)
      let from_toplevel a =
        a.location.Location.loc_start.Lexing.pos_fname = "//toplevel//" in
      List.partition from_toplevel (list_explanations ())
    in
    begin match l with
    | [] -> ()
    | l -> Format.fprintf ppf "@,%a" print_located_explanations l
    end;
    (* if there are name collisions in a toplevel session,
       display at least one generic hint by namespace *)
    print_toplevel_hint ppf ltop

  let exists () = M.cardinal !explanations >0
end

module Naming_context = struct

module M = String.Map
module S = String.Set

let enabled = ref true
let enable b = enabled := b

(** Name mapping *)
type mapping =
  | Need_unique_name of int Ident.Map.t
  (** The same name has already been attributed to multiple types.
      The [map] argument contains the specific binding time attributed to each
      types.
  *)
  | Uniquely_associated_to of Ident.t * out_name
  (** For now, the name [Ident.name id] has been attributed to [id],
      [out_name] is used to expand this name if a conflict arises
      at a later point
  *)
  | Associated_to_pervasives of out_name
  (** [Associated_to_pervasives out_name] is used when the item
      [Stdlib.$name] has been associated to the name [$name].
      Upon a conflict, this name will be expanded to ["Stdlib." ^ name ] *)

let hid_start = 0

let add_hid_id id map =
  let new_id = 1 + Ident.Map.fold (fun _ -> Int.max) map hid_start in
  new_id, Ident.Map.add id new_id  map

let find_hid id map =
  try Ident.Map.find id map, map with
    Not_found -> add_hid_id id map

let pervasives name = "Stdlib." ^ name

let map = Array.make Namespace.size M.empty
let get namespace = map.(Namespace.id namespace)
let set namespace x = map.(Namespace.id namespace) <- x

(* Names used in recursive definitions are not considered when determining
   if a name is already attributed in the current environment.
   This is a complementary version of hidden_rec_items used by short-path. *)
let protected = ref S.empty

(* When dealing with functor arguments, identity becomes fuzzy because the same
   syntactic argument may be represented by different identifiers during the
   error processing, we are thus disabling disambiguation on the argument name
*)
let fuzzy = ref S.empty
let with_arg id f =
  protect_refs [ R(fuzzy, S.add (Ident.name id) !fuzzy) ] f
let fuzzy_id namespace id = namespace = Module && S.mem (Ident.name id) !fuzzy

let with_hidden ids f =
  let update m id = S.add (Ident.name id.ident) m in
  protect_refs [ R(protected, List.fold_left update !protected ids)] f

let pervasives_name namespace name =
  match namespace, !enabled with
  | None, _ | _, true -> Out_name.create name
  | Some namespace, false ->
      match M.find name (get namespace) with
      | Associated_to_pervasives r -> r
      | Need_unique_name _ -> Out_name.create (pervasives name)
      | Uniquely_associated_to (id',r) ->
          let hid, map = add_hid_id id' Ident.Map.empty in
          Out_name.set r (human_unique hid id');
          Conflicts.collect_explanation namespace hid id';
          set namespace @@ M.add name (Need_unique_name map) (get namespace);
          Out_name.create (pervasives name)
      | exception Not_found ->
          let r = Out_name.create name in
          set namespace @@ M.add name (Associated_to_pervasives r) (get namespace);
          r

(** Lookup for preexisting named item within the current {!printing_env} *)
let env_ident namespace name =
  if S.mem name !protected then None else
  match Namespace.lookup namespace name with
  | Pident id -> Some id
  | _ -> None
  | exception Not_found -> None

(** Associate a name to the identifier [id] within [namespace] *)
let ident_name_simple namespace id =
  match namespace, !enabled with
  | None, _ | _, false -> Out_name.create (Ident.name id)
  | Some namespace, true ->
    if fuzzy_id namespace id then Out_name.create (Ident.name id)
    else
      let name = Ident.name id in
      match M.find name (get namespace) with
      | Uniquely_associated_to (id',r) when Ident.same id id' ->
          r
      | Need_unique_name map ->
          let hid, m = find_hid id map in
          Conflicts.collect_explanation namespace hid id;
          set namespace @@ M.add name (Need_unique_name m) (get namespace);
          Out_name.create (human_unique hid id)
      | Uniquely_associated_to (id',r) ->
          let hid', m = find_hid id' Ident.Map.empty in
          let hid, m = find_hid id m in
          Out_name.set r (human_unique hid' id');
          List.iter (fun (id,hid) -> Conflicts.collect_explanation namespace hid id)
            [id, hid; id', hid' ];
          set namespace @@ M.add name (Need_unique_name m) (get namespace);
          Out_name.create (human_unique hid id)
      | Associated_to_pervasives r ->
          Out_name.set r ("Stdlib." ^ Out_name.print r);
          let hid, m = find_hid id Ident.Map.empty in
          set namespace @@ M.add name (Need_unique_name m) (get namespace);
          Out_name.create (human_unique hid id)
      | exception Not_found ->
          let r = Out_name.create name in
          set namespace
          @@ M.add name (Uniquely_associated_to (id,r) ) (get namespace);
          r

(** Same as {!ident_name_simple} but lookup to existing named identifiers
    in the current {!printing_env} *)
let ident_name namespace id =
  begin match env_ident namespace (Ident.name id) with
  | Some id' -> ignore (ident_name_simple namespace id')
  | None -> ()
  end;
  ident_name_simple namespace id

let reset () =
  Array.iteri ( fun i _ -> map.(i) <- M.empty ) map

let with_ctx f =
  let old = Array.copy map in
  try_finally f
    ~always:(fun () -> Array.blit old 0 map 0 (Array.length map))

end
let ident_name = Naming_context.ident_name
let reset_naming_context = Naming_context.reset

let ident ppf id = pp_print_string ppf
    (Out_name.print (Naming_context.ident_name_simple None id))

let namespaced_ident namespace  id =
  Out_name.print (Naming_context.ident_name (Some namespace) id)

let instance_name global =
  (* Construct the stopgap syntax and then shove it in a string with the
     attribute after it. *)
  (* CR lmaurer: This is hacky and it loses the state of the [out_name]s that
     comprise the [out_ident]. Should presumably have a new constructor for
     [out_ident] instead? *)
  let rec string_of_global global =
    (* We can avoid calling [ident_name_simple] here because instance names are
       always global (which is bad - but the syntax is currently bad anyway) *)
    let ({ head; args } : Global_module.Name.t) = global in
    String.concat "" (head :: List.map string_of_arg args)
  and string_of_arg arg =
    let ({ param; value } : Global_module.Name.argument) = arg in
    sprintf "(%s)(%s)"
      (Global_module.Parameter_name.to_string param) (string_of_global value)
  in
  let printed_name =
    string_of_global global ^ " [@jane.non_erasable.instances]"
  in
  { printed_name }


(* Print a path *)

let ident_stdlib = Ident.create_persistent "Stdlib"

let non_shadowed_pervasive = function
  | Pdot(Pident id, s) as path ->
      Ident.same id ident_stdlib &&
      (match in_printing_env (Env.find_type_by_name (Lident s)) with
       | (path', _) -> Path.same path path'
       | exception Not_found -> true)
  | _ -> false

let find_double_underscore s =
  let len = String.length s in
  let rec loop i =
    if i + 1 >= len then
      None
    else if s.[i] = '_' && s.[i + 1] = '_' then
      Some i
    else
      loop (i + 1)
  in
  loop 0

let rec module_path_is_an_alias_of env path ~alias_of =
  match Env.find_module path env with
  | { md_type = Mty_alias path'; _ } ->
    Path.same path' alias_of ||
    module_path_is_an_alias_of env path' ~alias_of
  | _ -> false
  | exception Not_found -> false

let expand_longident_head name =
  match find_double_underscore name with
  | None -> None
  | Some i ->
    Some
      (Ldot
        (Lident (String.sub name 0 i),
          Unit_info.modulize
            (String.sub name (i + 2) (String.length name - i - 2))))

(* Simple heuristic to print Foo__bar.* as Foo.Bar.* when Foo.Bar is an alias
   for Foo__bar. This pattern is used by the stdlib. *)
let rec rewrite_double_underscore_paths env p =
  match p with
  | Pdot (p, s) ->
    Pdot (rewrite_double_underscore_paths env p, s)
  | Papply (a, b) ->
    Papply (rewrite_double_underscore_paths env a,
            rewrite_double_underscore_paths env b)
  | Pextra_ty (p, extra) ->
    Pextra_ty (rewrite_double_underscore_paths env p, extra)
  | Pident id ->
    let name = Ident.name id in
    match expand_longident_head name with
    | None -> p
    | Some better_lid ->
      match Env.find_module_by_name better_lid env with
      | exception Not_found -> p
      | p', _ ->
          if module_path_is_an_alias_of env p' ~alias_of:p then
            p'
          else
          p

let rewrite_double_underscore_paths env p =
  if env == Env.empty then
    p
  else
    rewrite_double_underscore_paths env p

let rec rewrite_double_underscore_longidents env (l : Longident.t) =
  match l with
  | Ldot (l, s) ->
    Ldot (rewrite_double_underscore_longidents env l, s)
  | Lapply (a, b) ->
    Lapply (rewrite_double_underscore_longidents env a,
            rewrite_double_underscore_longidents env b)
  | Lident name ->
    match expand_longident_head name with
    | None -> l
    | Some l' ->
      match Env.find_module_by_name l env, Env.find_module_by_name l' env with
      | exception Not_found -> l
      | (p, _), (p', _) ->
          if module_path_is_an_alias_of env p' ~alias_of:p then
            l'
          else
          l

let rec tree_of_path namespace = function
  | Pident id ->
      Oide_ident (ident_name namespace id)
  | Pdot(_, s) as path when non_shadowed_pervasive path ->
      Oide_ident (Naming_context.pervasives_name namespace s)
  | Pdot(p, s) ->
      Oide_dot (tree_of_path (Some Module) p, s)
  | Papply(p1, p2) ->
      Oide_apply (tree_of_path (Some Module) p1, tree_of_path (Some Module) p2)
  | Pextra_ty (p, extra) -> begin
      (* inline record types are syntactically prevented from escaping their
         binding scope, and are never shown to users. *)
      match extra with
        Pcstr_ty s ->
          Oide_dot (tree_of_path (Some Type) p, s)
      | Pext_ty ->
          tree_of_path None p
      | Punboxed_ty ->
          Oide_hash (tree_of_path namespace p)
    end

let tree_of_path namespace = function
  | Pident id when Ident.is_instance id ->
    (* Only when the instance name is the entire path (which is the only place
       a human could write it) is it worth printing the human-writable stopgap
       syntax for instance names *)
    Oide_ident (instance_name (Ident.to_global_exn id))
  | p -> tree_of_path namespace p

let tree_of_path namespace p =
  tree_of_path namespace (rewrite_double_underscore_paths !printing_env p)

let path ppf p =
  !Oprint.out_ident ppf (tree_of_path None p)

let string_of_path p =
  Format.asprintf "%a" path p

let strings_of_paths namespace p =
  reset_naming_context ();
  let trees = List.map (tree_of_path namespace) p in
  List.map (Format.asprintf "%a" !Oprint.out_ident) trees

let () = Env.print_path := path
let () = Jkind.set_printtyp_path path

(* Print a recursive annotation *)

let tree_of_rec = function
  | Trec_not -> Orec_not
  | Trec_first -> Orec_first
  | Trec_next -> Orec_next

(* Print a raw type expression, with sharing *)

let raw_list pr ppf = function
    [] -> fprintf ppf "[]"
  | a :: l ->
      fprintf ppf "@[<1>[%a%t]@]" pr a
        (fun ppf -> List.iter (fun x -> fprintf ppf ";@,%a" pr x) l)

let kind_vars = ref []
let kind_count = ref 0

let string_of_field_kind v =
  match field_kind_repr v with
  | Fpublic -> "Fpublic"
  | Fabsent -> "Fabsent"
  | Fprivate -> "Fprivate"

let rec safe_repr v t =
  match Transient_expr.coerce t with
    {desc = Tlink t} when not (List.memq t v) ->
      safe_repr (t::v) t
  | t' -> t'

let rec list_of_memo = function
    Mnil -> []
  | Mcons (_priv, p, _t1, _t2, rem) -> p :: list_of_memo rem
  | Mlink rem -> list_of_memo !rem

let print_name ppf = function
    None -> fprintf ppf "None"
  | Some name -> fprintf ppf "\"%s\"" name

let string_of_label : Types.arg_label -> string = function
    Nolabel -> ""
  | Labelled s | Position s -> s
  | Optional s -> "?"^s

let visited = ref []
let rec raw_type ppf ty =
  let ty = safe_repr [] ty in
  if List.memq ty !visited then fprintf ppf "{id=%d}" ty.id else begin
    visited := ty :: !visited;
    fprintf ppf "@[<1>{id=%d;level=%d;scope=%d;desc=@,%a}@]" ty.id ty.level
      ty.scope raw_type_desc ty.desc
  end
and labeled_type ppf (label, ty) =
  begin match label with
    | Some s -> fprintf ppf "label=\"%s\" " s
    | None -> ()
  end;
  raw_type ppf ty

and raw_type_list tl = raw_list raw_type tl
and labeled_type_list tl = raw_list labeled_type tl
and raw_lid_type_list tl =
  raw_list (fun ppf (lid, typ) ->
             fprintf ppf "(@,%a,@,%a)" longident lid raw_type typ)
    tl
and raw_row_desc ppf row =
  let Row {fields; more; name; fixed; closed} = row_repr row in
  fprintf ppf
    "@[<hov1>{@[%s@,%a;@]@ @[%s@,%a;@]@ %s%B;@ %s%a;@ @[<1>%s%t@]}@]"
    "row_fields="
    (raw_list (fun ppf (l, f) ->
       fprintf ppf "@[%s,@ %a@]" l raw_field f))
    fields
    "row_more=" raw_type more
    "row_closed=" closed
    "row_fixed=" raw_row_fixed fixed
    "row_name="
    (fun ppf ->
       match name with None -> fprintf ppf "None"
                     | Some(p,tl) ->
                       fprintf ppf "Some(@,%a,@,%a)" path p raw_type_list tl)
and raw_type_desc ppf = function
    Tvar { name; jkind } ->
      fprintf ppf "Tvar (@,%a,@,%a)"
        print_name name Jkind.format jkind
  | Tarrow((l,arg,ret),t1,t2,c) ->
      fprintf ppf "@[<hov1>Tarrow((\"%s\",%a,%a),@,%a,@,%a,@,%s)@]"
        (string_of_label l)
        (Alloc.print ~verbose:true ()) arg
        (Alloc.print ~verbose:true ()) ret
        raw_type t1 raw_type t2
        (if is_commu_ok c then "Cok" else "Cunknown")
  | Ttuple tl ->
      fprintf ppf "@[<1>Ttuple@,%a@]" labeled_type_list tl
  | Tunboxed_tuple tl ->
      fprintf ppf "@[<1>Tunboxed_tuple@,%a@]" labeled_type_list tl
  | Tconstr (p, tl, abbrev) ->
      fprintf ppf "@[<hov1>Tconstr(@,%a,@,%a,@,%a)@]" path p
        raw_type_list tl
        (raw_list path) (list_of_memo !abbrev)
  | Tobject (t, nm) ->
      fprintf ppf "@[<hov1>Tobject(@,%a,@,@[<1>ref%t@])@]" raw_type t
        (fun ppf ->
          match !nm with None -> fprintf ppf " None"
          | Some(p,tl) ->
              fprintf ppf "(Some(@,%a,@,%a))" path p raw_type_list tl)
  | Tfield (f, k, t1, t2) ->
      fprintf ppf "@[<hov1>Tfield(@,%s,@,%s,@,%a,@;<0 -1>%a)@]" f
        (string_of_field_kind k)
        raw_type t1 raw_type t2
  | Tnil -> fprintf ppf "Tnil"
  | Tlink t -> fprintf ppf "@[<1>Tlink@,%a@]" raw_type t
  | Tsubst (t, None) -> fprintf ppf "@[<1>Tsubst@,(%a,None)@]" raw_type t
  | Tsubst (t, Some t') ->
      fprintf ppf "@[<1>Tsubst@,(%a,@ Some%a)@]" raw_type t raw_type t'
  | Tunivar { name; jkind } ->
      fprintf ppf "Tunivar (@,%a,@,%a)"
        print_name name Jkind.format jkind
  | Tpoly (t, tl) ->
      fprintf ppf "@[<hov1>Tpoly(@,%a,@,%a)@]"
        raw_type t
        raw_type_list tl
  | Tvariant row ->
    raw_row_desc ppf row
  | Tpackage (p, fl) ->
      fprintf ppf "@[<hov1>Tpackage(@,%a,@,%a)@]" path p
        raw_lid_type_list fl
  | Tof_kind jkind ->
    fprintf ppf "(type@ :@ %a)" Jkind.format jkind
and raw_row_fixed ppf = function
| None -> fprintf ppf "None"
| Some Types.Fixed_private -> fprintf ppf "Some Fixed_private"
| Some Types.Rigid -> fprintf ppf "Some Rigid"
| Some Types.Univar t -> fprintf ppf "Some(Univar(%a))" raw_type t
| Some Types.Reified p -> fprintf ppf "Some(Reified(%a))" path p
| Some Types.Fixed_existential -> fprintf ppf "Some Fixed_existential"

and raw_field ppf rf =
  match_row_field
    ~absent:(fun _ -> fprintf ppf "RFabsent")
    ~present:(function
      | None ->
          fprintf ppf "RFpresent None"
      | Some t ->
          fprintf ppf  "@[<1>RFpresent(Some@,%a)@]" raw_type t)
    ~either:(fun c tl m e ->
      fprintf ppf "@[<hov1>RFeither(%B,@,%a,@,%B,@,@[<1>ref%t@])@]" c
        raw_type_list tl m
        (fun ppf ->
          match e with None -> fprintf ppf " RFnone"
          | Some f -> fprintf ppf "@,@[<1>(%a)@]" raw_field f))
    rf

let raw_type_expr ppf t =
  visited := []; kind_vars := []; kind_count := 0;
  raw_type ppf t;
  visited := []; kind_vars := []

let () = Btype.print_raw := raw_type_expr

(* Normalize paths *)

type param_subst = Id | Nth of int | Map of int list

let is_nth = function
    Nth _ -> true
  | _ -> false

let compose l1 = function
  | Id -> Map l1
  | Map l2 -> Map (List.map (List.nth l1) l2)
  | Nth n  -> Nth (List.nth l1 n)

let apply_subst s1 tyl =
  if tyl = [] then []
  (* cf. PR#7543: Typemod.type_package doesn't respect type constructor arity *)
  else
    match s1 with
      Nth n1 -> [List.nth tyl n1]
    | Map l1 -> List.map (List.nth tyl) l1
    | Id -> tyl

(* In the [Paths] constructor, more preferred paths are stored later in the
   list. *)

type best_path = Paths of Path.t list | Best of Path.t

(** Short-paths cache: the five mutable variables below implement a one-slot
    cache for short-paths
 *)
let printing_old = ref Env.empty
let printing_pers = ref Compilation_unit.Name.Set.empty
(** {!printing_old} and  {!printing_pers} are the keys of the one-slot cache *)

let printing_depth = ref 0
let printing_cont = ref ([] : Env.iter_cont list)
let printing_map = ref Path.Map.empty
(**
   - {!printing_map} is the main value stored in the cache.
   Note that it is evaluated lazily and its value is updated during printing.
   - {!printing_dep} is the current exploration depth of the environment,
   it is used to determine whenever the {!printing_map} should be evaluated
   further before completing a request.
   - {!printing_cont} is the list of continuations needed to evaluate
   the {!printing_map} one level further (see also {!Env.run_iter_cont})
*)

let rec index l x =
  match l with
    [] -> raise Not_found
  | a :: l -> if eq_type x a then 0 else 1 + index l x

let rec uniq = function
    [] -> true
  | a :: l -> not (List.memq (a : int) l) && uniq l

let rec normalize_type_path ?(cache=false) env p =
  try
    let (params, ty, _) = Env.find_type_expansion p env in
    match get_desc ty with
      Tconstr (p1, tyl, _) ->
        if List.length params = List.length tyl
        && List.for_all2 eq_type params tyl
        then normalize_type_path ~cache env p1
        else if cache || List.length params <= List.length tyl
             || not (uniq (List.map get_id tyl)) then (p, Id)
        else
          let l1 = List.map (index params) tyl in
          let (p2, s2) = normalize_type_path ~cache env p1 in
          (p2, compose l1 s2)
    | _ ->
        (p, Nth (index params ty))
  with
    Not_found ->
      (Env.normalize_type_path None env p, Id)

let same_printing_env env =
  let used_pers = Env.used_persistent () in
  Env.same_types !printing_old env
  && Compilation_unit.Name.Set.equal !printing_pers used_pers

let set_printing_env env =
  printing_env := env;
  if !Clflags.real_paths ||
     !printing_env == Env.empty ||
     same_printing_env env then
    ()
  else begin
    (* printf "Reset printing_map@."; *)
    printing_old := env;
    printing_pers := Env.used_persistent ();
    printing_map := Path.Map.empty;
    printing_depth := 0;
    (* printf "Recompute printing_map.@."; *)
    let cont =
      Env.iter_types
        (fun p (p', _decl) ->
          let (p1, s1) = normalize_type_path env p' ~cache:true in
          (* Format.eprintf "%a -> %a = %a@." path p path p' path p1 *)
          if s1 = Id then
          try
            let r = Path.Map.find p1 !printing_map in
            match !r with
              Paths l -> r := Paths (p :: l)
            | Best p' -> r := Paths [p; p'] (* assert false *)
          with Not_found ->
            (* Jane Street: Often the best choice for printing [p1] is
               [p1] itself. And often [p1] is a path whose "penalty"
               would be reduced if the double-underscore rewrite
               applied.
            *)
            let rewritten_p1 = rewrite_double_underscore_paths env p1 in
            printing_map := Path.Map.add p1 (ref (Paths [ p; rewritten_p1 ])) !printing_map)
        env in
    printing_cont := [cont];
  end

let wrap_printing_env env f =
  set_printing_env env; reset_naming_context ();
  try_finally f ~always:(fun () -> set_printing_env Env.empty)

let wrap_printing_env ~error env f =
  if error then Env.without_cmis (wrap_printing_env env) f
  else wrap_printing_env env f

let wrap_printing_env_error env f =
  let wrap (loc : _ Location.loc) =
    { loc with txt =
        (fun fmt -> Env.without_cmis (fun () -> loc.txt fmt) ())
  (* CR nroberts: See https://github.com/oxcaml/oxcaml/pull/2529
     for an explanation of why this has drifted from upstream. *)
    }
  in
  let err : Location.error = wrap_printing_env ~error:true env f in
  { Location.kind = err.kind;
    main = wrap err.main;
    sub = List.map wrap err.sub;
  }

let rec lid_of_path = function
    Path.Pident id ->
      Longident.Lident (Ident.name id)
  | Path.Pdot (p1, s) | Path.Pextra_ty (p1, Pcstr_ty s)  ->
      Longident.Ldot (lid_of_path p1, s)
  | Path.Papply (p1, p2) ->
      Longident.Lapply (lid_of_path p1, lid_of_path p2)
  | Path.Pextra_ty (p, Pext_ty) -> lid_of_path p
  | Path.Pextra_ty (p, Punboxed_ty) ->
    match p with
    | Pident id -> Longident.Lident (Ident.name id ^ "#")
    | Pdot (p, s) -> Longident.Ldot (lid_of_path p, s ^ "#")
    | Papply _ | Pextra_ty _ -> assert false

let is_unambiguous path env =
  let l = Env.find_shadowed_types path env in
  List.exists (Path.same path) l || (* concrete paths are ok *)
  match l with
    [] -> true
  | p :: rem ->
      (* allow also coherent paths:  *)
      let normalize p = fst (normalize_type_path ~cache:true env p) in
      let p' = normalize p in
      List.for_all (fun p -> Path.same (normalize p) p') rem ||
      (* also allow repeatedly defining and opening (for toplevel) *)
      let id = lid_of_path p in
      List.for_all (fun p -> lid_of_path p = id) rem &&
      Path.same p (fst (Env.find_type_by_name id env))

let penalty_size = 10

let name_penalty s =
  if s <> "" && s.[0] = '_' then
    penalty_size
  else
    match find_double_underscore s with
    | None -> 1
    | Some _ -> penalty_size

let ambiguity_penalty path env =
  if is_unambiguous path env then 0 else penalty_size

let path_size path env =
  let rec size = function
      Pident id ->
        name_penalty (Ident.name id), -Ident.scope id
    | Pdot (p, id) | Pextra_ty (p, Pcstr_ty id) ->
        let (l, b) = size p in (name_penalty id + l, b)
    | Papply (p1, p2) ->
        let (l, b) = size p1 in
        (l + fst (size p2), b)
    | Pextra_ty (p, _) -> size p
  in
  let l, s = size path in
  l + ambiguity_penalty path env, s

let rec get_best_path r env =
  match !r with
    Best p' -> p'
  | Paths [] -> raise Not_found
  | Paths l ->
      r := Paths [];
      List.iter
        (fun p ->
          (* Format.eprintf "evaluating %a@." path p; *)
          match !r with
            Best p' when path_size p env >= path_size p' env -> ()
          | _ -> r := Best p)
        (List.rev l);
      get_best_path r env

let best_type_path p =
  if !printing_env == Env.empty
  then (p, Id)
  else if !Clflags.real_paths
  then (p, Id)
  else
    let (p', s) = normalize_type_path !printing_env p in
    let get_path () =
      try
        get_best_path (Path.Map.find p' !printing_map) !printing_env
      with Not_found -> rewrite_double_underscore_paths !printing_env p'
    in
    while !printing_cont <> [] &&
      fst (path_size (get_path ()) !printing_env) > !printing_depth
    do
      printing_cont := List.map snd (Env.run_iter_cont !printing_cont);
      incr printing_depth;
    done;
    let p'' = get_path () in
    (* Format.eprintf "%a = %a -> %a@." path p path p' path p''; *)
    (p'', s)

(* Print a type expression *)

let proxy ty = Transient_expr.repr (proxy ty)

(* When printing a type scheme, we print weak names.  When printing a plain
   type, we do not.  This type controls that behavior *)
type type_or_scheme = Type | Type_scheme

let is_non_gen mode ty =
  match mode with
  | Type_scheme -> is_Tvar ty && get_level ty <> generic_level
  | Type        -> false

let nameable_row row =
  row_name row <> None &&
  List.for_all
    (fun (_, f) ->
       match row_field_repr f with
       | Reither(c, l, _) ->
           row_closed row && if c then l = [] else List.length l = 1
       | _ -> true)
    (row_fields row)

(* This specialized version of [Btype.iter_type_expr] normalizes and
   short-circuits the traversal of the [type_expr], so that it covers only the
   subterms that would be printed by the type printer. *)
let printer_iter_type_expr f ty =
  match get_desc ty with
  | Tconstr(p, tyl, _) ->
      let (_p', s) = best_type_path p in
      List.iter f (apply_subst s tyl)
  | Tvariant row -> begin
      match row_name row with
      | Some(_p, tyl) when nameable_row row ->
          List.iter f tyl
      | _ ->
          iter_row f row
    end
  | Tobject (fi, nm) -> begin
      match !nm with
      | None ->
          let fields, _ = flatten_fields fi in
          List.iter
            (fun (_, kind, ty) ->
               if field_kind_repr kind = Fpublic then
                 f ty)
            fields
      | Some (_, l) ->
          List.iter f (List.tl l)
    end
  | Tfield(_, kind, ty1, ty2) ->
      if field_kind_repr kind = Fpublic then
        f ty1;
      f ty2
  | _ ->
      Btype.iter_type_expr f ty

module Internal_names : sig

  val reset : unit -> unit

  val add : Path.t -> unit

  val print_explanations : Env.t -> Format.formatter -> unit

end = struct

  let names = ref Ident.Set.empty

  let reset () =
    names := Ident.Set.empty

  let add p =
    match p with
    | Pident id ->
        let name = Ident.name id in
        if String.length name > 0 && name.[0] = '$' then begin
          names := Ident.Set.add id !names
        end
    | Pdot _ | Papply _ | Pextra_ty _ -> ()

  let print_explanations env ppf =
    let constrs =
      Ident.Set.fold
        (fun id acc ->
          let p = Pident id in
          match Env.find_type p env with
          | exception Not_found -> acc
          | decl ->
              match type_origin decl with
              | Existential constr ->
                  let prev = String.Map.find_opt constr acc in
                  let prev = Option.value ~default:[] prev in
                  String.Map.add constr (tree_of_path None p :: prev) acc
              | Definition | Rec_check_regularity -> acc)
        !names String.Map.empty
    in
    String.Map.iter
      (fun constr out_idents ->
        match out_idents with
        | [] -> ()
        | [out_ident] ->
            fprintf ppf
              "@ @[<2>@{<hint>Hint@}:@ %a@ is an existential type@ \
               bound by the constructor@ %a.@]"
              (Style.as_inline_code !Oprint.out_ident) out_ident
              Style.inline_code constr
        | out_ident :: out_idents ->
            fprintf ppf
              "@ @[<2>@{<hint>Hint@}:@ %a@ and %a@ are existential types@ \
               bound by the constructor@ %a.@]"
              (Format.pp_print_list
                 ~pp_sep:(fun ppf () -> fprintf ppf ",@ ")
                 (Style.as_inline_code !Oprint.out_ident))
              (List.rev out_idents)
              (Style.as_inline_code !Oprint.out_ident) out_ident
              Style.inline_code constr)
      constrs

end

module Names : sig
  val reset_names : unit -> unit

  val add_named_vars : type_expr -> unit
  val add_subst : (type_expr * type_expr) list -> unit

  val new_name : unit -> string
  val new_var_name : non_gen:bool -> type_expr -> unit -> string

  val name_of_type : (unit -> string) -> transient_expr -> string
  val check_name_of_type : non_gen:bool -> transient_expr -> unit

  val remove_names : transient_expr list -> unit

  val with_local_names : (unit -> 'a) -> 'a

  (* Refresh the weak variable map in the toplevel; for [print_items], which is
     itself for the toplevel *)
  val refresh_weak : unit -> unit
end = struct
  (* We map from types to names, but not directly; we also store a substitution,
     which maps from types to types.  The lookup process is
     "type -> apply substitution -> find name".  The substitution is presumed to
     be one-shot. *)
  let names = ref ([] : (transient_expr * string) list)
  let name_subst = ref ([] : (transient_expr * transient_expr) list)
  let name_counter = ref 0
  let named_vars = ref ([] : string list)
  let visited_for_named_vars = ref ([] : transient_expr list)

  let weak_counter = ref 1
  let weak_var_map = ref TypeMap.empty
  let named_weak_vars = ref String.Set.empty

  let reset_names () =
    names := [];
    name_subst := [];
    name_counter := 0;
    named_vars := [];
    visited_for_named_vars := []

  let add_named_var tty =
    match tty.desc with
      Tvar { name = Some name } | Tunivar { name = Some name } ->
        if List.mem name !named_vars then () else
        named_vars := name :: !named_vars
    | _ -> ()

  let rec add_named_vars ty =
    let tty = Transient_expr.repr ty in
    let px = proxy ty in
    if not (List.memq px !visited_for_named_vars) then begin
      visited_for_named_vars := px :: !visited_for_named_vars;
      match tty.desc with
      | Tvar _ | Tunivar _ ->
          add_named_var tty
      | _ ->
          printer_iter_type_expr add_named_vars ty
    end

  let substitute ty =
    match List.assq ty !name_subst with
    | ty' -> ty'
    | exception Not_found -> ty

  let add_subst subst =
    name_subst :=
      List.map (fun (t1,t2) -> Transient_expr.repr t1, Transient_expr.repr t2)
        subst
      @ !name_subst

  let name_is_already_used name =
    List.mem name !named_vars
    || List.exists (fun (_, name') -> name = name') !names
    || String.Set.mem name !named_weak_vars

  let rec new_name () =
    let name = Misc.letter_of_int !name_counter in
    incr name_counter;
    if name_is_already_used name then new_name () else name

  let rec new_weak_name ty () =
    let name = "weak" ^ Int.to_string !weak_counter in
    incr weak_counter;
    if name_is_already_used name then new_weak_name ty ()
    else begin
        named_weak_vars := String.Set.add name !named_weak_vars;
        weak_var_map := TypeMap.add ty name !weak_var_map;
        name
      end

  let new_var_name ~non_gen ty () =
    if non_gen then new_weak_name ty ()
    else new_name ()

  let name_of_type name_generator t =
    (* We've already been through repr at this stage, so t is our representative
       of the union-find class. *)
    let t = substitute t in
    try List.assq t !names with Not_found ->
      try TransientTypeMap.find t !weak_var_map with Not_found ->
      let name =
        match t.desc with
          Tvar { name = Some name } | Tunivar { name = Some name } ->
            (* Some part of the type we've already printed has assigned another
             * unification variable to that name. We want to keep the name, so
             * try adding a number until we find a name that's not taken. *)
            let available name =
              List.for_all
                (fun (_, name') -> name <> name')
                !names
            in
            if available name then name
            else
              let suffixed i = name ^ Int.to_string i in
              let i = Misc.find_first_mono (fun i -> available (suffixed i)) in
              suffixed i
        | _ ->
            (* No name available, create a new one *)
            name_generator ()
      in
      (* Exception for type declarations *)
      if name <> "_" then names := (t, name) :: !names;
      name

  let check_name_of_type ~non_gen px =
    let name_gen = new_var_name ~non_gen (Transient_expr.type_expr px) in
    ignore(name_of_type name_gen px)

  let remove_names tyl =
    let tyl = List.map substitute tyl in
    names := List.filter (fun (ty,_) -> not (List.memq ty tyl)) !names

  let with_local_names f =
    let old_names = !names in
    let old_subst = !name_subst in
    names      := [];
    name_subst := [];
    try_finally
      ~always:(fun () ->
        names      := old_names;
        name_subst := old_subst)
      f

  let refresh_weak () =
    let refresh t name (m,s) =
      if is_non_gen Type_scheme t then
        begin
          TypeMap.add t name m,
          String.Set.add name s
        end
      else m, s in
    let m, s =
      TypeMap.fold refresh !weak_var_map (TypeMap.empty ,String.Set.empty) in
    named_weak_vars := s;
    weak_var_map := m
end

let reserve_names ty =
  normalize_type ty;
  Names.add_named_vars ty

let visited_objects = ref ([] : transient_expr list)
let aliased = ref ([] : transient_expr list)
let delayed = ref ([] : transient_expr list)
let printed_aliases = ref ([] : transient_expr list)

(* [printed_aliases] is a subset of [aliased] that records only those aliased
   types that have actually been printed; this allows us to avoid naming loops
   that the user will never see. *)

let add_delayed t =
  if not (List.memq t !delayed) then delayed := t :: !delayed

let is_aliased_proxy px = List.memq px !aliased

let add_alias_proxy px =
  if not (is_aliased_proxy px) then
    aliased := px :: !aliased

let add_alias ty = add_alias_proxy (proxy ty)

let add_printed_alias_proxy ~non_gen px =
  Names.check_name_of_type ~non_gen px;
  printed_aliases := px :: !printed_aliases

let add_printed_alias ty = add_printed_alias_proxy (proxy ty)

let aliasable ty =
  match get_desc ty with
    Tvar _ | Tunivar _ | Tpoly _ -> false
  | Tconstr (p, _, _) ->
      not (is_nth (snd (best_type_path p)))
  | _ -> true

let should_visit_object ty =
  match get_desc ty with
  | Tvariant row -> not (static_row row)
  | Tobject _ -> opened_object ty
  | _ -> false

let rec mark_loops_rec visited ty =
  let px = proxy ty in
  if List.memq px visited && aliasable ty then add_alias_proxy px else
    let tty = Transient_expr.repr ty in
    let visited = px :: visited in
    match tty.desc with
    | Tvariant _ | Tobject _ ->
        if List.memq px !visited_objects then add_alias_proxy px else begin
          if should_visit_object ty then
            visited_objects := px :: !visited_objects;
          printer_iter_type_expr (mark_loops_rec visited) ty
        end
    | Tpoly(ty, tyl) ->
        List.iter add_alias tyl;
        mark_loops_rec visited ty
    | _ ->
        printer_iter_type_expr (mark_loops_rec visited) ty

let mark_loops ty =
  mark_loops_rec [] ty

let prepare_type ty =
  reserve_names ty;
  mark_loops ty

let reset_loop_marks () =
  visited_objects := []; aliased := []; delayed := []; printed_aliases := []

let reset_except_context () =
  Names.reset_names (); reset_loop_marks (); Internal_names.reset ()

let reset () =
  reset_naming_context (); Conflicts.reset ();
  reset_except_context ()

let prepare_for_printing tyl =
  reset_except_context ();
  List.iter prepare_type tyl

let add_type_to_preparation = prepare_type

(* Disabled in classic mode when printing an unification error *)
let print_labels = ref true

let out_jkind_of_const_jkind jkind =
  Ojkind_const (Jkind.Const.to_out_jkind_const jkind)

(* CR layouts v2.8: This is just like [Jkind.format], and likely needs to
   be overhauled with [with]-types. *)
let rec out_jkind_of_desc (desc : 'd Jkind.Desc.t) =
  match desc.layout with
  | Sort (Var n) ->
    Ojkind_var ("'_representable_layout_" ^
                Int.to_string (Jkind.Sort.Var.get_print_number n))
  (* Analyze a product before calling [get_const]: the machinery in
     [Jkind.Const.to_out_jkind_const] works better for atomic layouts, not
     products. *)
  | Product lays ->
    Ojkind_product
      (List.map (fun layout -> out_jkind_of_desc { desc with layout }) lays)
  | _ -> match Jkind.Desc.get_const desc with
    | Some c -> out_jkind_of_const_jkind c
    | None -> assert false (* handled above *)

(* returns None for [value], according to (C2.1) from
   Note [When to print jkind annotations] *)
(* CR layouts v2.8: This should use the annotation in the jkind, if there
   is one. But first that annotation needs to be in Typedtree, not in
   Parsetree. *)
let out_jkind_option_of_jkind jkind =
  let desc = Jkind.get jkind in
  let elide =
    (* CR layouts: We ignore nullability here to avoid needlessly printing
       ['a : value_or_null] when it's not relevant (most cases).
       Unfortunately, this makes error messages really confusing, because
       we don't consider jkind annotations. *)
    Jkind.is_value_for_printing ~ignore_null:true jkind (* C2.1 *)
    || (match desc.layout with
        | Sort (Var _) -> not !Clflags.verbose_types (* X1 *)
        | _ -> false)
  in
  if elide then None else Some (out_jkind_of_desc desc)

let alias_nongen_row mode px ty =
    match get_desc ty with
    | Tvariant _ | Tobject _ ->
        if is_non_gen mode (Transient_expr.type_expr px) then
          add_alias_proxy px
    | _ -> ()

let outcome_label : Types.arg_label -> Outcometree.arg_label = function
  | Nolabel -> Nolabel
  | Labelled l -> Labelled l
  | Optional l -> Optional l
  | Position l -> Position l

let rec all_or_none f = function
  | [] -> Some []
  | x :: xs ->
    Option.bind (f x) (fun y ->
      Option.bind (all_or_none f xs) (fun ys ->
        Some (y :: ys)
        )
      )

let tree_of_modality_new (t: Parsetree.modality loc) =
  let Modality s = t.txt in Ogf_new s

let tree_of_modality_old (t: Parsetree.modality loc) =
  match t.txt with
  | Modality "global" -> Some (Ogf_legacy Ogf_global)
  | _ -> None

let tree_of_modalities mut t =
  let t = Typemode.untransl_modalities mut t in
  match all_or_none tree_of_modality_old t with
  | Some l -> l
  | None -> List.map tree_of_modality_new t

let tree_of_modalities_new mut t =
  let l = Typemode.untransl_modalities mut t in
  List.map (fun ({txt = Parsetree.Modality s; _}) -> s) l

(** [tree_of_mode m l] finds the outcome node in [l] that corresponds to [m].
Raise if not found. *)
let tree_of_mode_old (t : Parsetree.mode loc) =
  match t.txt with
  | Mode "local" -> Some (Omd_legacy Omd_local)
  | _ -> None

let tree_of_mode_new (t: Parsetree.mode loc) =
  let Mode s = t.txt in Omd_new s

let tree_of_modes (modes : Mode.Alloc.Const.t) =
  let diff = Mode.Alloc.Const.diff modes Mode.Alloc.Const.legacy in

  (* [yielding] has implied defaults depending on [areality]: *)
  let yielding =
    match modes.areality, modes.yielding with
    | Local, Yielding | Global, Unyielding -> None
    | _, _ -> Some modes.yielding
  in

  (* [contention] has implied defaults based on [visibility]: *)
  let contention =
    match modes.visibility, modes.contention with
    | Immutable, Contended | Read, Shared | Read_write, Uncontended -> None
    | _, _ -> Some modes.contention
  in

  (* [portability] has implied defaults based on [statefulness]: *)
  let portability =
    match modes.statefulness, modes.portability with
    | Stateless, Portable | (Observing | Stateful), Nonportable -> None
    | _, _ -> Some modes.portability
  in

  let diff = {diff with yielding; contention; portability} in
  (* The mapping passed to [tree_of_mode] must cover all non-legacy modes *)
  let l = Typemode.untransl_mode_annots diff in
  match all_or_none tree_of_mode_old l with
  | Some l -> l
  | None -> List.map tree_of_mode_new l

(* [alloc_mode] is the mode that our printing has expressed on [ty]. For the
  example [A -> local_ (B -> C)], we will call [tree_of_typexp] on (B -> C) with
  alloc_mode = local. This is helpful for reproducing the mode currying logic in
  [ctype.ml], so that parsing and printing roundtrip. *)
let rec tree_of_typexp mode alloc_mode ty =
  let px = proxy ty in
  if List.memq px !printed_aliases && not (List.memq px !delayed) then
   let non_gen = is_non_gen mode (Transient_expr.type_expr px) in
   let name = Names.name_of_type (Names.new_var_name ~non_gen ty) px in
   Otyp_var (non_gen, name) else

  let pr_typ () =
    let tty = Transient_expr.repr ty in
    match tty.desc with
    | Tvar _ ->
        let non_gen = is_non_gen mode ty in
        let name_gen = Names.new_var_name ~non_gen ty in
        Otyp_var (non_gen, Names.name_of_type name_gen tty)
    | Tarrow ((l, marg, mret), ty1, ty2, _) ->
        (* In this branch we do some mutation that needs to be reverted, as
           printing should not mutate states. *)
        let snap = Btype.snapshot () in
        let lab =
          if !print_labels || is_omittable l then outcome_label l
          else Nolabel
        in
        (* [marg] will contain undetermined axes. It would be imprecise if we
           don't print anything for those axes, since user would interpret that
           as legacy. The best we can do is to zap to legacy and if they do land
           at legacy, we will be able to omit printing them. *)
        let arg_mode = Alloc.zap_to_legacy marg in
        let t1 =
          if is_optional l then
            match
              get_desc (Ctype.expand_head !printing_env (tpoly_get_mono ty1))
            with
            | Tconstr(path, [ty], _)
              when Path.same path Predef.path_option ->
                tree_of_typexp mode arg_mode ty
            | _ -> Otyp_stuff "<hidden>"
          else
            tree_of_typexp mode arg_mode ty1
        in
        let acc_mode = curry_mode alloc_mode arg_mode in
        let (rm, t2) = tree_of_ret_typ_mutating mode acc_mode (mret, ty2) in
        Btype.backtrack snap;
        Otyp_arrow (lab, tree_of_modes arg_mode, t1, rm, t2)
    | Ttuple labeled_tyl ->
        Otyp_tuple (tree_of_labeled_typlist mode labeled_tyl)
    | Tunboxed_tuple labeled_tyl ->
        Otyp_unboxed_tuple (tree_of_labeled_typlist mode labeled_tyl)
    | Tconstr(p, tyl, _abbrev) ->
        let p', s = best_type_path p in
        let tyl' = apply_subst s tyl in
        if is_nth s && not (tyl'=[])
        then tree_of_typexp mode Alloc.Const.legacy (List.hd tyl')
        else begin
          Internal_names.add p';
          Otyp_constr (tree_of_path (Some Type) p', tree_of_typlist mode tyl')
        end
    | Tvariant row ->
        let Row {fields; name; closed; _} = row_repr row in
        let fields =
          if closed then
            List.filter (fun (_, f) -> row_field_repr f <> Rabsent)
              fields
          else fields in
        let present =
          List.filter
            (fun (_, f) ->
               match row_field_repr f with
               | Rpresent _ -> true
               | _ -> false)
            fields in
        let all_present = List.length present = List.length fields in
        begin match name with
        | Some(p, tyl) when nameable_row row ->
            let (p', s) = best_type_path p in
            let id = tree_of_path (Some Type) p' in
            let args = tree_of_typlist mode (apply_subst s tyl) in
            let out_variant =
              if is_nth s then List.hd args else Otyp_constr (id, args) in
            if closed && all_present then
              out_variant
            else
              let tags =
                if all_present then None else Some (List.map fst present) in
              Otyp_variant (Ovar_typ out_variant, closed, tags)
        | _ ->
            let fields = List.map (tree_of_row_field mode) fields in
            let tags =
              if all_present then None else Some (List.map fst present) in
            Otyp_variant (Ovar_fields fields, closed, tags)
        end
    | Tobject (fi, nm) ->
        tree_of_typobject mode fi !nm
    | Tnil | Tfield _ ->
        tree_of_typobject mode ty None
    | Tsubst _ ->
        (* This case should only happen when debugging the compiler *)
        Otyp_stuff "<Tsubst>"
    | Tlink _ ->
        fatal_error "Printtyp.tree_of_typexp"
    | Tpoly (ty, []) ->
        tree_of_typexp mode alloc_mode ty
    | Tpoly (ty, tyl) ->
        (*let print_names () =
          List.iter (fun (_, name) -> prerr_string (name ^ " ")) !names;
          prerr_string "; " in *)
        let tyl = List.map Transient_expr.repr tyl in
        let old_delayed = !delayed in
        (* Make the names delayed, so that the real type is
           printed once when used as proxy *)
        List.iter add_delayed tyl;
        let tl = tree_of_qtvs tyl in
        let tr = Otyp_poly (tl, tree_of_typexp mode alloc_mode ty) in
        (* Forget names when we leave scope *)
        Names.remove_names tyl;
        delayed := old_delayed; tr
    | Tunivar _ ->
        Otyp_var (false, Names.name_of_type Names.new_name tty)
    | Tpackage (p, fl) ->
        let fl =
          List.map
            (fun (li, ty) -> (
              String.concat "." (Longident.flatten li),
              tree_of_typexp mode Alloc.Const.legacy ty
            )) fl in
        Otyp_module (tree_of_path (Some Module_type) p, fl)
    | Tof_kind jkind ->
      Otyp_of_kind (out_jkind_of_desc (Jkind.get jkind))
  in
  if List.memq px !delayed then delayed := List.filter ((!=) px) !delayed;
  alias_nongen_row mode px ty;
  if is_aliased_proxy px && aliasable ty then begin
    let non_gen = is_non_gen mode (Transient_expr.type_expr px) in
    add_printed_alias_proxy ~non_gen px;
    (* add_printed_alias chose a name, thus the name generator
       doesn't matter.*)
    let alias = Names.name_of_type (Names.new_var_name ~non_gen ty) px in
    Otyp_alias {non_gen;  aliased = pr_typ (); alias } end
  else pr_typ ()

(* qtvs = quantified type variables *)
(* this silently drops any arguments that are not generic Tvar or Tunivar *)
and tree_of_qtvs qtvs =
  let tree_of_qtv v : (string * out_jkind option) option =
    let tree jkind = Some (Names.name_of_type Names.new_name v,
                            out_jkind_option_of_jkind jkind)
    in
    match v.desc with
    | Tvar { jkind } when v.level = generic_level -> tree jkind
    | Tunivar { jkind } -> tree jkind
    | _ -> None
  in
  List.filter_map tree_of_qtv qtvs

and tree_of_row_field mode (l, f) =
  match row_field_repr f with
  | Rpresent None | Reither(true, [], _) -> (l, false, [])
  | Rpresent(Some ty) -> (l, false, [tree_of_typexp mode Alloc.Const.legacy ty])
  | Reither(c, tyl, _) ->
      if c (* contradiction: constant constructor with an argument *)
      then (l, true, tree_of_typlist mode tyl)
      else (l, false, tree_of_typlist mode tyl)
  | Rabsent -> (l, false, [] (* actually, an error *))

and tree_of_typlist mode tyl =
  List.map (tree_of_typexp mode Alloc.Const.legacy) tyl

and tree_of_labeled_typlist mode tyl =
  List.map (fun (label, ty) -> label, tree_of_typexp mode Alloc.Const.legacy ty) tyl

and tree_of_typ_gf {ca_type=ty; ca_modalities=gf; _} =
  (tree_of_typexp Type Alloc.Const.legacy ty,
   tree_of_modalities Immutable gf)

(** We are on the RHS of an arrow type, where [ty] is the return type, and [m]
    is the return mode. This function decides the printed modes on [ty].
    - If [ty] is another arrow type, [acc_mode] is the mode that has accumulated
      from the currying, and thus the mode that the user would interpret as on
      [ty] if it doesn't have parens around it.
    - If [ty] is not an arrow type, [acc_mode] is meaningless.

    NB: This function might mutate states; the caller is responsible for
    reverting them. *)
and tree_of_ret_typ_mutating mode acc_mode (m, ty) =
  match get_desc ty with
  | Tarrow _ -> begin
      (* We first try to equate [m] with the [acc_mode]; if that succeeds, we
        can omit parens and modes. *)
      match Alloc.equate (Alloc.of_const acc_mode) m with
      | Ok () ->
        let ty = tree_of_typexp mode acc_mode ty in
        (Orm_no_parens, ty)
      | Error _ ->
        (* In this branch we need to print parens. [m] might have undetermined
        axes and we adopt a similar logic to the [marg] above. *)
        let m = Alloc.zap_to_legacy m in
        let ty = tree_of_typexp mode m ty in
        (Orm_parens (tree_of_modes m), ty)
      end
  | _ ->
    let m = Alloc.zap_to_legacy m in
    let ty = tree_of_typexp mode m ty in
    (Orm_not_arrow (tree_of_modes m), ty)

and tree_of_typobject mode fi nm =
  begin match nm with
  | None ->
      let pr_fields fi =
        let (fields, rest) = flatten_fields fi in
        let present_fields =
          List.fold_right
            (fun (n, k, t) l ->
               match field_kind_repr k with
               | Fpublic -> (n, t) :: l
               | _ -> l)
            fields [] in
        let sorted_fields =
          List.sort
            (fun (n, _) (n', _) -> String.compare n n') present_fields in
        tree_of_typfields mode rest sorted_fields in
      let (fields, open_row) = pr_fields fi in
      Otyp_object {fields; open_row}
  | Some (p, _ty :: tyl) ->
      let args = tree_of_typlist mode tyl in
      let (p', s) = best_type_path p in
      assert (s = Id);
      Otyp_class (tree_of_path (Some Type) p', args)
  | _ ->
      fatal_error "Printtyp.tree_of_typobject"
  end

and tree_of_typfields mode rest = function
  | [] ->
      let open_row =
        match get_desc rest with
        | Tvar _ | Tunivar _ | Tconstr _-> true
        | Tnil -> false
        | _ -> fatal_error "typfields (1)"
      in
      ([], open_row)
  | (s, t) :: l ->
      let field = (s, tree_of_typexp mode Alloc.Const.legacy t) in
      let (fields, rest) = tree_of_typfields mode rest l in
      (field :: fields, rest)

let tree_of_typexp mode ty = tree_of_typexp mode Alloc.Const.legacy ty

let typexp mode ppf ty =
  !Oprint.out_type ppf (tree_of_typexp mode ty)

(* Only used for printing a single modality in error message *)
let modality ?(id = fun _ppf -> ()) ppf modality =
  if Mode.Modality.is_id modality then id ppf
  else
    modality
    |> Typemode.untransl_modality
    |> tree_of_modality_new
    |> !Oprint.out_modality ppf

let prepared_type_expr ppf ty = typexp Type ppf ty

let type_expr ppf ty =
  (* [type_expr] is used directly by error message printers,
     we mark eventual loops ourself to avoid any misuse and stack overflow *)
  prepare_for_printing [ty];
  prepared_type_expr ppf ty

(* "Half-prepared" type expression: [ty] should have had its names reserved, but
   should not have had its loops marked. *)
let type_expr_with_reserved_names ppf ty =
  reset_loop_marks ();
  mark_loops ty;
  prepared_type_expr ppf ty

let shared_type_scheme ppf ty =
  prepare_type ty;
  typexp Type_scheme ppf ty

let prepared_type_scheme ppf ty = typexp Type_scheme ppf ty

let type_scheme ppf ty =
  prepare_for_printing [ty];
  prepared_type_scheme ppf ty

let type_path ppf p =
  let (p', s) = best_type_path p in
  let p'' = if (s = Id) then p' else p in
  let t = tree_of_path (Some Type) p'' in
  !Oprint.out_ident ppf t

let tree_of_type_scheme ty =
  prepare_for_printing [ty];
  tree_of_typexp Type_scheme ty

let () =
  Env.print_type_expr := type_expr;
  Jkind.set_outcometree_of_type (fun ty ->
    prepare_for_printing [ty];
    tree_of_typexp Type ty);
  Jkind.set_outcometree_of_modalities_new tree_of_modalities_new;
  Jkind.set_print_type_expr type_expr;
  Jkind.set_raw_type_expr raw_type_expr

(* Print one type declaration *)

let tree_of_constraints params =
  List.fold_right
    (fun ty list ->
       let ty' = unalias ty in
       if proxy ty != proxy ty' then
         let tr = tree_of_typexp Type_scheme ty in
         (tr, tree_of_typexp Type_scheme ty') :: list
       else list)
    params []

let filter_params tyl =
  let params =
    List.fold_left
      (fun tyl ty ->
        if List.exists (eq_type ty) tyl
        then newty2 ~level:generic_level (Ttuple [None, ty]) :: tyl
        else ty :: tyl)
      (* Two parameters might be identical due to a constraint but we need to
         print them differently in order to make the output syntactically valid.
         We use [Ttuple [ty]] because it is printed as [ty]. *)
      (* Replacing fold_left by fold_right does not work! *)
      [] tyl
  in List.rev params

let prepare_type_constructor_arguments args =
  List.iter prepare_type (tys_of_constr_args args)

(* returns an empty list if no variables in the list have a jkind annotation *)
let zap_qtvs_if_boring qtvs =
  if List.exists (fun (_v, l) -> Option.is_some l) qtvs
  then qtvs
  else []

(* get the free variables with their jkinds; do this *after* converting the
   type itself, so that the type names are available.
   This implements Case (C3) from Note [When to print jkind annotations]. *)
let extract_qtvs tyl =
  let fvs = Ctype.free_non_row_variables_of_list tyl in
  (* The [Ctype.free*variables] family of functions returns the free
     variables in reverse order they were encountered in the list of types.
  *)
  let fvs = List.rev fvs in
  let tfvs = List.map Transient_expr.repr fvs in
  let vars_jkinds = tree_of_qtvs tfvs in
  zap_qtvs_if_boring vars_jkinds

let param_jkind ty =
  match get_desc ty with
  | Tvar { jkind; _ } | Tunivar { jkind; _ } ->
     out_jkind_option_of_jkind jkind
  | _ -> None (* this is (C2.2) from Note [When to print jkind annotations] *)

let tree_of_label l =
  let mut =
    match l.ld_mutable with
    | Mutable { mode; atomic } ->
        let atomic =
          match atomic with
          | Atomic -> Atomic
          | Nonatomic -> Nonatomic
        in
        let mut =
          let open Value.Comonadic in
          match equate mode legacy with
          | Ok () -> Om_mutable (None, atomic)
          | Error _ -> Om_mutable (Some "<non-legacy>", atomic)
        in
        mut
    | Immutable -> Om_immutable
  in
  let ld_modalities = tree_of_modalities l.ld_mutable l.ld_modalities in
  (Ident.name l.ld_id, mut, tree_of_typexp Type l.ld_type, ld_modalities)

let tree_of_constructor_arguments = function
  | Cstr_tuple l -> List.map tree_of_typ_gf l
  | Cstr_record l -> [ Otyp_record (List.map tree_of_label l), [] ]

let tree_of_constructor_args_and_ret_type args ret_type =
  match ret_type with
  | None -> (tree_of_constructor_arguments args, None)
  | Some res ->
      let out_ret = tree_of_typexp Type res in
      let out_args = tree_of_constructor_arguments args in
      let qtvs = extract_qtvs (res :: tys_of_constr_args args) in
      (out_args, Some (qtvs, out_ret))

let tree_of_single_constructor cd =
  let name = Ident.name cd.cd_id in
  let args, ret = tree_of_constructor_args_and_ret_type cd.cd_args cd.cd_res in
  {
      ocstr_name = name;
      ocstr_args = args;
      ocstr_return_type = ret;
  }

(* When printing GADT constructor, we need to forget the naming decision we took
  for the type parameters and constraints. Indeed, in
  {[
  type 'a t = X: 'a -> 'b t
   ]}
  It is fine to print both the type parameter ['a] and the existentially
  quantified ['a] in the definition of the constructor X as ['a]
 *)
let tree_of_constructor_in_decl cd =
  match cd.cd_res with
  | None -> tree_of_single_constructor cd
  | Some _ -> Names.with_local_names (fun () -> tree_of_single_constructor cd)

let prepare_decl id decl =
  let params = filter_params decl.type_params in
  begin match decl.type_manifest with
  | Some ty ->
      let vars = free_variables ty in
      List.iter
        (fun ty ->
          match get_desc ty with
          | Tvar { name = Some "_"; jkind }
              when List.exists (eq_type ty) vars ->
            set_type_desc ty (Tvar {name = None; jkind})
          | _ -> ())
        params
  | None -> ()
  end;
  List.iter add_alias params;
  List.iter prepare_type params;
  List.iter (add_printed_alias ~non_gen:false) params;
  let ty_manifest =
    match decl.type_manifest with
    | None -> None
    | Some ty ->
        let ty =
          (* Special hack to hide variant name *)
          match get_desc ty with
            Tvariant row ->
              begin match row_name row with
                Some (Pident id', _) when Ident.same id id' ->
                  newgenty (Tvariant (set_row_name row None))
              | _ -> ty
              end
          | _ -> ty
        in
        prepare_type ty;
        Some ty
  in
  begin match decl.type_kind with
  | Type_abstract _ -> ()
  | Type_variant (cstrs, _rep,_umc) ->
      List.iter
        (fun c ->
           prepare_type_constructor_arguments c.cd_args;
           Option.iter prepare_type c.cd_res)
        cstrs
  | Type_record(l, _rep,_umc) ->
      List.iter (fun l -> prepare_type l.ld_type) l
  | Type_record_unboxed_product(l, _rep,_umc) ->
      List.iter (fun l -> prepare_type l.ld_type) l
  | Type_open -> ()
  end;
  ty_manifest, params

let tree_of_type_decl id decl =
  let ty_manifest, params = prepare_decl id decl in
  let type_param ot_variance ot_jkind =
    function
    | Otyp_var (ot_non_gen, ot_name) ->
        {ot_non_gen; ot_name; ot_variance; ot_jkind}
    | _ -> {ot_non_gen=false; ot_name="?"; ot_variance; ot_jkind}
  in
  let type_defined decl =
    let abstr =
      match decl.type_kind with
        Type_abstract _ ->
          decl.type_manifest = None || decl.type_private = Private
      | Type_record _ ->
          decl.type_private = Private
      | Type_record_unboxed_product _ ->
          decl.type_private = Private
      | Type_variant (tll, _rep,_umc) ->
          decl.type_private = Private ||
          List.exists (fun cd -> cd.cd_res <> None) tll
      | Type_open ->
          decl.type_manifest = None
    in
    let vari =
      List.map2
        (fun ty v ->
          let is_var = is_Tvar ty in
          if abstr || not is_var then
            let inj =
              type_kind_is_abstract decl && Variance.mem Inj v &&
              match decl.type_manifest with
              | None -> true
              | Some ty -> (* only abstract or private row types *)
                  decl.type_private = Private &&
                  Btype.is_constr_row ~allow_ident:true (Btype.row_of_type ty)
            and (co, cn) = Variance.get_upper v in
            (if not cn then Covariant else
             if not co then Contravariant else NoVariance),
            (if inj then Injective else NoInjectivity)
          else (NoVariance, NoInjectivity))
        decl.type_params decl.type_variance
    in
    let mk_param ty variance =
      let jkind = param_jkind ty in
      type_param variance jkind (tree_of_typexp Type ty)
    in
    (Ident.name id,
     List.map2 mk_param params vari)
  in
  let tree_of_manifest ty1 =
    match ty_manifest with
    | None -> ty1
    | Some ty -> Otyp_manifest (tree_of_typexp Type ty, ty1)
  in
  let (name, args) = type_defined decl in
  let constraints = tree_of_constraints params in
  let ty, priv, unboxed, or_null_reexport, unsafe_mode_crossing =
    match decl.type_kind with
    | Type_abstract _ ->
        begin match ty_manifest with
        | None -> (Otyp_abstract, Public, false, false, false)
        | Some ty ->
            tree_of_typexp Type ty, decl.type_private, false, false, false
        end
    | Type_variant (cstrs, rep, umc) ->
        let unboxed =
          match rep with
          | Variant_unboxed -> true
          | Variant_boxed _ | Variant_extensible | Variant_with_null -> false
        in
        (* CR layouts v3.5: remove when [Variant_with_null] is merged into
           [Variant_unboxed]. *)
        let or_null_reexport =
          match rep with
          | Variant_with_null -> true
          | Variant_boxed _ | Variant_unboxed | Variant_extensible -> false
        in
        tree_of_manifest (Otyp_sum (List.map tree_of_constructor_in_decl cstrs)),
        decl.type_private,
        unboxed,
        or_null_reexport,
        (Option.is_some umc)
    | Type_record(lbls, rep, umc) ->
        tree_of_manifest (Otyp_record (List.map tree_of_label lbls)),
        decl.type_private,
        (match rep with Record_unboxed -> true | _ -> false),
        false,
        (Option.is_some umc)
    | Type_record_unboxed_product(lbls, Record_unboxed_product, umc) ->
        tree_of_manifest
          (Otyp_record_unboxed_product (List.map tree_of_label lbls)),
        decl.type_private,
        false,
        false,
        (Option.is_some umc)
    | Type_open ->
        tree_of_manifest Otyp_open,
        decl.type_private,
        false,
        false,
        false
  in
  (* The algorithm for setting [lay] here is described as Case (C1) in
     Note [When to print jkind annotations] *)
  let is_value = Jkind.is_value_for_printing ~ignore_null:false decl.type_jkind in
  let otype_jkind =
    match ty, is_value, unsafe_mode_crossing with
    | (Otyp_abstract, false, _) | (_, _, true) ->
        (* The two cases of (C1) from the Note correspond to Otyp_abstract.
           Anything but the default must be user-written, so we print the
           user-written annotation. *)
        (* unsafe_mode_crossing corresponds to C1.2 *)
        Some (out_jkind_of_desc (Jkind.get decl.type_jkind))
    | _ -> None (* other cases have no jkind annotation *)
  in
  let attrs =
    if unsafe_mode_crossing
    then [{ oattr_name = "unsafe_allow_any_mode_crossing" }]
    else []
  in
    { otype_name = name;
      otype_params = args;
      otype_type = ty;
      otype_private = priv;
      otype_jkind;
      otype_unboxed = unboxed;
      otype_or_null_reexport = or_null_reexport;
      otype_cstrs = constraints;
      otype_attributes = attrs }

let add_type_decl_to_preparation id decl =
   ignore @@ prepare_decl id decl

let tree_of_prepared_type_decl id decl =
  tree_of_type_decl id decl

let tree_of_type_decl id decl =
  reset_except_context();
  tree_of_type_decl id decl

let add_constructor_to_preparation c =
  prepare_type_constructor_arguments c.cd_args;
  Option.iter prepare_type c.cd_res

let prepared_constructor ppf c =
  !Oprint.out_constr ppf (tree_of_single_constructor c)

let constructor ppf c =
  reset_except_context ();
  add_constructor_to_preparation c;
  prepared_constructor ppf c

let label ppf l =
  reset_except_context ();
  prepare_type l.ld_type;
  !Oprint.out_label ppf (tree_of_label l)

let tree_of_type_declaration id decl rs =
  Osig_type (tree_of_type_decl id decl, tree_of_rec rs)

let tree_of_prepared_type_declaration id decl rs =
  Osig_type (tree_of_prepared_type_decl id decl, tree_of_rec rs)

let type_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_type_declaration id decl Trec_first)

let add_type_declaration_to_preparation id decl =
  add_type_decl_to_preparation id decl

let prepared_type_declaration id ppf decl =
  !Oprint.out_sig_item ppf
    (tree_of_prepared_type_declaration id decl Trec_first)

let constructor_arguments ppf a =
  let tys = tree_of_constructor_arguments a in
  !Oprint.out_constr_args ppf tys

(* Print an extension declaration *)

(* When printing extension constructor, it is important to ensure that
after printing the constructor, we are still in the scope of the constructor.
For GADT constructor, this can be done by printing the type parameters inside
their own isolated scope. This ensures that in
{[
   type 'b t += A: 'b -> 'b any t
]}
the type parameter `'b` is not bound when printing the type variable `'b` from
the constructor definition from the type parameter.

Contrarily, for non-gadt constructor, we must keep the same scope for
the type parameters and the constructor because a type constraint may
have changed the name of the type parameter:
{[
type -'a t = .. constraint <x:'a. 'a t -> 'a> = 'a
(* the universal 'a is here to steal the name 'a from the type parameter *)
type 'a t = X of 'a
]} *)


let add_extension_constructor_to_preparation ext =
  let ty_params = filter_params ext.ext_type_params in
  List.iter add_alias ty_params;
  List.iter prepare_type ty_params;
  prepare_type_constructor_arguments ext.ext_args;
  Option.iter prepare_type ext.ext_ret_type

let prepared_tree_of_extension_constructor
   id ext es
  =
  let ty_name = Path.name ext.ext_type_path in
  let ty_params = filter_params ext.ext_type_params in
  let type_param =
    function
    | Otyp_var (_, id) -> id
    | _ -> "?"
  in
  let param_scope f =
    match ext.ext_ret_type with
    | None ->
        (* normal constructor: same scope for parameters and the constructor *)
        f ()
    | Some _ ->
        (* gadt constructor: isolated scope for the type parameters *)
        Names.with_local_names f
  in
  let ty_params =
    param_scope
      (fun () ->
         List.iter (add_printed_alias ~non_gen:false) ty_params;
         List.map (fun ty -> type_param (tree_of_typexp Type ty)) ty_params
      )
  in
  let name = Ident.name id in
  let args, ret =
    tree_of_constructor_args_and_ret_type
      ext.ext_args
      ext.ext_ret_type
  in
  let ext =
    { oext_name = name;
      oext_type_name = ty_name;
      oext_type_params = ty_params;
      oext_args = args;
      oext_ret_type = ret;
      oext_private = ext.ext_private }
  in
  let es =
    match es with
        Text_first -> Oext_first
      | Text_next -> Oext_next
      | Text_exception -> Oext_exception
  in
    Osig_typext (ext, es)

let tree_of_extension_constructor id ext es =
  reset_except_context ();
  add_extension_constructor_to_preparation ext;
  prepared_tree_of_extension_constructor id ext es

let extension_constructor id ppf ext =
  !Oprint.out_sig_item ppf (tree_of_extension_constructor id ext Text_first)

let prepared_extension_constructor id ppf ext =
  !Oprint.out_sig_item ppf
    (prepared_tree_of_extension_constructor id ext Text_first)

let extension_only_constructor id ppf ext =
  reset_except_context ();
  prepare_type_constructor_arguments ext.ext_args;
  Option.iter prepare_type ext.ext_ret_type;
  let name = Ident.name id in
  let args, ret =
    tree_of_constructor_args_and_ret_type
      ext.ext_args
      ext.ext_ret_type
  in
  Format.fprintf ppf "@[<hv>%a@]"
    !Oprint.out_constr {
      ocstr_name = name;
      ocstr_args = args;
      ocstr_return_type = ret;
    }

(* Print a value declaration *)

let tree_of_value_description id decl =
  (* Format.eprintf "@[%a@]@." raw_type_expr decl.val_type; *)
  let id = Ident.name id in
  let ty = tree_of_type_scheme decl.val_type in
  (* Important: process the fvs *after* the type; tree_of_type_scheme
     resets the naming context *)
  let snap = Btype.snapshot () in
  let moda =
    Ctype.zap_modalities_to_floor_if_modes_enabled_at Alpha decl.val_modalities
  in
  let qtvs = extract_qtvs [decl.val_type] in
  let apparent_arity =
    let rec count n typ =
      match get_desc typ with
      | Tarrow (_,_,typ,_) -> count (n+1) typ
      | _ -> n
    in
    count 0 decl.val_type
  in
  let attrs =
    match Zero_alloc.get decl.val_zero_alloc with
    | Default_zero_alloc | Ignore_assert_all -> []
    | Check { strict; opt; arity; custom_error_msg; loc = _; } ->
      [{ oattr_name =
           String.concat ""
             ["zero_alloc";
              if strict then " strict" else "";
              if opt then " opt" else "";
              if arity = apparent_arity then "" else
                Printf.sprintf " arity %d" arity;
              match custom_error_msg with
              | None -> ""
              | Some msg -> Printf.sprintf " custom_error_message %S" msg
             ] }]
    | Assume { strict; never_returns_normally; arity; _ } ->
      [{ oattr_name =
           String.concat ""
             ["zero_alloc assume";
              if strict then " strict" else "";
              if never_returns_normally then " never_returns_normally" else "";
              if arity = apparent_arity then "" else
                Printf.sprintf " arity %d" arity;
             ]
       }]
  in
  let vd =
    { oval_name = id;
      oval_type = Otyp_poly(qtvs, ty);
      oval_modalities = tree_of_modalities_new Immutable moda;
      oval_prims = [];
      oval_attributes = attrs
    }
  in
  let vd =
    match decl.val_kind with
    | Val_prim p -> Primitive.print p vd
    | _ -> vd
  in
  let r = Osig_value vd in
  Btype.backtrack snap;
  r

let value_description id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_value_description id decl)

(* Print a class type *)

let method_type priv ty =
  match priv, get_desc ty with
  | Mpublic, Tpoly(ty, tyl) -> (ty, tyl)
  | _ , _ -> (ty, [])

let prepare_method _lab (priv, _virt, ty) =
  let ty, _ = method_type priv ty in
  prepare_type ty

let tree_of_method mode (lab, priv, virt, ty) =
  let (ty, tyl) = method_type priv ty in
  let tty = tree_of_typexp mode ty in
  let tyl = List.map Transient_expr.repr tyl in
  let qtvs = tree_of_qtvs tyl in
  let qtvs = zap_qtvs_if_boring qtvs in
  Names.remove_names tyl;
  let priv = priv <> Mpublic in
  let virt = virt = Virtual in
  Ocsg_method (lab, priv, virt, Otyp_poly(qtvs, tty))

let rec prepare_class_type params = function
  | Cty_constr (_p, tyl, cty) ->
      let row = Btype.self_type_row cty in
      if List.memq (proxy row) !visited_objects
      || not (List.for_all is_Tvar params)
      || deep_occur_list row tyl
      then prepare_class_type params cty
      else List.iter prepare_type tyl
  | Cty_signature sign ->
      (* Self may have a name *)
      let px = proxy sign.csig_self_row in
      if List.memq px !visited_objects then add_alias_proxy px
      else visited_objects := px :: !visited_objects;
      Vars.iter (fun _ (_, _, ty) -> prepare_type ty) sign.csig_vars;
      Meths.iter prepare_method sign.csig_meths
  | Cty_arrow (_, ty, cty) ->
      prepare_type ty;
      prepare_class_type params cty

let rec tree_of_class_type mode params =
  function
  | Cty_constr (p', tyl, cty) ->
      let row = Btype.self_type_row cty in
      if List.memq (proxy row) !visited_objects
      || not (List.for_all is_Tvar params)
      then
        tree_of_class_type mode params cty
      else
        let namespace = Namespace.best_class_namespace p' in
        Octy_constr (tree_of_path namespace p', tree_of_typlist Type_scheme tyl)
  | Cty_signature sign ->
      let px = proxy sign.csig_self_row in
      let self_ty =
        if is_aliased_proxy px then
          Some
            (Otyp_var (false, Names.name_of_type Names.new_name px))
        else None
      in
      let csil = [] in
      let csil =
        List.fold_left
          (fun csil (ty1, ty2) -> Ocsg_constraint (ty1, ty2) :: csil)
          csil (tree_of_constraints params)
      in
      let all_vars =
        Vars.fold (fun l (m, v, t) all -> (l, m, v, t) :: all) sign.csig_vars []
      in
      (* Consequence of PR#3607: order of Map.fold has changed! *)
      let all_vars = List.rev all_vars in
      let csil =
        List.fold_left
          (fun csil (l, m, v, t) ->
            Ocsg_value (l, m = Asttypes.Mutable, v = Virtual, tree_of_typexp mode t)
            :: csil)
          csil all_vars
      in
      let all_meths =
        Meths.fold
          (fun l (p, v, t) all -> (l, p, v, t) :: all)
          sign.csig_meths []
      in
      let all_meths = List.rev all_meths in
      let csil =
        List.fold_left
          (fun csil meth -> tree_of_method mode meth :: csil)
          csil all_meths
      in
      Octy_signature (self_ty, List.rev csil)
  | Cty_arrow (l, ty, cty) ->
      let lab =
        if !print_labels || is_omittable l then outcome_label l
        else Nolabel
      in
      let tr =
       if is_optional l then
         match get_desc (Ctype.expand_head !printing_env ty) with
         | Tconstr(path, [ty], _) when Path.same path Predef.path_option ->
             tree_of_typexp mode ty
         | _ -> Otyp_stuff "<hidden>"
       else tree_of_typexp mode ty in
      Octy_arrow (lab, tr, tree_of_class_type mode params cty)

let class_type ppf cty =
  reset ();
  prepare_class_type [] cty;
  !Oprint.out_class_type ppf (tree_of_class_type Type [] cty)

let tree_of_class_param param variance =
  let ot_variance =
    if is_Tvar param then Asttypes.(NoVariance, NoInjectivity) else variance in
  (* CR layouts: fix next line when adding support for jkind
     annotations on class type parameters *)
  let ot_jkind = param_jkind param in
  match tree_of_typexp Type_scheme param with
    Otyp_var (ot_non_gen, ot_name) -> {ot_non_gen; ot_name; ot_variance; ot_jkind}
  | _ -> {ot_non_gen=false; ot_name="?"; ot_variance; ot_jkind}

let class_variance =
  let open Variance in let open Asttypes in
  List.map (fun v ->
    (if not (mem May_pos v) then Contravariant else
     if not (mem May_neg v) then Covariant else NoVariance),
    NoInjectivity)

let tree_of_class_declaration id cl rs =
  let params = filter_params cl.cty_params in

  reset_except_context ();
  List.iter add_alias params;
  prepare_class_type params cl.cty_type;
  let px = proxy (Btype.self_type_row cl.cty_type) in
  List.iter prepare_type params;

  List.iter (add_printed_alias ~non_gen:false) params;
  if is_aliased_proxy px then add_printed_alias_proxy ~non_gen:false px;

  let vir_flag = cl.cty_new = None in
  Osig_class
    (vir_flag, Ident.name id,
     List.map2 tree_of_class_param params (class_variance cl.cty_variance),
     tree_of_class_type Type_scheme params cl.cty_type,
     tree_of_rec rs)

let class_declaration id ppf cl =
  !Oprint.out_sig_item ppf (tree_of_class_declaration id cl Trec_first)

let tree_of_cltype_declaration id cl rs =
  let params = cl.clty_params in

  reset_except_context ();
  List.iter add_alias params;
  prepare_class_type params cl.clty_type;
  let px = proxy (Btype.self_type_row cl.clty_type) in
  List.iter prepare_type params;

  List.iter (add_printed_alias ~non_gen:false) params;
  if is_aliased_proxy px then (add_printed_alias_proxy ~non_gen:false) px;

  let sign = Btype.signature_of_class_type cl.clty_type in
  let has_virtual_vars =
    Vars.fold (fun _ (_,vr,_) b -> vr = Virtual || b)
      sign.csig_vars false
  in
  let has_virtual_meths =
    Meths.fold (fun _ (_,vr,_) b -> vr = Virtual || b)
      sign.csig_meths false
  in
  Osig_class_type
    (has_virtual_vars || has_virtual_meths, Ident.name id,
     List.map2 tree_of_class_param params (class_variance cl.clty_variance),
     tree_of_class_type Type_scheme params cl.clty_type,
     tree_of_rec rs)

let cltype_declaration id ppf cl =
  !Oprint.out_sig_item ppf (tree_of_cltype_declaration id cl Trec_first)

(* Print a module type *)

let wrap_env fenv ftree arg =
  (* We save the current value of the short-path cache *)
  (* From keys *)
  let env = !printing_env in
  let old_pers = !printing_pers in
  (* to data *)
  let old_map = !printing_map in
  let old_depth = !printing_depth in
  let old_cont = !printing_cont in
  set_printing_env (fenv env);
  let tree = ftree arg in
  if !Clflags.real_paths
     || same_printing_env env then ()
   (* our cached key is still live in the cache, and we want to keep all
      progress made on the computation of the [printing_map] *)
  else begin
    (* we restore the snapshotted cache before calling set_printing_env *)
    printing_old := env;
    printing_pers := old_pers;
    printing_depth := old_depth;
    printing_cont := old_cont;
    printing_map := old_map
  end;
  set_printing_env env;
  tree

let dummy =
  {
    type_params = [];
    type_arity = 0;
    type_kind = Type_abstract Definition;
    type_jkind = Jkind.Builtin.any ~why:Dummy_jkind;
    type_private = Public;
    type_manifest = None;
    type_variance = [];
    type_separability = [];
    type_is_newtype = false;
    type_expansion_scope = Btype.lowest_level;
    type_loc = Location.none;
    type_attributes = [];
    type_unboxed_default = false;
    type_uid = Uid.internal_not_actually_unique;
    type_unboxed_version = None;
  }

(** we hide items being defined from short-path to avoid shortening
    [type t = Path.To.t] into [type t = t].
*)

let ident_sigitem = function
  | Types.Sig_type(ident,_,_,_) ->  {hide=true;ident}
  | Types.Sig_class(ident,_,_,_)
  | Types.Sig_class_type (ident,_,_,_)
  | Types.Sig_module(ident,_, _,_,_)
  | Types.Sig_value (ident,_,_)
  | Types.Sig_modtype (ident,_,_)
  | Types.Sig_typext (ident,_,_,_)   ->  {hide=false; ident }

let hide ids env =
  let hide_id id env =
    (* Global idents cannot be renamed *)
    if id.hide && not (Ident.is_global_or_predef id.ident) then
      Env.add_type ~check:false (Ident.rename id.ident) dummy env
    else env
  in
  List.fold_right hide_id ids env

let with_hidden_items ids f =
  let with_hidden_in_printing_env ids f =
    wrap_env (hide ids) (Naming_context.with_hidden ids) f
  in
  if not !Clflags.real_paths then
    with_hidden_in_printing_env ids f
  else
    Naming_context.with_hidden ids f


let add_sigitem env x =
  Env.add_signature (Signature_group.flatten x) env

let expand_module_type =
  ref ((fun _env _mty -> assert false) :
      Env.t -> module_type -> module_type)

(** How to abbreviate signatures *)
module Abbrev = struct
  (* The code is substantially simpler if [width] is mutable. Strictly speaking, [depth]
     doesn't have to be mutable here but mixed mutability would be quite confusing. *)
  type t =
    { (* To what depth to unfold the module tree *)
      mutable depth : int
      (* How many signature items to print in total across all signatures *)
    ; mutable width : int
    }

  (** Standard abbreviation heuristic *)
  let abbrev () =
    { depth = 4
    ; width = 16
    }

  (** Don't print any signature items *)
  let ellipsis () =
    { depth = 0
    ; width =  0
    }

  (** Should we print anything in this signature *)
  let exhausted = function
    | Some {depth; width} -> depth <= 0 || width <= 0
    | None -> false

  (** Run [f] at one deeper unfolding level *)
  let deeper t f =
    match t with
    | Some t ->
        let saved = t.depth in
        t.depth <- t.depth - 1;
        let x = f () in
        t.depth <- saved;
        x
    | None -> f ()

  (** Reduce the remaining width by the number of items in [sg] and return the number of
      items to print in [sg] and a flag that inidicates whether [sg] is being trimmed. *)
  let items t sg =
    match t with
    | Some t ->
        let n = List.length sg in
        let k = min t.width n in
        t.width <- t.width - n;
        Some k, (k < n)
    | None ->
        None, false
end

let rec tree_of_modtype ?abbrev = function
  | Mty_ident p ->
      Omty_ident (tree_of_path (Some Module_type) p)
  | Mty_signature sg ->
      Omty_signature (tree_of_signature ?abbrev sg)
  | Mty_functor(param, ty_res) ->
      let param, env =
        tree_of_functor_parameter ?abbrev param
      in
      let res = wrap_env env (tree_of_modtype ?abbrev) ty_res in
      Omty_functor (param, res)
  | Mty_alias p ->
      Omty_alias (tree_of_path (Some Module) p)
  | Mty_strengthen _ as mty ->
      begin match !expand_module_type !printing_env mty with
      | Mty_strengthen (mty,p,a) ->
          let unaliasable =
            not (Aliasability.is_aliasable a)
            && not (Env.is_functor_arg p !printing_env)
          in
          Omty_strengthen
            (tree_of_modtype ?abbrev mty, tree_of_path (Some Module) p, unaliasable)
      | mty -> tree_of_modtype ?abbrev mty
      end

and tree_of_functor_parameter ?abbrev = function
  | Unit ->
      None, fun k -> k
  | Named (param, ty_arg) ->
      let name, env =
        match param with
        | None -> None, fun env -> env
        | Some id ->
            Some (Ident.name id),
            fun k -> Env.add_module ~arg:true id Mp_present ty_arg k
      in
      Some (name, tree_of_modtype ?abbrev ty_arg), env

and tree_of_signature ?abbrev = function
  | [] -> []
  | _ when Abbrev.exhausted abbrev -> [Osig_ellipsis]
  | sg ->
    Abbrev.deeper abbrev (fun () ->
      wrap_env (fun env -> env)(fun sg ->
        (* Only expand signatures to 'abbrev.depth' depth and print at most 'abbrev.width'
           items overall. We just keep decreasing 'abbrev.width' during the traversal but
           make sure that we expand the current signature up to 'abbrev.width' before
           expanding it's components. Below, 'max_items' is the number of items we should
           print in the current signature and 'abbrev.width' is then be the remaining
           number of items. This is simpler to implement than proper breadth-first. *)
        let max_items, trimmed = Abbrev.items abbrev sg in
        let tree_groups = tree_of_signature_rec ?abbrev ?max_items !printing_env sg in
        let items = List.concat_map (fun (_env,l) -> List.map snd l) tree_groups in
        if trimmed then items @ [Osig_ellipsis] else items
        ) sg
    )

and tree_of_signature_rec ?abbrev ?max_items env' sg =
  let structured = List.of_seq (Signature_group.seq sg) in
  (* Don't descent into more than 'max_items' (if set) elements to save time. *)
  let collect_trees_of_rec_group max_items group =
    match max_items with
    | Some n when n <= 0 -> (max_items, (!printing_env, []))
    | Some _ | None ->
        let env = !printing_env in
        let env', group_trees =
          Naming_context.with_ctx
            (fun () -> trees_of_recursive_sigitem_group ?abbrev env group)
        in
        set_printing_env env';
        let max_items, group_trees = match max_items with
          | None -> None, group_trees
          | Some n ->
              let rec take n acc xs =
                match n, xs with
                | 0, _ | _, [] -> n, List.rev acc
                | n, x :: xs -> take (n-1) (x :: acc) xs
              in
              let n, group_trees = take n [] group_trees in
              Some n, group_trees
        in
        max_items, (env, group_trees)
  in
  set_printing_env env';
  snd (List.fold_left_map collect_trees_of_rec_group max_items structured)

and trees_of_recursive_sigitem_group ?abbrev env
    (syntactic_group: Signature_group.rec_group) =
  let display (x:Signature_group.sig_item) = x.src, tree_of_sigitem ?abbrev x.src in
  let env = Env.add_signature syntactic_group.pre_ghosts env in
  match syntactic_group.group with
  | Not_rec x -> add_sigitem env x, [display x]
  | Rec_group items ->
      let ids = List.map (fun x -> ident_sigitem x.Signature_group.src) items in
      List.fold_left add_sigitem env items,
      with_hidden_items ids (fun () -> List.map display items)

and tree_of_sigitem ?abbrev = function
  | Sig_value(id, decl, _) ->
      tree_of_value_description id decl
  | Sig_type(id, decl, rs, _) ->
      tree_of_type_declaration id decl rs
  | Sig_typext(id, ext, es, _) ->
      tree_of_extension_constructor id ext es
  | Sig_module(id, _, md, rs, _) ->
      let abbrev =
        if List.exists (function
            | Parsetree.{attr_name = {txt="..."}; attr_payload = PStr []} -> true
            | _ -> false)
            md.md_attributes
          then Some (Abbrev.ellipsis ())
          else abbrev
      in
      tree_of_module ?abbrev id md rs
  | Sig_modtype(id, decl, _) ->
      tree_of_modtype_declaration ?abbrev id decl
  | Sig_class(id, decl, rs, _) ->
      tree_of_class_declaration id decl rs
  | Sig_class_type(id, decl, rs, _) ->
      tree_of_cltype_declaration id decl rs

and tree_of_modtype_declaration ?abbrev id decl =
  let mty =
    match decl.mtd_type with
    | None -> Omty_abstract
    | Some mty -> tree_of_modtype ?abbrev mty
  in
  Osig_modtype (Ident.name id, mty)

and tree_of_module ?abbrev id md rs =
  let snap = Btype.snapshot () in
  let moda = Ctype.zap_modalities_to_floor_if_at_least Alpha md.md_modalities in
  let r =
    Osig_module (Ident.name id, tree_of_modtype ?abbrev md.md_type,
    tree_of_modalities_new Immutable moda,
    tree_of_rec rs)
  in
  Btype.backtrack snap;
  r

let rec functor_parameters ~sep custom_printer = function
  | [] -> ignore
  | [id,param] ->
      Format.dprintf "%t%t"
        (custom_printer param)
        (functor_param ~sep ~custom_printer id [])
  | (id,param) :: q ->
      Format.dprintf "%t%a%t"
        (custom_printer param)
        sep ()
        (functor_param ~sep ~custom_printer id q)
and functor_param ~sep ~custom_printer id q =
  match id with
  | None -> functor_parameters ~sep custom_printer q
  | Some id ->
      Naming_context.with_arg id
        (fun () -> functor_parameters ~sep custom_printer q)



let modtype ppf mty = !Oprint.out_module_type ppf (tree_of_modtype mty)
let modtype_declaration id ppf decl =
  !Oprint.out_sig_item ppf (tree_of_modtype_declaration id decl)

(* For the toplevel: merge with tree_of_signature? *)

let print_items showval env x =
  Names.refresh_weak();
  reset_naming_context ();
  Conflicts.reset ();
  let extend_val env (sigitem,outcome) = outcome, showval env sigitem in
  let post_process (env,l) = List.map (extend_val env) l in
  List.concat_map post_process @@ tree_of_signature_rec env x

(* Print a signature body (used by -i when compiling a .ml) *)

let print_signature ppf tree =
  fprintf ppf "@[<v>%a@]" !Oprint.out_signature tree

let signature ppf sg =
  fprintf ppf "%a" print_signature (tree_of_signature sg)

(* Print a signature body (used by -i when compiling a .ml) *)
let printed_signature sourcefile ppf sg =
  (* we are tracking any collision event for warning 63 *)
  Conflicts.reset ();
  reset_naming_context ();
  let t = tree_of_signature sg in
  if Warnings.(is_active @@ Erroneous_printed_signature "")
  && Conflicts.exists ()
  then begin
    let conflicts = Format.asprintf "%t" Conflicts.print_explanations in
    Location.prerr_warning (Location.in_file sourcefile)
      (Warnings.Erroneous_printed_signature conflicts);
    Warnings.check_fatal ()
  end;
  fprintf ppf "%a" print_signature t

(* Trace-specific printing *)

(* A configuration type that controls which trace we print.  This could be
   exposed, but we instead expose three separate
   [report_{unification,equality,moregen}_error] functions.  This also lets us
   give the unification case an extra optional argument without adding it to the
   equality and moregen cases. *)
type 'variety trace_format =
  | Unification : Errortrace.unification trace_format
  | Equality    : Errortrace.comparison  trace_format
  | Moregen     : Errortrace.comparison  trace_format

let incompatibility_phrase (type variety) : variety trace_format -> string =
  function
  | Unification -> "is not compatible with type"
  | Equality    -> "is not equal to type"
  | Moregen     -> "is not compatible with type"

(* Print a unification error *)

let same_path t t' =
  eq_type t t' ||
  match get_desc t, get_desc t' with
    Tconstr(p,tl,_), Tconstr(p',tl',_) ->
      let (p1, s1) = best_type_path p and (p2, s2)  = best_type_path p' in
      begin match s1, s2 with
        Nth n1, Nth n2 when n1 = n2 -> true
      | (Id | Map _), (Id | Map _) when Path.same p1 p2 ->
          let tl = apply_subst s1 tl and tl' = apply_subst s2 tl' in
          List.length tl = List.length tl' &&
          List.for_all2 eq_type tl tl'
      | _ -> false
      end
  | _ ->
      false

type 'a diff = Same of 'a | Diff of 'a * 'a

let trees_of_type_expansion'
      ~var_jkinds mode Errortrace.{ty = t; expanded = t'} =
  let tree_of_typexp' ty =
    let out = tree_of_typexp mode ty in
    if var_jkinds then
      match get_desc ty with
      | Tvar { jkind; _ } | Tunivar { jkind; _ } ->
          let okind = out_jkind_of_desc (Jkind.get jkind) in
          Otyp_jkind_annot (out, okind)
      | _ ->
          out
    else
      out
  in
  reset_loop_marks ();
  mark_loops t;
  if same_path t t'
  then begin add_delayed (proxy t); Same (tree_of_typexp' t) end
  else begin
    mark_loops t';
    let t' = if proxy t == proxy t' then unalias t' else t' in
    (* beware order matter due to side effect,
       e.g. when printing object types *)
    let first = tree_of_typexp' t in
    let second = tree_of_typexp' t' in
    if first = second then Same first
    else Diff(first,second)
  end

let trees_of_type_expansion =
  trees_of_type_expansion' ~var_jkinds:false

let type_expansion ppf = function
  | Same t -> Style.as_inline_code !Oprint.out_type ppf t
  | Diff(t,t') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        (Style.as_inline_code !Oprint.out_type) t
        (Style.as_inline_code !Oprint.out_type) t'

let trees_of_trace mode =
  List.map (Errortrace.map_diff (trees_of_type_expansion mode))

let trees_of_type_path_expansion (tp,tp') =
  if Path.same tp tp' then Same(tree_of_path (Some Type) tp) else
    Diff(tree_of_path (Some Type) tp, tree_of_path (Some Type) tp')

let type_path_expansion ppf = function
  | Same p -> Style.as_inline_code !Oprint.out_ident ppf p
  | Diff(p,p') ->
      fprintf ppf "@[<2>%a@ =@ %a@]"
        (Style.as_inline_code !Oprint.out_ident) p
        (Style.as_inline_code !Oprint.out_ident) p'

let rec trace fst txt ppf = function
  | {Errortrace.got; expected} :: rem ->
      if not fst then fprintf ppf "@,";
      fprintf ppf "@[Type@;<1 2>%a@ %s@;<1 2>%a@]%a"
       type_expansion got txt type_expansion expected
       (trace false txt) rem
  | _ -> ()

type printing_status =
  | Discard
  | Keep
  | Optional_refinement
  (** An [Optional_refinement] printing status is attributed to trace
      elements that are focusing on a new subpart of a structural type.
      Since the whole type should have been printed earlier in the trace,
      we only print those elements if they are the last printed element
      of a trace, and there is no explicit explanation for the
      type error.
  *)

let diff_printing_status Errortrace.{ got      = {ty = t1; expanded = t1'};
                                      expected = {ty = t2; expanded = t2'} } =
  if  is_constr_row ~allow_ident:true t1'
   || is_constr_row ~allow_ident:true t2'
  then Discard
  else if same_path t1 t1' && same_path t2 t2' then Optional_refinement
  else Keep

let printing_status = function
  | Errortrace.Diff d -> diff_printing_status d
  | Errortrace.Escape {kind = Constraint} -> Keep
  | _ -> Keep

(** Flatten the trace and remove elements that are always discarded
    during printing *)

(* Takes [printing_status] to change behavior for [Subtype] *)
let prepare_any_trace printing_status tr =
  let clean_trace x l = match printing_status x with
    | Keep -> x :: l
    | Optional_refinement when l = [] -> [x]
    | Optional_refinement | Discard -> l
  in
  match tr with
  | [] -> []
  | elt :: rem -> elt :: List.fold_right clean_trace rem []

let prepare_trace f tr =
  prepare_any_trace printing_status (Errortrace.map f tr)

(** Keep elements that are [Diff _ ] and take the decision
    for the last element, require a prepared trace *)
let rec filter_trace keep_last = function
  | [] -> []
  | [Errortrace.Diff d as elt]
    when printing_status elt = Optional_refinement ->
    if keep_last then [d] else []
  | Errortrace.Diff d :: rem -> d :: filter_trace keep_last rem
  | _ :: rem -> filter_trace keep_last rem

let type_path_list =
  Format.pp_print_list ~pp_sep:(fun ppf () -> Format.pp_print_break ppf 2 0)
    type_path_expansion

(* Hide variant name and var, to force printing the expanded type *)
let hide_variant_name t =
  match get_desc t with
  | Tvariant row ->
      let Row {fields; more; name; fixed; closed} = row_repr row in
      if name = None then t else
      newty2 ~level:(get_level t)
        (Tvariant
           (create_row ~fields ~fixed ~closed ~name:None
              ~more:(newvar2 (get_level more)
                       (Jkind.Builtin.value ~why:Row_variable))))
  | _ -> t

let prepare_expansion Errortrace.{ty; expanded} =
  let expanded = hide_variant_name expanded in
  reserve_names ty;
  if not (same_path ty expanded) then reserve_names expanded;
  Errortrace.{ty; expanded}

let may_prepare_expansion compact (Errortrace.{ty; expanded} as ty_exp) =
  match get_desc expanded with
    Tvariant _ | Tobject _ when compact ->
      reserve_names ty; Errortrace.{ty; expanded = ty}
  | _ -> prepare_expansion ty_exp

let print_path p =
  Format.dprintf "%a" !Oprint.out_ident (tree_of_path (Some Type) p)

let print_tag ppf s = Style.inline_code ppf ("`" ^ s)

let print_tags =
  let comma ppf () = Format.fprintf ppf ",@ " in
  Format.pp_print_list ~pp_sep:comma print_tag

let is_unit_arg env ty =
  let ty, vars = tpoly_get_poly ty in
  if vars <> [] then false
  else begin
    match get_desc (Ctype.expand_head env ty) with
    | Tconstr (p, _, _) -> Path.same p Predef.path_unit
    | _ -> false
  end

let unifiable env ty1 ty2 =
  let snap = Btype.snapshot () in
  let res =
    try Ctype.unify env ty1 ty2; true
    with Unify _ -> false
  in
  Btype.backtrack snap;
  res

let explanation_diff env t3 t4 : (Format.formatter -> unit) option =
  match get_desc t3, get_desc t4 with
  | Tarrow (_, ty1, ty2, _), _
    when is_unit_arg env ty1 && unifiable env ty2 t4 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[@{<hint>Hint@}: Did you forget to provide %a as argument?@]"
          Style.inline_code "()"
        )
  | _, Tarrow (_, ty1, ty2, _)
    when is_unit_arg env ty1 && unifiable env t3 ty2 ->
      Some (fun ppf ->
        fprintf ppf
          "@,@[@{<hint>Hint@}: Did you forget to wrap the expression using \
           %a?@]"
          Style.inline_code "fun () ->"
        )
  | _ ->
      None

let explain_fixed_row_case ppf = function
  | Errortrace.Cannot_be_closed ->
      fprintf ppf "it cannot be closed"
  | Errortrace.Cannot_add_tags tags ->
      fprintf ppf "it may not allow the tag(s) %a"
        print_tags tags

let explain_fixed_row pos expl = match expl with
  | Fixed_private ->
    dprintf "The %a variant type is private" Errortrace.print_pos pos
  | Univar x ->
    reserve_names x;
    dprintf "The %a variant type is bound to the universal type variable %a"
      Errortrace.print_pos pos
      (Style.as_inline_code type_expr_with_reserved_names) x
  | Reified p ->
    dprintf "The %a variant type is bound to %a"
      Errortrace.print_pos pos
      (Style.as_inline_code
         (fun ppf p ->
           Internal_names.add p;
           print_path p ppf))
      p
  | Rigid -> ignore
  | Fixed_existential -> ignore

let explain_variant (type variety) : variety Errortrace.variant -> _ = function
  (* Common *)
  | Errortrace.Incompatible_types_for s ->
      Some(dprintf "@,Types for tag %a are incompatible"
             print_tag s
          )
  (* Unification *)
  | Errortrace.No_intersection ->
      Some(dprintf "@,These two variant types have no intersection")
  | Errortrace.No_tags(pos,fields) -> Some(
      dprintf
        "@,@[The %a variant type does not allow tag(s)@ @[<hov>%a@]@]"
        Errortrace.print_pos pos
        print_tags (List.map fst fields)
    )
  | Errortrace.Fixed_row (pos,
                          k,
                          (Univar _ | Reified _ | Fixed_private as e)) ->
      Some (
        dprintf "@,@[%t,@ %a@]" (explain_fixed_row pos e)
          explain_fixed_row_case k
      )
  | Errortrace.Fixed_row (_,_, (Rigid | Fixed_existential)) ->
      (* this case never happens *)
      None
  (* Equality & Moregen *)
  | Errortrace.Presence_not_guaranteed_for (pos, s) -> Some(
      dprintf
        "@,@[The tag %a is guaranteed to be present in the %a variant type,\
         @ but not in the %a@]"
        print_tag s
        Errortrace.print_pos (Errortrace.swap_position pos)
        Errortrace.print_pos pos
    )
  | Errortrace.Openness pos ->
      Some(dprintf "@,The %a variant type is open and the %a is not"
             Errortrace.print_pos pos
             Errortrace.print_pos (Errortrace.swap_position pos))

let explain_escape pre = function
  | Errortrace.Univ u ->
      reserve_names u;
      Some(
        dprintf "%t@,The universal variable %a would escape its scope"
          pre
          (Style.as_inline_code type_expr_with_reserved_names) u
      )
  | Errortrace.Constructor p -> Some(
      dprintf
        "%t@,@[The type constructor@;<1 2>%a@ would escape its scope@]"
        pre (Style.as_inline_code path) p
    )
  | Errortrace.Module_type p -> Some(
      dprintf
        "%t@,@[The module type@;<1 2>%a@ would escape its scope@]"
        pre (Style.as_inline_code path) p
    )
  | Errortrace.Equation Errortrace.{ty = _; expanded = t} ->
      reserve_names t;
      Some(
        dprintf "%t @,@[<hov>This instance of %a is ambiguous:@ %s@]"
          pre
          (Style.as_inline_code type_expr_with_reserved_names) t
          "it would escape the scope of its equation"
      )
  | Errortrace.Self ->
      Some (dprintf "%t@,Self type cannot escape its class" pre)
  | Errortrace.Constraint ->
      None

let explain_object (type variety) : variety Errortrace.obj -> _ = function
  | Errortrace.Missing_field (pos,f) -> Some(
      dprintf "@,@[The %a object type has no method %a@]"
        Errortrace.print_pos pos Style.inline_code f
    )
  | Errortrace.Abstract_row pos -> Some(
      dprintf
        "@,@[The %a object type has an abstract row, it cannot be closed@]"
        Errortrace.print_pos pos
    )
  | Errortrace.Self_cannot_be_closed ->
      Some (dprintf "@,Self type cannot be unified with a closed object type")

let explain_incompatible_fields name (diff: Types.type_expr Errortrace.diff) =
  reserve_names diff.got;
  reserve_names diff.expected;
  dprintf "@,@[The method %a has type@ %a,@ \
  but the expected method type was@ %a@]"
    Style.inline_code name
    (Style.as_inline_code type_expr_with_reserved_names) diff.got
    (Style.as_inline_code type_expr_with_reserved_names) diff.expected

let explanation (type variety) intro prev env
  : (Errortrace.expanded_type, variety) Errortrace.elt -> _ = function
  | Errortrace.Diff {got; expected} ->
    explanation_diff env got.expanded expected.expanded
  | Errortrace.Escape {kind; context} ->
    let pre =
      match context, kind, prev with
      | Some ctx, _, _ ->
        reserve_names ctx;
        dprintf "@[%t@;<1 2>%a@]" intro
          (Style.as_inline_code type_expr_with_reserved_names) ctx
      | None, Univ _, Some(Errortrace.Incompatible_fields {name; diff}) ->
        explain_incompatible_fields name diff
      | _ -> ignore
    in
    explain_escape pre kind
  | Errortrace.Incompatible_fields { name; diff} ->
    Some(explain_incompatible_fields name diff)
  | Errortrace.Variant v ->
    explain_variant v
  | Errortrace.Obj o ->
    explain_object o
  | Errortrace.Rec_occur(x,y) ->
    reserve_names x;
    reserve_names y;
    begin match get_desc x with
    | Tvar _ | Tunivar _  ->
        Some(fun ppf ->
          reset_loop_marks ();
          mark_loops x;
          mark_loops y;
          dprintf "@,@[<hov>The type variable %a occurs inside@ %a@]"
            (Style.as_inline_code prepared_type_expr) x
            (Style.as_inline_code prepared_type_expr) y
            ppf)
    | _ ->
        (* We had a delayed unification of the type variable with
           a non-variable after the occur check. *)
        Some ignore
        (* There is no need to search further for an explanation, but
           we don't want to print a message of the form:
             {[ The type int occurs inside int list -> 'a |}
        *)
    end
  | Errortrace.Bad_jkind (t,e) ->
      Some (dprintf "@ @[<hov>%a@]"
              (Jkind.Violation.report_with_offender
                 ~offender:(fun ppf -> type_expr ppf t)) e)
  | Errortrace.Bad_jkind_sort (t,e) ->
      Some (dprintf "@ @[<hov>%a@]"
              (Jkind.Violation.report_with_offender_sort
                 ~offender:(fun ppf -> type_expr ppf t)) e)
  | Errortrace.Unequal_var_jkinds (t1,k1,t2,k2) ->
      let fmt_history t k ppf =
        Jkind.(format_history ~intro:(
          dprintf "The layout of %a is %a" prepared_type_expr t format k) ppf k)
      in
      Some (dprintf "@ because the layouts of their variables are different.\
                     @ @[<v>%t@;%t@]"
              (fmt_history t1 k1) (fmt_history t2 k2))
  | Errortrace.Unequal_tof_kind_jkinds (k1, k2) ->
      let fmt_history which k ppf =
        Jkind.(format_history ~intro:(
          dprintf "The kind of %s is %a" which format k) ppf k)
      in
      Some (dprintf "@ because their kinds are different.\
                     @ @[<v>%t@;%t@]"
              (fmt_history "the first" k1) (fmt_history "the second" k2))

let mismatch intro env trace =
  Errortrace.explain trace (fun ~prev h -> explanation intro prev env h)

let explain mis ppf =
  match mis with
  | None -> ()
  | Some explain -> explain ppf

let warn_on_missing_def env ppf t =
  match get_desc t with
  | Tconstr (p,_,_) ->
    begin match Env.find_type p env with
    | exception Not_found ->
        fprintf ppf
          "@,@[<hov>Type %a is abstract because@ no corresponding\
           @ cmi file@ was found@ in path.@]" (Style.as_inline_code path) p
    | { type_manifest = Some _; _ } -> ()
    | { type_manifest = None; _ } as decl ->
        match type_origin decl with
        | Rec_check_regularity ->
            fprintf ppf
              "@,@[<hov>Type %a was considered abstract@ when checking\
               @ constraints@ in this@ recursive type definition.@]"
              (Style.as_inline_code path) p
        | Definition | Existential _ -> ()
      end
  | _ -> ()

let prepare_expansion_head empty_tr = function
  | Errortrace.Diff d ->
      Some (Errortrace.map_diff (may_prepare_expansion empty_tr) d)
  | _ -> None

let head_error_printer ~var_jkinds mode txt_got txt_but = function
  | None -> ignore
  | Some d ->
      let d =
        Errortrace.map_diff (trees_of_type_expansion' ~var_jkinds mode) d
      in
      dprintf "%t@;<1 2>%a@ %t@;<1 2>%a"
        txt_got type_expansion d.Errortrace.got
        txt_but type_expansion d.Errortrace.expected

let warn_on_missing_defs env ppf = function
  | None -> ()
  | Some Errortrace.{got      = {ty=te1; expanded=_};
                     expected = {ty=te2; expanded=_} } ->
      warn_on_missing_def env ppf te1;
      warn_on_missing_def env ppf te2

(* [subst] comes out of equality, and is [[]] otherwise *)
let error trace_format mode subst env tr txt1 ppf txt2 ty_expect_explanation =
  reset ();
  (* We want to substitute in the opposite order from [Eqtype] *)
  Names.add_subst (List.map (fun (ty1,ty2) -> ty2,ty1) subst);
  let tr =
    prepare_trace
      (fun ty_exp ->
         Errortrace.{ty_exp with expanded = hide_variant_name ty_exp.expanded})
      tr
  in
  let jkind_error = match Misc.last tr with
    | Some (Bad_jkind _ | Bad_jkind_sort _ | Unequal_var_jkinds _
           | Unequal_tof_kind_jkinds _) ->
        true
    | Some (Diff _ | Escape _ | Variant _ | Obj _ | Incompatible_fields _
           | Rec_occur _)
    | None ->
        false
  in
  let mis = mismatch txt1 env tr in
  match tr with
  | [] -> assert false
  | elt :: tr ->
    try
      print_labels := not !Clflags.classic;
      let tr = filter_trace (mis = None) tr in
      let head = prepare_expansion_head (tr=[]) elt in
      let tr = List.map (Errortrace.map_diff prepare_expansion) tr in
      let head_error =
        head_error_printer ~var_jkinds:jkind_error mode txt1 txt2 head
      in
      let tr = trees_of_trace mode tr in
      fprintf ppf
        "@[<v>\
          @[%t%t@]%a%t\
         @]"
        head_error
        ty_expect_explanation
        (trace false (incompatibility_phrase trace_format)) tr
        (explain mis);
      if env <> Env.empty && not jkind_error
       (* the jkinds mechanism has its own way of reporting missing cmis *)
      then warn_on_missing_defs env ppf head;
      Internal_names.print_explanations env ppf;
      Conflicts.print_explanations ppf;
      print_labels := true
    with exn ->
      print_labels := true;
      raise exn

let report_error trace_format ppf mode env tr
      ?(subst = [])
      ?(type_expected_explanation = fun _ -> ())
      txt1 txt2 =
  wrap_printing_env ~error:true env (fun () ->
    error trace_format mode subst env tr txt1 ppf txt2
      type_expected_explanation)

let report_unification_error ?type_expected_explanation
      ppf env ({trace} : Errortrace.unification_error) =
  report_error ?type_expected_explanation Unification ppf Type env
    ?subst:None trace

let report_equality_error
      ppf mode env ({subst; trace} : Errortrace.equality_error) =
  report_error Equality ppf mode env
    ~subst ?type_expected_explanation:None trace

let report_moregen_error
      ppf mode env ({trace} : Errortrace.moregen_error) =
  report_error Moregen ppf mode env
    ?subst:None ?type_expected_explanation:None trace

let report_comparison_error ppf mode env = function
  | Errortrace.Equality_error error -> report_equality_error ppf mode env error
  | Errortrace.Moregen_error  error -> report_moregen_error  ppf mode env error

module Subtype = struct
  (* There's a frustrating amount of code duplication between this module and
     the outside code, particularly in [prepare_trace] and [filter_trace].
     Unfortunately, [Subtype] is *just* similar enough to have code duplication,
     while being *just* different enough (it's only [Diff]) for the abstraction
     to be nonobvious.  Someday, perhaps... *)

  let printing_status = function
    | Errortrace.Subtype.Diff d -> diff_printing_status d

  let prepare_unification_trace = prepare_trace

  let prepare_trace f tr =
    prepare_any_trace printing_status (Errortrace.Subtype.map f tr)

  let trace filter_trace get_diff fst keep_last txt ppf tr =
    print_labels := not !Clflags.classic;
    try match tr with
      | elt :: tr' ->
        let diffed_elt = get_diff elt in
        let tr =
          trees_of_trace Type
          @@ List.map (Errortrace.map_diff prepare_expansion)
          @@ filter_trace keep_last tr' in
        let tr =
          match fst, diffed_elt with
          | true, Some elt -> elt :: tr
          | _, _ -> tr
        in
        trace fst txt ppf tr;
        print_labels := true
      | _ -> ()
    with exn ->
      print_labels := true;
      raise exn

  let rec filter_subtype_trace keep_last = function
    | [] -> []
    | [Errortrace.Subtype.Diff d as elt]
      when printing_status elt = Optional_refinement ->
        if keep_last then [d] else []
    | Errortrace.Subtype.Diff d :: rem ->
        d :: filter_subtype_trace keep_last rem

  let unification_get_diff = function
    | Errortrace.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)
    | _ -> None

  let subtype_get_diff = function
    | Errortrace.Subtype.Diff diff ->
        Some (Errortrace.map_diff (trees_of_type_expansion Type) diff)

  let report_error
        ppf
        env
        (Errortrace.Subtype.{trace = tr_sub; unification_trace = tr_unif})
        txt1 =
    wrap_printing_env ~error:true env (fun () ->
      reset ();
      let tr_sub = prepare_trace prepare_expansion tr_sub in
      let tr_unif = prepare_unification_trace prepare_expansion tr_unif in
      let keep_first = match tr_unif with
        | [Obj _ | Variant _ | Escape _ ] | [] -> true
        | _ -> false in
      fprintf ppf "@[<v>%a"
        (trace filter_subtype_trace subtype_get_diff true keep_first txt1)
        tr_sub;
      if tr_unif = [] then fprintf ppf "@]" else
        let mis = mismatch (dprintf "Within this type") env tr_unif in
        fprintf ppf "%a%t%t@]"
          (trace filter_trace unification_get_diff false
             (mis = None) "is not compatible with type") tr_unif
          (explain mis)
          Conflicts.print_explanations
    )
end

let report_ambiguous_type_error ppf env tp0 tpl txt1 txt2 txt3 =
  wrap_printing_env ~error:true env (fun () ->
    reset ();
    let tp0 = trees_of_type_path_expansion tp0 in
      match tpl with
      [] -> assert false
    | [tp] ->
        fprintf ppf
          "@[%t@;<1 2>%a@ \
             %t@;<1 2>%a\
           @]"
          txt1 type_path_expansion (trees_of_type_path_expansion tp)
          txt3 type_path_expansion tp0
    | _ ->
        fprintf ppf
          "@[%t@;<1 2>@[<hv>%a@]\
             @ %t@;<1 2>%a\
           @]"
          txt2 type_path_list (List.map trees_of_type_path_expansion tpl)
          txt3 type_path_expansion tp0)

(* Adapt functions to exposed interface *)
let abbreviate ~abbrev f =
  f ?abbrev:(if abbrev then Some (Abbrev.abbrev ()) else None)

let tree_of_path = tree_of_path None
let tree_of_module ident ?(ellipsis = false) =
  tree_of_module ident ?abbrev:(if ellipsis then Some (Abbrev.ellipsis ()) else None)
let tree_of_signature sg = tree_of_signature sg
let tree_of_modtype ?(abbrev = false) ty =
  abbreviate ~abbrev tree_of_modtype ty
let tree_of_modtype_declaration ?(abbrev = false) id md =
  abbreviate ~abbrev tree_of_modtype_declaration id md
let type_expansion mode ppf ty_exp =
  type_expansion ppf (trees_of_type_expansion mode ty_exp)
let tree_of_type_declaration ident td rs =
  with_hidden_items [{hide=true; ident}]
    (fun () -> tree_of_type_declaration ident td rs)
