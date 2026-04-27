(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* # Unboxing

   Unboxing in the reaper starts by considering the set of values whose runtime
   representation can be changed (other than by replacing fields that are never
   read by a poison value). The criterion used for this is that the value must
   have a unique allocation point for each of its uses that read from it.
   Formally, a value allocated at a given point $x$ can be unboxed if, for each
   usage $y$ of $x$ that reads from $x$ for one of the fields [Block],
   [Value_slot], [Function_slot], [Is_int] or [Get_tag], (but not [Call_witness]
   which connects the call witness which is not read at runtime from $x$, nor
   [Apply] or [Code_id_of_call_witnes] which are only read from call witnesses),
   $y$ has known sources, and the only source of $y$ is $x$.

   In the case where $x$ has unknown usages, we can assume any field defined in
   $x$ that is not local might be read from it. As such, as soon as $x$ has a
   non-local field other than [Call_witness], the representation of $x$ may not
   be changed. Besides, if $x$ has a local field $f$ that is read from a value
   with an unknown source, the criterion above fails as well.

   Likewise, we identify the functions whose calling convention can be changed,
   which are those where at each application point, we know the call witness.

   Once we have identified the values whose runtime representation can be
   changed, we must decide which of those to unbox. There are additionnal
   criteria for this:

   - Symbols may not be unboxed, as non-value symbols are not possible, and
   symbols may contain non-symbol non-constant values that cannot be turned into
   a symbol.

   - A closure that is indirectly called may not be unboxed, where a closure is
   considered indirectly called if, at the point of apply, we are unable to
   identify precisely which call witness is in the closure.

   - A value that is stored inside another whose representation cannot be
   changed cannot be unboxed.

   - A value which is passed as argument to, or is the return value of, a
   function whose calling convention cannot be changed, cannot be unboxed
   either.

   Once the decisions are taken, we compute the required variables for each
   value that is unboxed. For that, we simply use one variable for each field
   that has a use, or we recursively compute the variables needed if the field
   is unboxed as well.

   Likewise, for values whose representation is changed, we compute for each
   field the variables needed to represent it at the point of the allocation. *)

module PTA = Points_to_analysis
open Global_flow_graph.Relations
open! PTA.Relations
open! Datalog_helpers.Syntax
open Datalog_helpers

module Unboxed_fields = struct
  type 'a t = 'a u Field.Map.t

  and 'a u =
    | Not_unboxed of 'a
    | Unboxed of 'a t

  let rec print_u pp_elem ppf = function
    | Not_unboxed x -> pp_elem ppf x
    | Unboxed fields -> print pp_elem ppf fields

  and print pp_elem ppf fields = Field.Map.print (print_u pp_elem) ppf fields

  let rec fold_with_kind (f : Flambda_kind.t -> 'a -> 'b -> 'b) (fields : 'a t)
      acc =
    Field.Map.fold
      (fun field elt acc ->
        match elt with
        | Not_unboxed elt -> f (Field.kind field) elt acc
        | Unboxed fields -> fold_with_kind f fields acc)
      fields acc

  let rec mapi_u (not_unboxed : 'a -> 'b -> 'c) (unboxed : Field.t -> 'a -> 'a)
      (acc : 'a) (uf : 'b u) : 'c u =
    match uf with
    | Not_unboxed x -> Not_unboxed (not_unboxed acc x)
    | Unboxed f -> Unboxed (mapi not_unboxed unboxed acc f)

  and mapi not_unboxed unboxed acc f =
    Field.Map.mapi
      (fun field uf -> mapi_u not_unboxed unboxed (unboxed field acc) uf)
      f

  let map f uf = mapi (fun () x -> f x) (fun _ () -> ()) () uf

  let map_u f uf =
    match uf with
    | Not_unboxed x -> Not_unboxed (f x)
    | Unboxed fields -> Unboxed (map f fields)

  (* This is not symmetrical!! [fields1] must define a subset of [fields2], but
     does not have to define all of them. *)
  let rec fold2_subset_u f fields1 fields2 acc =
    match fields1, fields2 with
    | Not_unboxed x1, Not_unboxed x2 -> f x1 x2 acc
    | Not_unboxed _, Unboxed _ | Unboxed _, Not_unboxed _ ->
      Misc.fatal_errorf "[fold2_unboxed_subset]"
    | Unboxed fields1, Unboxed fields2 -> fold2_subset f fields1 fields2 acc

  and fold2_subset f fields1 fields2 acc =
    Field.Map.fold
      (fun field f1 acc ->
        match Field.Map.find field fields2 with
        | exception Not_found ->
          Misc.fatal_errorf "@[<v 2>@[%a@]:@ @[%a@]@]@." Format.pp_print_text
            "Expected a subset of unboxed fields, but the following field is \
             not in the superset"
            Field.print field
        | f2 -> fold2_subset_u f f1 f2 acc)
      fields1 acc

  let rec fold2_subset_with_kind f fields1 fields2 acc =
    Field.Map.fold
      (fun field f1 acc ->
        match Field.Map.find field fields2 with
        | exception Not_found ->
          Misc.fatal_errorf "@[<v 2>@[%a@]:@ @[%a@]@]@." Format.pp_print_text
            "Expected a subset of unboxed fields, but the following field is \
             not in the superset"
            Field.print field
        | f2 -> (
          match f1, f2 with
          | Not_unboxed x1, Not_unboxed x2 -> f (Field.kind field) x1 x2 acc
          | Not_unboxed _, Unboxed _ | Unboxed _, Not_unboxed _ ->
            Misc.fatal_errorf "[fold2_unboxed_subset]"
          | Unboxed fields1, Unboxed fields2 ->
            fold2_subset_with_kind f fields1 fields2 acc))
      fields1 acc
end

(* CR-someday ncourant: track fields that are known to be constant, here and in
   changed_representation, to avoid having them be represented. This is a bit
   complex for two main reasons:

   - At this point in the dependency solver, we do not know the specific value
   of the constant but only that it is one (an alias to all_constants)

   - For symbols, this could break dominator scoping. *)
type unboxed = Variable.t Unboxed_fields.t

type changed_representation =
  (* CR ncourant: [Block_representation] is currently never produced, because we
     need to rewrite the value_kinds to account for changed representations
     before enabling it *)
  | Block_representation of
      (int * Flambda_primitive.Block_access_kind.t) Unboxed_fields.t * int
  | Closure_representation of
      Value_slot.t Unboxed_fields.t
      * Function_slot.t Function_slot.Map.t (* old -> new *)
      * Function_slot.t (* OLD current function slot *)

let pp_changed_representation ff = function
  | Block_representation (fields, size) ->
    Format.fprintf ff "(fields %a) (size %d)"
      (Unboxed_fields.print (fun ff (field, _) -> Format.pp_print_int ff field))
      fields size
  | Closure_representation (fields, function_slots, fs) ->
    Format.fprintf ff "(fields %a) (function_slots %a) (current %a)"
      (Unboxed_fields.print Value_slot.print)
      fields
      (Function_slot.Map.print Function_slot.print)
      function_slots Function_slot.print fs

let cannot_change_witness_calling_convention =
  rel1 "cannot_change_witness_calling_convention" Cols.[n]

let cannot_change_calling_convention =
  rel1 "cannot_change_calling_convention" Cols.[n]

let cannot_change_representation0 = rel1 "cannot_change_representation0" Cols.[n]

let cannot_change_representation1 = rel1 "cannot_change_representation1" Cols.[n]

let cannot_change_representation = rel1 "cannot_change_representation" Cols.[n]

let cannot_unbox0_tbl = Datalog.create_relation ~name:"cannot_unbox0" Cols.[n]

let cannot_unbox0 x = cannot_unbox0_tbl % [x]

let cannot_unbox = rel1 "cannot_unbox" Cols.[n]

let to_unbox = rel1 "to_unbox" Cols.[n]

let to_change_representation = rel1 "to_change_representation" Cols.[n]

let datalog_rules =
  saturate_in_order
    [ (* If any usage is possible, do not change the representation. Note that
         this rule will change in the future, when local value slots are
         properly tracked: a closure will only local value slots that has
         any_use will still be able to have its representation changed. *)
      (* (let$ [x] = ["x"] in [any_usage x] ==> cannot_change_representation0
         x); *)
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ any_usage x;
         unless1 Field.is_local field;
         when1 Field.is_real_field field;
         constructor ~base:x field ~from:y ]
       ==> cannot_change_representation0 x);
      (* If a block with a local field escapes, and that field is read again
         from an [any_source] value, prevent changing the representation. This
         ensures that for a block whose representation is changed, we can know
         the source at each point. *)
      (let$ [x; field; y; z] = ["x"; "field"; "y"; "z"] in
       [ any_usage x;
         when1 Field.is_local field;
         reading_field field z;
         constructor ~base:x field ~from:y ]
       ==> cannot_change_representation0 x);
      (* Likewise, if a block with a local field escapes, and that field is read
         again from a value with several sources, prevent changing the
         representation. *)
      (let$ [usage; field; source1; source2; v] =
         ["usage"; "field"; "source1"; "source2"; "v"]
       in
       [ rev_accessor ~base:usage field ~to_:v;
         has_usage v;
         when1 Field.is_local field;
         sources usage source1;
         has_source source1;
         sources usage source2;
         has_source source2;
         distinct Cols.n source1 source2 ]
       ==> cannot_change_representation0 source1);
      (* If there exists an alias which has another source, and which uses any
         real field of our allocation, we cannot change the representation. This
         currently requires 4 rules due to the absence of disjunction in the
         datalog engine. *)
      (let$ [allocation_id; alias; alias_source; field; v] =
         ["allocation_id"; "alias"; "alias_source"; "field"; "v"]
       in
       [ usages allocation_id alias;
         sources alias alias_source;
         has_source alias_source;
         distinct Cols.n alias_source allocation_id;
         when1 Field.is_real_field field;
         rev_accessor ~base:alias field ~to_:v;
         has_usage v ]
       ==> cannot_change_representation0 allocation_id);
      (let$ [allocation_id; alias; field; v] =
         ["allocation_id"; "alias"; "field"; "v"]
       in
       [ usages allocation_id alias;
         any_source alias;
         when1 Field.is_real_field field;
         rev_accessor ~base:alias field ~to_:v;
         has_usage v ]
       ==> cannot_change_representation0 allocation_id);
      (* If the allocation has a source distinct from itself, its representation
         cannot be changed (in fact, in that case, it shouldn't even be an
         allocation). *)
      (let$ [allocation_id; source] = ["allocation_id"; "source"] in
       [ sources allocation_id source;
         has_source source;
         distinct Cols.n source allocation_id ]
       ==> cannot_change_representation0 allocation_id);
      (* Used but not its own source: either from any source, or it has no
         source at all and it is dead code. In either case, do not unbox or
         change the representation. *)
      (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
       [ usages allocation_id usage;
         has_usage usage;
         ~~(sources allocation_id allocation_id) ]
       ==> cannot_change_representation0 allocation_id);
      (let$ [allocation_id] = ["allocation_id"] in
       [any_source allocation_id]
       ==> cannot_change_representation0 allocation_id);
      (let$ [call_witness; code_id] = ["call_witness"; "code_id"] in
       [ constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id ]
       ==> cannot_change_representation0 call_witness);
      (let$ [x] = ["x"] in
       [any_usage x] ==> cannot_change_witness_calling_convention x);
      (let$ [allocation_id; alias; alias_source; v] =
         ["allocation_id"; "alias"; "alias_source"; "v"]
       in
       [ usages allocation_id alias;
         sources alias alias_source;
         has_source alias_source;
         distinct Cols.n alias_source allocation_id;
         rev_accessor ~base:alias !!Field.code_id_of_call_witness ~to_:v;
         has_usage v ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id; alias; v] = ["allocation_id"; "alias"; "v"] in
       [ usages allocation_id alias;
         any_source alias;
         rev_accessor ~base:alias !!Field.code_id_of_call_witness ~to_:v;
         has_usage v ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id; source] = ["allocation_id"; "source"] in
       [ sources allocation_id source;
         has_source source;
         distinct Cols.n source allocation_id ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (* Used but not its own source: either from any source, or it has no
         source at all and it is dead code. In either case, do not unbox *)
      (let$ [allocation_id; usage] = ["allocation_id"; "usage"] in
       [ usages allocation_id usage;
         has_usage usage;
         ~~(sources allocation_id allocation_id) ]
       ==> cannot_change_witness_calling_convention allocation_id);
      (let$ [allocation_id] = ["allocation_id"] in
       [any_source allocation_id]
       ==> cannot_change_witness_calling_convention allocation_id);
      (* If the calling convention of a witness cannot be changed, the calling
         convention of its code_id cannot be either. From now on,
         [cannot_change_witness_calling_convention] should no longer be used. *)
      (let$ [call_witness; code_id] = ["call_witness"; "code_id"] in
       [ constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:code_id;
         has_usage call_witness;
         cannot_change_witness_calling_convention call_witness ]
       ==> cannot_change_calling_convention code_id);
      (* CR ncourant: we're preventing changing the calling convention of
         functions called with Indirect_unknown_arity. We could still allow
         changing the calling convention, but this would require wrappers for
         over- and partial applications, as well as untupling. As these wrappers
         are complex to write correctly, this is not done yet. *)
      (let$ [call_witness; codeid; set_of_closures] =
         ["call_witness"; "codeid"; "set_of_closures"]
       in
       [ rev_constructor ~from:call_witness
           !!Field.unknown_arity_call_witness
           ~base:set_of_closures;
         has_usage call_witness;
         constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid ]
       ==> cannot_change_calling_convention codeid);
      (* If the representation of any closure in a set of closures cannot be
         changed, the representation of all the closures in the set cannot be
         changed. *)
      (let$ [x] = ["x"] in
       [cannot_change_representation0 x] ==> cannot_change_representation1 x);
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ constructor ~base:x field ~from:y;
         when1 Field.is_function_slot field;
         cannot_change_representation0 x ]
       ==> cannot_change_representation1 y);
      (let$ [x] = ["x"] in
       [cannot_change_representation1 x] ==> cannot_change_representation x);
      (* Due to value_kinds rewriting not taking representation changes into
         account for now, blocks cannot have their representation changed, so we
         prevent it here. *)
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ constructor ~base:x field ~from:y;
         when1
           (fun f ->
             match Field.view f with
             | Block _ | Is_int | Get_tag -> true
             | Value_slot _ | Function_slot _ | Call_witness _
             | Return_of_call _ | Code_id_of_call_witness ->
               false)
           field ]
       ==> cannot_change_representation x);
      (* The use of [cannot_change_representation1] is here to still allow
         unboxing of blocks, even if we cannot change their representation due
         to the value_kind limitation. *)
      (let$ [x] = ["x"] in
       [cannot_change_representation1 x] ==> cannot_unbox0 x);
      (* This is repeated from the earlier occurrence in
         [cannot_change_representation0]. It is here because in the future, when
         we want to allow the changing of the representation of local value
         slots, it will remain necessary. *)
      (let$ [x] = ["x"] in
       [any_usage x] ==> cannot_unbox0 x);
      (* (let$ [x; field] = ["x"; "field"] in [ field_of_constructor_is_used x
         field; when1 field_cannot_be_destructured field ] ==> cannot_unbox0
         x); *)
      (* Unboxing a closure requires changing its calling convention, as we must
         pass the value slots as extra arguments. Thus, we prevent unboxing of
         closures if their calling convention cannot be changed. *)
      (let$ [x; call_witness; codeid] = ["x"; "call_witness"; "codeid"] in
       [ constructor ~base:x !!Field.known_arity_call_witness ~from:call_witness;
         constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid;
         cannot_change_calling_convention codeid ]
       ==> cannot_unbox0 x);
      (* An allocation that is one of the results of a function can only be
         unboxed if the function's calling conventation can be changed. *)
      (let$ [alias; allocation_id; relation; call_witness; codeid] =
         ["alias"; "allocation_id"; "relation"; "call_witness"; "codeid"]
       in
       [ sources alias allocation_id;
         has_source allocation_id;
         rev_constructor ~from:alias relation ~base:call_witness;
         when1
           (fun f ->
             match[@ocaml.warning "-4"] Field.view f with
             | Return_of_call _ -> true
             | _ -> false)
           relation;
         constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid;
         cannot_change_calling_convention codeid ]
       ==> cannot_unbox0 allocation_id);
      (* Likewise, an allocation passed as a parameter of a function can only be
         unboxed if the function's calling convention can be changed. *)
      (* CR ncourant: note that this can fail to trigger if the alias is
         any_source but has no use! This is not a problem but makes it necessary
         to replace unused params in calls with poison values. In the future, we
         could modify this check to ensure it only triggers if the variable is
         indeed used, allowing slightly more unboxing. *)
      (let$ [alias; allocation_id; relation; call_witness; codeid] =
         ["alias"; "allocation_id"; "relation"; "call_witness"; "codeid"]
       in
       [ sources alias allocation_id;
         has_source allocation_id;
         rev_parameter ~to_:alias relation ~base:call_witness;
         constructor ~base:call_witness
           !!Field.code_id_of_call_witness
           ~from:codeid;
         cannot_change_calling_convention codeid ]
       ==> cannot_unbox0 allocation_id);
      (* Cannot unbox parameters of [Indirect_unknown_arity] calls, even if they
         do not escape. *)
      (* (let$ [usage; allocation_id; relation; _v] = ["usage"; "allocation_id";
         "relation"; "_v"] in [ sources usage allocation_id; argument usage
         relation _v; filter (fun [f] -> match CoField.decode f with | Param
         (Unknown_arity_code_pointer, _) -> true | Param
         (Known_arity_code_pointer, _) -> false) [relation] ] ==> cannot_unbox0
         allocation_id); *)
      (* CR ncourant: I'm not sure this is useful? *)
      (* CR-someday ncourant: allowing a symbol to be unboxed is difficult, due
         to symbols being always values; thus we prevent it. *)
      (let$ [x; _source] = ["x"; "_source"] in
       [ sources x _source;
         when1
           (fun x ->
             Code_id_or_name.pattern_match x
               ~symbol:(fun _ -> true)
               ~var:(fun _ -> false)
               ~code_id:(fun _ -> false))
           x ]
       ==> cannot_unbox0 x);
      (* An allocation that is stored in another can only be unboxed if either
         the representation of the other allocation can be changed, of it the
         field it is stored in is never read, as in that case a poison value
         will be stored instead. *)
      (let$ [alias; allocation_id; relation; to_] =
         ["alias"; "allocation_id"; "relation"; "to_"]
       in
       [ sources alias allocation_id;
         rev_constructor ~from:alias relation ~base:to_;
         field_of_constructor_is_used to_ relation;
         cannot_change_representation to_;
         when1 Field.is_real_field relation;
         cannot_unbox0 to_ ]
       ==> cannot_unbox0 allocation_id);
      (* As previously: if any closure of a set of closures cannot be unboxed,
         then every closure in the set cannot be unboxed. *)
      (let$ [x] = ["x"] in
       [cannot_unbox0 x] ==> cannot_unbox x);
      (let$ [x; field; y] = ["x"; "field"; "y"] in
       [ cannot_unbox0 x;
         constructor ~base:x field ~from:y;
         when1 Field.is_function_slot field ]
       ==> cannot_unbox y);
      (* Compute allocations to unbox or to change representation. This requires
         the rules to be executed in order. *)
      (let$ [x] = ["x"] in
       [has_usage x; ~~(cannot_unbox x)] ==> to_unbox x);
      (let$ [x] = ["x"] in
       [has_usage x; ~~(cannot_change_representation x); ~~(to_unbox x)]
       ==> to_change_representation x) ]

type result =
  { db : Datalog.database;
    unboxed_fields : unboxed Code_id_or_name.Map.t;
    changed_representation :
      (changed_representation * Code_id_or_name.t) Code_id_or_name.Map.t
  }

let pp_result ppf res = Format.fprintf ppf "%a@." Datalog.print res.db

let rec mk_unboxed_fields ~has_to_be_unboxed ~mk db unboxed_block fields
    name_prefix =
  Field.Map.filter_map
    (fun field field_use ->
      match Field.view field with
      | Function_slot _ | Code_id_of_call_witness | Return_of_call _ ->
        Misc.fatal_errorf "Unexpected field kind %a in [mk_unboxed_fields]"
          Field.print field
      | Call_witness _ -> None
      | Block _ | Value_slot _ | Is_int | Get_tag -> (
        let field_source = PTA.get_single_field_source db unboxed_block field in
        match field_source with
        | No_source -> None
        | One _ | Many -> (
          let new_name =
            Format.asprintf "%s_field_%a" name_prefix
              Field.print_for_variable_name field
          in
          let[@local] default () =
            Some (Unboxed_fields.Not_unboxed (mk (Field.kind field) new_name))
          in
          match (field_use : _ Or_unknown.t) with
          | Unknown -> default ()
          | Known flow_to ->
            if Code_id_or_name.Map.is_empty flow_to
            then Misc.fatal_errorf "Empty set in [get_fields]";
            if
              Code_id_or_name.Map.for_all
                (fun k () -> has_to_be_unboxed k)
                flow_to
            then
              let new_unboxed_block =
                match field_source with
                | No_source ->
                  Misc.fatal_errorf
                    "Unexpected [No_source] for field %a in \
                     [mk_unboxed_fields] when creating nested unboxed fields"
                    Field.print field
                | Many ->
                  Misc.fatal_errorf
                    "[mk_unboxed_fields]: unboxed fields, but [Many] sources"
                | One v -> v
              in
              let usages = PTA.get_direct_usages db flow_to in
              let unboxed_fields =
                mk_unboxed_fields ~has_to_be_unboxed ~mk db new_unboxed_block
                  (PTA.get_fields db
                     (PTA.add_usages_through_function_slots
                        ~follow_known_arity_calls:true db (Usages usages)))
                  new_name
              in
              Some (Unboxed_fields.Unboxed unboxed_fields)
            else if
              Code_id_or_name.Map.exists
                (fun k () -> has_to_be_unboxed k)
                flow_to
            then
              Misc.fatal_errorf
                "Field %a of %s flows to both unboxed and non-unboxed variables"
                Field.print field name_prefix
            else default ())))
    fields

let has_to_be_unboxed =
  let^? [x], [alloc_point] = ["x"], ["alloc_point"] in
  [allocation_point_dominator x alloc_point; to_unbox alloc_point]

let query_to_unbox =
  query
    (let$ [x; y] = ["x"; "y"] in
     [to_unbox x; dominated_by_allocation_point x y] =>? [x; y])

let query_to_change_representation =
  query
    (let$ [x] = ["x"] in
     [to_change_representation x] =>? [x])

let query_dominated_by =
  query
    (let^$ [x], [y] = ["x"], ["y"] in
     [dominated_by_allocation_point x y] =>? [y])

let perform_analysis db ~stats =
  let db =
    Profile.record_call ~accumulate:true "compute_unboxing_decisions" (fun () ->
        (* We need to do this after [field_of_constructor_is_used] is computed,
           so that we prevent unboxing based on the number of fields actually
           used. *)
        let db =
          let max_unbox_size = Flambda_features.reaper_max_unbox_size () in
          (* CR-someday ncourant: it is unfortunate we need to go through the
             raw table API here *)
          Datalog.set_table cannot_unbox0_tbl
            (Code_id_or_name.Map.filter_map
               (fun _block fields ->
                 let num_used_fields =
                   Field.Map.fold
                     (fun field () acc ->
                       if
                         Field.is_real_field field
                         && not (Field.is_function_slot field)
                       then acc + 1
                       else acc)
                     fields 0
                 in
                 if num_used_fields > max_unbox_size then Some () else None)
               (Datalog.get_table field_of_constructor_is_used_tbl db))
            db
        in
        List.fold_left
          (fun db rule -> Datalog.Schedule.run ~stats rule db)
          db datalog_rules)
  in
  let name_of_node =
    if Flambda_features.debug_reaper "nostamps"
    then
      fun node ->
        Code_id_or_name.pattern_match node ~code_id:Code_id.name
          ~symbol:Symbol.linkage_name_as_string ~var:Variable.name
    else
      fun node ->
        Flambda_colours.without_colours ~f:(fun () ->
            Format.asprintf "%a" Code_id_or_name.print node)
  in
  let has_to_be_unboxed code_or_name = has_to_be_unboxed [code_or_name] db in
  let unboxed, changed_representation =
    Profile.record_call ~accumulate:true "compute_unboxing_variables" (fun () ->
        let unboxed =
          Datalog.Cursor.fold query_to_unbox db ~init:Code_id_or_name.Map.empty
            ~f:(fun [code_or_name; to_patch] unboxed ->
              (* CR-someday ncourant: produce ghost makeblocks/set of closures
                 for debugging *)
              let new_name =
                Format.asprintf "%s_into_%s"
                  (name_of_node code_or_name)
                  (name_of_node to_patch)
              in
              let usages =
                PTA.get_direct_usages db
                  (Code_id_or_name.Map.singleton to_patch ())
              in
              let fields =
                mk_unboxed_fields ~has_to_be_unboxed
                  ~mk:(fun kind name -> Variable.create name kind)
                  db code_or_name
                  (PTA.get_fields db
                     (PTA.add_usages_through_function_slots
                        ~follow_known_arity_calls:true db (Usages usages)))
                  new_name
              in
              Code_id_or_name.Map.add to_patch fields unboxed)
        in
        if Flambda_features.debug_reaper "unbox"
        then
          Format.printf "TO UNBOX: %a@."
            (Code_id_or_name.Map.print (Unboxed_fields.print Variable.print))
            unboxed;
        let changed_representation = ref Code_id_or_name.Map.empty in
        Datalog.Cursor.iter query_to_change_representation db
          ~f:(fun [code_id_or_name] ->
            (* This can happen because we change the representation of each
               function slot of a set of closures at the same time. *)
            if Code_id_or_name.Map.mem code_id_or_name !changed_representation
            then ()
            else
              let add_to_s repr alloc_point =
                Datalog.Cursor.iter_with_parameters query_dominated_by
                  [alloc_point] db ~f:(fun [c] ->
                    changed_representation
                      := Code_id_or_name.Map.add c (repr, alloc_point)
                           !changed_representation)
              in
              match PTA.get_set_of_closures_def db code_id_or_name with
              | Not_a_set_of_closures ->
                let r = ref ~-1 in
                let mk _kind _name =
                  (* XXX fixme, disabled for now *)
                  (* TODO depending on the kind, use two counters; then produce a
               mixed block; map_unboxed_fields should help with that *)
                  incr r;
                  ( !r,
                    Flambda_primitive.(
                      Block_access_kind.Values
                        { tag = Unknown;
                          size = Unknown;
                          field_kind = Block_access_field_kind.Any_value
                        }) )
                in
                let usages =
                  PTA.get_direct_usages db
                    (Code_id_or_name.Map.singleton code_id_or_name ())
                in
                let uses =
                  PTA.add_usages_through_function_slots
                    ~follow_known_arity_calls:false db (Usages usages)
                in
                let repr =
                  mk_unboxed_fields ~has_to_be_unboxed ~mk db code_id_or_name
                    (PTA.get_fields db uses) ""
                in
                add_to_s (Block_representation (repr, !r + 1)) code_id_or_name
              | Set_of_closures l ->
                let mk kind name =
                  Value_slot.create
                    (Compilation_unit.get_current_exn ())
                    ~name ~is_always_immediate:false kind
                in
                let fields =
                  PTA.get_fields_usage_of_constructors db
                    (List.fold_left
                       (fun acc (_, x) -> Code_id_or_name.Map.add x () acc)
                       Code_id_or_name.Map.empty l)
                in
                let repr =
                  mk_unboxed_fields ~has_to_be_unboxed ~mk db code_id_or_name
                    fields "unboxed"
                in
                let fss =
                  List.fold_left
                    (fun acc (fs, _) ->
                      Function_slot.Map.add fs
                        (Function_slot.create
                           (Compilation_unit.get_current_exn ())
                           ~name:(Function_slot.name fs)
                           ~is_always_immediate:false Flambda_kind.value)
                        acc)
                    Function_slot.Map.empty l
                in
                List.iter
                  (fun (fs, f) ->
                    add_to_s (Closure_representation (repr, fss, fs)) f)
                  l);
        if Flambda_features.debug_reaper "unbox"
        then
          Format.eprintf "@.TO_CHG: %a@."
            (Code_id_or_name.Map.print (fun ff (repr, alloc_point) ->
                 Format.fprintf ff "[from %a]%a" Code_id_or_name.print
                   alloc_point pp_changed_representation repr))
            !changed_representation;
        unboxed, !changed_representation)
  in
  if
    Flambda_features.reaper_unbox ()
    && Flambda_features.reaper_change_calling_conventions ()
  then { db; unboxed_fields = unboxed; changed_representation }
  else
    { db;
      unboxed_fields = Code_id_or_name.Map.empty;
      changed_representation = Code_id_or_name.Map.empty
    }

let cannot_change_calling_convention_query =
  let^? [x], [] = ["x"], [] in
  [cannot_change_calling_convention x]

let cannot_change_calling_convention uses v =
  (not (Flambda_features.reaper_change_calling_conventions ()))
  || (not (Compilation_unit.is_current (Code_id.get_compilation_unit v)))
  || cannot_change_calling_convention_query [Code_id_or_name.code_id v] uses.db
