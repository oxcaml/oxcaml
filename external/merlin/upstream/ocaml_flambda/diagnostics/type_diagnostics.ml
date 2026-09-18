module Nlg = Diagnostic_nlg

type error =
  | Atomic_field_must_be_mutable of
      { loc : Location.t;
        name : string
      }
  | Non_value_atomic_field of Location.t
  | Mutable_field_in_unboxed_record of Location.t
  | Atomic_field_in_pattern of
      { loc : Location.t;
        field : Longident.t
      }
  | Non_atomic_field_access of
      { loc : Location.t;
        field : Longident.t
      }
  | Modalities_on_atomic_field of
      { loc : Location.t;
        field : Longident.t
      }
  | Invalid_atomic_access of Location.t
  | Bad_tail_annotation of
      { loc : Location.t;
        kind : [`Conflict | `Not_a_tailcall]
      }
  | Unsafe_mode_crossing_on_invalid_type_kind of Location.t

let field_name field =
  Option.value (Nlg.longident_name field) ~default:"this field"

let diagnose = function
  | Atomic_field_must_be_mutable { loc; name } ->
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc [Nlg.code name];
              Nlg.txt " is declared ";
              Nlg.code "[@atomic]";
              Nlg.txt " but is not mutable" ]
          |> Nlg.with_children
               [ Nlg.rule
                   [ Nlg.txt
                       "atomicity describes how a field is written, so only a \
                        mutable field can be atomic" ];
                 Nlg.suggestion
                   [ Nlg.txt "add ";
                     Nlg.code "mutable";
                     Nlg.txt ", or drop the ";
                     Nlg.code "[@atomic]" ] ] ] ]
  | Non_value_atomic_field loc ->
    let subject = Nlg.subject ~span:loc [Nlg.Phrase.Text "this field"] in
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc
                [ Nlg.mention ~case:Subject subject;
                  Nlg.copula;
                  Nlg.txt " declared ";
                  Nlg.code "[@atomic]" ] ];
          Nlg.but
            [ Nlg.mention ~case:Possessive subject;
              Nlg.txt " type does not have layout ";
              Nlg.code "value" ]
          |> Nlg.with_children
               [ Nlg.rule
                   [ Nlg.txt
                       "atomic access is implemented on values, which are \
                        word-sized and visible to the collector; unboxed \
                        layouts have no atomic representation" ];
                 Nlg.suggestion
                   [ Nlg.txt "use the boxed type, or drop the ";
                     Nlg.code "[@atomic]" ] ] ] ]
  | Mutable_field_in_unboxed_record loc ->
    let subject = Nlg.subject ~span:loc [Nlg.Phrase.Text "this label"] in
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc
                [ Nlg.mention ~case:Subject subject;
                  Nlg.copula;
                  Nlg.txt " declared ";
                  Nlg.code "mutable" ];
              Nlg.txt ", but ";
              Nlg.mention ~case:Subject subject;
              Nlg.txt " belongs to an unboxed record" ]
          |> Nlg.with_children
               [ Nlg.rule
                   [ Nlg.txt
                       "an unboxed record has no heap block and no identity, \
                        so there is no cell to mutate" ];
                 Nlg.suggestion
                   [ Nlg.txt
                       "use a boxed record, or store the unboxed record in a ";
                     Nlg.code "mutable";
                     Nlg.txt " field of one" ] ] ] ]
  | Atomic_field_in_pattern { loc; field } ->
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc
                [Nlg.txt "this pattern matches on "; Nlg.code (field_name field)];
              Nlg.txt ", which is an atomic field" ]
          |> Nlg.with_children
               [ Nlg.rule
                   [ Nlg.txt
                       "atomic fields are forbidden in patterns: the field may \
                        be read zero, one or several times depending on the \
                        patterns around it, so it is hard to reason about when \
                        the atomic read happens" ];
                 Nlg.suggestion
                   [ Nlg.txt "match the field with ";
                     Nlg.code "_";
                     Nlg.txt
                       " and read it in the body -- a wildcard is allowed, so \
                        every field can still be listed" ] ] ] ]
  | Non_atomic_field_access { loc; field } ->
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc [Nlg.code "[%atomic.loc]"];
              Nlg.txt " needs an atomic field" ];
          Nlg.but
            [Nlg.code (field_name field); Nlg.txt " is not declared atomic"]
          |> Nlg.with_children
               [ Nlg.suggestion
                   [ Nlg.txt "declare the field as ";
                     Nlg.code "mutable ... [@atomic]" ] ] ] ]
  | Modalities_on_atomic_field { loc; field } ->
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc [Nlg.code (field_name field)];
              Nlg.txt " carries a modality of its own" ];
          Nlg.but
            [ Nlg.txt "a field given to ";
              Nlg.code "[%atomic.loc]";
              Nlg.txt " may carry only the modalities implied by ";
              Nlg.code "mutable" ]
          |> Nlg.with_children
               [ Nlg.suggestion
                   [Nlg.txt "remove the modality from the field's declaration"]
               ] ] ]
  | Invalid_atomic_access loc ->
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc [Nlg.code "[%atomic.loc]"];
              Nlg.txt " takes a record field access, like ";
              Nlg.code "r.x";
              Nlg.txt ", but this payload is not one" ] ] ]
  | Bad_tail_annotation { loc; kind } ->
    let subject = Nlg.subject ~span:loc [Nlg.Phrase.Text "this call"] in
    let statement : _ Nlg.Phrase.t =
      [ Nlg.ref_source loc
          [ Nlg.mention ~case:Subject subject;
            Nlg.copula;
            Nlg.txt " annotated ";
            Nlg.code "[@tail]" ] ]
    in
    begin match kind with
    | `Conflict ->
      [ Nlg.block
          [ Nlg.state statement;
            Nlg.but
              [ Nlg.mention ~case:Possessive subject;
                Nlg.txt " tail-call annotations contradict each other" ]
            |> Nlg.with_children
                 [ Nlg.rule
                     [ Nlg.txt "a call cannot be required to be a tail call by ";
                       Nlg.code "[@tail]";
                       Nlg.txt " and required not to be by ";
                       Nlg.code "[@nontail]" ];
                   Nlg.suggestion [Nlg.txt "keep only one tail-call annotation"]
                 ] ] ]
    | `Not_a_tailcall ->
      [ Nlg.block
          [ Nlg.state statement;
            Nlg.but
              [ Nlg.mention ~case:Subject subject;
                Nlg.copula;
                Nlg.txt " not in tail position" ]
            |> Nlg.with_children
                 [ Nlg.rule
                     [ Nlg.txt
                         "a call is a tail call only when its result is the \
                          enclosing function's result" ];
                   Nlg.suggestion
                     [ Nlg.txt "use ";
                       Nlg.code "[@tail hint]";
                       Nlg.txt
                         " to ask for the optimisation only where it applies" ]
                 ] ] ]
    end
  | Unsafe_mode_crossing_on_invalid_type_kind loc ->
    [ Nlg.block
        [ Nlg.state
            [ Nlg.ref_source loc
                [ Nlg.txt "this declaration is marked ";
                  Nlg.code "[@@unsafe_allow_any_mode_crossing]" ] ];
          Nlg.but
            [ Nlg.txt
                "the attribute applies only to records, unboxed products and \
                 variants" ]
          |> Nlg.with_children
               [ Nlg.rule
                   [ Nlg.txt
                       "the attribute overrides the mode bounds computed from \
                        a type's fields or constructors; a type with neither \
                        has nothing to override" ] ] ] ]
