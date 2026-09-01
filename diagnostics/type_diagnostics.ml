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
        kind : [ `Conflict | `Not_a_tailcall ]
      }

let rec longident_name (lid : Longident.t) =
  match lid with
  | Lident name -> Some name
  | Ldot (prefix, name) ->
    Option.map
      (fun prefix -> prefix ^ "." ^ name.txt)
      (longident_name prefix.txt)
  | Lapply _ -> None

let field_name field =
  Option.value (longident_name field) ~default:"this field"

let story ~claim ?contrast ?(background = []) ?(suggestions = []) () =
  Nlg.realize_without_terms
    [Nlg.story ~claim ?contrast ~background ~suggestions ()]

let diagnose = function
  | Atomic_field_must_be_mutable { loc; name } ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.code name];
          Nlg.txt " is declared ";
          Nlg.code "[@atomic]";
          Nlg.txt " but is not mutable" ]
      ~background:
        [ [ Nlg.txt
              "atomicity describes how a field is written, so only a mutable \
               field can be atomic" ] ]
      ~suggestions:
        [ [ Nlg.txt "add ";
            Nlg.code "mutable";
            Nlg.txt ", or drop the ";
            Nlg.code "[@atomic]" ] ]
      ()
  | Non_value_atomic_field loc ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.txt "this field is declared "];
          Nlg.code "[@atomic]" ]
      ~contrast:[Nlg.txt "but its type does not have layout "; Nlg.code "value"]
      ~background:
        [ [ Nlg.txt
              "atomic access is implemented on values, which are word-sized \
               and visible to the collector; unboxed layouts have no atomic \
               representation" ] ]
      ~suggestions:
        [[Nlg.txt "use the boxed type, or drop the "; Nlg.code "[@atomic]"]]
      ()
  | Mutable_field_in_unboxed_record loc ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.txt "this label is declared "];
          Nlg.code "mutable";
          Nlg.txt ", but it belongs to an unboxed record" ]
      ~background:
        [ [ Nlg.txt
              "an unboxed record has no heap block and no identity, so there \
               is no cell to mutate" ] ]
      ~suggestions:
        [ [ Nlg.txt "use a boxed record, or store the unboxed record in a ";
            Nlg.code "mutable";
            Nlg.txt " field of one" ] ]
      ()
  | Atomic_field_in_pattern { loc; field } ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.txt "this pattern matches on "];
          Nlg.code (field_name field);
          Nlg.txt ", which is an atomic field" ]
      ~background:
        [ [ Nlg.txt
              "atomic fields are forbidden in patterns: the field may be read \
               zero, one or several times depending on the patterns around it, \
               so it is hard to reason about when the atomic read happens" ] ]
      ~suggestions:
        [ [ Nlg.txt "match the field with ";
            Nlg.code "_";
            Nlg.txt
              " and read it in the body -- a wildcard is allowed, so every \
               field can still be listed" ] ]
      ()
  | Non_atomic_field_access { loc; field } ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.code "[%atomic.loc]"];
          Nlg.txt " needs an atomic field" ]
      ~contrast:
        [ Nlg.txt "but ";
          Nlg.code (field_name field);
          Nlg.txt " is not declared atomic" ]
      ~suggestions:
        [[Nlg.txt "declare the field as "; Nlg.code "mutable ... [@atomic]"]]
      ()
  | Modalities_on_atomic_field { loc; field } ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.code (field_name field)];
          Nlg.txt " carries a modality of its own" ]
      ~contrast:
        [ Nlg.txt "but a field given to ";
          Nlg.code "[%atomic.loc]";
          Nlg.txt " may carry only the modalities implied by ";
          Nlg.code "mutable" ]
      ~suggestions:
        [[Nlg.txt "remove the modality from the field's declaration"]]
      ()
  | Invalid_atomic_access loc ->
    story
      ~claim:
        [ Nlg.ref_source loc [Nlg.code "[%atomic.loc]"];
          Nlg.txt " takes a record field access, like ";
          Nlg.code "r.x";
          Nlg.txt ", but this payload is not one" ]
      ()
  | Bad_tail_annotation { loc; kind } ->
    let claim =
      [ Nlg.ref_source loc [Nlg.txt "this call is annotated "];
        Nlg.code "[@tail]" ]
    in
    begin match kind with
    | `Conflict ->
      story ~claim
        ~contrast:
          [Nlg.txt "but its tail-call annotations contradict each other"]
        ~background:
          [ [ Nlg.txt "a call cannot be required to be a tail call by ";
              Nlg.code "[@tail]";
              Nlg.txt " and required not to be by ";
              Nlg.code "[@nontail]" ] ]
        ~suggestions:[[Nlg.txt "keep only one tail-call annotation"]]
        ()
    | `Not_a_tailcall ->
      story ~claim ~contrast:[Nlg.txt "but it is not in tail position"]
        ~background:
          [ [ Nlg.txt
                "a call is a tail call only when its result is the enclosing \
                 function's result" ] ]
        ~suggestions:
          [ [ Nlg.txt "use ";
              Nlg.code "[@tail hint]";
              Nlg.txt " to ask for the optimisation only where it applies" ] ]
        ()
    end
