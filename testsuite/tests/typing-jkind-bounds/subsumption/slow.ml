(* TEST
    flags = "-extension layouts_alpha";
    expect;
*)

module T0 = struct
  type 'codegen record = { spec : unit inner_record }
  and 'codegen variant = { spec : unit inner_variant }
  and 'codegen inner_record = { fields : unit field list }
  and 'codegen inner_variant = { cases : unit field list }
  and 'codegen optional = { contains : unit value }
  and 'codegen indexable = { element : unit value }
  and 'codegen foo = { element : unit value }
  and 'codegen bar = { element : unit value }
  and 'codegen derived = { spec : unit value }
  and 'codegen field = { inner : unit leaf }
  and 'codegen leaf = unit value
  and 'codegen value =
    unit record
    * unit variant
    * unit optional
    * unit indexable
    * unit derived
    * unit foo
    * unit bar
end

type q
type r
type t : immutable_data with r = q T0.value
(* The with r is just to make sure ikinds are triggered.
   Plain mod bounds still go through the old code path. *)

let _ : _ =
  if false then
    let _ : _ T0.value ref = ref (failwith "") in
    ()
[%%expect{|
module T0 :
  sig
    type 'codegen record = { spec : unit inner_record; }
    and 'codegen variant = { spec : unit inner_variant; }
    and 'codegen inner_record = { fields : unit field list; }
    and 'codegen inner_variant = { cases : unit field list; }
    and 'codegen optional = { contains : unit value; }
    and 'codegen indexable = { element : unit value; }
    and 'codegen foo = { element : unit value; }
    and 'codegen bar = { element : unit value; }
    and 'codegen derived = { spec : unit value; }
    and 'codegen field = { inner : unit leaf; }
    and 'codegen leaf = unit value
    and 'codegen value =
        unit record * unit variant * unit optional * unit indexable *
        unit derived * unit foo * unit bar
  end
type q
type r
Line 25, characters 0-43:
25 | type t : immutable_data with r = q T0.value
     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type "q T0.value" is
           immutable_data
             with unit T0.bar

             with unit T0.derived

             with unit T0.foo

             with unit T0.indexable

             with unit T0.optional

             with unit T0.record

             with unit T0.variant
         because it's a tuple type.
       But the kind of type "q T0.value" must be a subkind of
           immutable_data with r
         because of the definition of t at line 25, characters 0-43.
|}]
