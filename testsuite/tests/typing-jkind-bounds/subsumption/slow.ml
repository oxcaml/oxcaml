(* TEST
    flags = "-extension layouts_alpha -ikinds";
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
type t = q T0.value
Exception: Failure "".
|}]
