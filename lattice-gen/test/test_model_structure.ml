let run () =
  let model =
    Test_support.resolve_model
      ~name:"model-structure"
      ~source:
        {|
A = [ Lo < Hi ]
B = [ Red < Blue ]

f : A -> B = [
  Lo -> Red;
  Hi -> Blue;
]

E = [ Cold < Warm < Hot ]

g_to_e : B -> E = [
  Red -> Cold;
  Blue -> Hot;
]

fg_compose : A -> E = compose(g_to_e, f)
fg_compose_unicode : A -> E = g_to_e ∘ f

P = {
  src : A;
  dst : B;
}

Q = {
  left : A;
  right : B;
}

R = {
  first : A;
  second : B;
}

Mix = {
  x : A;
  y : B^op;
}

Qop = {
  left : A;
  right : B^op;
}

id_p : P -> P = {
  src = src;
  dst = dst;
}

rename : P -> Q = {
  left = src;
  right = dst;
}

unrename : Q -> P = {
  src = left;
  dst = right;
}

rename_undeclared : P -> R = {
  first = src;
  second = dst;
}

C = [ Down < Up ]

D = [ Left < Right ]

u : C -> D = [
  Down -> Left;
  Up -> Right;
]

v : D -> C = [
  Left -> Down;
  Right -> Up;
]

id_q : Q -> Q = {
  left = left;
  right = right;
}

Small = [ Low < High ]

Tall = [ Low < Mid < High ]

s_to_mid : Small -> Tall = [
  Low -> Low;
  High -> Mid;
]

mid_to_s : Tall -> Small = [
  Low -> Low;
  Mid -> High;
  High -> High;
]

small_as_tall : Small -> Tall = [
  Low -> Low;
  High -> High;
]

f -| g -| h
id_p ⊣ id_p
rename -| unrename
rename_undeclared -| unrename_inferred
s_to_mid -| mid_to_s

Q2 = {
  lifted : A;
}

QB = {
  lifted_b : B;
}

QE = {
  lifted_e : E;
}

use_g : P -> Q2 = {
  lifted = g(dst);
}

use_h : P -> QB = {
  lifted_b = h(src);
}

use_compose : P -> QE = {
  lifted_e = (g_to_e ∘ f)(src);
}

mix_to_qop : Mix -> Qop = {
  left = x;
  right = y;
}

filled_q : P -> Q = {
  left = src;
  right = max;
}

Pair = {
  left : A;
  right : A;
}

Single = {
  value : A;
}

join_pair : Pair -> Single = {
  value = join(left, right);
}

meet_pair : Pair -> Single = {
  value = meet(left, right);
}

U2 = [ Aliased > Unique ]

C2 = [ Contended > Shared > Uncontended ]

M2 = {
  uniqueness : U2;
  contention : C2;
}

unique_cap2 : U2^op -> C2^op = [
  Aliased -> Contended;
  Unique -> Uncontended;
]

unique_implies_uncontended2 : M2^op -> M2^op = {
  uniqueness = uniqueness;
  contention = join(contention, unique_cap2(uniqueness));
}
|}
  in
  let product =
    match Model.String_map.find "P" model.lattices with
    | Model.Product product -> product
    | Model.Base _ -> failwith "expected product lattice"
  in
  Test_support.ensure
    (List.map (fun (field : Model.field) -> field.name) product.fields = [ "src"; "dst" ])
    "unexpected product fields";
  Test_support.ensure
    (List.map (fun (axis : Model.axis_object) -> axis.name) product.axes = [ "src"; "dst" ])
    "unexpected derived axes";
  let f =
    match Model.String_map.find "f" model.morphs with
    | Model.Primitive primitive -> primitive
    | Model.Bridge _ | Model.Composed _ -> failwith "expected primitive morph"
  in
  let f_core = f.core in
  Test_support.ensure
    (Option.equal String.equal f_core.left_name (Some "g"))
    "expected left adjoint of f to match g";
  Test_support.ensure
    (Option.equal String.equal f_core.right_name (Some "g"))
    "expected right adjoint of f to match g";
  let g =
    match Model.String_map.find "g" model.morphs with
    | Model.Primitive primitive -> primitive
    | Model.Bridge _ | Model.Composed _ -> failwith "expected primitive morph"
  in
  Test_support.ensure
    (Option.equal String.equal g.core.left_name (Some "f"))
    "expected inferred g left adjoint to match f";
  Test_support.ensure
    (Option.is_some g.core.right_name)
    "expected inferred g to expose a named right adjoint";
  let h =
    match Model.String_map.find "h" model.morphs with
    | Model.Primitive primitive -> primitive
    | Model.Bridge _ | Model.Composed _ -> failwith "expected primitive morph"
  in
  Test_support.ensure
    (Option.equal String.equal h.core.left_name (Some "g"))
    "expected inferred h left adjoint to match g";
  Test_support.ensure
    (Array.to_list h.core.map = Array.to_list f_core.map)
    "expected inferred h to match the second right-adjoint step from f";
  let fg_compose =
    match Model.String_map.find "fg_compose" model.morphs with
    | Model.Composed composed -> composed
    | Model.Primitive _ | Model.Bridge _ -> failwith "expected composed morph"
  in
  let fg_compose_unicode =
    match Model.String_map.find "fg_compose_unicode" model.morphs with
    | Model.Composed composed -> composed
    | Model.Primitive _ | Model.Bridge _ -> failwith "expected composed morph"
  in
  Test_support.ensure
    (Array.to_list fg_compose.core.map = [ 0; 2 ])
    "expected composed morph to compose maps extensionally";
  Test_support.ensure
    (Array.to_list fg_compose_unicode.core.map
     = Array.to_list fg_compose.core.map)
    "expected Unicode compose syntax to match compose(...)";
  let id_p =
    match Model.String_map.find "id_p" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Option.equal String.equal id_p.core.left_name (Some "id_p"))
    "expected identity bridge to be its own left adjoint";
  Test_support.ensure
    (Option.equal String.equal id_p.core.right_name (Some "id_p"))
    "expected identity bridge to be its own right adjoint";
  let rename =
    match Model.String_map.find "rename" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Option.equal String.equal rename.core.left_name (Some "unrename"))
    "expected rename bridge left adjoint to match unrename";
  Test_support.ensure
    (Option.equal String.equal rename.core.right_name (Some "unrename"))
    "expected rename bridge right adjoint to match unrename";
  let rename_undeclared =
    match Model.String_map.find "rename_undeclared" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Option.is_some rename_undeclared.core.left_adjoint)
    "expected rename_undeclared to have a left adjoint";
  Test_support.ensure
    (Option.is_some rename_undeclared.core.right_adjoint)
    "expected rename_undeclared to have a right adjoint";
  Test_support.ensure
    (Option.equal String.equal rename_undeclared.core.right_name
       (Some "unrename_inferred"))
    "expected rename_undeclared right adjoint name to be inferred";
  Test_support.ensure
    (Option.is_some rename_undeclared.core.left_name)
    "expected rename_undeclared left adjoint name to be discoverable";
  Test_support.ensure
    (Option.is_some rename_undeclared.core.right_name)
    "expected rename_undeclared right adjoint name to be visible";
  let unrename_inferred =
    match Model.String_map.find "unrename_inferred" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ ->
      failwith "expected inferred bridge morph"
  in
  Test_support.ensure
    (Array.to_list unrename_inferred.core.map
     = Array.to_list (Option.get rename_undeclared.core.right_adjoint))
    "expected inferred bridge morph to match rename_undeclared right adjoint";
  Test_support.ensure
    (Option.equal String.equal unrename_inferred.core.left_name
       (Some "rename_undeclared"))
    "expected inferred bridge left adjoint name to point back to rename_undeclared";
  let use_g =
    match Model.String_map.find "use_g" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Array.length use_g.core.map = 4)
    "expected later bridge using inferred g to resolve";
  let use_h =
    match Model.String_map.find "use_h" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Array.length use_h.core.map = 4)
    "expected later bridge using inferred h to resolve";
  let use_compose =
    match Model.String_map.find "use_compose" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Array.length use_compose.core.map = 4)
    "expected bridge using composed morph syntax to resolve";
  let mix_to_qop =
    match Model.String_map.find "mix_to_qop" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Array.length mix_to_qop.core.map = 4)
    "unexpected mixed-polarity product size";
  let filled_q =
    match Model.String_map.find "filled_q" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (List.length filled_q.assignments = 2)
    "unexpected assignment count in filled bridge";
  Test_support.ensure
    (Array.for_all (fun target -> target = 1 || target = 3) filled_q.core.map)
    "expected max-filled bridge to pin the right field to top";
  let join_pair =
    match Model.String_map.find "join_pair" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Array.to_list join_pair.core.map = [ 0; 1; 1; 1 ])
    "unexpected join bridge semantics";
  Test_support.ensure
    (Option.is_none join_pair.core.left_adjoint)
    "expected join bridge to lack a left adjoint";
  Test_support.ensure
    (Option.is_some join_pair.core.right_adjoint)
    "expected join bridge to have a right adjoint";
  let meet_pair =
    match Model.String_map.find "meet_pair" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Array.to_list meet_pair.core.map = [ 0; 0; 0; 1 ])
    "unexpected meet bridge semantics";
  Test_support.ensure
    (Option.is_some meet_pair.core.left_adjoint)
    "expected meet bridge to have a left adjoint";
  Test_support.ensure
    (Option.is_none meet_pair.core.right_adjoint)
    "expected meet bridge to lack a right adjoint";
  let unique_implies_uncontended2 =
    match Model.String_map.find "unique_implies_uncontended2" model.morphs with
    | Model.Bridge bridge -> bridge
    | Model.Primitive _ | Model.Composed _ -> failwith "expected product bridge"
  in
  Test_support.ensure
    (Option.is_none unique_implies_uncontended2.core.left_adjoint)
    "expected unique_implies_uncontended2 to lack a left adjoint";
  Test_support.ensure
    (Option.is_some unique_implies_uncontended2.core.right_adjoint)
    "expected unique_implies_uncontended2 to have a right adjoint";
  let warning_messages =
    List.map (fun (warning : Model.warning) -> warning.message) model.warnings
  in
  Test_support.ensure
    (List.exists
       (fun message ->
         String.equal
           message
           {|morphism "u" could be added to an adjoint chain: u -| v|})
       warning_messages)
    "expected a warning for the missing standalone adjoint chain";
  Test_support.ensure
    (List.exists
       (fun message ->
         String.equal
           message
           {|adjoint chain can be extended: s_to_mid -| mid_to_s -| small_as_tall|})
       warning_messages)
    "expected a warning for the extendable adjoint chain";
  Test_support.ensure
    (List.exists
       (fun message ->
         String.equal
           message
           {|morphism "unique_implies_uncontended2" has unnamed right adjoint; add an adjoint chain to synthesize it|})
       warning_messages)
    "expected a warning for an unnamed adjoint";
  Test_support.ensure
    (not
       (List.exists
          (fun message -> Test_support.contains message "fg_compose")
          warning_messages))
    "did not expect a warning for composed morphs";
  Test_support.ensure
    (not
       (List.exists
          (fun message -> Test_support.contains message "id_q")
          warning_messages))
    "did not expect a warning for the self-adjoint singleton morph"
