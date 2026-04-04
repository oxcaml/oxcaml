let run () =
  let model =
    Test_support.resolve_model
      ~name:"model-structure"
      ~source:
        {|
Locality = [
  Global < Local
]

Linearity = [
  Many < Once
]

Alloc = {
  areality : Locality;
  linearity : Linearity ^op;
}
|}
  in
  let product =
    match Model.String_map.find "Alloc" model.lattices with
    | Model.Product product -> product
    | Model.Base _ -> failwith "expected product lattice"
  in
  Test_support.ensure
    (List.length product.fields = 2)
    "expected two fields in product";
  Test_support.ensure
    (List.length product.axes = 2)
    "expected two derived axes in product";
  let axis_names = List.map (fun (axis : Model.axis_object) -> axis.name) product.axes in
  Test_support.ensure
    (axis_names = [ "areality"; "linearity" ])
    "unexpected axis names";
  let axis_ctors =
    List.map (fun (axis : Model.axis_object) -> axis.ctor_name) product.axes
  in
  Test_support.ensure
    (axis_ctors = [ "Areality"; "Linearity" ])
    "unexpected axis constructor names";
  let linearity_axis =
    List.find (fun (axis : Model.axis_object) -> axis.name = "linearity") product.axes
  in
  Test_support.ensure linearity_axis.declared_opposite "expected ^op to be preserved on axis";
  Test_support.ensure
    (linearity_axis.proj_name = "proj_linearity")
    "unexpected proj name";
  Test_support.ensure
    (linearity_axis.min_with_name = "min_with_linearity")
    "unexpected min_with name";
  Test_support.ensure
    (linearity_axis.max_with_name = "max_with_linearity")
    "unexpected max_with name";
  let object_ = Model.String_map.find "Alloc" model.objects in
  Test_support.ensure
    (object_.opposite_name = "Alloc_op")
    "unexpected opposite object name";
  match object_.shape with
  | Model.Base_object -> failwith "expected product object"
  | Model.Product_object axes ->
    Test_support.ensure
      (List.map (fun (axis : Model.axis_object) -> axis.name) axes = axis_names)
      "object axes do not match product axes"
