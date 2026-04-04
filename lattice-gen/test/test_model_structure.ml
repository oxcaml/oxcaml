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
  let solver_object = Model.String_map.find "Alloc" model.solver_objects in
  let solver_object_op = Model.String_map.find "Alloc_op" model.solver_objects in
  Test_support.ensure
    (solver_object.object_ctor_name = "Obj_Alloc")
    "unexpected solver object constructor name";
  Test_support.ensure
    (match solver_object.orientation with
     | Model.Positive -> true
     | Model.Negative -> false)
    "expected positive solver orientation for base product";
  Test_support.ensure
    (match solver_object_op.orientation with
     | Model.Positive -> true
     | Model.Negative -> false)
    "expected positive solver orientation for opposite product";
  match object_.shape with
  | Model.Base_object -> failwith "expected product object"
  | Model.Product_object axes ->
    Test_support.ensure
      (List.map (fun (axis : Model.axis_object) -> axis.name) axes = axis_names)
      "object axes do not match product axes"
  ;
  match solver_object.shape with
  | Model.Solver_base -> failwith "expected packed product solver object"
  | Model.Solver_product axes ->
    Test_support.ensure
      (List.length axes = 2)
      "expected packed solver axes";
    let line_axis =
      List.find
        (fun (axis : Model.solver_axis) -> axis.axis.name = "linearity")
        axes
    in
    Test_support.ensure
      (line_axis.carrier_object_name = "Linearity_op")
      "unexpected solver carrier object for declared ^op field";
    Test_support.ensure
      (line_axis.proj_ctor_name = "Proj_Alloc_Linearity")
      "unexpected solver proj constructor name";
    Test_support.ensure
      (line_axis.min_with_ctor_name = "Min_with_Alloc_Linearity")
      "unexpected solver min_with constructor name";
    Test_support.ensure
      (line_axis.max_with_ctor_name = "Max_with_Alloc_Linearity")
      "unexpected solver max_with constructor name";
    match solver_object_op.shape with
    | Model.Solver_base -> failwith "expected packed opposite solver object"
    | Model.Solver_product axes_op ->
      let line_axis_op =
        List.find
          (fun (axis : Model.solver_axis) -> axis.axis.name = "linearity")
          axes_op
      in
      Test_support.ensure
        (line_axis_op.carrier_object_name = "Linearity")
        "unexpected opposite solver carrier object for ^op field"
