module Modality = Mode.Modality
module Const = Modality.Const

let annotation offset txt =
  let loc_start =
    { Lexing.pos_fname = "modality_annotation_test.ml";
      pos_lnum = 1;
      pos_bol = 0;
      pos_cnum = offset
    }
  in
  let loc_end = { loc_start with pos_cnum = offset + String.length txt } in
  { Location.txt;
    loc = { Location.loc_start; loc_end; loc_ghost = false }
  }

let translate ?(default = Const.id) annotations =
  let annotations =
    List.map (Location.map (fun txt -> Parsetree.Modality txt)) annotations
  in
  let translated =
    Typemode.transl_modalities_with_default ~maturity:Stable ~default
      annotations
  in
  translated.moda_modalities

let expect_annotation label axis modality expected =
  if Const.annotation axis modality <> expected then failwith label

let () =
  Language_extension.enable Mode Stable;
  let portability = Modality.Axis.Comonadic Portability in
  let contention = Modality.Axis.Monadic Contention in
  let portable = annotation 10 "portable" in
  let inherited = translate [portable] in
  expect_annotation "explicit annotation" portability inherited (Some portable);
  expect_annotation "inherited annotation" portability
    (translate ~default:inherited []) (Some portable);
  expect_annotation "unannotated axis" contention inherited None;
  let shareable = annotation 30 "shareable" in
  let overridden = translate ~default:inherited [shareable] in
  expect_annotation "explicit override" portability overridden (Some shareable);
  let nonportable = annotation 50 "nonportable" in
  let identity = translate ~default:inherited [nonportable] in
  assert (Const.is_id identity);
  expect_annotation "identity removes inherited bound" portability identity None;
  let stateless = annotation 70 "stateless" in
  let implied = translate [stateless] in
  expect_annotation "implied portability" portability implied (Some stateless);
  expect_annotation "override of implied portability" portability
    (translate [stateless; shareable]) (Some shareable);
  let immutable = annotation 90 "immutable" in
  expect_annotation "implied contention" contention
    (translate [immutable]) (Some immutable);
  let another_portable = annotation 110 "portable" in
  let same_bound = translate [another_portable] in
  assert (Result.is_ok (Const.equate inherited same_bound));
  assert (Const.diff inherited same_bound = []);
  List.iter
    (fun composed ->
      expect_annotation "composition keeps stronger bound" portability
        composed (Some portable))
    [ Const.concat ~then_:inherited overridden;
      Const.concat ~then_:overridden inherited ];
  expect_annotation "composition prefers outer annotation on a tie"
    portability (Const.concat ~then_:same_bound inherited)
    (Some another_portable);
  expect_annotation "composition with identity" portability
    (Const.concat ~then_:Const.id inherited) (Some portable);
  let contended = annotation 130 "contended" in
  let both = Const.concat ~then_:inherited (translate [contended]) in
  expect_annotation "composition preserves other axes" contention both
    (Some contended);
  let corruptible = annotation 150 "corruptible" in
  let combined =
    Const.concat ~then_:(translate [corruptible]) (translate [shareable])
  in
  assert (Const.proj portability combined = Meet_const Portable);
  expect_annotation "combined bound has no single originating annotation"
    portability combined None;
  List.iter
    (fun roundtripped ->
      expect_annotation "constant conversion preserves annotations"
        portability roundtripped (Some portable))
    [ Modality.to_const_exn (Modality.of_const inherited);
      Modality.zap_to_id (Modality.of_const inherited);
      Modality.zap_to_floor (Modality.of_const inherited) ];
  expect_annotation "setting a bound without an origin clears its annotation"
    portability (Const.set portability (Meet_const Portable) inherited) None
