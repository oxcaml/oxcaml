open Ppxlib

let () =
  let instrument =
    let transformation ctxt str =
      let loc =
        match str with
        | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
        | hd :: _ -> hd.pstr_loc
      in
      Location.raise_errorf ~loc
        "Raising a located exception during the first instrumentation phase"
    in
    Driver.Instrument.V2.make ~position:Driver.Instrument.Before transformation
  in
  Driver.V2.(register_transformation ~instrument "a_raise_exc")

let () =
  Driver.V2.(
    register_transformation
      ~impl:(fun ctxt str ->
        let loc =
          match str with
          | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
          | hd :: _ -> hd.pstr_loc
        in
        Location.raise_errorf ~loc
          "Raising a located exception during the Global transformation phase")
      "b_raise_exc_second")

let () =
  let instrument =
    let transformation ctxt str =
      let loc =
        match str with
        | [] -> Location.in_file (Expansion_context.Base.input_name ctxt)
        | hd :: _ -> hd.pstr_loc
      in
      Location.raise_errorf ~loc
        "Raising a located exception during the Last instrumentation phase"
    in
    Driver.Instrument.V2.make ~position:Driver.Instrument.After transformation
  in
  Driver.V2.(register_transformation ~instrument "c_raise_exc")

let () = Ppxlib.Driver.standalone ()
