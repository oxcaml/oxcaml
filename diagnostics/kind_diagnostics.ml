module Nlg = Diagnostic_nlg

type error =
  | Crossing of
      { loc : Location.t;
        subject : string;
        error : Ikind.subjkind_error
      }

let crossing ~loc ~subject error =
  let axes =
    Ikind.subjkind_error_violating_axes error
    |> List.map (fun (Jkind_axis.Axis.Pack axis) -> Jkind_axis.Axis.name axis)
  in
  match axes with
  | [] -> None
  | axes ->
    let suffix = match axes with [_] -> " axis" | _ -> " axes" in
    Some
      [ Nlg.plain
          ~claim:
               [ Nlg.ref_source loc
                   [ Nlg.txt
                       (subject ^ " does not cross the "
                      ^ String.concat ", " axes ^ suffix) ] ]
             ~contrast:
               [Nlg.txt "but the kind it is checked against requires it to"]
             ~background:
               [ [ Nlg.txt "a ";
                   Nlg.code "mod";
                   Nlg.txt
                     " annotation claims a type's values may be used at the \
                      stronger mode on those axes, whatever mode they are held \
                      at" ] ]
             () ]

let diagnose (Crossing { loc; subject; error }) = crossing ~loc ~subject error
