type error =
  | Crossing of
      { loc : Location.t;
        subject : string;
        error : Ikind.subjkind_error
      }

val diagnose : error -> Diagnostic_term.t Diagnostic_nlg.story list option
