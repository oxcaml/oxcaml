type t =
  { blocks : Jsir.Addr.t Flambda2_identifiers.Continuation.Map.t;
    vars : Jsir.Var.t Flambda2_identifiers.Variable.Map.t
  }

let create () =
  { blocks = Flambda2_identifiers.Continuation.Map.empty;
    vars = Flambda2_identifiers.Variable.Map.empty
  }

let add_block { blocks; vars } cont addr =
  { blocks = Flambda2_identifiers.Continuation.Map.add cont addr blocks; vars }

let add_var { blocks; vars } fvar jvar =
  { blocks; vars = Flambda2_identifiers.Variable.Map.add fvar jvar vars }

let get_block_exn { blocks; vars = _ } cont =
  Flambda2_identifiers.Continuation.Map.find cont blocks

let get_var_exn { blocks = _; vars } fvar =
  Flambda2_identifiers.Variable.Map.find fvar vars
