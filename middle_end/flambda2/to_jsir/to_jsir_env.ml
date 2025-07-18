module FI = Flambda2_identifiers

type t =
  { module_symbol : FI.Symbol.t;
    return_continuation : FI.Continuation.t;
    exn_continuation : FI.Continuation.t;
    continuations : Jsir.Addr.t FI.Continuation.Map.t;
    vars : Jsir.Var.t FI.Variable.Map.t;
    symbols : Jsir.Var.t FI.Symbol.Map.t
  }

let create ~module_symbol ~return_continuation ~exn_continuation =
  { module_symbol;
    return_continuation;
    exn_continuation;
    continuations = FI.Continuation.Map.empty;
    vars = FI.Variable.Map.empty;
    symbols = FI.Symbol.Map.empty
  }

let module_symbol t = t.module_symbol

let add_continuation t cont addr =
  { t with continuations = FI.Continuation.Map.add cont addr t.continuations }

let add_var t fvar jvar = { t with vars = FI.Variable.Map.add fvar jvar t.vars }

let add_symbol t symbol jvar =
  { t with symbols = FI.Symbol.Map.add symbol jvar t.symbols }

type continuation =
  | Return
  | Exception
  | Block of Jsir.Addr.t

let get_continuation_exn t cont =
  if cont = t.return_continuation
  then Return
  else if cont = t.exn_continuation
  then Exception
  else Block (FI.Continuation.Map.find cont t.continuations)

let get_var_exn t fvar = FI.Variable.Map.find fvar t.vars

let get_symbol_exn t symbol = FI.Symbol.Map.find symbol t.symbols

let add_alias_of_var_exn t ~var ~alias_of =
  let jvar = get_var_exn t alias_of in
  { t with vars = FI.Variable.Map.add var jvar t.vars }

let add_alias_of_symbol_exn t ~var ~alias_of =
  let jvar = get_symbol_exn t alias_of in
  { t with vars = FI.Variable.Map.add var jvar t.vars }
