type t =
  { module_symbol : Symbol.t;
    return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    continuations : Jsir.Addr.t Continuation.Map.t;
    vars : Jsir.Var.t Variable.Map.t;
    symbols : Jsir.Var.t Symbol.Map.t
  }

let create ~module_symbol ~return_continuation ~exn_continuation =
  { module_symbol;
    return_continuation;
    exn_continuation;
    continuations = Continuation.Map.empty;
    vars = Variable.Map.empty;
    symbols = Symbol.Map.empty
  }

let module_symbol t = t.module_symbol

let add_continuation t cont addr =
  { t with continuations = Continuation.Map.add cont addr t.continuations }

let add_var t fvar jvar = { t with vars = Variable.Map.add fvar jvar t.vars }

let add_symbol t symbol jvar =
  { t with symbols = Symbol.Map.add symbol jvar t.symbols }

type continuation =
  | Return
  | Exception
  | Block of Jsir.Addr.t

let get_continuation_exn t cont =
  if Continuation.equal cont t.return_continuation
  then Return
  else if Continuation.equal cont t.exn_continuation
  then Exception
  else Block (Continuation.Map.find cont t.continuations)

let get_var_exn t fvar = Variable.Map.find fvar t.vars

let get_symbol_exn t symbol = Symbol.Map.find symbol t.symbols

let add_alias_of_var_exn t ~var ~alias_of =
  let jvar = get_var_exn t alias_of in
  { t with vars = Variable.Map.add var jvar t.vars }

let add_alias_of_symbol_exn t ~var ~alias_of =
  let jvar = get_symbol_exn t alias_of in
  { t with vars = Variable.Map.add var jvar t.vars }
