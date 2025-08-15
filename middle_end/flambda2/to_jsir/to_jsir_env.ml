type exn_handler =
  { addr : Jsir.Addr.t;
    exn_param : Jsir.Var.t;
    extra_args : Jsir.Var.t list
  }

type t =
  { module_symbol : Symbol.t;
    return_continuation : Continuation.t;
    exn_continuation : Continuation.t;
    continuations : Jsir.Addr.t Continuation.Map.t;
    exn_handlers : exn_handler Continuation.Map.t;
    vars : Jsir.Var.t Variable.Map.t;
    symbols : Jsir.Var.t Symbol.Map.t;
    code_ids : (Jsir.Addr.t * Jsir.Var.t list) Code_id.Map.t;
    function_slots : Jsir.Var.t Function_slot.Map.t;
    value_slots : Jsir.Var.t Value_slot.Map.t;
    traps : Continuation.t list
  }

let create ~module_symbol ~return_continuation ~exn_continuation =
  { module_symbol;
    return_continuation;
    exn_continuation;
    continuations = Continuation.Map.empty;
    exn_handlers = Continuation.Map.empty;
    vars = Variable.Map.empty;
    symbols = Symbol.Map.empty;
    code_ids = Code_id.Map.empty;
    function_slots = Function_slot.Map.empty;
    value_slots = Value_slot.Map.empty;
    traps = []
  }

let return_continuation t = t.return_continuation

let exn_continuation t = t.exn_continuation

let enter_function_body t ~return_continuation ~exn_continuation =
  { t with return_continuation; exn_continuation }

let module_symbol t = t.module_symbol

let add_continuation t cont addr =
  { t with continuations = Continuation.Map.add cont addr t.continuations }

let add_exn_handler t cont ~addr ~exn_param ~extra_args =
  { t with
    exn_handlers =
      Continuation.Map.add cont { addr; exn_param; extra_args } t.exn_handlers
  }

let add_var t fvar jvar = { t with vars = Variable.Map.add fvar jvar t.vars }

let add_symbol t symbol jvar =
  { t with symbols = Symbol.Map.add symbol jvar t.symbols }

let add_code_id t code_id ~addr ~params =
  { t with code_ids = Code_id.Map.add code_id (addr, params) t.code_ids }

let add_function_slot t fslot jvar =
  { t with function_slots = Function_slot.Map.add fslot jvar t.function_slots }

let add_value_slot t vslot jvar =
  { t with value_slots = Value_slot.Map.add vslot jvar t.value_slots }

let get_continuation_exn t cont = Continuation.Map.find cont t.continuations

let get_exn_handler_exn t cont = Continuation.Map.find cont t.exn_handlers

let get_var_exn t fvar = Variable.Map.find fvar t.vars

let get_symbol t symbol = Symbol.Map.find_opt symbol t.symbols

let get_symbol_exn t symbol = Symbol.Map.find symbol t.symbols

let get_code_id_exn t code_id = Code_id.Map.find code_id t.code_ids

let get_function_slot t fslot =
  Function_slot.Map.find_opt fslot t.function_slots

let get_function_slot_exn t fslot =
  Function_slot.Map.find fslot t.function_slots

let get_value_slot t vslot = Value_slot.Map.find_opt vslot t.value_slots

let get_value_slot_exn t vslot = Value_slot.Map.find vslot t.value_slots

let add_var_alias_of_var_exn t ~var ~alias_of =
  let jvar = get_var_exn t alias_of in
  { t with vars = Variable.Map.add var jvar t.vars }

let add_symbol_alias_of_var_exn t ~symbol ~alias_of =
  let jvar = get_var_exn t alias_of in
  { t with symbols = Symbol.Map.add symbol jvar t.symbols }

let add_var_alias_of_symbol_exn t ~var ~alias_of =
  let jvar =
    match get_symbol t alias_of with
    | None ->
      Misc.fatal_errorf "Symbol %a not found in the environment" Symbol.print
        alias_of
    | Some v -> v
  in
  { t with vars = Variable.Map.add var jvar t.vars }

let add_if_not_found map item ~mem ~add =
  if mem item map
  then map
  else
    let var = Jsir.Var.fresh () in
    add item var map

let add_symbol_if_not_found t symbol =
  { t with
    symbols =
      add_if_not_found t.symbols symbol ~mem:Symbol.Map.mem ~add:Symbol.Map.add
  }

let add_function_slot_if_not_found t slot =
  { t with
    function_slots =
      add_if_not_found t.function_slots slot ~mem:Function_slot.Map.mem
        ~add:Function_slot.Map.add
  }

let add_value_slot_if_not_found t slot =
  { t with
    value_slots =
      add_if_not_found t.value_slots slot ~mem:Value_slot.Map.mem
        ~add:Value_slot.Map.add
  }
