(******************************************************************************
 *                                  OxCaml                                    *
 *                               Jane Street                                  *
 * -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2026 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module Position_hash = struct
  type t = int32

  let of_int32 t = t

  let to_int32 t = t

  let equal = Int32.equal

  let compare = Int32.unsigned_compare

  let is_function_entry t = Int32.equal (Int32.logand t 1l) 1l

  (* The entry bit lets the decoder find the call targets of the profile (which
     call sites reached which functions, unknown statically for indirect calls)
     for function reordering. *)
  let of_prehash ~is_function_entry prehash =
    Int32.logor
      (Int32.logand (Fdo_prehash.to_int32 prehash) (-2l))
      (if is_function_entry then 1l else 0l)

  module Tbl = Hashtbl.Make (struct
    type nonrec t = t

    let equal = equal

    (* Already mixed. *)
    let hash t = Int32.to_int t land max_int
  end)
end

module Body_hash = struct
  type t = int32

  let of_int32 t = t

  let to_int32 t = t

  let equal = Int32.equal

  let to_string t = Printf.sprintf "%08lx" t

  let prehash t = Fdo_prehash.of_int (Int32.to_int t)
end

module Tag = struct
  type t =
    | Original
    | Specialized
    | Branch
    | Interior_call
    | Module_level_call

  let to_int = function
    | Original -> 0
    | Specialized -> 1
    | Branch -> 2
    | Interior_call -> 3
    | Module_level_call -> 4

  let node tag components = Fdo_prehash.node ~tag:(to_int tag) components
end

(* A name (e.g. a demangled name from a scope path), with a discriminator to
   distinguish those with the same name. *)
module Name = struct
  type t =
    { name_str : string;
      discriminator : int;
      prehash : Fdo_prehash.t
    }

  let create ~name_str ~discriminator =
    let prehash =
      Tag.node Original
        [Fdo_prehash.of_string name_str; Fdo_prehash.of_int discriminator]
    in
    { name_str; discriminator; prehash }

  let equal a b =
    String.equal a.name_str b.name_str
    && Int.equal a.discriminator b.discriminator

  let to_string { name_str; discriminator; prehash = _ } =
    Printf.sprintf "%S:%d" name_str discriminator
end

(* A construct numbered in a function's Lambda body. *)
module Interior_position = struct
  type t =
    { body_hash : Body_hash.t;
      index : int
    }

  let equal a b =
    Body_hash.equal a.body_hash b.body_hash && Int.equal a.index b.index

  let prehash { body_hash; index } =
    Fdo_prehash.combine (Body_hash.prehash body_hash) (Fdo_prehash.of_int index)

  let to_string { body_hash; index } =
    Printf.sprintf "%s:%d" (Body_hash.to_string body_hash) index
end

(* How the position of a call is represented. [Module_level] calls are evaluated
   in a [let] or [module] binding of a structure ([binder]), and numbered within
   it. They are a special case since the names bound in structures are stable
   across edits. *)
module Call_position = struct
  type t =
    | Interior of Interior_position.t
    | Module_level of
        { binder : Name.t;
          index : int
        }

  let equal a b =
    match a, b with
    | Interior a, Interior b -> Interior_position.equal a b
    | Module_level a, Module_level b ->
      Name.equal a.binder b.binder && Int.equal a.index b.index
    | (Interior _ | Module_level _), _ -> false

  let prehash ~function_ = function
    | Interior position ->
      Tag.node Interior_call [function_; Interior_position.prehash position]
    | Module_level { binder; index } ->
      Tag.node Module_level_call
        [function_; binder.prehash; Fdo_prehash.of_int index]

  let to_string ~function_ = function
    | Interior position ->
      Printf.sprintf "%s:%s:call" function_
        (Interior_position.to_string position)
    | Module_level { binder; index } ->
      Printf.sprintf "%s:module(%s):%d:call" function_ (Name.to_string binder)
        index
end

(* A point within a function. *)
module Edge = struct
  type t =
    | Entry
    | Branch of
        { position : Interior_position.t;
          arm : int
        }
    | Call of Call_position.t

  let equal a b =
    match a, b with
    | Entry, Entry -> true
    | Branch a, Branch b ->
      Interior_position.equal a.position b.position && Int.equal a.arm b.arm
    | Call a, Call b -> Call_position.equal a b
    | (Entry | Branch _ | Call _), _ -> false

  let prehash ~function_ = function
    | Entry -> function_
    | Branch { position; arm } ->
      Tag.node Branch
        [function_; Interior_position.prehash position; Fdo_prehash.of_int arm]
    | Call call -> Call_position.prehash ~function_ call

  let to_string ~function_ = function
    | Entry -> function_
    | Branch { position; arm } ->
      Printf.sprintf "%s:%s:branch(%d)" function_
        (Interior_position.to_string position)
        arm
    | Call call -> Call_position.to_string ~function_ call
end

(* A function, potentially specialized in the sites [copied_at] whose inlining
   created it. A call in a copy contributes its callsite in the original
   followed by the copy's context (see [Position.specialize]), so that copies
   made in different orders get the same context. *)
module Function = struct
  module Callsite = struct
    type t =
      { caller : Name.t;
        call : Call_position.t
      }

    let equal a b =
      Name.equal a.caller b.caller && Call_position.equal a.call b.call

    let prehash { caller; call } =
      Call_position.prehash ~function_:caller.prehash call

    let to_string { caller; call } =
      Call_position.to_string ~function_:(Name.to_string caller) call
  end

  type t =
    { name : Name.t;
      copied_at : Callsite.t list;
          (** Empty for the original. Innermost first. *)
      prehash : Fdo_prehash.t
    }

  let create (name : Name.t) copied_at =
    let prehash =
      match copied_at with
      | [] -> name.prehash
      | _ :: _ ->
        Tag.node Specialized
          (name.prehash :: List.map Callsite.prehash copied_at)
    in
    { name; copied_at; prehash }

  let create_original name = create name []

  let copy t ~at = create t.name (t.copied_at @ at)

  let name t = t.name

  let entry_position_hash t =
    Position_hash.of_prehash ~is_function_entry:true t.prehash

  let equal a b =
    Name.equal a.name b.name
    && List.equal Callsite.equal a.copied_at b.copied_at

  let to_string { name; copied_at; prehash = _ } =
    let specializations =
      List.map
        (fun callsite -> "@(" ^ Callsite.to_string callsite ^ ")")
        copied_at
    in
    String.concat "" (Name.to_string name :: specializations)
end

module Position = struct
  type t =
    { function_ : Function.t;
      edge : Edge.t;
      prehash : Fdo_prehash.t
    }

  let create (function_ : Function.t) edge =
    { function_;
      edge;
      prehash = Edge.prehash ~function_:function_.prehash edge
    }

  let create_function_entry function_ = create function_ Entry

  let create_branch ~function_ ~body_hash ~index ~arm =
    create function_ (Branch { position = { body_hash; index }; arm })

  let create_call ~function_ ~body_hash ~index =
    create function_ (Call (Interior { body_hash; index }))

  let create_module_level_call ~function_ ~binder ~index =
    create function_ (Call (Module_level { binder; index }))

  let function_ t = t.function_

  let body_hash t =
    match t.edge with
    | Branch { position; _ } | Call (Interior position) ->
      Some position.body_hash
    | Entry | Call (Module_level _) -> None

  let is_function_entry t =
    match t.edge with Entry -> true | Branch _ | Call _ -> false

  let hash t =
    Position_hash.of_prehash ~is_function_entry:(is_function_entry t) t.prehash

  let equal a b =
    Function.equal a.function_ b.function_ && Edge.equal a.edge b.edge

  let to_string t =
    Edge.to_string ~function_:(Function.to_string t.function_) t.edge

  (* The call's callsite in the original, followed by the context of the copy it
     is in. *)
  let copy_context t : Function.Callsite.t list =
    match t.edge with
    | Call call -> { caller = t.function_.name; call } :: t.function_.copied_at
    | Entry | Branch _ ->
      Misc.fatal_errorf "Fdo_counter.specialize: %s is not a call site"
        (to_string t)

  let specialize t ~at =
    create
      (Function.copy t.function_ ~at:(List.concat_map copy_context at))
      t.edge
end
