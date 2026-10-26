(* The operations on pseudo-instrumentation counters: inlining, specialization,
   attaching the entry counters of inlined calls to edges, and hashing. Each
   section models a source program and prints the counters it would get. *)

module F = Fdo_counter

let print name s = Printf.printf "%s: %s\n" name s

let print_counter name counter = print name (F.to_string counter)

let print_hash name hash =
  print name (Printf.sprintf "%08lx" (F.Position_hash.to_int32 hash))

let check name cond = print name (if cond then "ok" else "FAILED")

let name name_str = F.Name.create ~name_str ~discriminator:0

let original name_str = F.Function.create_original (name name_str)

let body n = F.Body_hash.of_int32 (Int32.of_int n)

let entry function_ = F.Position.create_function_entry function_

let branch function_ body_hash index arm =
  F.Position.create_branch ~function_ ~body_hash ~index ~arm

let call function_ body_hash index =
  F.Position.create_call ~function_ ~body_hash ~index

let module_level_call function_ binder index =
  F.Position.create_module_level_call ~function_ ~binder ~index

let counter ?(inlined_at = []) position : F.t =
  { position; inlining_stack = inlined_at }

(* a.ml:

   let f x = if x then g () else 0

   [f]'s counters: its entry, the two edges of the [if] (construct 0) and the
   call [g ()] (construct 1). Its body is hashed as [0xa]. *)
let f = original "A.f"

let body_f = body 0xa

let f_entry = entry f

let f_then = branch f body_f 0 1

let f_else = branch f body_f 0 0

let f_call = call f body_f 1

let () =
  print_counter "entry" (counter f_entry);
  print_counter "then edge" (counter f_then);
  print_counter "else edge" (counter f_else);
  print_counter "call site" (counter f_call);
  check "edges of one construct differ" (not (F.Position.equal f_then f_else));
  check "interior positions depend on the body"
    (not (F.Position.equal f_call (call f (body 0xed) 1)));
  check "entries do not" (F.Position.equal f_entry (entry (original "A.f")))

(* c.ml:

   let v = A.f true

   The call is in [C]'s toplevel code, which is not hashed: its position is
   numbered within the binding [v] instead. *)
let c_toplevel = original "C"

let v_call = module_level_call c_toplevel (name "C.v") 0

let () =
  print_counter "module-level call site" (counter v_call);
  check "module-level calls have no body hash"
    (Option.is_none (F.Position.body_hash v_call))

(* b.ml:

   let g y = A.f y

   When [f] is inlined into [g], [f]'s counters get the call site in [g] as
   their inlining context. [g] itself may then be inlined elsewhere. *)
let g = original "B.g"

let g_call = call g (body 0xb) 0

let h_call = call (original "D.h") (body 0xd) 0

let () =
  let inlined = F.inline (counter f_then) ~at:(counter g_call) in
  let twice = F.inline inlined ~at:(counter h_call) in
  print_counter "inlined into g" inlined;
  print_counter "then into h" twice;
  check "context is innermost first"
    (F.equal twice (counter f_then ~inlined_at:[g_call; h_call]));
  check "inlining a call site keeps its position"
    (F.equal
       (F.inline (counter g_call) ~at:(counter h_call))
       (counter g_call ~inlined_at:[h_call]));
  check "inlining an entry keeps the function identity"
    (F.equal
       (F.inline (counter f_entry) ~at:(counter g_call))
       (counter f_entry ~inlined_at:[g_call]))

(* a.ml:

   let make n = fun x -> x + n

   c.ml:

   let add3 = A.make 3

   module M = Functor (X)

   Inlining [make] at the module-level call copies the inner function; the copy
   is named after the call site. The same happens to the functions of a functor
   applied in a structure. The copy's interior positions keep their numbering
   and body hash under the new name. *)
let fn = original "A.make.(fun)@x,int_add,n"

let fn_entry = entry fn

let fn_then = branch fn (body 0xf) 0 1

let add3_call = module_level_call c_toplevel (name "C.add3") 0

let m_call = module_level_call c_toplevel (name "C.M") 0

let () =
  let specialized = F.specialize (counter fn_entry) ~at:(counter add3_call) in
  print_counter "copy of the inner function" specialized;
  print_counter "its then edge"
    (F.specialize (counter fn_then) ~at:(counter add3_call));
  print_counter "functor instance function"
    (F.specialize (counter fn_entry) ~at:(counter m_call));
  print_counter "copied again" (F.specialize specialized ~at:(counter m_call));
  check "the copy is a new function"
    (not (F.equal specialized (counter fn_entry)));
  check "copies share the original's name"
    (F.Name.equal
       (F.Function.name (F.Position.function_ specialized.position))
       (F.Function.name fn))

(* As above, but [make] was itself inlined into [g] first, so the call that
   makes the copy carries an inlining context. Only the outermost position of a
   counter, the one in the copied function, is renamed; inner positions belong
   to callees inlined into the copy. *)
let () =
  let site = counter g_call ~inlined_at:[h_call] in
  print_counter "copy made at an inlined call"
    (F.specialize (counter fn_entry) ~at:site);
  print_counter "counter inlined into the copy"
    (F.specialize (counter f_then ~inlined_at:[fn_then]) ~at:site)

(* a.ml:

   let make n = fun x -> let module M = Functor (X) in ...

   A module-level call in a copied function: the function is renamed, the binder
   is not. *)
let () =
  let m_call = module_level_call fn (name "A.make.(fun).M") 0 in
  print_counter "module-level call in a copy"
    (F.specialize (counter m_call) ~at:(counter add3_call))

(* a.ml:

   let make k = let h x = if x > k then x else k in h

   let outer k = let f y = (make k) y in f

   c.ml:

   let top () = A.outer 1

   Inlining [outer] at [top]'s call copies [f], and inlining [make] at its call
   in [f] copies [h]. Either can happen first: [make] is inlined in the original
   [f] before [f] is copied, or only in the copy of [f] (e.g. if [make] is only
   known there). Both give the same copy of [h]: a call in a copy is recorded as
   its callsite in the original, followed by the copy's context. *)
let h_entry = entry (original "A.make.h")

let make_call = call (original "A.outer.f") (body 0x5) 0

let outer_call = call (original "C.top") (body 0xc) 0

let () =
  let made_first =
    F.specialize
      (F.specialize (counter h_entry) ~at:(counter make_call))
      ~at:(counter outer_call)
  in
  let made_in_copy =
    F.specialize (counter h_entry)
      ~at:(F.specialize (counter make_call) ~at:(counter outer_call))
  in
  print_counter "made in f, then copied with it" made_first;
  print_counter "made in the copy of f" made_in_copy;
  check "the order of copying does not matter"
    (F.equal made_first made_in_copy
    && List.equal F.Position_hash.equal (F.hash made_first)
         (F.hash made_in_copy))

(* Each edge records the entry counters of the calls inlined through it, so the
   profile of inlined callees can be split by edge. *)
let () =
  let inlined_calls =
    [counter f_entry ~inlined_at:[g_call]; counter f_entry ~inlined_at:[h_call]]
  in
  let edge = [counter f_then] in
  let counters = F.add_all edge inlined_calls in
  check "inlined calls follow the edge's counters"
    (List.equal F.equal counters (counter f_then :: inlined_calls));
  check "duplicates are not added"
    (List.equal F.equal (F.add_all counters [List.hd inlined_calls]) counters);
  check "first occurrence wins"
    (List.equal F.equal
       (F.add_all
          [counter f_then]
          [counter f_else; counter f_else; counter f_then])
       [counter f_then; counter f_else])

(* Hashes are what metadata and profiles store. *)
let () =
  let specialized =
    F.Position.function_
      (F.specialize (counter f_entry) ~at:(counter add3_call)).position
  in
  print_hash "entry hash" (F.Position.hash f_entry);
  print_hash "branch hash" (F.Position.hash f_then);
  print_hash "module-level call hash" (F.Position.hash v_call);
  print_hash "specialized entry hash"
    (F.Function.entry_position_hash specialized);
  check "the entry hash is the function's"
    (F.Position_hash.equal (F.Position.hash f_entry)
       (F.Function.entry_position_hash f));
  check "only entries carry the entry bit"
    (F.Position_hash.is_function_entry (F.Position.hash f_entry)
    && (not (F.Position_hash.is_function_entry (F.Position.hash f_then)))
    && not (F.Position_hash.is_function_entry (F.Position.hash v_call)));
  check "the entry bit survives specialization"
    (F.Position_hash.is_function_entry
       (F.Function.entry_position_hash specialized));
  check "a counter hashes its position and each inlining level"
    (List.equal F.Position_hash.equal
       (F.hash (counter f_then ~inlined_at:[g_call; h_call]))
       [F.Position.hash f_then; F.Position.hash g_call; F.Position.hash h_call]);
  check "specialization recomputes the hash"
    (List.equal F.Position_hash.equal
       (F.hash (F.specialize (counter f_then) ~at:(counter add3_call)))
       (F.hash (counter (branch specialized body_f 0 1))));
  check "an edge and a later construct have distinct hashes"
    (List.for_all
       (fun later ->
         not
           (F.Position_hash.equal
              (F.Position.hash (branch f body_f 0 2))
              (F.Position.hash later)))
       [branch f body_f 2 0; branch f body_f 2 1; call f body_f 2]);
  check "hash conversion preserves all bits"
    (Int32.equal
       (F.Position_hash.to_int32 (F.Position_hash.of_int32 0xffffffffl))
       0xffffffffl);
  check "body hash conversion preserves all bits"
    (Int32.equal
       (F.Body_hash.to_int32 (F.Body_hash.of_int32 0xffffffffl))
       0xffffffffl);
  check "hash ordering is unsigned"
    (F.Position_hash.compare
       (F.Position_hash.of_int32 0x80000000l)
       (F.Position_hash.of_int32 0x7fffffffl)
    > 0)
