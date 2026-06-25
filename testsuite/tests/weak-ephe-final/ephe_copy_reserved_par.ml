(* TEST
 flambda2;
 multicore;
 native;
*)

(* Regression test for a bug in [ephe_get_field_copy] (runtime/weak.c).

   That function loops, allocating [copy] for the field's current value and
   reusing it on the next iteration if the tag and size still match. The reuse
   check used to ignore the reserved (mixed-block) header bits, which encode the
   scannable prefix. If another domain replaces the field, between the two reads
   of the loop, with a value of the same tag and size but a different scannable
   prefix, [ephe_copy_and_darken] copies the new value's contents using the new
   value's layout into a block whose header still carries the old reserved bits.

   Two corrupting directions, both exercised below:
   - source fully scannable, copy header says scannable=1: heap pointers are
     stored into fields the GC treats as a non-scannable (flat) suffix, so their
     targets are never marked and get collected -> dangling pointers.
   - source scannable=1, copy header says scannable=3: raw float# bits are
     memcpy'd into fields the GC scans as pointers -> the marker dereferences
     arbitrary bits -> crash.

   The two values share a tag (0) and size (3) but differ in their reserved
   bits: [mixed] is a mixed block with scannable prefix 1, [boxed] is an
   ordinary block with scannable prefix 3. We force both into the same weak
   slot with [Obj.magic] (in real code the slot would be reached via a
   polymorphic container). *)

type mixed = { m_tag : int; f1 : float#; f2 : float# }
type boxed = { b_tag : int; b : string; c : string }

let mixed_tag = 0
let boxed_tag = 1
let str_len = 7

let make_mixed () = Sys.opaque_identity { m_tag = mixed_tag; f1 = #1.0; f2 = #2.0 }

let make_boxed () =
  (* Fresh, uniquely-allocated strings so that, once the slot moves on, the
     copy's fields are the only thing keeping them alive. *)
  Sys.opaque_identity
    { b_tag = boxed_tag;
      b = String.make str_len 'x';
      c = String.make str_len 'y' }

(* One shared weak slot of [boxed], also holding magicked [mixed] values. *)
let table : boxed Weak.t = Weak.create 1

let num_mutators = 2
let num_copiers = 2
let iters = 300_000
let keep = 64
let gc_every = 512

let mutator () =
  for _ = 1 to iters do
    Weak.set table 0 (Some (Obj.magic (make_mixed ()) : boxed));
    Weak.set table 0 (Some (make_boxed ()))
  done

let copier () =
  (* Keep recent copies live so a corrupted block (direction 2) is scanned by
     the major GC while still reachable. *)
  let recent = Array.make keep None in
  for i = 0 to iters - 1 do
    (match Weak.get_copy table 0 with
     | None -> ()
     | Some r -> recent.(i mod keep) <- Some r);
    if i mod gc_every = 0 then begin
      Gc.full_major ();
      (* Direction 1: a [boxed]-tagged copy whose string fields were stored
         but not scanned now dangles; reading them faults or returns garbage.
         A copy of a [mixed] value (tag 0) must NOT have b/c dereferenced -
         those slots hold raw float bits. *)
      Array.iter
        (function
          | Some r when r.b_tag = boxed_tag ->
            assert (String.length r.b = str_len);
            assert (String.length r.c = str_len);
            assert (r.b.[0] = 'x');
            assert (r.c.[0] = 'y')
          | _ -> ())
        recent
    end
  done

let () =
  Weak.set table 0 (Some (make_boxed ()));
  let ms = Array.init num_mutators (fun _ -> Domain.spawn mutator) in
  let cs = Array.init num_copiers (fun _ -> Domain.spawn copier) in
  Array.iter Domain.join ms;
  Array.iter Domain.join cs;
  print_endline "ok"
