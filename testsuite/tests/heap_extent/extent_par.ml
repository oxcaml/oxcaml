(* TEST
 modules = "extent_stubs.c";
 multicore;
 { bytecode; }
 { native; }
*)

(* Multi-domain tests of heap extents: blocks referenced from the
   major heaps of several domains becoming garbage, and extents
   orphaned by terminating domains being adopted and swept by another
   domain.

   Extent blocks are only ever bound to locals inside functions, never
   in top-level units (in native code those locals stay live to the
   end of the program). *)

external make_extent : int array -> Obj.t array = "heap_extent_make"
external freed_count : unit -> int = "heap_extent_freed_count"

let gc_until cond =
  let rec go n =
    if not (cond ()) && n > 0 then (Gc.full_major (); go (n - 1))
  in
  go 40

let check msg b =
  Printf.printf "%s: %s\n%!" msg (if b then "ok" else "FAILED")

let num_domains = 4

(* Blocks of an extent owned by the main domain are referenced from
   the major heaps of several other domains, then become garbage. *)

let shared_extent_test () =
  let blocks = make_extent (Array.make 64 3) in
  (* Each block holds a heap-allocated ref reachable only through the
     extent, so cross-domain marking must traverse extent fields. *)
  Array.iteri (fun i b -> Obj.set_field b 0 (Obj.repr (ref (i * 3)))) blocks;
  Gc.minor (); (* promote the refs before sharing across domains *)
  let ready = Atomic.make 0 in
  let drop = Atomic.make false in
  let domains =
    List.init num_domains (fun d ->
        Domain.spawn (fun () ->
            (* Build references to the extent blocks in this domain's
               major heap. *)
            let mine = Array.map (fun b -> ref (Some b)) blocks in
            Gc.minor ();
            Gc.full_major ();
            Atomic.incr ready;
            while not (Atomic.get drop) do
              ignore (Sys.opaque_identity (Array.make 64 d));
              Domain.cpu_relax ()
            done;
            (* Check the blocks are intact, then drop them. *)
            let ok = ref true in
            Array.iteri
              (fun i r ->
                (match !r with
                | Some b ->
                    if !(Obj.obj (Obj.field b 0) : int ref) <> i * 3 then
                      ok := false
                | None -> ok := false);
                r := None)
              mine;
            Gc.full_major ();
            !ok))
  in
  while Atomic.get ready < num_domains do Domain.cpu_relax () done;
  Gc.full_major ();
  check "extent live while referenced from other domains"
    (freed_count () = 0);
  Atomic.set drop true;
  let oks = List.map Domain.join domains in
  check "blocks intact in all domains" (List.for_all (fun x -> x) oks);
  (* The other domains have dropped their references; the main domain
     still holds [blocks] until this function returns. *)
  Gc.full_major ();
  check "extent live while referenced from main domain only"
    (freed_count () = 0);
  ignore (Sys.opaque_identity blocks)

let () =
  shared_extent_test ();
  gc_until (fun () -> freed_count () = 1);
  check "extent freed after all domains dropped it" (freed_count () = 1)

(* An extent created by a domain that then terminates is orphaned
   along with the rest of that domain's shared heap; another domain
   adopts it and becomes responsible for sweeping and freeing it. *)

let orphan_test () =
  (* The blocks are published through an atomic rather than returned
     from the domain: a domain's result is rooted (via its termination
     sync value) until teardown completes, and teardown can be delayed
     arbitrarily while [gc_until] keeps interrupting it with STW
     sections, which would keep the extent alive. *)
  let shared = Atomic.make None in
  Domain.join
    (Domain.spawn (fun () ->
         let b = make_extent [| 4; 4; 4; 4 |] in
         (* Heap-allocated strings reachable only through the extent;
            they must survive orphaning and adoption. *)
         Array.iteri
           (fun i blk ->
             Obj.set_field blk 0 (Obj.repr (string_of_int (100 + i))))
           b;
         Atomic.set shared (Some b)));
  let blocks =
    match Atomic.get shared with
    | Some b -> b
    | None -> assert false
  in
  Atomic.set shared None;
  Gc.full_major ();
  Gc.full_major ();
  check "orphaned extent not freed while blocks live" (freed_count () = 1);
  let ok = ref true in
  Array.iteri
    (fun i blk ->
      if int_of_string (Obj.obj (Obj.field blk 0)) <> 100 + i then
        ok := false)
    blocks;
  check "orphaned extent blocks intact after adoption" !ok;
  ignore (Sys.opaque_identity blocks)

let () =
  orphan_test ();
  gc_until (fun () -> freed_count () = 2);
  check "adopted extent freed when dead" (freed_count () = 2)

(* Several domains create extents concurrently and publish one block
   of each; the other blocks become garbage immediately. The creating
   domains then terminate, orphaning all the extents while one block
   in each is still live. *)

let extents_per_domain = 16

let many_orphans_test () =
  let published = Array.make (num_domains * extents_per_domain) None in
  let domains =
    List.init num_domains (fun d ->
        Domain.spawn (fun () ->
            for i = 0 to extents_per_domain - 1 do
              let b = make_extent [| 2; 3; 6 |] in
              Obj.set_field b.(2) 1 (Obj.repr (ref ((d * 1000) + i)));
              published.((d * extents_per_domain) + i) <- Some b.(2)
            done;
            Gc.full_major ()))
  in
  List.iter Domain.join domains;
  Gc.full_major ();
  Gc.full_major ();
  check "orphaned extents not freed while published blocks live"
    (freed_count () = 2);
  let ok = ref true in
  Array.iteri
    (fun idx o ->
      match o with
      | Some blk ->
          let d = idx / extents_per_domain in
          let i = idx mod extents_per_domain in
          if !(Obj.obj (Obj.field blk 1) : int ref) <> (d * 1000) + i then
            ok := false
      | None -> ok := false)
    published;
  check "published blocks intact" !ok;
  Array.fill published 0 (Array.length published) None;
  ignore (Sys.opaque_identity published)

let () =
  many_orphans_test ();
  let expected = 2 + (num_domains * extents_per_domain) in
  gc_until (fun () -> freed_count () = expected);
  check "all orphaned extents freed" (freed_count () = expected)
