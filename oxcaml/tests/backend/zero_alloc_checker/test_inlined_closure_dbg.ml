(*= Same shape as [test_assume_inlined_closure.ml], without the call-site
   [@zero_alloc assume], so that the check fails and prints the debuginfo of
   the closure allocation inside the inlined [matches].

   The point of this test is the "inlined from" frame on the allocation of the
   closure [fun y -> ...]: it must carry the [matches]-inlined-into-[f] frame,
   like the tail call to [eval] that follows it.  This is the same
   [Debuginfo.t] that drives the DWARF inlined-frame information
   (-gdwarf-inlined-frames): without the frame, the DW_TAG_inlined_subroutine
   for [matches] under [f] covers only the tail call, and the closure
   allocation is attributed directly to [f] at a line that is not in [f].  To
   see this directly:
     ocamlopt.opt -O3 -g -gdwarf-inlined-frames -gdwarf-compression=none -c \
       test_inlined_closure_dbg.ml
     llvm-dwarfdump --debug-info test_inlined_closure_dbg.o *)

type 'a t =
  | Base of 'a
  | And of 'a t * 'a t
  | Not of 'a t

let rec eval t f =
  match t with
  | Base x -> f x
  | And (a, b) -> eval a f && eval b f
  | Not a -> not (eval a f)

let matches t ~x = eval t (fun y -> Int.equal y x)

let[@zero_alloc] f t x = matches t ~x
