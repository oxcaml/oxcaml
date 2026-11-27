[@@@alert "-unstable"]

(* The wrapper that a probe site calls has a stack frame of its own, for the
   arguments of the handler and the registers that are live at the probe site,
   and it calls the handler. When stack checks are enabled, the wrapper checks
   the stack as any other function that makes a call does, and so does the
   function that contains the probe site.

   The wrapper of [narrow] allocates less than the stack threshold, the wrapper
   of [wide] more. Both handlers are leaf functions with small frames, which do
   not check the stack themselves. The values named [y] and [z] are live across
   the probe; [w] is a float that only the handler uses, which some optimization
   settings pass to it unboxed. *)

let fired = ref 0

let handler_total = ref 0

let floats = [| 1.; 2.; 3.; 4. |]

let[@inline never] narrow n =
  let x1 = Sys.opaque_identity (n + 1) in
  let x2 = Sys.opaque_identity (n + 2) in
  let x3 = Sys.opaque_identity (n + 3) in
  let x4 = Sys.opaque_identity (n + 4) in
  let x5 = Sys.opaque_identity (n + 5) in
  let x6 = Sys.opaque_identity (n + 6) in
  let x7 = Sys.opaque_identity (n + 7) in
  let x8 = Sys.opaque_identity (n + 8) in
  let x9 = Sys.opaque_identity (n + 9) in
  let x10 = Sys.opaque_identity (n + 10) in
  let x11 = Sys.opaque_identity (n + 11) in
  let x12 = Sys.opaque_identity (n + 12) in
  let x13 = Sys.opaque_identity (n + 13) in
  let x14 = Sys.opaque_identity (n + 14) in
  let x15 = Sys.opaque_identity (n + 15) in
  let x16 = Sys.opaque_identity (n + 16) in
  let y1 = Sys.opaque_identity (n - 1) in
  let y2 = Sys.opaque_identity (n - 2) in
  let y3 = Sys.opaque_identity (n - 3) in
  let y4 = Sys.opaque_identity (n - 4) in
  let y5 = Sys.opaque_identity (n - 5) in
  let y6 = Sys.opaque_identity (n - 6) in
  let y7 = Sys.opaque_identity (n - 7) in
  let y8 = Sys.opaque_identity (n - 8) in
  let y9 = Sys.opaque_identity (n - 9) in
  let y10 = Sys.opaque_identity (n - 10) in
  let y11 = Sys.opaque_identity (n - 11) in
  let y12 = Sys.opaque_identity (n - 12) in
  let y13 = Sys.opaque_identity (n - 13) in
  let y14 = Sys.opaque_identity (n - 14) in
  let y15 = Sys.opaque_identity (n - 15) in
  let y16 = Sys.opaque_identity (n - 16) in
  let y17 = Sys.opaque_identity (n - 17) in
  let y18 = Sys.opaque_identity (n - 18) in
  let w = Array.unsafe_get floats 0 in
  [%probe
    "narrow" ~enabled_at_init:true
      (incr fired;
       handler_total
         := !handler_total + int_of_float w + x1 + x2 + x3 + x4 + x5 + x6 + x7
            + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16)];
  n + y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10 + y11 + y12 + y13 + y14
  + y15 + y16 + y17 + y18

let[@inline never] wide n =
  let x1 = Sys.opaque_identity (n + 1) in
  let x2 = Sys.opaque_identity (n + 2) in
  let x3 = Sys.opaque_identity (n + 3) in
  let x4 = Sys.opaque_identity (n + 4) in
  let x5 = Sys.opaque_identity (n + 5) in
  let x6 = Sys.opaque_identity (n + 6) in
  let x7 = Sys.opaque_identity (n + 7) in
  let x8 = Sys.opaque_identity (n + 8) in
  let x9 = Sys.opaque_identity (n + 9) in
  let x10 = Sys.opaque_identity (n + 10) in
  let x11 = Sys.opaque_identity (n + 11) in
  let x12 = Sys.opaque_identity (n + 12) in
  let x13 = Sys.opaque_identity (n + 13) in
  let x14 = Sys.opaque_identity (n + 14) in
  let x15 = Sys.opaque_identity (n + 15) in
  let x16 = Sys.opaque_identity (n + 16) in
  let x17 = Sys.opaque_identity (n + 17) in
  let x18 = Sys.opaque_identity (n + 18) in
  let x19 = Sys.opaque_identity (n + 19) in
  let x20 = Sys.opaque_identity (n + 20) in
  let x21 = Sys.opaque_identity (n + 21) in
  let x22 = Sys.opaque_identity (n + 22) in
  let x23 = Sys.opaque_identity (n + 23) in
  let x24 = Sys.opaque_identity (n + 24) in
  let x25 = Sys.opaque_identity (n + 25) in
  let x26 = Sys.opaque_identity (n + 26) in
  let x27 = Sys.opaque_identity (n + 27) in
  let x28 = Sys.opaque_identity (n + 28) in
  let x29 = Sys.opaque_identity (n + 29) in
  let x30 = Sys.opaque_identity (n + 30) in
  let x31 = Sys.opaque_identity (n + 31) in
  let x32 = Sys.opaque_identity (n + 32) in
  let y1 = Sys.opaque_identity (n - 1) in
  let y2 = Sys.opaque_identity (n - 2) in
  let y3 = Sys.opaque_identity (n - 3) in
  let y4 = Sys.opaque_identity (n - 4) in
  let y5 = Sys.opaque_identity (n - 5) in
  let y6 = Sys.opaque_identity (n - 6) in
  let y7 = Sys.opaque_identity (n - 7) in
  let y8 = Sys.opaque_identity (n - 8) in
  let z1 = Array.unsafe_get floats 0 in
  let z2 = Array.unsafe_get floats 1 in
  let z3 = Array.unsafe_get floats 2 in
  let z4 = Array.unsafe_get floats 3 in
  [%probe
    "wide" ~enabled_at_init:true
      (incr fired;
       handler_total
         := !handler_total + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10
            + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21
            + x22 + x23 + x24 + x25 + x26 + x27 + x28 + x29 + x30 + x31 + x32)];
  n + y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + int_of_float (z1 +. z2 +. z3 +. z4)

(* The frame of this function is big enough for its stack check to come before
   anything else it does. *)
let[@inline never] call fire n =
  let a1 = Sys.opaque_identity (n + 1) in
  let a2 = Sys.opaque_identity (n + 2) in
  let a3 = Sys.opaque_identity (n + 3) in
  let a4 = Sys.opaque_identity (n + 4) in
  let a5 = Sys.opaque_identity (n + 5) in
  let a6 = Sys.opaque_identity (n + 6) in
  let a7 = Sys.opaque_identity (n + 7) in
  let a8 = Sys.opaque_identity (n + 8) in
  let a9 = Sys.opaque_identity (n + 9) in
  let a10 = Sys.opaque_identity (n + 10) in
  let a11 = Sys.opaque_identity (n + 11) in
  let a12 = Sys.opaque_identity (n + 12) in
  let a13 = Sys.opaque_identity (n + 13) in
  let a14 = Sys.opaque_identity (n + 14) in
  let a15 = Sys.opaque_identity (n + 15) in
  let a16 = Sys.opaque_identity (n + 16) in
  let a17 = Sys.opaque_identity (n + 17) in
  let a18 = Sys.opaque_identity (n + 18) in
  let a19 = Sys.opaque_identity (n + 19) in
  let a20 = Sys.opaque_identity (n + 20) in
  let a21 = Sys.opaque_identity (n + 21) in
  let a22 = Sys.opaque_identity (n + 22) in
  let a23 = Sys.opaque_identity (n + 23) in
  let a24 = Sys.opaque_identity (n + 24) in
  let a25 = Sys.opaque_identity (n + 25) in
  let a26 = Sys.opaque_identity (n + 26) in
  let a27 = Sys.opaque_identity (n + 27) in
  let a28 = Sys.opaque_identity (n + 28) in
  let a29 = Sys.opaque_identity (n + 29) in
  let a30 = Sys.opaque_identity (n + 30) in
  fire n + a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 + a12 + a13
  + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22 + a23 + a24 + a25 + a26
  + a27 + a28 + a29 + a30

let rec deep fire depth n =
  if depth = 0 then call fire n else 1 + deep fire (depth - 1) n

(* Each fiber starts with a small stack, which grows as the recursion gets
   deeper: over this range of depths, the stack has to grow now in the function
   that contains the probe, now in the wrapper. *)
let () =
  let fire =
    match Sys.argv.(1) with
    | "narrow" -> narrow
    | "wide" -> wide
    | name -> failwith ("no probe named " ^ name)
  in
  let total = ref 0 in
  for depth = 0 to 300 do
    total
      := !total
         + Effect.Deep.match_with (deep fire depth) depth
             { retc = Fun.id; exnc = raise; effc = (fun _ -> None) }
  done;
  Printf.printf "%s: fired %d times, handler total %d, total %d\n" Sys.argv.(1)
    !fired !handler_total !total
