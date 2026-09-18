(* TEST
 flambda2;
 {
   flags = "-extension layouts_beta";
   native;
 }
*)

(* Regression test: tail call to a different function where the results
   overflow past both the register bank (10 registers) and the Domainstate
   area (64 slots) into actual Outgoing/Incoming stack slots.  The fallback
   "call + return" path in cfg_selectgen previously generated a Move
   instruction from an Outgoing stack slot to an Incoming stack slot, which
   the amd64 emitter cannot handle:
     Fatal error: Illegal move between registers
       (pin:anon:V/...[arg[0]] to pin:anon:V/...[par[0]])
   75 integer results = 10 in registers + 64 in Domainstate + 1 on the stack,
   which is the minimum needed to reach the Outgoing/Incoming slots. *)

let[@inline never] produce ()
  : #(int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int) =
  #(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
    21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
    31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
    41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
    51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
    61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
    71, 72, 73, 74, 75)

(* Tail-calling a *different* function whose results overflow to the stack
   triggers the fallback "call + return" code path in cfg_selectgen. *)
let[@inline never] forward ()
  : #(int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int * int * int * int * int * int
    * int * int * int * int * int) =
  produce ()

let () =
  let #(v0, v1, v2, v3, v4, v5, v6, v7, v8, v9,
        v10, v11, v12, v13, v14, v15, v16, v17, v18, v19,
        v20, v21, v22, v23, v24, v25, v26, v27, v28, v29,
        v30, v31, v32, v33, v34, v35, v36, v37, v38, v39,
        v40, v41, v42, v43, v44, v45, v46, v47, v48, v49,
        v50, v51, v52, v53, v54, v55, v56, v57, v58, v59,
        v60, v61, v62, v63, v64, v65, v66, v67, v68, v69,
        v70, v71, v72, v73, v74) = forward () in
  Printf.printf
    "%d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d %d %d %d %d %d \
     %d %d %d %d %d\n"
    v0 v1 v2 v3 v4 v5 v6 v7 v8 v9
    v10 v11 v12 v13 v14 v15 v16 v17 v18 v19
    v20 v21 v22 v23 v24 v25 v26 v27 v28 v29
    v30 v31 v32 v33 v34 v35 v36 v37 v38 v39
    v40 v41 v42 v43 v44 v45 v46 v47 v48 v49
    v50 v51 v52 v53 v54 v55 v56 v57 v58 v59
    v60 v61 v62 v63 v64 v65 v66 v67 v68 v69
    v70 v71 v72 v73 v74
