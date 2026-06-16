(* TEST
   runtime5;
   flags = "-dlambda -drawlambda";
   expect;
*)

type _ Effect.t += Ping : unit Effect.t

(* 1. Demonstrate the unsafe (unyielding) version *)

let[@inline never] ping () = Effect.perform Ping

[%%expect{|
(let (Ping/296 = (makeblock_unique 248 "Ping" (caml_fresh_oo_id 0)))
  (apply (field_imm 1 (global Toploop!)) "Ping/296" Ping/296))
(let (Ping/296 = (makeblock_unique 248 "Ping" (caml_fresh_oo_id 0)))
  (apply (field_imm 1 (global Toploop!)) "Ping/296" Ping/296))
type _ Stdlib.Effect.t += Ping : unit Effect.t
(let
  (Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   ping/297 =
     (function {nlocal = 0} param/299[value<int>] never_inline : int
       (perform Ping/296)))
  (apply (field_imm 1 (global Toploop!)) "ping" ping/297))
(let
  (Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   ping/297 =
     (function {nlocal = 0} param/299[value<int>] never_inline : int
       (perform Ping/296)))
  (apply (field_imm 1 (global Toploop!)) "ping" ping/297))
val ping : unit -> unit = <fun>
|}]

let () =
  Effect.Deep.try_with
    (fun () -> ping ())
    ()
    { effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Ping -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
          Effect.Deep.continue k ())
        | _ -> None
    }

[%%expect{|
(let
  (ping/297 =? (apply (field_imm 0 (global Toploop!)) "ping")
   Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   *match*/328 =[value<int>]
     (apply[unyielding] (field_imm 4 (field_imm 6 (global Stdlib__Effect!)))
       (function {nlocal = 0} param/311[value<int>] : int
         (apply[unyielding] ping/297 0))
       0
       (makeblock 0 (*)
         (function {nlocal = 0} e/324 : (consts (0)) (non_consts ([0: ?]))
           (catch
             (if (%eq e/324 Ping/296)
               (makeblock 0 (*)
                 (function {nlocal = 0} k/325 : int
                   (apply[unyielding]
                     (field_imm 0 (field_imm 6 (global Stdlib__Effect!)))
                     k/325 0)))
               (exit 1))
            with (1) 0)))))
  0)
(let
  (ping/297 =? (apply (field_imm 0 (global Toploop!)) "ping")
   Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   *match*/328 =[value<int>]
     (apply[unyielding] (field_imm 4 (field_imm 6 (global Stdlib__Effect!)))
       (function {nlocal = 0} param/311[value<int>] : int
         (apply[unyielding] ping/297 0))
       0
       (makeblock 0 (*)
         (function {nlocal = 0} e/324 : (consts (0)) (non_consts ([0: ?]))
           (if (%eq e/324 Ping/296)
             (makeblock 0 (*)
               (function {nlocal = 0} k/325 : int
                 (apply[unyielding]
                   (field_imm 0 (field_imm 6 (global Stdlib__Effect!))) k/325
                   0)))
             0)))))
  0)
|}]

(* 2. Demonstrate the safe (yielding) version *)

let[@inline never] ping (h @ local) = Effect.Safe.perform h Ping [@nontail]
[%%expect{|
(let
  (Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   ping/329 =
     (function {nlocal = 1} h/330[L][#()] never_inline : int
       (applynontail (field_imm 0 (field_imm 5 (global Stdlib__Effect!)))
         h/330 Ping/296)))
  (apply (field_imm 1 (global Toploop!)) "ping" ping/329))
(let
  (Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   ping/329 =
     (function {nlocal = 1} h/330[L][#()] never_inline : int
       (applynontail (field_imm 0 (field_imm 5 (global Stdlib__Effect!)))
         h/330 Ping/296)))
  (apply (field_imm 1 (global Toploop!)) "ping" ping/329))
val ping : Effect.Handler.t @ local -> unit = <fun>
|}]

let () =
  Effect.Deep.Safe.try_with
    (fun h () -> ping h)
    ()
    { effc = fun (type a) (e : a Effect.t) ->
        match e with
        | Ping -> Some (fun (k : (a, _) Effect.Deep.continuation) ->
          Effect.Deep.continue k ())
        | _ -> None
    }

[%%expect{|
(let
  (ping/329 =? (apply (field_imm 0 (global Toploop!)) "ping")
   Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   *match*/342 =[value<int>]
     (apply[unyielding]
       (field_imm 1 (field_imm 5 (field_imm 6 (global Stdlib__Effect!))))
       (function {nlocal = 2} h/335[L][#()] param/336[value<int>] : int
         (apply ping/329 h/335))
       0
       (makeblock 0 (*)
         (function {nlocal = 0} e/338 : (consts (0)) (non_consts ([0: ?]))
           (catch
             (if (%eq e/338 Ping/296)
               (makeblock 0 (*)
                 (function {nlocal = 0} k/339 : int
                   (apply[unyielding]
                     (field_imm 0 (field_imm 6 (global Stdlib__Effect!)))
                     k/339 0)))
               (exit 5))
            with (5) 0)))))
  0)
(let
  (ping/329 =? (apply (field_imm 0 (global Toploop!)) "ping")
   Ping/296 =? (apply (field_imm 0 (global Toploop!)) "Ping/296")
   *match*/342 =[value<int>]
     (apply[unyielding]
       (field_imm 1 (field_imm 5 (field_imm 6 (global Stdlib__Effect!))))
       (function {nlocal = 2} h/335[L][#()] param/336[value<int>] : int
         (apply ping/329 h/335))
       0
       (makeblock 0 (*)
         (function {nlocal = 0} e/338 : (consts (0)) (non_consts ([0: ?]))
           (if (%eq e/338 Ping/296)
             (makeblock 0 (*)
               (function {nlocal = 0} k/339 : int
                 (apply[unyielding]
                   (field_imm 0 (field_imm 6 (global Stdlib__Effect!))) k/339
                   0)))
             0)))))
  0)
|}]
