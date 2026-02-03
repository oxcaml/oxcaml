(* TEST
   include unix;
   hasunix;
   runtime5;
   poll_insertion;
   flags += "-alert -unsafe_multidomain -w -21";
   { native; }
*)

open Effect
open Effect.Deep

type (-'a, 'x, +'b) cont : value mod non_float

type last_fiber [@@immediate]

external cont_set_last_fiber :
  _ cont -> last_fiber -> unit = "%setfield1"

external reperform :
  'a t -> ('a, _, 'b) cont -> last_fiber -> 'b = "%reperform"

external resume : ('a, _, 'b) cont -> ('c -> 'a) -> 'c -> 'b = "%resume"

type ('a,'x,'b) effc = 'a t -> ('a, 'x, 'b) cont -> last_fiber -> 'b

type tick_outcome =
  | Preempt
  | Continue

external with_stack_preemptible :
  ('x -> 'b) ->
  (exn -> 'b) ->
  ('a . ('a,'x,'b) effc) ->
  (unit -> tick_outcome) ->
  ('d -> 'x) ->
  'd ->
  'b = "%with_stack_preemptible"

type ('a,'b) continuation =
  | Cont : ('a,'x,'b) cont -> ('a, 'b) continuation [@@unboxed]

let continue (Cont k) v = resume k (fun x-> x) v

type ('a,'b) preemptible_handler =
  { retc: 'a -> 'b;
    exnc: exn -> 'b;
    effc: 'c.'c t -> (('c,'b) continuation -> 'b) option;
    tickc: unit -> tick_outcome }

let match_with_preemptible comp arg handler =
  let effc eff k last_fiber =
    match handler.effc eff with
    | Some f ->
      cont_set_last_fiber k last_fiber;
      f (Cont k)
    | None -> reperform eff k last_fiber
  in
  with_stack_preemptible handler.retc handler.exnc effc handler.tickc comp arg

(* CR aspsmith: Remove in favor of automatically starting when we run a
   preemptible fiber *)
external enable_tick_thread : bool -> unit = "caml_enable_tick_thread"

(***********)

let perform_normal_effect () =
  print_endline "# Perform normal effect";
  let open struct
    type _ Effect.t +=
      | Go : unit Effect.t
  end
  in
  let happened = ref false in
  match_with_preemptible
    (fun () -> perform Go)
    ()
    { retc = Fun.id
    ; exnc = raise
    ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Go -> Some (fun (k : (a, _) continuation) ->
          happened := true;
          continue k ())
        | _ -> None)
    ; tickc = (fun () -> failwith "Should not tick")
    };
  assert !happened;
  print_endline "OK"
;;

let preempt_on_tick () =
  print_endline "# Preempt on tick";
  enable_tick_thread true;
  let preempted = ref false in
  match_with_preemptible
    (fun () ->
       let start_at = Sys.time () in
       while not (!preempted) do
         if Sys.time () -. start_at > 5.
         then failwith "Didn't get preempted after 5s!"
       done)
    ()
    { retc = Fun.id
    ; exnc = raise
    ; effc = (fun (type a) (eff : a Effect.t) ->
        match eff with
        | Preemption -> Some (fun (k : (a, _) continuation) ->
          preempted := true;
          continue k ())
        | _ -> None)
    ; tickc = (fun () -> Preempt)
    };
  assert !preempted;
  print_endline "OK"
;;


let () =
  perform_normal_effect ();
  preempt_on_tick ()
;;
