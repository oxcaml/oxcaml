(* TEST
 modules = "dynamic_callback_.c";
 {
   bytecode;
 }{
   native;
 }
*)

module Dynamic = struct
  type last_fiber : immediate
  type (-'a, +'b) cont
  type 'a t

  external reperform :
    'a Effect.t -> ('a, 'b) cont -> last_fiber -> 'b = "%reperform"

  external with_stack :
    ('a -> 'b) ->
    (exn -> 'b) ->
    ('c Effect.t -> ('c, 'b) cont -> last_fiber -> 'b) ->
    ('e -> 'a) ->
    'e ->
    'b = "%with_stack"

  external with_stack_bind :
    ('a -> 'b) ->
    (exn -> 'b) ->
    ('c Effect.t -> ('c, 'b) cont -> last_fiber -> 'b) ->
    'd t ->
    'd ->
    ('e -> 'a) ->
    'e ->
    'b = "%with_stack_bind"

  external make : 'a -> 'a t = "caml_dynamic_make"
  external get : 'a t -> 'a = "caml_dynamic_get"

  let with_temporarily d v ~f =
    let effc eff k last_fiber = reperform eff k last_fiber in
    with_stack_bind (fun x -> x) (fun exn -> raise exn) effc d v f ()

  let on_fresh_stack f =
    let effc eff k last_fiber = reperform eff k last_fiber in
    with_stack (fun x -> x) (fun exn -> raise exn) effc f ()
end

external call_from_c : (unit -> unit) -> unit = "dynamic_callback_call"

let () =
  let d = Dynamic.make 0 in
  Dynamic.with_temporarily d 17 ~f:(fun () ->
    Dynamic.on_fresh_stack (fun () ->
      Printf.printf "before callback [expect 17]: %s\n"
        (string_of_int (Dynamic.get d));
      call_from_c (fun () ->
        Printf.printf "inside callback [expect 0]: %s\n"
          (string_of_int (Dynamic.get d)));
      Printf.printf "after callback [expect 17]: %s\n"
        (string_of_int (Dynamic.get d))))
