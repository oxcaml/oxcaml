type marker = Marker

let implementation x = x + 1

external expose : (unit -> int) -> unit = "partition_test_expose_lazy"
external pure_js_expr : string -> 'a = "caml_pure_js_expr"
external fun_call : 'a -> 'b array -> 'c = "caml_js_fun_call"

let () =
  Shared.record "lazy";
  Shared.counter := !Shared.counter + Unique.value;
  let state = !Shared.state in
  let snapshot = state.value in
  Shared.slot := Some (fun () -> (snapshot * 100) + state.value);
  Shared.factory
  := Some
       (fun () ->
         Shared.record "STATIC_PARTITION_LATE_ONLY_SENTINEL";
         23);
  (fun_call
     (pure_js_expr "(function(){joo_global_object.partitionTest.lazyFragment = true;})")
     [||]
   : unit);
  expose (fun () -> !Shared.counter);
  (fun_call
     (pure_js_expr
        "(function(){if(joo_global_object.partitionTest.failAfterRegistration) throw new \
         Error('initializer failed after registration');})")
     [||]
   : unit)
;;
