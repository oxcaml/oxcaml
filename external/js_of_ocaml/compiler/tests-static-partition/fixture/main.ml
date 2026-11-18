external expose : (unit -> int) -> unit = "partition_test_expose_main"
external expose_named : string -> 'a -> unit = "partition_test_expose"
external pure_js_expr : string -> 'a = "caml_pure_js_expr"
external fun_call : 'a -> 'b array -> 'c = "caml_js_fun_call"

type implementation_export = { implementation : int -> int } [@@boxed]

(* A module exporting one value has the same layout as this one-field record. *)
external as_export : 'a -> implementation_export = "%identity"

let () =
  Shared.record "main";
  Shared.counter := 1;
  expose_named "bucketCount" Numeric_buckets.count;
  expose_named "callExport" (fun module_value ->
    (as_export module_value).implementation 41);
  let before = !Shared.slot in
  expose_named "before" (fun () ->
    match before with
    | None -> -1
    | Some f -> f ());
  expose_named "probe" (fun () ->
    match !Shared.slot with
    | None -> -1
    | Some f -> f ());
  expose_named "probeFactory" (fun () ->
    match !Shared.factory with
    | None -> -1
    | Some f -> f ());
  expose_named "setState" (fun value -> !Shared.state.value <- value);
  expose_named "replaceState" (fun value -> Shared.state := { value });
  expose_named "currentState" (fun () -> !Shared.state.value);
  (fun_call
     (pure_js_expr "(function(){joo_global_object.partitionTest.mainFragment = true;})")
     [||]
   : unit);
  expose (fun () -> !Shared.counter)
;;
