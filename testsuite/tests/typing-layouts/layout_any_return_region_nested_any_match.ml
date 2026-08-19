(* TEST
 flags = "-extension layouts_beta -extension mode";
 flambda2;
 {
   native;
 }{
   bytecode;
 }
*)

(* As [layout_any_return_nested_any_match] but with a local region: nested
   matches whose leaves are all the same sort (value) form a single
   direct-return layout through the region-close path. *)

external opaque_local : local_ 'a -> local_ 'a = "%opaque"

type (_ : any) direct_w =
  | Direct_int : int direct_w
  | Direct_string : string direct_w

type (_ : any) nested_w =
  | Nested : ('a : any). 'a direct_w -> 'a nested_w

let[@inline never] nested_int_vs_string : type (a : any). a nested_w -> a =
  fun w ->
    let local_ x = ref 0 in
    let local_ _y = opaque_local x in
    match w with
    | Nested inner ->
      match inner with
      | Direct_int -> 42
      | Direct_string -> "x"

type nested_calls =
  { call_int : unit -> int;
    call_string : unit -> string
  }

let[@inline never] make_nested_calls
    (f : ('a : any). 'a nested_w -> 'a) =
  { call_int = (fun () -> f (Nested Direct_int));
    call_string = (fun () -> f (Nested Direct_string))
  }

type (_ : any) nested_call_w =
  | Nested_call_int : int nested_call_w
  | Nested_call_string : string nested_call_w

type (_ : any) nested_call_outer_w =
  | Nested_call : ('a : any). 'a nested_call_w -> 'a nested_call_outer_w

let[@inline never] nested_call_vs_string
    : type (a : any). a nested_call_outer_w -> a =
  fun w ->
    let g () = 7 in
    match w with
    | Nested_call inner ->
      match inner with
      | Nested_call_int -> g ()
      | Nested_call_string -> "s"

type nested_call_calls =
  { call_value : unit -> int;
    call_string : unit -> string
  }

let[@inline never] make_nested_call_calls
    (f : ('a : any). 'a nested_call_outer_w -> 'a) =
  { call_value = (fun () -> f (Nested_call Nested_call_int));
    call_string = (fun () -> f (Nested_call Nested_call_string))
  }

let () =
  assert (nested_int_vs_string (Nested Direct_int) = 42);
  assert (String.equal (nested_int_vs_string (Nested Direct_string)) "x");
  let calls = make_nested_calls nested_int_vs_string in
  assert (calls.call_int () = 42);
  assert (String.equal (calls.call_string ()) "x");
  assert (nested_call_vs_string (Nested_call Nested_call_int) = 7);
  assert
    (String.equal
       (nested_call_vs_string (Nested_call Nested_call_string)) "s");
  let call_calls = make_nested_call_calls nested_call_vs_string in
  assert (call_calls.call_value () = 7);
  assert (String.equal (call_calls.call_string ()) "s");
  ()
