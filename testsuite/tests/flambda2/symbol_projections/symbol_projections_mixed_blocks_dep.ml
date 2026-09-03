[@@@ocaml.flambda_o3]

external getenv : string -> string = "caml_sys_getenv"

type t_int64 = { x : int64#; y : int }
type t_int32 = { x : int32#; y : int }
type t_nativeint = { x : nativeint#; y : int }
type t_float = { x : float#; y : int }
type t_float32 = { x : float32#; y : int }

let unboxed_int64 : t_int64 =
  match getenv "FOO" with
  | exception _ -> { x = #1L; y = 1 }
  | _ -> { x = #2L; y = 2 }

let unboxed_int32 : t_int32 =
  match getenv "FOO" with
  | exception _ -> { x = #1l; y = 1 }
  | _ -> { x = #2l; y = 2 }

let unboxed_nativeint : t_nativeint =
  match getenv "FOO" with
  | exception _ -> { x = #1n; y = 1 }
  | _ -> { x = #2n; y = 2 }

let unboxed_float : t_float =
  match getenv "FOO" with
  | exception _ -> { x = #1.0; y = 1 }
  | _ -> { x = #2.0; y = 2 }

let unboxed_float32 : t_float32 =
  match getenv "FOO" with
  | exception _ -> { x = #1.0s; y = 1 }
  | _ -> { x = #2.0s; y = 2 }

let the_unboxed_int64 : int64# = unboxed_int64.x
let the_unboxed_int32 : int32# = unboxed_int32.x
let the_unboxed_nativeint : nativeint# = unboxed_nativeint.x
let the_unboxed_float : float# = unboxed_float.x
let the_unboxed_float32 : float32# = unboxed_float32.x
