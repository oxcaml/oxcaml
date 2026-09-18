(* TEST
 {
   bytecode;
 }
 {
   native;
 }
*)

external makearray_dynamic : ('a : any mod separable). int -> 'a -> 'a array =
  "%makearray_dynamic" [@@layout_poly]

let f32 () = print_endline "f32"; #0.s
let f64 () = print_endline "f64"; #0.
let i8 () = print_endline "i8"; #0s
let i16 () = print_endline "i16"; #0S
let i32 () = print_endline "i32"; #0l
let i64 () = print_endline "i64"; #0L
let inat () = print_endline "inat"; #0n
let scannable () = print_endline "scannable"; #(0, "string")
let ignorable () = print_endline "ignorable"; #(0, #0.)

let _ = Sys.opaque_identity (makearray_dynamic 5 (f32 ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (f64 ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (i8 ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (i16 ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (i32 ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (i64 ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (inat ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (scannable ()))
let _ = Sys.opaque_identity (makearray_dynamic 5 (ignorable ()))

let length1 () = print_endline "length1"; 5

let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0.s)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0.)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0s)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0S)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0l)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0L)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #0n)
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #(0, "string"))
let _ = Sys.opaque_identity (makearray_dynamic (length1 ()) #(0, #0.))

let _ =
  let length2 () = print_endline "length2"; 5 in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0.s) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0.) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0s) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0S) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0l) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0L) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #0n) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #(0, "string")) in
  let _ = Sys.opaque_identity (makearray_dynamic (length2 ()) #(0, #0.)) in
  ()
