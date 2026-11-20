(* TEST
 flambda2;
 (* The inlining asked for below is refused, with warning 55. *)
 flags = "-extension layout_poly_alpha -w -55";
 {
   ocamlopt_flags = "-O3";
   native;
 }{
   ocamlopt_flags = "-O3 -flambda2-simplify-stubs";
   native;
 }{
   ocamlopt_flags = "-Oclassic";
   native;
 }
*)

(* Templates defined in the default values of optional arguments, which are in
   the wrappers for those arguments: stubs. With [-flambda2-simplify-stubs],
   stubs can be inlined before they are simplified, which must not happen to
   one that contains the definition of a template (see [template_copies.ml]). *)

let rec self_call ?(k =
                    let poly_ t y = #(y, y) in
                    fun (x : int) ->
                      let #(a, _) = t x in
                      a + 1) n =
  if n <= 0 then k 0 else k n + (self_call [@inlined]) (n - 1)

let rec from_sibling ?(k =
                       let poly_ t y = #(y, y) in
                       fun (x : int) ->
                         let #(a, _) = t x in
                         a + 1) n =
  if n <= 0 then k 0 else sibling (n - 1)

and sibling n = (from_sibling [@inlined]) n + 1

let rec unrolled ?(k =
                   let poly_ t y = #(y, y) in
                   fun (x : int) ->
                     let #(a, _) = t x in
                     a + 1) n =
  if n <= 0 then k 0 else k n + (unrolled [@unrolled 2]) (n - 1)

let () =
  let n = Sys.opaque_identity 3 in
  Printf.printf "%d %d %d\n" (self_call n) (from_sibling n) (unrolled n)
