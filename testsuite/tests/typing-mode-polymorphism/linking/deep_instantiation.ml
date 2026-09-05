(* TEST
 readonly_files = "lib.ml";
 setup-ocamlc.byte-build-env;
 flags += "-extension mode_polymorphism_alpha";
 module = "lib.ml";
 ocamlc.byte;
 flags += " -no-extension mode_polymorphism_alpha -I ocamlc.byte \
   ocamlc.byte/lib.cmo";
 expect;
*)

let deep () =
  let d2 () =
    let d3 () =
      let g @ local = Lib.curried 1 in
      let t = g 2 3 in
      t
    in
    d3 ()
  in
  d2 ()
[%%expect{|
val deep : unit -> int * int * int = <fun>
|}]

module F (X : sig val n : int end) = struct
  let g = Lib.curried X.n
  let h = Lib.id
end
module A = F (struct let n = 1 end)
let r () = A.g 2 3
[%%expect{|
module F :
  functor (X : sig val n : int end) ->
    sig
      val g : '_weak1 -> '_weak2 -> int * '_weak1 * '_weak2
      val h : 'a -> 'a
    end
module A :
  sig
    val g : '_weak1 -> '_weak2 -> int * '_weak1 * '_weak2
    val h : 'a -> 'a
  end
val r : unit -> int * int * int = <fun>
|}]

let localmod () =
  let module M = struct
    let g = Lib.curried 1
  end in
  M.g 2 3
[%%expect{|
val localmod : unit -> int * int * int = <fun>
|}]

let obj_use =
  let o = object
    method m x = Lib.id x
    method n =
      let g = Lib.curried 1 in
      g 2 3
  end in
  o#m 1
[%%expect{|
val obj_use : int = 1
|}]
