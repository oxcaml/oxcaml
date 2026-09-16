(* TEST
 {
   toplevel;
 }{
   toplevel.opt;
 }
*)

(* Test [eval_address]. The easiest way to do this is to install printers
   located in mixed modules. *)

module M = struct
  let foo = "hello"
  let unboxed_product = #(#(10, #10.0), "ten", #10L)
  module N = struct
    let one = 1
    module K = struct
      let zero = #0.0
      let print_int out i =
        Format.fprintf out "%s" (match i with 5 -> "five" | _ -> "?")
    end
    let two = 2
  end
  let bar = #42L
  let baz = "world"
end;;

#install_printer M.N.K.print_int;;
5;;
