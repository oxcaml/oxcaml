(* TEST
 include unix;
 flags += " -nolabels ";
 hasunix;
 {
   bytecode;
 }{
   native;
 }{
   jsir;
 }
*)

module U : module type of Unix = UnixLabels

let ()  =
  ()
