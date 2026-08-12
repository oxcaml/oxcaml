(* TEST
 flags = " -w -a ";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* A large `module rec` group whose members have manifest-type cycles, so an
   unroll depth of 1 is insufficient: the check must deepen to a larger depth
   to accept it. Exercises the escalation path. *)

module rec DirElt0 : sig type t = Root | Sub of DirHash0.t end
  = struct type t = Root | Sub of DirHash0.t end
and DirCompare0 : sig type t = DirElt0.t end
  = struct type t = DirElt0.t end
and DirHash0 : sig type t = DirElt0.t list end
  = struct type t = DirCompare0.t list end
and DirElt1 : sig type t = Root | Sub of DirHash1.t end
  = struct type t = Root | Sub of DirHash1.t end
and DirCompare1 : sig type t = DirElt1.t end
  = struct type t = DirElt1.t end
and DirHash1 : sig type t = DirElt1.t list end
  = struct type t = DirCompare1.t list end
and DirElt2 : sig type t = Root | Sub of DirHash2.t end
  = struct type t = Root | Sub of DirHash2.t end
and DirCompare2 : sig type t = DirElt2.t end
  = struct type t = DirElt2.t end
and DirHash2 : sig type t = DirElt2.t list end
  = struct type t = DirCompare2.t list end
and DirElt3 : sig type t = Root | Sub of DirHash3.t end
  = struct type t = Root | Sub of DirHash3.t end
and DirCompare3 : sig type t = DirElt3.t end
  = struct type t = DirElt3.t end
and DirHash3 : sig type t = DirElt3.t list end
  = struct type t = DirCompare3.t list end
and DirElt4 : sig type t = Root | Sub of DirHash4.t end
  = struct type t = Root | Sub of DirHash4.t end
and DirCompare4 : sig type t = DirElt4.t end
  = struct type t = DirElt4.t end
and DirHash4 : sig type t = DirElt4.t list end
  = struct type t = DirCompare4.t list end
and DirElt5 : sig type t = Root | Sub of DirHash5.t end
  = struct type t = Root | Sub of DirHash5.t end
and DirCompare5 : sig type t = DirElt5.t end
  = struct type t = DirElt5.t end
and DirHash5 : sig type t = DirElt5.t list end
  = struct type t = DirCompare5.t list end
and DirElt6 : sig type t = Root | Sub of DirHash6.t end
  = struct type t = Root | Sub of DirHash6.t end
and DirCompare6 : sig type t = DirElt6.t end
  = struct type t = DirElt6.t end
and DirHash6 : sig type t = DirElt6.t list end
  = struct type t = DirCompare6.t list end
and DirElt7 : sig type t = Root | Sub of DirHash7.t end
  = struct type t = Root | Sub of DirHash7.t end
and DirCompare7 : sig type t = DirElt7.t end
  = struct type t = DirElt7.t end
and DirHash7 : sig type t = DirElt7.t list end
  = struct type t = DirCompare7.t list end
and DirElt8 : sig type t = Root | Sub of DirHash8.t end
  = struct type t = Root | Sub of DirHash8.t end
and DirCompare8 : sig type t = DirElt8.t end
  = struct type t = DirElt8.t end
and DirHash8 : sig type t = DirElt8.t list end
  = struct type t = DirCompare8.t list end
and DirElt9 : sig type t = Root | Sub of DirHash9.t end
  = struct type t = Root | Sub of DirHash9.t end
and DirCompare9 : sig type t = DirElt9.t end
  = struct type t = DirElt9.t end
and DirHash9 : sig type t = DirElt9.t list end
  = struct type t = DirCompare9.t list end
and DirElt10 : sig type t = Root | Sub of DirHash10.t end
  = struct type t = Root | Sub of DirHash10.t end
and DirCompare10 : sig type t = DirElt10.t end
  = struct type t = DirElt10.t end
and DirHash10 : sig type t = DirElt10.t list end
  = struct type t = DirCompare10.t list end
and DirElt11 : sig type t = Root | Sub of DirHash11.t end
  = struct type t = Root | Sub of DirHash11.t end
and DirCompare11 : sig type t = DirElt11.t end
  = struct type t = DirElt11.t end
and DirHash11 : sig type t = DirElt11.t list end
  = struct type t = DirCompare11.t list end
and DirElt12 : sig type t = Root | Sub of DirHash12.t end
  = struct type t = Root | Sub of DirHash12.t end
and DirCompare12 : sig type t = DirElt12.t end
  = struct type t = DirElt12.t end
and DirHash12 : sig type t = DirElt12.t list end
  = struct type t = DirCompare12.t list end
and DirElt13 : sig type t = Root | Sub of DirHash13.t end
  = struct type t = Root | Sub of DirHash13.t end
and DirCompare13 : sig type t = DirElt13.t end
  = struct type t = DirElt13.t end
and DirHash13 : sig type t = DirElt13.t list end
  = struct type t = DirCompare13.t list end
and DirElt14 : sig type t = Root | Sub of DirHash14.t end
  = struct type t = Root | Sub of DirHash14.t end
and DirCompare14 : sig type t = DirElt14.t end
  = struct type t = DirElt14.t end
and DirHash14 : sig type t = DirElt14.t list end
  = struct type t = DirCompare14.t list end
and DirElt15 : sig type t = Root | Sub of DirHash15.t end
  = struct type t = Root | Sub of DirHash15.t end
and DirCompare15 : sig type t = DirElt15.t end
  = struct type t = DirElt15.t end
and DirHash15 : sig type t = DirElt15.t list end
  = struct type t = DirCompare15.t list end
and DirElt16 : sig type t = Root | Sub of DirHash16.t end
  = struct type t = Root | Sub of DirHash16.t end
and DirCompare16 : sig type t = DirElt16.t end
  = struct type t = DirElt16.t end
and DirHash16 : sig type t = DirElt16.t list end
  = struct type t = DirCompare16.t list end
and DirElt17 : sig type t = Root | Sub of DirHash17.t end
  = struct type t = Root | Sub of DirHash17.t end
and DirCompare17 : sig type t = DirElt17.t end
  = struct type t = DirElt17.t end
and DirHash17 : sig type t = DirElt17.t list end
  = struct type t = DirCompare17.t list end
and DirElt18 : sig type t = Root | Sub of DirHash18.t end
  = struct type t = Root | Sub of DirHash18.t end
and DirCompare18 : sig type t = DirElt18.t end
  = struct type t = DirElt18.t end
and DirHash18 : sig type t = DirElt18.t list end
  = struct type t = DirCompare18.t list end
and DirElt19 : sig type t = Root | Sub of DirHash19.t end
  = struct type t = Root | Sub of DirHash19.t end
and DirCompare19 : sig type t = DirElt19.t end
  = struct type t = DirElt19.t end
and DirHash19 : sig type t = DirElt19.t list end
  = struct type t = DirCompare19.t list end
and DirElt20 : sig type t = Root | Sub of DirHash20.t end
  = struct type t = Root | Sub of DirHash20.t end
and DirCompare20 : sig type t = DirElt20.t end
  = struct type t = DirElt20.t end
and DirHash20 : sig type t = DirElt20.t list end
  = struct type t = DirCompare20.t list end
and DirElt21 : sig type t = Root | Sub of DirHash21.t end
  = struct type t = Root | Sub of DirHash21.t end
and DirCompare21 : sig type t = DirElt21.t end
  = struct type t = DirElt21.t end
and DirHash21 : sig type t = DirElt21.t list end
  = struct type t = DirCompare21.t list end
and DirElt22 : sig type t = Root | Sub of DirHash22.t end
  = struct type t = Root | Sub of DirHash22.t end
and DirCompare22 : sig type t = DirElt22.t end
  = struct type t = DirElt22.t end
and DirHash22 : sig type t = DirElt22.t list end
  = struct type t = DirCompare22.t list end
and DirElt23 : sig type t = Root | Sub of DirHash23.t end
  = struct type t = Root | Sub of DirHash23.t end
and DirCompare23 : sig type t = DirElt23.t end
  = struct type t = DirElt23.t end
and DirHash23 : sig type t = DirElt23.t list end
  = struct type t = DirCompare23.t list end
and DirElt24 : sig type t = Root | Sub of DirHash24.t end
  = struct type t = Root | Sub of DirHash24.t end
and DirCompare24 : sig type t = DirElt24.t end
  = struct type t = DirElt24.t end
and DirHash24 : sig type t = DirElt24.t list end
  = struct type t = DirCompare24.t list end
and DirElt25 : sig type t = Root | Sub of DirHash25.t end
  = struct type t = Root | Sub of DirHash25.t end
and DirCompare25 : sig type t = DirElt25.t end
  = struct type t = DirElt25.t end
and DirHash25 : sig type t = DirElt25.t list end
  = struct type t = DirCompare25.t list end
and DirElt26 : sig type t = Root | Sub of DirHash26.t end
  = struct type t = Root | Sub of DirHash26.t end
and DirCompare26 : sig type t = DirElt26.t end
  = struct type t = DirElt26.t end
and DirHash26 : sig type t = DirElt26.t list end
  = struct type t = DirCompare26.t list end
and DirElt27 : sig type t = Root | Sub of DirHash27.t end
  = struct type t = Root | Sub of DirHash27.t end
and DirCompare27 : sig type t = DirElt27.t end
  = struct type t = DirElt27.t end
and DirHash27 : sig type t = DirElt27.t list end
  = struct type t = DirCompare27.t list end
and DirElt28 : sig type t = Root | Sub of DirHash28.t end
  = struct type t = Root | Sub of DirHash28.t end
and DirCompare28 : sig type t = DirElt28.t end
  = struct type t = DirElt28.t end
and DirHash28 : sig type t = DirElt28.t list end
  = struct type t = DirCompare28.t list end
and DirElt29 : sig type t = Root | Sub of DirHash29.t end
  = struct type t = Root | Sub of DirHash29.t end
and DirCompare29 : sig type t = DirElt29.t end
  = struct type t = DirElt29.t end
and DirHash29 : sig type t = DirElt29.t list end
  = struct type t = DirCompare29.t list end
