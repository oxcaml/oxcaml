[@@@ocaml.warnerror "+a-40-41-42"]

(* Regression test for the lane order of vector constants built by the
   vectorizer ([Simd_selection.vectorize_operation], [create_const_vec]): the
   first scalar instruction of a group accesses the lowest address and must
   provide the least significant lane of the vector constant. The constants
   below do not fit in 32 bits, so that the stores are not turned into
   [Istore_int] by selection and the constants are materialized into registers
   (forming a vectorizable group of [Const_int] instructions). Before the fix,
   the lanes were packed in reverse order, storing the constants at each other's
   addresses. *)

type t2 =
  { mutable d0 : int;
    mutable d1 : int
  }

let[@opaque] set_pair_consts (r : t2) : t2 =
  r.d0 <- 0x123456789;
  r.d1 <- 0xabcdef012;
  r

type t4 =
  { mutable e0 : int;
    mutable e1 : int;
    mutable e2 : int;
    mutable e3 : int
  }

let[@opaque] set_four_consts (r : t4) : t4 =
  r.e0 <- 0x100000001;
  r.e1 <- 0x200000002;
  r.e2 <- 0x300000003;
  r.e3 <- 0x400000004;
  r

let print_t2 ppf (t : t2) = Format.fprintf ppf "{ d0 = %x ; d1 = %x }" t.d0 t.d1

let print_t4 ppf (t : t4) =
  Format.fprintf ppf "{ e0 = %x ; e1 = %x ; e2 = %x ; e3 = %x }" t.e0 t.e1 t.e2
    t.e3

let () =
  Format.printf "set_pair_consts %a\n" print_t2
    (set_pair_consts { d0 = 0; d1 = 0 });
  Format.printf "set_four_consts %a\n" print_t4
    (set_four_consts { e0 = 0; e1 = 0; e2 = 0; e3 = 0 })
