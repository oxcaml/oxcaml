(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   native;
 } {
   bytecode;
 }
*)

module Int64_u = struct
  include Stdlib_upstream_compatible.Int64_u
end

module Mixed_record = struct
  (* the value field comes first, so flambda2 reordering is a noop *)
  type t = { mutable atomic : int [@atomic]; mutable nonatomic : int64# }

  let () =
    Printf.printf "== Mixed record (no reordering) ==\n";
    let t = { atomic = 42; nonatomic = #13L } in
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.atomic <- 43; Printf.printf "set t.atomic <- 43\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
end

module Mixed_record_reorder = struct
  (* flambda2 will reorder so [atomic] comes before [nonatomic] *)
  type t = { mutable nonatomic : int64#; mutable atomic : int [@atomic] }

  let () =
    Printf.printf "== Mixed record (reordering) ==\n";
    let t = { atomic = 42; nonatomic = #13L } in
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.atomic <- 43; Printf.printf "set t.atomic <- 43\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
end

module Mixed_inline_record = struct
  (* use an inline record *)
  type t = A of { mutable atomic : int [@atomic]; mutable nonatomic : int64# }

  let () =
    Printf.printf "== Mixed inline record (no reordering) ==\n";
    let (A t) = A { atomic = 42; nonatomic = #13L } in
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.atomic <- 43; Printf.printf "set t.atomic <- 43\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
end

module Mixed_inline_record_reorder = struct
  type t = A of { mutable nonatomic : int64#; mutable atomic : int [@atomic] }

  let () =
    Printf.printf "== Mixed inline record (reordering) ==\n";
    let (A t) = A { atomic = 42; nonatomic = #13L } in
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.atomic <- 43; Printf.printf "set t.atomic <- 43\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic: %d\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
end

module Mixed_record_nonimmediate = struct
  (* atomic field is not an immediate *)
  type t = { mutable atomic : string [@atomic]; mutable nonatomic : int64# }

  let () =
    Printf.printf "== Mixed record with non-immediate (no reordering) ==\n";
    let t = { atomic = "hello"; nonatomic = #13L } in
    Printf.printf "t.atomic: %s\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.atomic <- "world"; Printf.printf "set t.atomic <- \"world\"\n";
    Printf.printf "t.atomic: %s\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic: %s\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
end

module Mixed_record_nonimmediate_reorder = struct
  type t = { mutable nonatomic : int64#; mutable atomic : string [@atomic]; }

  let () =
    Printf.printf "== Mixed record with non-immediate (reordering) ==\n";
    let t = { atomic = "hello"; nonatomic = #13L } in
    Printf.printf "t.atomic: %s\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.atomic <- "world"; Printf.printf "set t.atomic <- \"world\"\n";
    Printf.printf "t.atomic: %s\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic: %s\n" t.atomic;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
end

module Mixed_record_sandwich = struct
  (* a non-value field sandwiched between two atomic value fields *)
  type t =
    { mutable atomic1 : int [@atomic];
      mutable nonatomic : int64#;
      mutable atomic2 : string [@atomic]
    }

  let () =
    Printf.printf "== Mixed record (sandwiched non-value field) ==\n";
    let t = { atomic1 = 42; nonatomic = #13L; atomic2 = "hello" } in
    Printf.printf "t.atomic1: %d\n" t.atomic1;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
    Printf.printf "t.atomic2: %s\n" t.atomic2;

    t.atomic1 <- 43; Printf.printf "set t.atomic1 <- 43\n";
    Printf.printf "t.atomic1: %d\n" t.atomic1;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
    Printf.printf "t.atomic2: %s\n" t.atomic2;

    t.nonatomic <- #14L; Printf.printf "set t.nonatomic <- 14\n";
    Printf.printf "t.atomic1: %d\n" t.atomic1;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
    Printf.printf "t.atomic2: %s\n" t.atomic2;

    t.atomic2 <- "world"; Printf.printf "set t.atomic2 <- \"world\"\n";
    Printf.printf "t.atomic1: %d\n" t.atomic1;
    Printf.printf "t.nonatomic: %Ld\n" (Int64_u.to_int64 t.nonatomic);
    Printf.printf "t.atomic2: %s\n" t.atomic2
end
