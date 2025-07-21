let () =
  Format.printf
    {|
(-15) + 4 = %d
(-15) - 4 = %d
(-15) * 4 = %d
(-15) / 4 = %d
(-15) mod 4 = %d
(-15) land 4 = %d
(-15) lor 4 = %d
(-15) lxor 4 = %d
lnot (-15) = %d
(-15) lsl 4 = %d
(-15) lsr 4 = %d
(-15) asr 4 = %d
  |}
    (Int_ops.add ()) (Int_ops.sub ()) (Int_ops.mul ()) (Int_ops.div ())
    (Int_ops.mod_ ()) (Int_ops.land_ ()) (Int_ops.lor_ ()) (Int_ops.lxor_ ())
    (Int_ops.lnot_ ()) (Int_ops.lsl_ ()) (Int_ops.lsr_ ()) (Int_ops.asr_ ())
