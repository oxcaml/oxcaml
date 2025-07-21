source_filename = "float_ops.ml"

@camlFloat_ops__data_begin = global i64 0
define void @camlFloat_ops__code_begin() { ret void }

; camlFloat_ops__add_0_7_code [ float_ops.ml:11,26--78 ]
define double @camlFloat_ops__add_0_7_code(double %0, double %1)  {
  %3 = alloca double ; pin:anon:F/13[%xmm0] 
  %4 = alloca double ; pin:anon:F/14[%xmm1] 
  %5 = alloca double ; x:F/29 
  %6 = alloca double ; y:F/30 
  %7 = alloca double ; anon:F/31 
  store double %0, ptr %3
  store double %1, ptr %4
  br label %L1
L1:
  ; prologue 
  ; goto 101 
  ; goto 101 
  br label %L101
L101:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %8 = load double, ptr %3
  store double %8, ptr %5
  ; y:F/30 := pin:anon:F/14[%xmm1] 
  %9 = load double, ptr %4
  store double %9, ptr %6
  ; anon:F/31 := x:F/29 +. y:F/30 [ float_ops.ml:11,41--78 ]
  %10 = load double, ptr %5
  %11 = load double, ptr %6
  %12 = fadd double %10, %11
  store double %12, ptr %7
  ; pin:anon:F/13[%xmm0] := anon:F/31 
  %13 = load double, ptr %7
  store double %13, ptr %3
  ; reload retaddr 
  ; Return pin:anon:F/13[%xmm0] 
  ; Return pin:anon:F/13[%xmm0] 
  %14 = load double, ptr %3
  ret double %14
}

; camlFloat_ops__sub_1_8_code [ float_ops.ml:13,26--78 ]
define double @camlFloat_ops__sub_1_8_code(double %0, double %1)  {
  %3 = alloca double ; pin:anon:F/13[%xmm0] 
  %4 = alloca double ; pin:anon:F/14[%xmm1] 
  %5 = alloca double ; x:F/29 
  %6 = alloca double ; y:F/30 
  %7 = alloca double ; anon:F/31 
  store double %0, ptr %3
  store double %1, ptr %4
  br label %L1
L1:
  ; prologue 
  ; goto 105 
  ; goto 105 
  br label %L105
L105:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %8 = load double, ptr %3
  store double %8, ptr %5
  ; y:F/30 := pin:anon:F/14[%xmm1] 
  %9 = load double, ptr %4
  store double %9, ptr %6
  ; anon:F/31 := x:F/29 -. y:F/30 [ float_ops.ml:13,41--78 ]
  %10 = load double, ptr %5
  %11 = load double, ptr %6
  %12 = fsub double %10, %11
  store double %12, ptr %7
  ; pin:anon:F/13[%xmm0] := anon:F/31 
  %13 = load double, ptr %7
  store double %13, ptr %3
  ; reload retaddr 
  ; Return pin:anon:F/13[%xmm0] 
  ; Return pin:anon:F/13[%xmm0] 
  %14 = load double, ptr %3
  ret double %14
}

; camlFloat_ops__mul_2_9_code [ float_ops.ml:15,26--78 ]
define double @camlFloat_ops__mul_2_9_code(double %0, double %1)  {
  %3 = alloca double ; pin:anon:F/13[%xmm0] 
  %4 = alloca double ; pin:anon:F/14[%xmm1] 
  %5 = alloca double ; x:F/29 
  %6 = alloca double ; y:F/30 
  %7 = alloca double ; anon:F/31 
  store double %0, ptr %3
  store double %1, ptr %4
  br label %L1
L1:
  ; prologue 
  ; goto 109 
  ; goto 109 
  br label %L109
L109:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %8 = load double, ptr %3
  store double %8, ptr %5
  ; y:F/30 := pin:anon:F/14[%xmm1] 
  %9 = load double, ptr %4
  store double %9, ptr %6
  ; anon:F/31 := x:F/29 *. y:F/30 [ float_ops.ml:15,41--78 ]
  %10 = load double, ptr %5
  %11 = load double, ptr %6
  %12 = fmul double %10, %11
  store double %12, ptr %7
  ; pin:anon:F/13[%xmm0] := anon:F/31 
  %13 = load double, ptr %7
  store double %13, ptr %3
  ; reload retaddr 
  ; Return pin:anon:F/13[%xmm0] 
  ; Return pin:anon:F/13[%xmm0] 
  %14 = load double, ptr %3
  ret double %14
}

; camlFloat_ops__div_3_10_code [ float_ops.ml:17,26--78 ]
define double @camlFloat_ops__div_3_10_code(double %0, double %1)  {
  %3 = alloca double ; pin:anon:F/13[%xmm0] 
  %4 = alloca double ; pin:anon:F/14[%xmm1] 
  %5 = alloca double ; x:F/29 
  %6 = alloca double ; y:F/30 
  %7 = alloca double ; anon:F/31 
  store double %0, ptr %3
  store double %1, ptr %4
  br label %L1
L1:
  ; prologue 
  ; goto 113 
  ; goto 113 
  br label %L113
L113:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %8 = load double, ptr %3
  store double %8, ptr %5
  ; y:F/30 := pin:anon:F/14[%xmm1] 
  %9 = load double, ptr %4
  store double %9, ptr %6
  ; anon:F/31 := x:F/29 /. y:F/30 [ float_ops.ml:17,41--78 ]
  %10 = load double, ptr %5
  %11 = load double, ptr %6
  %12 = fdiv double %10, %11
  store double %12, ptr %7
  ; pin:anon:F/13[%xmm0] := anon:F/31 
  %13 = load double, ptr %7
  store double %13, ptr %3
  ; reload retaddr 
  ; Return pin:anon:F/13[%xmm0] 
  ; Return pin:anon:F/13[%xmm0] 
  %14 = load double, ptr %3
  ret double %14
}

; camlFloat_ops__neg_4_11_code [ float_ops.ml:19,26--63 ]
define double @camlFloat_ops__neg_4_11_code(double %0)  {
  %2 = alloca double ; pin:anon:F/13[%xmm0] 
  %3 = alloca double ; x:F/29 
  %4 = alloca double ; anon:F/30 
  store double %0, ptr %2
  br label %L1
L1:
  ; prologue 
  ; goto 117 
  ; goto 117 
  br label %L117
L117:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %5 = load double, ptr %2
  store double %5, ptr %3
  ; anon:F/30 := x:F/29 
  %6 = load double, ptr %3
  store double %6, ptr %4
  ; anon:F/30 := neg anon:F/30 [ float_ops.ml:19,39--63 ]
  %7 = load double, ptr %4
  %8 = fneg double %7
  store double %8, ptr %4
  ; pin:anon:F/13[%xmm0] := anon:F/30 
  %9 = load double, ptr %4
  store double %9, ptr %2
  ; reload retaddr 
  ; Return pin:anon:F/13[%xmm0] 
  ; Return pin:anon:F/13[%xmm0] 
  %10 = load double, ptr %2
  ret double %10
}

; camlFloat_ops__abs_5_12_code [ float_ops.ml:22,26--63 ]
define double @camlFloat_ops__abs_5_12_code(double %0)  {
  %2 = alloca double ; pin:anon:F/13[%xmm0] 
  %3 = alloca double ; x:F/29 
  %4 = alloca double ; anon:F/30 
  store double %0, ptr %2
  br label %L1
L1:
  ; prologue 
  ; goto 121 
  ; goto 121 
  br label %L121
L121:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %5 = load double, ptr %2
  store double %5, ptr %3
  ; anon:F/30 := x:F/29 
  %6 = load double, ptr %3
  store double %6, ptr %4
  ; anon:F/30 := abs anon:F/30 [ float_ops.ml:22,39--63 ]
  %7 = load double, ptr %4
  %8 = call double @llvm.fabs.f64(double %7)
  store double %8, ptr %4
  ; pin:anon:F/13[%xmm0] := anon:F/30 
  %9 = load double, ptr %4
  store double %9, ptr %2
  ; reload retaddr 
  ; Return pin:anon:F/13[%xmm0] 
  ; Return pin:anon:F/13[%xmm0] 
  %10 = load double, ptr %2
  ret double %10
}

; camlFloat_ops__compare_6_13_code [ float_ops.ml:24,30--77 ]
define i64 @camlFloat_ops__compare_6_13_code(double %0, double %1)  {
  %3 = alloca double ; pin:anon:F/13[%xmm0] 
  %4 = alloca double ; pin:anon:F/14[%xmm1] 
  %5 = alloca i64 ; pin:anon:I/0[%rax] 
  %6 = alloca double ; x:F/29 
  %7 = alloca double ; y:F/30 
  %8 = alloca i64 ; anon:I/31 
  %9 = alloca double ; anon:F/32 
  %10 = alloca i64 ; anon:I/33 
  %11 = alloca double ; anon:F/34 
  %12 = alloca i64 ; anon:I/35 
  %13 = alloca i64 ; anon:I/36 
  %14 = alloca double ; anon:F/37 
  %15 = alloca i64 ; anon:I/38 
  %16 = alloca double ; anon:F/39 
  %17 = alloca i64 ; anon:I/40 
  %18 = alloca i64 ; anon:I/41 
  %19 = alloca i64 ; anon:I/42 
  %20 = alloca i64 ; anon:I/43 
  store double %0, ptr %3
  store double %1, ptr %4
  br label %L1
L1:
  ; prologue 
  ; goto 125 
  ; goto 125 
  br label %L125
L125:                                                ; preds = %L1
  ; x:F/29 := pin:anon:F/13[%xmm0] 
  %21 = load double, ptr %3
  store double %21, ptr %6
  ; y:F/30 := pin:anon:F/14[%xmm1] 
  %22 = load double, ptr %4
  store double %22, ptr %7
  ; anon:I/31 anon:F/32 := y:F/30 == y:F/30 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %23 = load double, ptr %7
  %24 = load double, ptr %7
  %25 = fcmp oeq double %23, %24
  %26 = zext i1 %25 to i64
  store i64 %26, ptr %8
  ; anon:I/33 anon:F/34 := x:F/29 == x:F/29 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %27 = load double, ptr %6
  %28 = load double, ptr %6
  %29 = fcmp oeq double %27, %28
  %30 = zext i1 %29 to i64
  store i64 %30, ptr %10
  ; anon:I/35 := anon:I/33 
  %31 = load i64, ptr %10
  store i64 %31, ptr %12
  ; anon:I/35 := anon:I/35 - anon:I/31 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %32 = load i64, ptr %12
  %33 = load i64, ptr %8
  %34 = sub i64 %32, %33
  store i64 %34, ptr %12
  ; anon:I/36 anon:F/37 := x:F/29 < y:F/30 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %35 = load double, ptr %6
  %36 = load double, ptr %7
  %37 = fcmp olt double %35, %36
  %38 = zext i1 %37 to i64
  store i64 %38, ptr %13
  ; anon:I/38 anon:F/39 := x:F/29 > y:F/30 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %39 = load double, ptr %6
  %40 = load double, ptr %7
  %41 = fcmp ogt double %39, %40
  %42 = zext i1 %41 to i64
  store i64 %42, ptr %15
  ; anon:I/40 := anon:I/38 
  %43 = load i64, ptr %15
  store i64 %43, ptr %17
  ; anon:I/40 := anon:I/40 - anon:I/36 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %44 = load i64, ptr %17
  %45 = load i64, ptr %13
  %46 = sub i64 %44, %45
  store i64 %46, ptr %17
  ; anon:I/41 := anon:I/40 
  %47 = load i64, ptr %17
  store i64 %47, ptr %18
  ; anon:I/41 := anon:I/41 + anon:I/35 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %48 = load i64, ptr %18
  %49 = load i64, ptr %12
  %50 = add i64 %48, %49
  store i64 %50, ptr %18
  ; anon:I/42 := anon:I/41 
  %51 = load i64, ptr %18
  store i64 %51, ptr %19
  ; anon:I/42 := anon:I/42 << 1 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %52 = load i64, ptr %19
  %53 = shl i64 %52, 1
  store i64 %53, ptr %19
  ; anon:I/43 := anon:I/42 
  %54 = load i64, ptr %19
  store i64 %54, ptr %20
  ; anon:I/43 := anon:I/43 + 1 [ float_ops.ml:24,36--77;float.ml:142,0--65 ]
  %55 = load i64, ptr %20
  %56 = add i64 %55, 1
  store i64 %56, ptr %20
  ; pin:anon:I/0[%rax] := anon:I/43 
  %57 = load i64, ptr %20
  store i64 %57, ptr %5
  ; reload retaddr 
  ; Return pin:anon:I/0[%rax] 
  ; Return pin:anon:I/0[%rax] 
  %58 = load i64, ptr %5
  ret i64 %58
}

; camlFloat_ops__entry 
define i64 @camlFloat_ops__entry()  {
  %1 = alloca i64 ; pin:anon:I/0[%rax] 
  %2 = alloca i64 ; *ret*:V/29 
  %3 = alloca i64 ; anon:I/30 
  %4 = alloca i64 ; anon:I/31 
  %5 = alloca i64 ; anon:I/32 
  br label %L1
L1:
  ; prologue 
  ; goto 140 
  ; goto 140 
  br label %L140
L140:                                                ; preds = %L1
  ; anon:I/30 := "camlFloat_ops" 
  store ptr @camlFloat_ops, ptr %3
  ; anon:I/31 := anon:I/30 
  %6 = load i64, ptr %3
  store i64 %6, ptr %4
  ; *ret*:V/29 := anon:I/31 
  %7 = load i64, ptr %4
  store i64 %7, ptr %2
  ; anon:I/32 := 1 
  store i64 1, ptr %5
  ; pin:anon:I/0[%rax] := anon:I/32 
  %8 = load i64, ptr %5
  store i64 %8, ptr %1
  ; reload retaddr 
  ; Return pin:anon:I/0[%rax] 
  ; Return pin:anon:I/0[%rax] 
  %9 = load i64, ptr %1
  ret i64 %9
}

@camlFloat_ops__gc_roots = global { i64, i64 } { i64 0, i64 1792 }
@camlFloat_ops = global { ptr, i64 } { ptr @camlFloat_ops__Pmakeblock253, i64 7936 }
@camlFloat_ops__Pmakeblock253 = global { ptr, ptr, ptr, ptr, ptr, ptr, ptr, i64 } { ptr @camlFloat_ops__add_7, ptr @camlFloat_ops__sub_8, ptr @camlFloat_ops__mul_9, ptr @camlFloat_ops__div_10, ptr @camlFloat_ops__neg_11, ptr @camlFloat_ops__abs_12, ptr @camlFloat_ops__compare_13, i64 4087 }
@camlFloat_ops__compare_13 = global { ptr, i64, ptr, i64 } { ptr @caml_curryF_F, i64 180143985094819847, ptr @camlFloat_ops__compare_6_13_code, i64 3063 }
@camlFloat_ops__abs_12 = global { ptr, i64, i64 } { ptr @camlFloat_ops__abs_5_12_code, i64 108086391056891909, i64 3063 }
@camlFloat_ops__neg_11 = global { ptr, i64, i64 } { ptr @camlFloat_ops__neg_4_11_code, i64 108086391056891909, i64 4087 }
@camlFloat_ops__div_10 = global { ptr, i64, ptr, i64 } { ptr @caml_curryF_F_RF, i64 180143985094819847, ptr @camlFloat_ops__div_3_10_code, i64 4087 }
@camlFloat_ops__mul_9 = global { ptr, i64, ptr, i64 } { ptr @caml_curryF_F_RF, i64 180143985094819847, ptr @camlFloat_ops__mul_2_9_code, i64 4087 }
@camlFloat_ops__sub_8 = global { ptr, i64, ptr, i64 } { ptr @caml_curryF_F_RF, i64 180143985094819847, ptr @camlFloat_ops__sub_1_8_code, i64 4087 }
@camlFloat_ops__add_7 = global { ptr, i64, ptr } { ptr @caml_curryF_F_RF, i64 180143985094819847, ptr @camlFloat_ops__add_0_7_code }
@caml_curryF_F = external global ptr
@caml_curryF_F_RF = external global ptr

@camlFloat_ops__data_end = global i64 0
define void @camlFloat_ops__code_end() { ret void }
@camlFloat_ops__frametable = global i64 0
