; caml_program 
define i64 @caml_program()  {
  %1 = alloca i64 ; pin:anon:I/0[%rax] 
  %2 = alloca i64 ; *id*:I/29 
  %3 = alloca i64 ; anon:I/30 
  %4 = alloca i64 ; anon:I/31 
  %5 = alloca i64 ; anon:I/32 
  %6 = alloca i64 ; anon:I/33 
  %7 = alloca i64 ; anon:A/34 
  %8 = alloca i64 ; anon:I/35 
  %9 = alloca i64 ; anon:I/36 
  %10 = alloca i64 ; anon:I/37 
  %11 = alloca i64 ; anon:I/38 
  %12 = alloca i64 ; anon:I/39 
  %13 = alloca i64 ; anon:I/40 
  %14 = alloca i64 ; anon:I/41 
  %15 = alloca i64 ; anon:I/42 
  br label %L1
L1:
  ; prologue 
  ; goto 152 
  ; goto 152 
  br label %L152
L152:                                                ; preds = %L1
  ; anon:I/30 := 0 
  store i64 0, ptr %3
  ; anon:I/31 := anon:I/30 
  %16 = load i64, ptr %3
  store i64 %16, ptr %4
  ; *id*:I/29 := anon:I/31 
  %17 = load i64, ptr %4
  store i64 %17, ptr %2
  ; goto 154 
  ; goto 154 
  br label %L154
L154:                                                ; preds = %L152, %L161
  ; if *id*:I/29 < s 12 goto 160 if *id*:I/29 = s 12 goto 172 if *id*:I/29 > s 12 goto 160 
  ; if *id*:I/29 < s 12 goto 160 if *id*:I/29 = s 12 goto 172 if *id*:I/29 > s 12 goto 160 
  %18 = load i64, ptr %2
  %19 = icmp slt i64 %18, 12
  br i1 %19, label %L160, label %20
20:
  %21 = load i64, ptr %2
  %22 = icmp sgt i64 %21, 12
  br i1 %22, label %L160, label %L172
L160:                                                ; preds = %L154
  ; anon:I/32 := *id*:I/29 
  %23 = load i64, ptr %2
  store i64 %23, ptr %5
  ; anon:I/32 := anon:I/32 * 8 
  %24 = load i64, ptr %5
  %25 = mul i64 %24, 8
  store i64 %25, ptr %5
  ; anon:I/33 := "caml_globals_entry_functions" 
  store ptr @caml_globals_entry_functions, ptr %6
  ; anon:A/34 := anon:I/33 
  %26 = load i64, ptr %6
  store i64 %26, ptr %7
  ; anon:A/34 := anon:A/34 + anon:I/32 
  %27 = load i64, ptr %7
  %28 = load i64, ptr %5
  %29 = add i64 %27, %28
  store i64 %29, ptr %7
  ; anon:I/35 := int [anon:A/34] 
  %30 = load i64, ptr %7
  %31 = add i64 %30, 0
  %32 = inttoptr i64 %31 to ptr
  %33 = load i64, ptr %32
  store i64 %33, ptr %8
  ; call anon:I/35 goto 161 
  ; call anon:I/35 goto 161 
