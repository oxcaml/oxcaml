#include <stdio.h>
#include <caml/mlvalues.h>

CAMLprim value print_and_add(value xv, value yv) {
    long x = Long_val(xv), y = Long_val(yv);
    printf("Hello from C! %ld %ld\n", x, y);
    return Val_long(x + y);
}

CAMLprim value too_many(value x1, value x2, value x3, value x4,  value x5,  value x6,
                        value x7, value x8, value x9, value x10, value x11, value x12) {
    printf("too_many arg list: %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld %ld\n",
        Long_val(x1),
        Long_val(x2),
        Long_val(x3),
        Long_val(x4),
        Long_val(x5),
        Long_val(x6),
        Long_val(x7),
        Long_val(x8),
        Long_val(x9),
        Long_val(x10),
        Long_val(x11),
        Long_val(x12));
    fflush(stdout);
    
    return x10;
}

CAMLprim value int_and_float(value i1, value f2, value i3, value f4) {
    printf("int_and_float arg list: %ld %f %ld %f\n",
        Long_val(i1),
        Double_val(f2),
        Long_val(i3),
        Double_val(f4));
    fflush(stdout);
    
    return Val_unit;
}
