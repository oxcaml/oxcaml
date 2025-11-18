#include <caml/mlvalues.h>
#include <caml/fail.h>

value test_out_of_fibers(value unit)
{
    caml_raise_out_of_fibers();
}