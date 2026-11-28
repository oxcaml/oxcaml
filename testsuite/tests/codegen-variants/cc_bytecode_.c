#define CAML_NAME_SPACE
#include <caml/mlvalues.h>

volatile int cc_bytecode_cell[1] = { 42 };

value cc_bytecode_answer(value unit)
{
  (void)unit;
  return Val_int(cc_bytecode_cell[0]);
}
