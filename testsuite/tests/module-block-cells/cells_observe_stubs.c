#include <caml/mlvalues.h>
#include <caml/alloc.h>

/* The module block of Cells_observe_lib and its cells.  A data symbol labels
   the first field of its block, so the symbol's address is the block. */
extern value camlCells_observe_lib;
extern value camlCells_observe_lib__cell0;
extern value camlCells_observe_lib__cell1;
extern value camlCells_observe_lib__cell2;
extern value camlCells_observe_lib__cell3;
extern value camlCells_observe_lib__cell4;

static value block(void)
{
  return (value)&camlCells_observe_lib;
}

static value cell(value i)
{
  switch (Long_val(i)) {
  case 0: return (value)&camlCells_observe_lib__cell0;
  case 1: return (value)&camlCells_observe_lib__cell1;
  case 2: return (value)&camlCells_observe_lib__cell2;
  case 3: return (value)&camlCells_observe_lib__cell3;
  default: return (value)&camlCells_observe_lib__cell4;
  }
}

value cells_observe_block_size(value unit)
{
  return Val_long(Wosize_val(block()));
}

value cells_observe_cell_size(value i)
{
  return Val_long(Wosize_val(cell(i)));
}

/* Raw words, so that flat (unboxed) fields can be compared too. */
value cells_observe_block_word(value i)
{
  return caml_copy_int64((int64_t)Field(block(), Long_val(i)));
}

value cells_observe_cell_word(value i, value j)
{
  return caml_copy_int64((int64_t)Field(cell(i), Long_val(j)));
}

/* The contents of a value field of a cell. */
value cells_observe_cell_field(value i, value j)
{
  return Field(cell(i), Long_val(j));
}
