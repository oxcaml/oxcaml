#include <signal.h>
#include <unwind.h>

#include <caml/mlvalues.h>

value out_of_line_call_signal(value unit)
{
  raise(SIGUSR1);
  return Val_unit;
}

struct walk {
  value functions; /* closures of the functions to find, innermost first */
  mlsize_t found;
};

static _Unwind_Reason_Code visit(struct _Unwind_Context *ctx, void *data)
{
  struct walk *w = data;
  void *fn = _Unwind_FindEnclosingFunction((void *)_Unwind_GetIP(ctx));

  if (fn == (void *)Code_val(Field(w->functions, w->found))) {
    w->found++;
    return w->found == Wosize_val(w->functions)
      ? _URC_END_OF_STACK : _URC_NO_REASON;
  }
  /* The frames of the functions must follow one another. */
  return w->found == 0 ? _URC_NO_REASON : _URC_END_OF_STACK;
}

/* Walk the stack with the unwinder, which follows the unwind information
   (CFI), and check that it reaches the first of the given functions and goes
   from each of them straight to the next. */
value out_of_line_call_unwinds_through(value functions)
{
#ifdef ASM_CFI_SUPPORTED
  struct walk w = { functions, 0 };
  _Unwind_Backtrace(visit, &w);
  return Val_bool(w.found == Wosize_val(functions));
#else
  /* The compiler emits no unwind information. */
  return Val_true;
#endif
}
