#include <stdint.h>
#include <stdio.h>
#include <unwind.h>

#include <caml/mlvalues.h>

/* What a frame pointer points to. */
struct frame_info {
  struct frame_info *prev; /* the caller's frame pointer */
  void *retaddr;
};

struct walk {
  const struct frame_info *fp; /* frame pointer of the next frame visited */
  void *pc;                    /* its program counter, if known */
  int frame;                   /* its number */
  int last_frame;
  enum { WALKING, AGREE, DISAGREE } result;
};

#define DWARF_RBP 6

static _Unwind_Reason_Code visit(struct _Unwind_Context *ctx, void *data)
{
  struct walk *w = data;
  void *pc = (void *)_Unwind_GetIP(ctx);
  const struct frame_info *fp =
    (const struct frame_info *)_Unwind_GetGR(ctx, DWARF_RBP);

  if (w->pc != NULL && pc != w->pc) {
    printf("frame %d: unwinder is at %p, frame pointers at %p\n",
           w->frame, pc, w->pc);
    w->result = DISAGREE;
  } else if (fp != w->fp) {
    printf("frame %d: unwinder has rbp = %p, frame pointers have %p\n",
           w->frame, (void *)fp, (void *)w->fp);
    w->result = DISAGREE;
  } else if (w->frame == w->last_frame) {
    w->result = AGREE;
  } else {
    w->frame++;
    w->pc = w->fp->retaddr;
    w->fp = w->fp->prev;
    return _URC_NO_REASON;
  }
  return _URC_END_OF_STACK;
}

/* Walk the stack with the unwinder, which finds the %rbp of each frame from
   the unwind information (CFI) of its callee, and check that it is the %rbp
   that the chain of frame pointers gives.  Frame 0 is this function;
   [frames] is the number of frames above it to check. */
value cfi_saved_rbp_frames_agree(value frames)
{
#ifdef ASM_CFI_SUPPORTED
  struct walk w =
    { __builtin_frame_address(0), NULL, 0, Int_val(frames), WALKING };
  _Unwind_Backtrace(visit, &w);
  if (w.result == WALKING)
    printf("unwinder stopped before frame %d\n", w.frame);
  fflush(stdout);
  return Val_bool(w.result == AGREE);
#else
  /* The compiler emits no unwind information. */
  return Val_true;
#endif
}
