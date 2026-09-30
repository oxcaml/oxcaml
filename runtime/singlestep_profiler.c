/* A minimal in-process emulator of Linux perf sampling with LBR branch stacks
   and call chains, for testing FDO profile collection in a
   hardware-independent and deterministic way.

   The primitive

     external trace : append:bool -> string -> (unit -> 'a) -> 'a
       = "caml_singlestep_trace"

   runs a closure with the x86 trap flag set, single-stepping the thread and
   recording every taken branch it executes. Every LBR_DEPTH branches, a sample
   is written to the named file (truncated first unless [append]) in the format
   of "perf script -F period,ip,brstack" for a profile recorded with call
   chains: the period (always 1), the call chain one frame per line (the
   sampled instruction, then the return addresses up the stack), and the branch
   stack, most recent branch first:

              1
     	          401234
     	          4011f0
      0x<from>/0x<to>/P/-/-/0//NON_SPEC_CORRECT_PATH  0x<from>/0x<to>/...

   oxcaml-fdo-decode consumes this via -perf-script-output. Consecutive samples
   overlap by one branch: the decoder only uses the oldest branch of a sample
   to set up its state (the code from its target to the next branch executed
   sequentially), so every branch and every straight-line stretch between two
   branches is counted exactly once. The branches left over at the end of the
   trace form a last, shorter sample.

   Taken branches are recognized by decoding the previously executed instruction
   just enough to detect control transfers: conditional jumps have fixed
   lengths, so their fallthrough address is known, and calls, returns and
   unconditional jumps always transfer. The call chain is a shadow stack: a call
   pushes the return address it stored together with the stack pointer, and a
   frame is popped as soon as the stack pointer rises above its slot, which
   covers returns as well as exceptions unwinding through it.

   Only intended for testing: taking two signals per executed instruction is
   orders of magnitude slower than hardware sampling, and only the calling
   thread is traced. */

#include <stdbool.h>
#define _GNU_SOURCE
#include <fcntl.h>
#include <signal.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <ucontext.h>
#include <unistd.h>

#include "caml/callback.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#define TRAP_FLAG 0x100ll
#define LBR_DEPTH 16
#define MAX_FRAMES 1024

struct entry {
  uint64_t source, target;
};

struct frame {
  uint64_t sp, return_address;
};

static volatile int tracing = 0;
static const char *volatile failure = NULL;
static int out_fd = -1;
static uint64_t prev_ip;
static int num_entries;
static struct entry entries[LBR_DEPTH];
static int num_frames;
static struct frame frames[MAX_FRAMES];

enum transfer { SEQUENTIAL, JUMP, CALL };

/* How execution went from [prev] (the previously executed instruction) to
   [cur], by decoding just enough of the instruction at [prev]. Anything
   unrecognized is treated as sequential execution. */
static enum transfer transfer_kind(uint64_t prev, uint64_t cur)
{
  const uint8_t *p = (const uint8_t *)prev;
  uint8_t b0 = p[0];
  /* jcc rel8 */
  if (b0 >= 0x70 && b0 <= 0x7f) return cur != prev + 2 ? JUMP : SEQUENTIAL;
  /* jcc rel32 */
  if (b0 == 0x0f && p[1] >= 0x80 && p[1] <= 0x8f)
    return cur != prev + 6 ? JUMP : SEQUENTIAL;
  /* loop*, jrcxz */
  if (b0 >= 0xe0 && b0 <= 0xe3) return cur != prev + 2 ? JUMP : SEQUENTIAL;
  /* call rel */
  if (b0 == 0xe8) return CALL;
  /* jmp rel */
  if (b0 == 0xe9 || b0 == 0xeb) return JUMP;
  /* ret */
  if (b0 == 0xc2 || b0 == 0xc3) return JUMP;
  /* REX prefix */
  if (b0 >= 0x40 && b0 <= 0x4f) b0 = *++p;
  /* call/jmp *r/m */
  if (b0 == 0xff) {
    uint8_t reg = (p[1] >> 3) & 7;
    if (reg == 2 || reg == 3) return CALL;
    if (reg == 4 || reg == 5) return JUMP;
  }
  return SEQUENTIAL;
}

static char *append_hex(char *pos, uint64_t v)
{
  char digits[16];
  int n = 0;
  if (v == 0) {
    *pos++ = '0';
    return pos;
  }
  while (v != 0) {
    digits[n++] = "0123456789abcdef"[v & 0xf];
    v >>= 4;
  }
  while (n > 0) *pos++ = digits[--n];
  return pos;
}

/* Right-aligned in a field of [width], as perf prints call chain frames. */
static char *append_hex_padded(char *pos, uint64_t v, int width)
{
  char digits[16];
  int n = 0;
  do {
    digits[n++] = "0123456789abcdef"[v & 0xf];
    v >>= 4;
  } while (v != 0);
  while (width-- > n) *pos++ = ' ';
  while (n > 0) *pos++ = digits[--n];
  return pos;
}

static void fail(const char *message)
{
  failure = message;
  tracing = 0;
}

/* Async-signal-safe: hand-rolled formatting and a plain write. */
static void write_sample(uint64_t ip)
{
  static char buf[64 + 18 * (MAX_FRAMES + 1) + 80 * LBR_DEPTH];
  char *pos = buf;
  int i;
  memcpy(pos, "         1 \n", 12);
  pos += 12;
  *pos++ = '\t';
  pos = append_hex_padded(pos, ip, 16);
  *pos++ = '\n';
  for (i = num_frames - 1; i >= 0; i--) {
    *pos++ = '\t';
    pos = append_hex_padded(pos, frames[i].return_address, 16);
    *pos++ = '\n';
  }
  for (i = num_entries - 1; i >= 0; i--) { /* most recent first */
    memcpy(pos, " 0x", 3);
    pos += 3;
    pos = append_hex(pos, entries[i].source);
    memcpy(pos, "/0x", 3);
    pos += 3;
    pos = append_hex(pos, entries[i].target);
    memcpy(pos, "/P/-/-/0//NON_SPEC_CORRECT_PATH ", 32);
    pos += 32;
  }
  *pos++ = '\n';
  /* The next sample starts with this one's most recent branch. */
  entries[0] = entries[num_entries - 1];
  num_entries = 1;
  {
    const char *p = buf;
    size_t len = pos - buf;
    while (len > 0) {
      ssize_t n = write(out_fd, p, len);
      if (n < 0) {
        fail("singlestep_trace: cannot write the trace");
        return;
      }
      p += n;
      len -= (size_t)n;
    }
  }
}

/* Runs after every instruction while the trap flag is set. The kernel clears
   the flag for the handler itself and restores it on return, so the handler's
   own instructions are not traced. */
static void trap_handler(int sig, siginfo_t *info, void *uctx)
{
  ucontext_t *uc = (ucontext_t *)uctx;
  uint64_t ip = (uint64_t)uc->uc_mcontext.gregs[REG_RIP];
  uint64_t sp = (uint64_t)uc->uc_mcontext.gregs[REG_RSP];
  (void)sig;
  (void)info;
  if (!tracing) {
    uc->uc_mcontext.gregs[REG_EFL] &= ~TRAP_FLAG;
    return;
  }
  enum transfer kind = prev_ip == 0 ? SEQUENTIAL : transfer_kind(prev_ip, ip);
  /* Frames whose return address slot has been popped, by a return or by
     unwinding. */
  while (num_frames > 0 && frames[num_frames - 1].sp < sp) num_frames--;
  if (kind == CALL) {
    if (num_frames == MAX_FRAMES) {
      fail("singlestep_trace: call stack too deep");
      uc->uc_mcontext.gregs[REG_EFL] &= ~TRAP_FLAG;
      return;
    }
    frames[num_frames].sp = sp;
    frames[num_frames].return_address = *(uint64_t *)sp;
    num_frames++;
  }
  if (kind != SEQUENTIAL) {
    entries[num_entries].source = prev_ip;
    entries[num_entries].target = ip;
    num_entries++;
  }
  prev_ip = ip;
  if (num_entries == LBR_DEPTH) {
    write_sample(ip);
    if (!tracing) uc->uc_mcontext.gregs[REG_EFL] &= ~TRAP_FLAG;
  }
}

/* Sets the trap flag; the first trap is taken after the following
   instruction. Skips the red zone in case this gets inlined into a leaf. */
static void set_trap_flag(void)
{
  __asm__ volatile("lea -128(%%rsp), %%rsp\n\t"
                   "pushfq\n\t"
                   "orq $0x100, (%%rsp)\n\t"
                   "popfq\n\t"
                   "lea 128(%%rsp), %%rsp" ::: "memory", "cc");
}

CAMLprim value caml_singlestep_trace(value append, value path, value closure)
{
  CAMLparam3(append, path, closure);
  CAMLlocal1(result);
  struct sigaction sa;
  if (tracing) caml_invalid_argument("singlestep_trace: already tracing");
  out_fd = open(String_val(path),
                O_WRONLY | O_CREAT | (Bool_val(append) ? O_APPEND : O_TRUNC),
                0644);
  if (out_fd < 0) caml_failwith("singlestep_trace: cannot open the output file");
  memset(&sa, 0, sizeof(sa));
  sigemptyset(&sa.sa_mask);
  sa.sa_flags = SA_SIGINFO | SA_RESTART | SA_ONSTACK;
  sa.sa_sigaction = trap_handler;
  sigaction(SIGTRAP, &sa, NULL);
  prev_ip = 0;
  num_entries = 0;
  num_frames = 0;
  failure = NULL;
  tracing = 1;
  set_trap_flag();
  result = caml_callback_exn(closure, Val_unit);
  /* The next trap clears the flag. */
  tracing = 0;
  if (failure == NULL && num_entries > 1) write_sample(prev_ip);
  close(out_fd);
  out_fd = -1;
  if (failure != NULL) caml_failwith((const char *)failure);
  if (Is_exception_result(result)) caml_raise(Extract_exception(result));
  CAMLreturn(result);
}
