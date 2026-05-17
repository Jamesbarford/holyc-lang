/* TempleOS-style try/catch/throw runtime.
 *
 * The compiler lowers `try { body } catch <stmt>` into:
 *   - alloca CatchFrame on the stack
 *   - call HCC_TryEnter(&frame)  ; setjmp under the hood
 *   - if return value == 0:  run body, then HCC_TryLeave
 *   - else (got here via longjmp): pop frame, run handler
 *
 * `throw(value)` becomes `HCC_Throw(value)`; the runtime stashes the
 * u64 in Fs->except_ch and longjmps to the most recent catch frame.
 *
 * The compiler reserves 256 bytes for each CatchFrame slot, which is
 * larger than every platform jmp_buf we target plus the chain pointer.
 * The HolyC `class CatchFrame` below pads to exactly 256 bytes so the
 * layout the runtime sees matches what the compiler allocated.
 *
 * Single-thread for now: Fs is a process-wide global. Multi-thread
 * support requires a pthread_key_t-backed Fs (future work). */
#include "./defs.HH"

public extern "c" I32 setjmp(U0 *env);
public extern "c" U0 longjmp(U0 *env, I32 val);

/* Compiler reserves 256 bytes per CatchFrame. We split that into a
 * 248-byte jmp_buf storage region (setjmp owns the format) plus the
 * 8-byte chain pointer the runtime walks. */
#define __JMP_BUF_BYTES__ 248

class CatchFrame;
class CatchFrame
{
  I8 jb[__JMP_BUF_BYTES__];
  CatchFrame *prev;
};

class HCFs
{
  CatchFrame *catch_top;  /* head of the active-try-frames chain */
  U64 except_ch;          /* value of the most recent throw */
};

/* Canonical storage + global pointer. tos.HH declares `Fs` as
 * `extern HCFs *Fs;` so user TUs reference rather than redefine. We
 * initialise lazily on first try, since HolyC lifts non-constant
 * global initialisers into a hidden Main wrapper that would conflict
 * with the user's Main. */
static HCFs _fs_storage;
public HCFs *Fs;

/* Push the frame onto the catch-stack. The setjmp call itself MUST
 * stay inline in the user function (it captures the caller's frame,
 * and longjmp to a returned function's frame is UB), so the chain
 * mutation is the only thing we delegate to a helper. */
public U0 HCC_PushFrame(CatchFrame *f)
{
  if (!Fs) Fs = &_fs_storage;
  f->prev = Fs->catch_top;
  Fs->catch_top = f;
}

public U0 HCC_TryLeave(CatchFrame *f)
{
  Fs->catch_top = f->prev;
}

public U0 HCC_Throw(U64 v)
{
  /* Fs is lazily initialised by HCC_PushFrame on the first `try`. A
   * throw before any `try` would otherwise null-deref on the assign
   * below. No prior try also means no catch frame, so go straight to
   * the uncaught path. */
  if (!Fs) {
    "uncaught throw: %X (no active try)\n", v;
    Exit(1);
  }
  Fs->except_ch = v;
  auto t = Fs->catch_top;
  if (!t) {
    "uncaught throw: %X\n", v;
    Exit(1);
  }
  longjmp(t->jb, 1);
}
