"Test - #link directive (AOT): ";
#include "testhelper.HC"

/* Two-stage test: build a small C shared library, then compile a
 * second HolyC program whose only route to that library is a
 * `#link "<path>"` directive (no .so on the hcc command line).
 * The name form is exercised with `#link <m>` (libm exists on both
 * macOS and Linux). */

I64 correct = 0;
I64 tests = 3;

/* No quotes or `%` in the C source keeps the shell printf simple. */
if (System("printf 'long HccLinkTestValue(void) { return 42; }' "
           "> /tmp/hcc-60-link.c && "
           "cc -shared -fPIC /tmp/hcc-60-link.c -o /tmp/hcc-60-link.so") == 0) {
  correct++;
}

System("printf '#link \"/tmp/hcc-60-link.so\"\n"
       "extern \"c\" I64 HccLinkTestValue();\n"
       "I32 Main() { if (HccLinkTestValue() == 42) return 0; return 1; }\n'"
       " > /tmp/hcc-60-link-path.HC");
if (System("../../hcc /tmp/hcc-60-link-path.HC -o /tmp/hcc-60-link-path && "
           "/tmp/hcc-60-link-path") == 0) {
  correct++;
}

System("printf '#link <m>\n"
       "extern \"c\" F64 cos(F64 x);\n"
       "I32 Main() { if (cos(0.0) == 1.0) return 0; return 1; }\n'"
       " > /tmp/hcc-60-link-name.HC");
if (System("../../hcc /tmp/hcc-60-link-name.HC -o /tmp/hcc-60-link-name && "
           "/tmp/hcc-60-link-name") == 0) {
  correct++;
}

System("rm -f /tmp/hcc-60-link.c /tmp/hcc-60-link.so"
       " /tmp/hcc-60-link-path.HC /tmp/hcc-60-link-path"
       " /tmp/hcc-60-link-name.HC /tmp/hcc-60-link-name");

PrintResult(correct, tests);
'===\n';
Exit(0);
