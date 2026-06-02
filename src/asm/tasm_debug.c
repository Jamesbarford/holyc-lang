/* Runtime helpers for PRINTREG.  Link this file into any program that
 * uses `PRINTREG` inside an `asm { }` block.
 *
 * Each helper takes the captured register value(s) plus identifying
 * metadata (reg_num, lane count, kind) and prints a human-readable line
 * to stderr.  Names are reconstructed at print time so the asm-block
 * expansion doesn't need an inline string-table.
 *
 * Symbol naming: all helpers are exported without a leading underscore.
 * On Mach-O the C compiler adds the underscore automatically (the asm
 * emitter writes `BL _tasm_print_x`).  On ELF, no prefix is applied.
 *
 * `kind` for the vector helper:
 *   0 = unsigned int lanes
 *   1 = signed   int lanes
 *   2 = float    lanes  (32-bit S form when lane_bits == 32; D when 64)
 *   3 = bfloat16 lanes  (lane_bits must be 16) */

#include <stdint.h>
#include <stdio.h>

void
tasm_print_x(uint64_t v, int reg_num)
{
    fprintf(stderr, "[printreg] x%d = 0x%016llx (%lld)\n", reg_num,
            (unsigned long long)v, (long long)v);
}

void
tasm_print_w(uint32_t v, int reg_num)
{
    fprintf(stderr, "[printreg] w%d = 0x%08x (%d)\n", reg_num, v, (int)v);
}

void
tasm_print_s(float v, int reg_num)
{
    union {
        float f;
        uint32_t u;
    } cvt;
    cvt.f = v;
    fprintf(stderr, "[printreg] s%d = %g (0x%08x)\n", reg_num, (double)v,
            cvt.u);
}

void
tasm_print_d(double v, int reg_num)
{
    union {
        double d;
        uint64_t u;
    } cvt;
    cvt.d = v;
    fprintf(stderr, "[printreg] d%d = %g (0x%016llx)\n", reg_num, v,
            (unsigned long long)cvt.u);
}

/* For vectors, the caller spills the 16-byte Q register to the stack and
 * hands us a pointer to those bytes, plus the arrangement. */
void
tasm_print_v(const void *p, int reg_num, int n_lanes, int lane_bits, int kind)
{
    const unsigned char *bytes = (const unsigned char *)p;
    const char elem = lane_bits == 8 ? 'b' :
            lane_bits == 16          ? 'h' :
            lane_bits == 32          ? 's' :
                                       'd';
    fprintf(stderr, "[printreg] v%d.%d%c = [", reg_num, n_lanes, elem);
    for (int i = 0; i < n_lanes; i++) {
        if (i) fputs(", ", stderr);
        const unsigned char *e = bytes + i * (lane_bits / 8);
        switch (kind) {
        case 0: { /* unsigned int */
            uint64_t v = 0;
            for (int b = 0; b < lane_bits / 8; b++)
                v |= ((uint64_t)e[b]) << (b * 8);
            fprintf(stderr, "%llu", (unsigned long long)v);
            break;
        }
        case 1: { /* signed int (sign-extend) */
            int64_t v = 0;
            for (int b = 0; b < lane_bits / 8; b++)
                v |= ((int64_t)e[b]) << (b * 8);
            int shift = 64 - lane_bits;
            v = (v << shift) >> shift;
            fprintf(stderr, "%lld", (long long)v);
            break;
        }
        case 2: { /* float */
            if (lane_bits == 32) {
                union {
                    uint32_t u;
                    float f;
                } cvt;
                cvt.u = (uint32_t)e[0] | ((uint32_t)e[1] << 8) |
                        ((uint32_t)e[2] << 16) | ((uint32_t)e[3] << 24);
                fprintf(stderr, "%g", (double)cvt.f);
            } else if (lane_bits == 64) {
                union {
                    uint64_t u;
                    double d;
                } cvt;
                cvt.u = 0;
                for (int b = 0; b < 8; b++)
                    cvt.u |= ((uint64_t)e[b]) << (b * 8);
                fprintf(stderr, "%g", cvt.d);
            } else fputs("?fp", stderr);
            break;
        }
        case 3: { /* bfloat16: 16-bit lanes, interpret as upper 16 of FP32 */
            uint32_t u = ((uint32_t)e[0] << 16) | ((uint32_t)e[1] << 24);
            union {
                uint32_t u;
                float f;
            } cvt;
            cvt.u = u;
            fprintf(stderr, "%g", (double)cvt.f);
            break;
        }
        default: fputs("?", stderr); break;
        }
    }
    fputs("]\n", stderr);
}
