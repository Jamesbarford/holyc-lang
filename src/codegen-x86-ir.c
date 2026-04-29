#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "codegen-x86-ir.h"
#include "containers.h"
#include "ir.h"
#include "ir-types.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"
#include "x86.h"

/* These ir.c internals aren't otherwise exposed; declare them locally. */
extern IrCtx *irCtxNew(Cctrl *cc);
extern IrValue *irFnGetVar(IrFunction *func, u32 lvar_id);

/*
 * IR -> x86_64 codegen for slice-0.
 *
 * Strategy: every IR temp gets its own stack slot. Each instruction loads
 * its sources into rax/rcx, performs the op, and writes the result to its
 * tmp's slot. This is naive but correct, matches the existing AST->x86
 * codegen's "%rax holds the current value" feel, and keeps register
 * allocation out of slice-0.
 *
 * Locals and parameters re-use the offsets that asmFunctionInit assigned
 * to AST nodes, so the prologue (and any future fix-ups to it) stays in
 * one place.
 */

typedef struct IrCgCtx {
    Cctrl *cc;
    AoStr *buf;
    IrFunction *func;
    /* Map<u32 IrValue.var.id -> int loff>. loff is sign-extended on read. */
    Map *id_to_loff;
    /* Bytes added to %rsp beyond what asmFunctionInit reserved, for IR tmps. */
    int extra_stack;
    /* Unique block-label prefix per function so multiple slice functions
     * compiled in the same translation unit don't collide. */
    int func_uid;
} IrCgCtx;

static int ircg_func_seq = 0;

static void irCgSetLoff(IrCgCtx *ctx, u32 var_id, int loff) {
    /* mapAddIntOrErr fails on duplicate; use it as an assert for distinctness. */
    int ok = mapAddIntOrErr(ctx->id_to_loff, var_id, (void *)(intptr_t)loff);
    if (!ok) {
        loggerPanic("ir-cg: duplicate loff mapping for var.id=%u\n", var_id);
    }
}

static int irCgGetLoff(IrCgCtx *ctx, IrValue *val) {
    assert(val != NULL);
    if (!mapHasInt(ctx->id_to_loff, val->as.var.id)) {
        loggerPanic("ir-cg: no slot for var.id=%u kind=%d\n",
                    val->as.var.id, val->kind);
    }
    return (int)(intptr_t)mapGetInt(ctx->id_to_loff, val->as.var.id);
}

/* Reserve a fresh stack slot for an SSA temp. */
static void irCgAllocTmp(IrCgCtx *ctx, IrValue *val, int starting_offset) {
    if (!val || val->kind != IR_VAL_TMP) return;
    if (mapHasInt(ctx->id_to_loff, val->as.var.id)) return;
    ctx->extra_stack += 8;
    irCgSetLoff(ctx, val->as.var.id, -(starting_offset + ctx->extra_stack));
}

/* Bind every variable known to the IR (params + alloca'd locals) to its
 * AST-driven loff. Walk the AST function's params and locals lists; the
 * eligibility predicate guarantees they're plain AST_LVAR ints. */
static void irCgBindAstLoffs(IrCgCtx *ctx, Ast *ast_func) {
    if (ast_func->params) {
        for (u64 i = 0; i < ast_func->params->size; ++i) {
            Ast *p = vecGet(Ast *, ast_func->params, i);
            IrValue *iv = irFnGetVar(ctx->func, p->lvar_id);
            if (iv) irCgSetLoff(ctx, iv->as.var.id, p->loff);
        }
    }
    listForEach(ast_func->locals) {
        Ast *l = (Ast *)it->value;
        u32 lid = (l->kind == AST_DEFAULT_PARAM) ? l->declvar->lvar_id
                                                  : l->lvar_id;
        IrValue *iv = irFnGetVar(ctx->func, lid);
        if (iv) irCgSetLoff(ctx, iv->as.var.id, l->loff);
    }
}

/* For everything else (return-value alloca, intermediate tmps), pre-allocate
 * stack slots so the prologue knows the total space to subtract. */
static void irCgAllocAllTmps(IrCgCtx *ctx, int starting_offset) {
    listForEach(ctx->func->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            irCgAllocTmp(ctx, instr->dst, starting_offset);
            irCgAllocTmp(ctx, instr->r1, starting_offset);
            irCgAllocTmp(ctx, instr->r2, starting_offset);
        }
    }
}

/* Load any IR value into the given 64-bit register (rax or rcx). */
static void irCgLoadToReg(IrCgCtx *ctx, IrValue *val, const char *reg) {
    switch (val->kind) {
    case IR_VAL_CONST_INT:
        aoStrCatPrintf(ctx->buf, "movq   $%lld, %%%s\n\t",
                       (long long)val->as._i64, reg);
        return;
    case IR_VAL_TMP:
    case IR_VAL_LOCAL:
    case IR_VAL_PARAM:
        aoStrCatPrintf(ctx->buf, "movq   %d(%%rbp), %%%s\n\t",
                       irCgGetLoff(ctx, val), reg);
        return;
    default:
        loggerPanic("ir-cg: cannot load value of kind %d\n", val->kind);
    }
}

static void irCgStoreReg(IrCgCtx *ctx, IrValue *dst, const char *reg) {
    aoStrCatPrintf(ctx->buf, "movq   %%%s, %d(%%rbp)\n\t",
                   reg, irCgGetLoff(ctx, dst));
}

/* Emit `<setcc> %al; movzbq %al, %rax` for a comparison kind. */
static void irCgEmitSetCC(IrCgCtx *ctx, IrCmpKind cmp) {
    const char *cc = NULL;
    switch (cmp) {
    case IR_CMP_EQ:  cc = "sete";  break;
    case IR_CMP_NE:  cc = "setne"; break;
    case IR_CMP_LT:  cc = "setl";  break;
    case IR_CMP_LE:  cc = "setle"; break;
    case IR_CMP_GT:  cc = "setg";  break;
    case IR_CMP_GE:  cc = "setge"; break;
    case IR_CMP_ULT: cc = "setb";  break;
    case IR_CMP_ULE: cc = "setbe"; break;
    case IR_CMP_UGT: cc = "seta";  break;
    case IR_CMP_UGE: cc = "setae"; break;
    default:
        loggerPanic("ir-cg: unsupported cmp kind %d for slice\n", cmp);
    }
    aoStrCatPrintf(ctx->buf, "%s   %%al\n\tmovzbq %%al, %%rax\n\t", cc);
}

static void irCgBlockLabel(IrCgCtx *ctx, IrBlock *block, char *out, int n) {
    snprintf(out, n, ".LIRBB%d_%u", ctx->func_uid, block->id);
}

/* Emit the body of a single IR instruction. Block boundaries / labels are
 * handled by the caller. */
static void irCgEmitInstr(IrCgCtx *ctx, IrInstr *instr) {
    switch (instr->op) {
    case IR_ALLOCA:
        /* Stack slot already reserved at prologue time. */
        break;

    case IR_LOAD:
        /* dst = *r1. For our slice every "address" is itself a slot, so
         * load = read that slot's value. */
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgStoreReg(ctx, instr->dst, "rax");
        break;

    case IR_STORE:
        /* *dst = r1. Same simplification: dst's slot holds the value. */
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgStoreReg(ctx, instr->dst, "rax");
        break;

    case IR_IADD:
    case IR_ISUB:
    case IR_IMUL:
    case IR_AND:
    case IR_OR:
    case IR_XOR: {
        const char *op = NULL;
        switch (instr->op) {
        case IR_IADD: op = "addq"; break;
        case IR_ISUB: op = "subq"; break;
        case IR_IMUL: op = "imulq"; break;
        case IR_AND:  op = "andq"; break;
        case IR_OR:   op = "orq";  break;
        case IR_XOR:  op = "xorq"; break;
        default: assert(0);
        }
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgLoadToReg(ctx, instr->r2, "rcx");
        aoStrCatPrintf(ctx->buf, "%s   %%rcx, %%rax\n\t", op);
        irCgStoreReg(ctx, instr->dst, "rax");
        break;
    }

    case IR_IDIV:
    case IR_UDIV:
    case IR_IREM:
    case IR_UREM: {
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgLoadToReg(ctx, instr->r2, "rcx");
        if (instr->op == IR_IDIV || instr->op == IR_IREM) {
            aoStrCatPrintf(ctx->buf, "cqto\n\tidivq %%rcx\n\t");
        } else {
            aoStrCatPrintf(ctx->buf, "xorq %%rdx, %%rdx\n\tdivq %%rcx\n\t");
        }
        const char *result = (instr->op == IR_IREM || instr->op == IR_UREM)
                             ? "rdx" : "rax";
        irCgStoreReg(ctx, instr->dst, result);
        break;
    }

    case IR_SHL:
    case IR_SHR:
    case IR_SAR: {
        const char *op = (instr->op == IR_SHL) ? "shlq"
                       : (instr->op == IR_SAR) ? "sarq" : "shrq";
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgLoadToReg(ctx, instr->r2, "rcx");
        aoStrCatPrintf(ctx->buf, "%s   %%cl, %%rax\n\t", op);
        irCgStoreReg(ctx, instr->dst, "rax");
        break;
    }

    case IR_ICMP: {
        irCgLoadToReg(ctx, instr->r1, "rax");
        irCgLoadToReg(ctx, instr->r2, "rcx");
        aoStrCatPrintf(ctx->buf, "cmpq   %%rcx, %%rax\n\t");
        irCgEmitSetCC(ctx, instr->extra.cmp_kind);
        irCgStoreReg(ctx, instr->dst, "rax");
        break;
    }

    case IR_BR: {
        char tlbl[64], flbl[64];
        irCgBlockLabel(ctx, instr->extra.blocks.target_block, tlbl, sizeof(tlbl));
        irCgBlockLabel(ctx, instr->extra.blocks.fallthrough_block, flbl, sizeof(flbl));
        irCgLoadToReg(ctx, instr->dst, "rax");
        aoStrCatPrintf(ctx->buf,
                       "testq  %%rax, %%rax\n\t"
                       "je     %s\n\t"
                       "jmp    %s\n\t",
                       flbl, tlbl);
        break;
    }

    case IR_JMP:
    case IR_LOOP: {
        char tlbl[64];
        irCgBlockLabel(ctx, instr->extra.blocks.target_block, tlbl, sizeof(tlbl));
        aoStrCatPrintf(ctx->buf, "jmp    %s\n\t", tlbl);
        break;
    }

    case IR_RET:
        if (instr->dst) {
            irCgLoadToReg(ctx, instr->dst, "rax");
        }
        aoStrCatPrintf(ctx->buf, "leave\n\tret\n");
        break;

    default:
        loggerPanic("ir-cg: unsupported op %d in slice\n", instr->op);
    }
}

void asmFunctionFromIr(Cctrl *cc, AoStr *buf, Ast *ast_func) {
    assert(ast_func->kind == AST_FUNC);

    /* 1. Standard prologue + AST loff assignment. */
    int stack_used = asmFunctionInit(cc, buf, ast_func);

    /* 2. Lower to IR. The IR arena is reset between programs by the global
     *    irMemoryRelease but persists for the duration of compilation, so it's
     *    fine to leave the structures in place after we emit assembly. */
    IrCtx *ir_ctx = irCtxNew(cc);
    IrFunction *func = irLowerFunction(ir_ctx, ast_func);

    /* 3. Bind known IR values to existing AST loffs. */
    IrCgCtx ctx;
    ctx.cc = cc;
    ctx.buf = buf;
    ctx.func = func;
    ctx.id_to_loff = mapNew(32, &map_uint_to_uint_type);
    ctx.extra_stack = 0;
    ctx.func_uid = ircg_func_seq++;
    irCgBindAstLoffs(&ctx, ast_func);

    /* 4. Reserve slots for return-value alloca and every SSA tmp. */
    irCgAllocAllTmps(&ctx, stack_used);

    /* 5. Extend the stack frame for tmp slots (16-byte aligned). */
    if (ctx.extra_stack > 0) {
        int extra_aligned = align(ctx.extra_stack, 16);
        aoStrCatPrintf(buf, "subq   $%d, %%rsp\n\t", extra_aligned);
    }

    /* 6. Walk blocks in order, emit label + instructions. The very first
     *    block is the entry; we don't strictly need a label on it, but
     *    emitting one keeps the codegen uniform and harmless. */
    listForEach(func->blocks) {
        IrBlock *block = (IrBlock *)it->value;
        char lbl[64];
        irCgBlockLabel(&ctx, block, lbl, sizeof(lbl));
        aoStrCatPrintf(buf, "%s:\n\t", lbl);
        listForEach(block->instructions) {
            IrInstr *instr = (IrInstr *)it->value;
            irCgEmitInstr(&ctx, instr);
        }
    }

    /* 7. Safety net: if the exit block didn't already terminate with `ret`,
     *    emit one. The exit block always ends in IR_RET, so this is a
     *    belt-and-braces match for asmFunction(). */
    if (!asmHasRet(buf)) {
        asmFunctionLeave(buf);
    }

    mapRelease(ctx.id_to_loff);
    free(ir_ctx->prog);
    free(ir_ctx);
}
