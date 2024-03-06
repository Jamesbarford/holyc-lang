#include <stdlib.h>
#include <math.h>
#include <stdint.h>

#include "ast.h"
#include "cctrl.h"
#include "ir.h"
#include "list.h"
#include "util.h"

uint64_t irIeee754Encode(double _f64) {
    if (_f64 == 0.0) return 0;  // Handle zero value explicitly

    // Calculate exponent and adjust fraction
    long double base2_exp = floorl(log2l(fabs(_f64)));
    long double exponet2_removed = ldexpl(_f64, -base2_exp - 1);

    // Initialize fraction and calculate it bit by bit
    uint64_t fraction = 0;
    long double digit = 0.5;  // Start with 1/2
    for (long i = 0; i != 53; i++) {
        if (exponet2_removed >= digit) {
            exponet2_removed -= digit;
            fraction |= 1ULL << (52 - i);
        }
        digit *= 0.5;  // Move to the next digit (1/4, 1/8, ...)
    }

    // Calculate exponent representation
    uint64_t exponent = ((1 << 10) - 1) + base2_exp;

    // Handle sign bit
    uint64_t sign = (_f64 < 0.0) ? 1 : 0;

    // Assemble the IEEE 754 representation
    return (sign << 63) |
           ((exponent & 0x7FF) << 52) |
           (fraction & ~(1ULL << 52));
}

void irEval(Cctrl *cc, Ast *ast);

IrOperand *irOperandNew(int kind) {
    IrOperand *op;
    if ((op = (IrOperand *)calloc(1,sizeof(IrOperand))) == NULL) {
        loggerPanic("OOM while allocating IrOperand\n");
    }
    op->kind = kind;
    return op;
}

IrInstruction *irInstNew(int op) {
    IrInstruction *inst;
    if ((inst = (IrInstruction *)malloc(sizeof(IrInstruction))) == NULL) {
        loggerPanic("OOM while allocating IrInstruction\n");
    }
    inst->arg1 = inst->arg2 = inst->arg3 = NULL;
    inst->op = op;
    return inst;
}

void irAssign(Cctrl *cc, Ast *ast) {
    AstType *type = (AstType*)ast->type;
    IrOperand *op;
    IrInstruction *inst;

    op = irOperandNew(-1);
    inst = irInstNew(IR_SAVE);

    switch (ast->kind) {
        case AST_LITERAL:
            switch (type->kind) {
                case AST_TYPE_INT:
                case AST_TYPE_CHAR: {
                    op->size = type->size;
                    if (!type->issigned) {
                        op->flags |= IR_FLAGS_UNSIGNED;
                    }
                    op->i64 = (long)ast->i64;
                    inst->arg1 = op;
                    break;
                }
                case AST_TYPE_FLOAT: {
                    op->size = type->size;
                    op->f64 = ast->f64;
                    op->i64 = irIeee754Encode(op->f64);
                    op->f64_label = AstMakeLabel();
                    inst->arg1 = op;
                    break;
                }
            }
            break;
        case AST_LVAR:
        case AST_FUNPTR:
            break;
    }
}

void irEvalFunc(Cctrl *cc, Ast *func) {
    /* We're only interested in function's with bodies */
    if (func->kind != AST_FUNC) return;
    Ast *ast;
    /* We're going to use this pointer to collect the function
     * body */
    List *saved = cc->ir_list;
    IrInstruction *inst = irInstNew(IR_FUNCTION);

    inst->fname = func->fname;
    ListAppend(cc->ir_list,inst);

    cc->ir_list = ListNew();

    ListForEach(func->params) {
       /* XXX: Save? */ 
    }

    ListForEach(func->body->stms) {
        ast = (Ast*)it->value;
        irEval(cc,ast);
    }

    inst->body = cc->ir_list;
    /* restore */
    cc->ir_list = saved;
}

void irEval(Cctrl *cc, Ast *ast) {
    switch (ast->kind) {
        case AST_FUNC:    irEvalFunc(cc,ast); break;
    }
}


void irMain(Cctrl *cc) {
    Ast *ast;
    ListForEach(cc->ast_list) {
        ast = (Ast*)it->value;
        irEval(cc,ast);
    }
}
