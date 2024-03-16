#include <assert.h>
#include <stdlib.h>
#include <math.h>
#include <stdint.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "dict.h"
#include "ir.h"
#include "lexer.h"
#include "list.h"
#include "util.h"


#define irGetRegister() register_num++
#define irResetRegister() register_num = 1

void irEval(Cctrl *cc, Ast *ast);
void irEvalIfCond(Cctrl *cc, Ast *ast, int invert);
void irLoadGlobal(Cctrl *cc, Ast *ast);
void irLoadLocal(Cctrl *cc, Ast *ast);
void irDecl(Cctrl *cc, Ast *ast);
void irCondLogicalOr(Cctrl *cc, Ast *ast);
void irCondLogicalAnd(Cctrl *cc, Ast *ast);
int irJumpConditional(Cctrl *cc, Ast *ast);
int irJumpConditionalInverted(Cctrl *cc, Ast *ast);

static long register_num = 0;
static int ir_has_initialisers = 0;

char *irNormaliseFunctionName(char *fname) {
    aoStr *newfn = AstNormaliseFunctionName(fname);
    if (!strncasecmp(fname, "Main", 4)) {
        if (ir_has_initialisers) {
            aoStrCatPrintf(newfn,"Fn");
        } else {
            aoStrToLowerCase(newfn);
        }
    }
    return aoStrMove(newfn);
}

char *irALUtoString(int alu) {
    switch (alu) {
        case IR_ADD:         return "ADD";
        case IR_SUB:         return "SUB";
        case IR_MUL:         return "MUL";
        case IR_XOR:         return "XOR";
        case IR_AND:         return "AND";
        case IR_OR:          return "OR";
        case IR_SHL:         return "SHL";
        case IR_SHR:         return "SHR";
        case IR_DIV:         return "DIV";
        case IR_MOD:         return "MOD";

        case IR_CMP:         return "CMP";

        case IR_EQU_EQU: return "EQU";
        case IR_NOT_EQU: return "NEQU";
        case IR_LT:      return "LT";     
        case IR_LTE:     return "LTE";  
        case IR_GT:      return "GT";
        case IR_GTE:     return "GTE";

        case IR_JMP:          return "JMP"; 
        case IR_JMP_LT:       return "JMP_LT"; 
        case IR_JMP_LTE:      return "JMP_LTE"; 
        case IR_JMP_GT:       return "JMP_GT"; 
        case IR_JMP_GTE:      return "JMP_GTE"; 
        case IR_JMP_EQ:       return "JMP_EQ"; 
        case IR_JMP_NOT_EQ:   return "JMP_NOT_EQ"; 
        case IR_JMP_NOT_ZERO: return "JMP_NOT_ZERO"; 
        case IR_JMP_ZERO:     return "JMP_ZERO"; 

        default: loggerPanic("Invalid alu %d\n",alu);
    }
}

char *irOpKindToString(IrOperand *op) {
    if (!op) {
        return "(null)";
    }
    int irkind = op->kind;
    switch (irkind) {
        case IR_LOAD:        return "IR_LOAD";
        case IR_LOAD_GLOBAL: return "IR_LOAD_GLOBAL";
        case IR_SAVE:        return "IR_SAVE";
        case IR_FUNCTION:    return "@func";
        case IR_FLOAT:       return "@f64";
        case IR_CHAR: {
            if (op->flags & IR_FLAGS_UNSIGNED) return "@u8";
            else                               return "@i8";
        }
        case IR_INT: {
            switch (op->size) {
                case 8: {
                    if (op->flags & IR_FLAGS_UNSIGNED) return "@u64";
                    else                               return "@i64";
                }
                case 4:
                    if (op->flags & IR_FLAGS_UNSIGNED) return "@u32";
                    else                               return "@i32";
                case 2:
                    if (op->flags & IR_FLAGS_UNSIGNED) return "@u16";
                    else                               return "@i16";
                case 1:
                    if (op->flags & IR_FLAGS_UNSIGNED) return "@u8";
                    else                               return "@i8";
                default:
                    loggerPanic("Invalid integer size: %d\n", op->size);
            }
        }
        case IR_STRING:      return "@string";
        case IR_ARRAY:       return "@array";
        case IR_VOID:        return "@void";
        case IR_POINTER:     return "@ptr";
        case IR_CLASS:       return "@class";

        case IR_ADD:         return "ADD";
        case IR_SUB:         return "SUB";
        case IR_MUL:         return "MUL";
        case IR_XOR:         return "XOR";
        case IR_AND:         return "AND";
        case IR_OR:          return "OR";
        case IR_SHL:         return "SHL";
        case IR_SHR:         return "SHR";
        case IR_DIV:         return "DIV";
        case IR_MOD:         return "MOD";
        
        case IR_CMP:         return "CMP";

        case IR_EQU_EQU:     return "EQU";
        case IR_NOT_EQU:     return "NEQU";
        case IR_LT:          return "LT";     
        case IR_LTE:         return "LTE";  
        case IR_GT:          return "GT";
        case IR_GTE:         return "GTE";

        case IR_JMP_LT:       return "JMP_LT"; 
        case IR_JMP_LTE:      return "JMP_LTE"; 
        case IR_JMP_GT:       return "JMP_GT"; 
        case IR_JMP_GTE:      return "JMP_GTE"; 
        case IR_JMP_EQ:       return "JMP_EQ"; 
        case IR_JMP_NOT_EQ:   return "JMP_NOT_EQ"; 
        case IR_JMP_NOT_ZERO: return "JMP_NOT_ZERO"; 
        case IR_JMP_ZERO:     return "JMP_ZERO"; 

        case IR_LABEL:        return "LABEL";

        default: loggerPanic("Could not convert ir kind %d "
                         "to a string\n", irkind);
    }
}

char *irRegisterOrValueToString(IrOperand *op) {
    aoStr *str = aoStrNew();
    char *op_str;

    switch (op->kind) {
        case IR_INT: {
            op_str = irOpKindToString(op);
            if (op->flags & IR_FLAGS_INTERMEDIATE) {
                aoStrCatPrintf(str,"%s %d", op_str, op->i64);
            } else {
                aoStrCatPrintf(str,"%-4s Rx%02d", op_str, op->reg);
            }
            break;
        }
        case IR_LOAD: {
            op_str = irOpKindToString(op);
            if (op->flags & IR_FLAGS_INTERMEDIATE) {
                aoStrCatPrintf(str,"%d", op->i64);
            } else {
                aoStrCatPrintf(str,"Rx%02d", op->reg);
            }
            break;
        }
        case IR_SAVE: {
            op_str = irOpKindToString(op);
            aoStrCatPrintf(str,"Rx%02d", op->reg);
            break;
        }

        default:
            loggerPanic("Unhandled kind: %s\n", irOpKindToString(op));

    }

    return aoStrMove(str);
}

void irToString(aoStr *str, IrInstruction *inst) {
    IrOperand *arg1;
    char *alu,*s_arg1,*s_arg2,*s_arg3;
    switch (inst->op) {
        case IR_LOAD: {
            arg1 = inst->arg1;
            aoStrCatPrintf(str,"\tLOAD\tRx%02d ", arg1->reg);
            if (arg1->flags & IR_FLAGS_INTERMEDIATE) {
                aoStrCatPrintf(str,"%s %d", irOpKindToString(arg1), arg1->i64);
            }
            aoStrPutChar(str,'\n');
            break;
        }

        case IR_LOAD_GLOBAL: aoStrCatPrintf(str,"\tLOAD_GLOBAL\t%sRx%d\n",
                                     irOpKindToString(inst->arg1), inst->arg1->reg); break;

        case IR_SAVE: {
            aoStrCatPrintf(str,"\tSAVE\t%sRx%d\n",
                irOpKindToString(inst->arg1), inst->arg1->reg);
            break;
        }
        case IR_FUNCTION: {
            aoStrCatPrintf(str, "%s\n", inst->fname->data);
            ListForEach(inst->body) {
                irToString(str,(IrInstruction *)it->value);
            }
            break;
        }

        case IR_CMP:
        case IR_ADD:
        case IR_SUB:
        case IR_MUL:
        case IR_XOR:
        case IR_AND:
        case IR_OR:
        case IR_SHL:
        case IR_SHR:
        case IR_DIV:
        case IR_MOD:
        case IR_EQU_EQU:
        case IR_NOT_EQU:
        case IR_LT:
        case IR_LTE:
        case IR_GT:
        case IR_GTE:
            alu = irALUtoString(inst->op);
            s_arg1 = irRegisterOrValueToString(inst->arg1);
            s_arg2 = irRegisterOrValueToString(inst->arg2);
            s_arg3 = irRegisterOrValueToString(inst->arg3);
            aoStrCatPrintf(str,"\t%-4s\t%s %s %s\n",alu, s_arg1, s_arg2,s_arg3);
            break;

        case IR_CMP_JMP:
            alu = "CMP";
            s_arg1 = irRegisterOrValueToString(inst->arg1);
            s_arg2 = irRegisterOrValueToString(inst->arg2);
            aoStrCatPrintf(str,"\t%-4s\t%s %s\n",alu, s_arg1, s_arg2);
            break;

        case IR_JMP_LT:
        case IR_JMP_LTE:
        case IR_JMP_GT:
        case IR_JMP_GTE:
        case IR_JMP_EQ:
        case IR_JMP_NOT_EQ:
        case IR_JMP_NOT_ZERO:
        case IR_JMP_ZERO:
        case IR_JMP:
            alu = irALUtoString(inst->op);
            s_arg1 = inst->arg1->label->data;
            aoStrCatPrintf(str,"\t%-4s\t%s\n",alu, s_arg1);
            break;
        
        case IR_LABEL:
            aoStrCatPrintf(str,"%s:\n",inst->arg1->label->data);
            break;

        default: loggerPanic("Could not convert ir kind %d "
                         "to a string\n", inst->op);
    }
}

int irInvertComparison(Ast *ast) {
    switch (ast->kind) {
        case '<':            return IR_GTE;
        case '>':            return IR_LTE;
        case TK_GREATER_EQU: return IR_LT;
        case TK_LESS_EQU:    return IR_GT;
        case TK_EQU_EQU:     return IR_NOT_EQU;
        case TK_NOT_EQU:     return IR_EQU_EQU;
        default: loggerPanic("Invalid comparison operator: %s\n",
                         AstKindToString(ast->kind));
    }
}

int irAstTypeKindToIr(int kind) {
    switch (kind) {
        case AST_TYPE_VOID:    return IR_VOID; 
        case AST_TYPE_INT:     return IR_INT;
        case AST_TYPE_FLOAT:   return IR_FLOAT; 
        case AST_TYPE_CHAR:    return IR_CHAR; 
        case AST_TYPE_ARRAY:   return IR_ARRAY; 
        case AST_TYPE_POINTER: return IR_POINTER; 
        case AST_TYPE_FUNC:    return IR_FUNCTION; 
        case AST_TYPE_CLASS:   return IR_CLASS; 
        default:
            loggerPanic("%s cannot be converted to ir\n",
                         AstKindToString(kind));
    }
}

uint64_t irIeee754Encode(double _f64) {
    if (_f64 == 0.0) return 0;  // Handle zero value explicitly

    // Calculate exponent and adjust fraction
    long double base2_exp = floorl(log2l(fabs(_f64)));
    long double exponet2_removed = ldexpl(_f64, -base2_exp - 1);

    // Initialize fraction and calculate it bit by bit
    uint64_t fraction = 0;
    long double digit = 0.5;  // Start with 1/2
    for (long i = 0; i != 53; i++) {
        /* x goes to the next row */
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

int irIsLogical(Ast *ast) {
    return ast->kind == TK_AND_AND || 
           ast->kind == TK_OR_OR;
}

int irLeftOrRightIsFloat(Ast *ast) {
    return AstIsFloatType(ast->left->type) ||
           AstIsFloatType(ast->right->type);
}

int irLeftOrRightIsUnsigned(Ast *ast) {
    return !ast->left->type->issigned ||
           !ast->right->type->issigned;
}

IrOperand *irOperandNew(int kind) {
    IrOperand *op;
    if ((op = (IrOperand *)calloc(1,sizeof(IrOperand))) == NULL) {
        loggerPanic("OOM while allocating IrOperand\n");
    }
    op->kind = kind;
    return op;
}

IrOperand *irOpLabelNew(aoStr *label) {
    IrOperand *op = irOperandNew(IR_LABEL);
    op->label = label;
    return op;
}

IrOperand *irIntermediateNew(Ast *ast) {
    IrOperand *op = irOperandNew(-1);
    AstType *type = ast->type;

    switch (ast->type->kind) {
        case AST_TYPE_INT:
        case AST_TYPE_CHAR: {
            op->size = type->size;
            op->kind = irAstTypeKindToIr(ast->type->kind);
            if (!type->issigned) {
                op->flags |= IR_FLAGS_UNSIGNED;
            }
            op->i64 = (long)ast->i64;
            break;
        }
        case AST_TYPE_FLOAT: {
            op->size = type->size;
            op->kind = IR_FLOAT;
            op->f64 = ast->f64;
            op->i64 = irIeee754Encode(op->f64);
            op->f64_label = AstMakeLabel();
            break;
        }
        default:
            loggerPanic("Cannot load intermediate value: %s\n",
                    AstToString(ast));
    }
    op->flags |= IR_FLAGS_INTERMEDIATE;
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

/* LOAD start =============================================================== */
void irLoadString(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_LOAD);
    IrOperand *op = irOperandNew(IR_STRING);
    /* Set the string */
    op->sval = ast->sval;
    op->slabel = ast->slabel;
    inst->arg1 = op;
    ListAppend(cc->ir_func->body,inst);
}

void irLoadLocal(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_LOAD);
    /* XXX: @ARM
     * x86 assumption that a 'MOV' should be adequate for 
     * pointers etc... it looks like LDR should be able to
     * handle it. */
    IrOperand *op = irOperandNew(irAstTypeKindToIr(ast->type->kind));
    op->reg = CctrlGetTmpRegister(cc,ast->lname->data,ast->lname->len);
    ast->reg = op->reg;

    op->size = ast->type->size;
    if (!ast->type->issigned) {
        op->flags |= IR_FLAGS_UNSIGNED;
    }
    inst->arg1 = op;
    ListAppend(cc->ir_func->body,inst);
}

void irLoadGlobal(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_LOAD_GLOBAL);
    IrOperand *op = irOperandNew(-1);
    op->gname = ast->gname;
    op->kind = irAstTypeKindToIr(ast->kind);
    inst->arg1 = op;
    ListAppend(cc->ir_func->body,inst);
}

void irLoadIntermediate(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_LOAD);
    inst->arg1 = irIntermediateNew(ast);
    inst->arg1->reg = irGetRegister();
    ListAppend(cc->ir_func->body,inst);
}
/* LOAD end ================================================================= */

void irSaveLocal(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_SAVE);
    IrOperand *op = irOperandNew(-1);
    int ir_kind = irAstTypeKindToIr(ast->type->kind);
    op->kind = ir_kind;
    if (!ast->type->issigned) {
        op->flags |= IR_FLAGS_UNSIGNED;
    }
    op->reg = irGetRegister();
    op->size = ast->type->size;
    inst->arg1 = op;
    ListAppend(cc->ir_func->body,inst);
}

/**
 * XXX: Array handling, this doesn't seem right. Feels like there 
 * should be an offset? Or maybe pass the ast to the assembly
 * to handle?
 */
void irArrayInit(Cctrl *cc, Ast *ast, AstType *type) {
    loggerPanic("irArrayInit not implemented\n");
    ListForEach(ast->arrayinit) {
        Ast *tmp = (Ast *)it->value;
        if (tmp->kind == AST_ARRAY_INIT) {
            irArrayInit(cc,tmp,type->ptr);
            continue;
        }
        switch (tmp->type->kind) {
            case AST_TYPE_CHAR:
            case AST_TYPE_INT: irSaveLocal(cc,tmp); break;
            default:
                irEval(cc,tmp);
                break;
        }
    }
}

void irDecl(Cctrl *cc, Ast *ast) {
    if (!ast->declinit) return;

    if (ast->declinit->kind == AST_ARRAY_INIT) {
        irArrayInit(cc,ast->declinit,ast->declvar->type);
    } else if (ast->declvar->type->kind == AST_TYPE_ARRAY) {
        assert(ast->declinit->kind == AST_STRING);
        /*XXX: @Unimplmented */
        loggerPanic("Unimplmented  ast->declvar->type->kind == AST_TYPE_ARRAY\n");
    } else if (ast->declinit->kind == AST_STRING) {
        /*XXX: @Unimplmented */
        loggerPanic("Unimplmented ast->declinit->kind == AST_STRING\n");
    } else if (ast->declinit->kind == AST_FUNC) {
        /*XXX: @Unimplmented */
        loggerPanic("Unimplmented ast->declinit->kind == AST_FUNC\n");
    } else {
        CctrlSetTmpRegister(cc,ast->declvar->lname->data,register_num);
        irEval(cc,ast->declinit);
    }
}

void irFloatToInt(Cctrl *cc, Ast *ast) {
    if (ast->type->kind == AST_TYPE_FLOAT) {
        IrInstruction *inst = irInstNew(IR_FLOAT_TO_INT);
        ListAppend(cc->ir_func->body,inst);
    }
}

void irIntToFloat(Cctrl *cc, Ast *ast) {
    if (ast->type->kind == AST_TYPE_INT) {
        IrInstruction *inst = irInstNew(IR_INT_TO_FLOAT);
        ListAppend(cc->ir_func->body,inst);
    }
}

void irArithmetic(Cctrl *cc, Ast *ast, int reverse) {
    Ast *LHS = ast->left;
    Ast *RHS = ast->right;
    Ast *tmp;
    IrOperand *arg2,*arg3;
    int irop = -1;
    unsigned long flags = 0;

    if (irLeftOrRightIsFloat(ast))    flags |= IR_FLAGS_FLOAT;
    if (irLeftOrRightIsUnsigned(ast)) flags |= IR_FLAGS_UNSIGNED;

    switch (ast->kind) {
    case '+':    irop = IR_ADD; break;
    case '-':    irop = IR_SUB; break;
    case '*':    irop = IR_MUL; break;
    case '^':    irop = IR_XOR; break;
    case '&':    irop = IR_AND; break;
    case '|':    irop = IR_OR;  break;
    case TK_SHL: irop = IR_SHL; break;
    case TK_SHR: irop = IR_SHR; break;
    case '/':    irop = IR_DIV; break;
    case '%':    irop = IR_MOD; break;
    default:
        loggerPanic("Unknown type: %s\n", AstKindToString(ast->kind));
    }

    IrInstruction *inst = irInstNew(irop);

    if (flags & IR_FLAGS_FLOAT) {
        if (irop != IR_ADD &&
            irop != IR_DIV && 
            irop != IR_MUL && 
            irop != IR_SUB)
        {
            loggerPanic("Invalid float arithmetic operator: %s\n",
                    AstKindToString(ast->kind));
        } 
    }

    if (reverse) {
        tmp = LHS;
        LHS = RHS;
        RHS = tmp;
    }

    if (LHS->kind == AST_LITERAL) {
        arg2 = irIntermediateNew(LHS);
    } else {
        irEval(cc,LHS);
        arg2 = irOperandNew(IR_LOAD);
        if (LHS->reg != 0) arg2->reg = LHS->reg;
        else               arg2->reg = register_num-1;
    }

    if (RHS->kind == AST_LITERAL) {
        arg3 = irIntermediateNew(RHS);
    } else {
        irEval(cc,RHS);
        arg3 = irOperandNew(IR_LOAD);
        if (RHS->reg != 0) arg3->reg = RHS->reg;
        else               arg3->reg = register_num-1;
    }

    inst->flags = flags;
    inst->arg1 = irOperandNew(IR_SAVE);
    inst->arg1->reg = irGetRegister();
    inst->arg2 = arg2;
    inst->arg3 = arg3;
    ListAppend(cc->ir_func->body,inst);
}

void irCompare(Cctrl *cc, Ast *ast, int cmp) {
    Ast *LHS = ast->left;
    Ast *RHS = ast->right;
    IrOperand *arg2,*arg3;
    IrInstruction *inst = irInstNew(cmp);
    unsigned long flags = 0;

    if (irLeftOrRightIsFloat(ast))    flags |= IR_FLAGS_FLOAT;
    if (irLeftOrRightIsUnsigned(ast)) flags |= IR_FLAGS_UNSIGNED;

    if (LHS->kind == AST_LITERAL) {
        arg2 = irIntermediateNew(LHS);
    } else {
        irEval(cc,LHS);
        arg2 = irOperandNew(IR_LOAD);
        if (LHS->reg != 0) arg2->reg = LHS->reg;
        else               arg2->reg = register_num-1;
    }

    if (RHS->kind == AST_LITERAL) {
        arg3 = irIntermediateNew(RHS);
    } else {
        irEval(cc,RHS);
        arg3 = irOperandNew(IR_LOAD);
        if (RHS->reg != 0) arg3->reg = RHS->reg;
        else               arg3->reg = register_num-1;
    }

    inst->flags = flags;
    inst->arg1 = irOperandNew(IR_SAVE);
    inst->arg1->reg = irGetRegister();
    inst->arg2 = arg2;
    inst->arg3 = arg3;
    ListAppend(cc->ir_func->body,inst);
}

void irCompareAndJump(Cctrl *cc, Ast *ast, aoStr *label, int invert) {
    IrInstruction *inst = irInstNew(IR_CMP_JMP);
    unsigned long flags = 0;
    int jmp;
    Ast *conds[2] = {ast->left,ast->right};
    IrOperand *ops[2];

    if (invert) jmp = irJumpConditionalInverted(cc,ast);
    else        jmp = irJumpConditional(cc,ast);

    if (irLeftOrRightIsFloat(ast))    flags |= IR_FLAGS_FLOAT;
    if (irLeftOrRightIsUnsigned(ast)) flags |= IR_FLAGS_UNSIGNED;

    /* evaluate left and right */
    for (int i = 0; i < static_size(conds); ++i) {
        Ast *cond = conds[i];
        if (cond->kind == AST_LITERAL) {
            ops[i] = irIntermediateNew(cond);
        } else {
            irEval(cc,cond);
            ops[i] = irOperandNew(IR_LOAD);
            if (cond->reg != 0) ops[i]->reg = cond->reg;
            else                ops[i]->reg = register_num-1;
        }
    }

    inst->flags = flags;
    inst->arg1 = ops[0];
    inst->arg2 = ops[1];
    ListAppend(cc->ir_func->body,inst);

    inst = irInstNew(jmp);
    inst->arg1 = irOpLabelNew(label);
    ListAppend(cc->ir_func->body,inst);
}

int opIsCompoundAssign(Ast *ast) {
    switch (ast->kind) {
    case '-':
    case '+':
    case '*':
    case '/':
    case TK_SHR:
    case TK_SHL:
    case '|':
    case '&':
    case '%':
        return 1;
    }
    return 0;
}

int irShouldReverseMaths(Ast *RHS) {
    return opIsCompoundAssign(RHS) && 
        RHS->right && (RHS->right->kind == AST_FUNCALL || 
        RHS->right->kind == AST_FUNPTR_CALL ||
        RHS->right->kind == AST_ASM_FUNCALL);
}

void irBinaryOp(Cctrl *cc, Ast *ast) {
    if (ast->kind == '=') {
        if (irShouldReverseMaths(ast->right)) {
            irArithmetic(cc,ast,1);
        } else {
            irEval(cc,ast->right);
        }

        if (ast->type->kind == AST_DEREF && AstIsFloatType(ast->left->type)) {
            /* XXX: irAssign() */
            /* XXX: @Unimplmented */
            loggerPanic("Unimplmented\n");
        } else {
            /* XXX: Load to register and convert  && irAssign() */
            loggerPanic("Unimplmented\n");
        }
    }

    switch (ast->kind) {
        case '<':            irCompare(cc,ast,IR_LT);      return;
        case '>':            irCompare(cc,ast,IR_GT);      return;
        case TK_GREATER_EQU: irCompare(cc,ast,IR_GTE);     return;
        case TK_LESS_EQU:    irCompare(cc,ast,IR_LTE);     return;
        case TK_EQU_EQU:     irCompare(cc,ast,IR_EQU_EQU); return;
        case TK_NOT_EQU:     irCompare(cc,ast,IR_NOT_EQU); return;
    }

    if (ast->type->kind == AST_TYPE_POINTER) {
        /* XXX: irPointerArithmetic() */
        return;
    }

    if (AstIsRangeOperator(ast->kind) && AstIsRangeOperator(ast->left->kind)) {
        /* XXX: irRangeOperation() */
        return;
    }

    if (AstIsIntType(ast->type) || AstIsFloatType(ast->type)) {
        irArithmetic(cc,ast,0);
    } else if (ast->type->kind == AST_TYPE_FUNC) {
        IrInstruction *inst = irInstNew(IR_LOAD);
        inst->arg1 = irOperandNew(IR_FUNCTION);
        if (ast->kind == AST_ASM_FUNC_BIND) {
            inst->arg1->fname = strndup(ast->asmfname->data,ast->asmfname->len);
        } else {
            inst->arg1->fname  = irNormaliseFunctionName(ast->fname->data);
        }
    } else {
        loggerPanic("Cannot handle>: %s\n", AstToString(ast));
    }
}

int irJumpConditional(Cctrl *cc, Ast *ast) {
    int jmp;
    switch (ast->kind) {
        case TK_EQU_EQU:      jmp = IR_JMP_EQ;     break;
        case TK_NOT_EQU:      jmp = IR_JMP_NOT_EQ; break;
        case '<':             jmp = IR_JMP_LT;     break;
        case TK_LESS_EQU:     jmp = IR_JMP_LTE;    break;
        case '>':             jmp = IR_JMP_GT;     break;
        case TK_GREATER_EQU:  jmp = IR_JMP_GTE;    break;
        default:
            loggerPanic("Unknown kind: %s\n",AstKindToString(ast->kind));
    }
    return jmp;
}

int irJumpConditionalInverted(Cctrl *cc, Ast *ast) {
    int jmp;
    switch (ast->kind) {
        case TK_EQU_EQU:      jmp = IR_JMP_NOT_EQ; break;
        case TK_NOT_EQU:      jmp = IR_JMP_EQ;     break;
        case '<':             jmp = IR_JMP_GTE;    break;
        case TK_LESS_EQU:     jmp = IR_JMP_GT;     break;
        case '>':             jmp = IR_JMP_LTE;    break;
        case TK_GREATER_EQU:  jmp = IR_JMP_LT;     break;
        default:
            loggerPanic("Unknown kind: %s\n",AstKindToString(ast->kind));
    }
    return jmp;
}

void irIf(Cctrl *cc, Ast *ast) {
    aoStr *tmp_label = cc->tmp_label;
    IrInstruction *inst;
    if (ast->then || ast->els) {
        cc->tmp_label = AstMakeLabel();
        /* If the condition is not a '||' or '&&' we want to invert the condition
         * this is because the JMP jumps past the body of the if */
        irEvalIfCond(cc,ast->cond,!irIsLogical(ast));
        inst = irInstNew(IR_LABEL);
        inst->arg1 = irOpLabelNew(cc->tmp_label);
        if (ast->then) irEval(cc,ast->then);
        if (ast->els)  irEval(cc,ast->els);
        ListAppend(cc->ir_func->body,inst);
    }
    cc->tmp_label = tmp_label;
}

void irCondLogical(Cctrl *cc, Ast *ast, int invert) {
    IrInstruction *inst;
    aoStr *label = AstMakeLabel();
    /* While a bit odd this is easier to modify than explicitly having the 
     * RHS and LHS logic */
    Ast *conds[2] = {ast->left, ast->right};

    for (int i = 0; i < static_size(conds); ++i) {
        Ast *cond = conds[i];
        /* If we are looking at an '||' or an '&&' we then want to evaluate 
         * that branch of the tree i.e '2<3 && (4>5 || 1)' the right handside 
         * is another condition that needs to be evaluted */
        if (irIsLogical(cond)) {
            unsigned long flags = 0;
            cc->tmp_label = label;
            /* Invert the condition if it is '&&'. We jump into the if body 
             * if it is an '||' and jump over the body if it is an '&&'. */
            irEvalIfCond(cc,cond,cond->kind == TK_AND_AND);
            /* Setup flags for the assembly to handle the work of using either
             * float operations or integer operations */
            if (irLeftOrRightIsUnsigned(cond)) flags |= IR_FLAGS_UNSIGNED;
            if (irLeftOrRightIsFloat(cond))    flags |= IR_FLAGS_FLOAT;
            inst = irInstNew(IR_LABEL);
            inst->arg1 = irOpLabelNew(cc->tmp_label);
            inst->flags = flags;
            ListAppend(cc->ir_func->body,inst);
        } else {
            irCompareAndJump(cc,cond,label,invert);
            cc->tmp_label = label;
        }
    }
}

void irCondLogicalAnd(Cctrl *cc, Ast *ast) {
    irCondLogical(cc,ast,1);
}

void irCondLogicalOr(Cctrl *cc, Ast *ast) {
    irCondLogical(cc,ast,0);
}

void irCompound(Cctrl *cc, List *stmts) {
    if (stmts) {
        /* Create a new scope for registers, this is so you can have variables
         * with the same name in different scopes. */
        cc->localenv = DictNewWithParent(cc->localenv);
        ListForEach(stmts) {
            irEval(cc,(Ast *)it->value);
        }
        cc->localenv = cc->localenv->parent;
    }
}

void irEvalIfCond(Cctrl *cc, Ast *ast, int invert) {
    switch (ast->kind) {
        case TK_AND_AND: irCondLogicalAnd(cc,ast); break;
        case TK_OR_OR:   irCondLogicalOr(cc,ast);  break;
        default:         irCompareAndJump(cc,ast,cc->tmp_label,invert); break;
    }
}

void irEvalWhileCond(Cctrl *cc, Ast *ast, aoStr *end_label, int invert) {
    switch (ast->kind) {
        case TK_AND_AND: irCondLogicalAnd(cc,ast);                  break;
        case TK_OR_OR:   irCondLogicalOr(cc,ast);                   break;
        default:         irCompareAndJump(cc,ast,end_label,invert); break;
    }
}

void irLabel(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_LABEL);
    IrOperand *op = irOperandNew(IR_LABEL);
    if (ast->sval) op->label = ast->sval;
    else           op->label = ast->slabel;
    inst->arg1 = op;
    ListAppend(cc->ir_func->body,inst);
}

void irJump(Cctrl *cc, Ast *ast) {
    IrInstruction *inst = irInstNew(IR_JMP);
    IrOperand *op = irOperandNew(IR_LABEL);
    op->label = ast->slabel;
    inst->arg1 = op;
    ListAppend(cc->ir_func->body,inst);
}

void irWhile(Cctrl *cc, Ast *ast) {
    IrInstruction *i1,*i2,*i3;
    IrOperand *op1,*op2,*op3;
    aoStr *tmp_label = cc->tmp_label;

    cc->tmp_label = ast->while_end;

    /* Loop head label */
    i1 = irInstNew(IR_LABEL);
    op1 = irOpLabelNew(ast->while_begin);
    i1->arg1 = op1;
    ListAppend(cc->ir_func->body,i1);

    /* Jump out of the loop if the condition is not met, by inverting the 
     * condition in the `while (<cond>)` */
    irEvalWhileCond(cc, ast->whilecond, ast->while_end, 1);

    cc->tmp_label = ast->while_end;

    if (ast->whilebody) {
        irEval(cc,ast->whilebody);
    }

    i2 = irInstNew(IR_JMP);
    op2 = irOperandNew(IR_LABEL);
    op2->label = ast->while_begin;
    i2->arg1 = op2;

    i3 = irInstNew(IR_LABEL);
    op3 = irOperandNew(IR_LABEL);
    op3->label = ast->while_end;
    i3->arg1 = op3;

    cc->tmp_label = tmp_label;

    ListAppend(cc->ir_func->body,i2);
    ListAppend(cc->ir_func->body,i3);
}

void irFunctionCall(Cctrl *cc, Ast *funcall) {
    /*XXX: @Unimplmented */
    loggerPanic("Unimplmented\n");
}

void irEval(Cctrl *cc, Ast *ast) {
    switch (ast->kind) {
        case AST_LITERAL:       irLoadIntermediate(cc,ast); break;
        case AST_STRING:        irLoadString(cc,ast);       break;
        case AST_LVAR:          irLoadLocal(cc,ast);        break;
        case AST_IF:            irIf(cc,ast);               break;
        case AST_GVAR:          irLoadGlobal(cc,ast);       break;
        case AST_DEFAULT_PARAM: irEval(cc,ast->declvar);    break;
        case AST_DECL:          irDecl(cc,ast);             break;
        case AST_COMPOUND_STMT: irCompound(cc,ast->stms);   break;
        case AST_LABEL:         irLabel(cc,ast);            break;
        case AST_WHILE:         irWhile(cc,ast);            break;

        case AST_FUNPTR_CALL:
        case AST_ASM_FUNCALL:
        case AST_FUNCALL:       irFunctionCall(cc,ast);     break;

        case AST_GOTO:
        case AST_BREAK:
        case AST_CONTINUE:      irJump(cc,ast);             break;
//        case TK_AND_AND:        irLogicalAnd(cc,ast);       break;
//        case TK_OR_OR:          irLogicalOr(cc,ast);        break;
//      
        default:                irBinaryOp(cc,ast);         break;
    }
}

void irEvalFunc(Cctrl *cc, Ast *func) {
    /* We're only interested in function's with bodies */
    if (func->kind != AST_FUNC) return;
    /* We're going to use this pointer to collect the function
     * body */
    IrInstruction *inst = irInstNew(IR_FUNCTION);
    cc->localenv = DictNewWithParent(NULL);
    inst->reg_count = 0;
    inst->fname = func->fname;
    inst->body = ListNew();

    cc->ir_func = inst;

    irResetRegister();

    ListAppend(cc->ir_list,inst);

    /* Allocate a register for each parameter */
    if (func->params) {
        ListForEach(func->params) { 
            Ast *ast = (Ast*)it->value;
            CctrlSetTmpRegister(cc,ast->lname->data,register_num);
            inst->reg_count++;
        }
    }

    if (func->body->stms) {
        ListForEach(func->body->stms) {
            Ast *ast = (Ast*)it->value;
            irEval(cc,ast);
        }
    }
    inst->var_to_reg = cc->localenv;
    cc->localenv = NULL;
}

void irPrint(List *ir_instructions) {
    ListForEach(ir_instructions) {
        IrInstruction *ir = (IrInstruction *)it->value;
        aoStr *str = aoStrNew();
        irToString(str, ir);
        printf("%s\n",str->data);
        aoStrRelease(str);
    }
}

/* prototype defined in cctrl.h */
void irMain(Cctrl *cc) {
    ListForEach(cc->ast_list) {
        Ast *ast = (Ast*)it->value;
        switch (ast->kind) {
            case AST_FUNC: irEvalFunc(cc,ast); break;
            case AST_ASM_FUNC_BIND:
            case AST_EXTERN_FUNC:
                continue;
            case AST_GVAR: {
                loggerWarning("Unhandled global\n");
                continue;
            }
            default:
                AstPrint(ast);
                loggerPanic("irMain: cannot handle: %s %s\n",
                        AstKindToString(ast->kind),
                        AstLValueToString(ast));
        }
    }
}
