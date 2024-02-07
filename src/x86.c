#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "config.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "prsutil.h"
#include "util.h"

#define VAR_ARG_MAX 20
#define ASM_INORDER_ARITHMETIC 0
#define ASM_REVERSE_ARITHMETIC 1

/**
 * First Argument: %rdi
 * Second Argument: %rsi
 * Third Argument: %rdx
 * Fourth Argument: %rcx (or %r10 on Linux kernel system calls)
 * Fifth Argument: %r8
 * Sixth Argument: %r9
 */
static char *REGISTERS[] = {"rdi", "rsi", "rdx", "rcx",
    "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"};
/**
 * the first 8 are function args
 */
static char *FLOAT_REGISTERS[] = {"xmm0", "xmm1", "xmm2", "xmm3",
    "xmm4", "xmm5", "xmm6", "xmm7", "xmm8", "xmm9", "xmm10", "xmm11",
    "xmm12", "xmm13", "xmm14", "xmm15"};

static int stack_pointer = 0;
static int has_initialisers = 0;

#define REG_RAX "rax"
#define REG_RDX "rdx"
#define REG_RBP "rbp"
#define REG_RSP "rsp"
#define REG_RCX "rcx"
#define REG_RIP "rip"
#define REG_RDI "rdi"

void AsmExpression(Cctrl *cc, aoStr *buf, Ast *ast);

uint64_t ieee754(double _f64) {
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

/* This is a hacky, but seemingly functional way of me being able
 * to run this on Macos and Linux */
char *AsmNormaliseFunctionName(char *fname) {
    aoStr *newfn = AstNormaliseFunctionName(fname);
    if (!strncasecmp(fname, "Main", 4)) {
        if (has_initialisers) {
            aoStrCatPrintf(newfn,"Fn");
        } else {
            aoStrToLowerCase(newfn);
        }
    }
    return aoStrMove(newfn);
}

void AsmRemovePreviousTab(aoStr *buf) {
    if (buf->data[buf->len-1] == '\t') {
        buf->len--;
    }
}

char *AsmGetMov(AstType *type) {
    if (type->kind == AST_TYPE_CLASS && type->is_intrinsic) {
        return "movq";
    }
    switch (type->size) {
        case 1: return "movb";
        case 2: return "movw";
        case 4: return "movl";
        case 8: return "movq";
        default:
            loggerPanic("Unsupported size: %d\n",type->size);
    }
}

char *AsmGetMovWithSign(AstType *type) {
    if (type->kind == AST_TYPE_CLASS && type->is_intrinsic) {
        return "movq";
    }
    switch (type->size) {
        case 1: 
            if (type->issigned) return "movsb";
            else                return "movzb";
        case 2:
            if (type->issigned) return "movswl";
            else                return "movzwl";
        case 4:
            if (type->issigned) return "movsl";
            else                return "mov";
        case 8: return "movq";
        default:
            loggerPanic("Unsupported size: %d\n",type->size);
    }
}

char *AsmGetPtrMove(int kind) {
    switch (kind) {
        case AST_TYPE_ARRAY: return "leaq";
        default:             return "mov";
    }
}

char *AsmGetLoadReg(AstType *type, char ch) {
    (void)type;
    if (ch == 'a') return "rax";
    else           return "rcx";
}

char *AsmGetLoadMov(AstType *type) {
    if (type->kind == AST_TYPE_CLASS
            && type->is_intrinsic) return "movq";
    switch (type->size) {
        case 1:
            if (type->issigned) return "movsbq";
            else                return "movzbq";
        case 2:
            if (type->issigned) return "movswq";
            else                return "movzwq";
        case 4: return "movslq";
        case 8:
            return "movq";
        default:
            loggerPanic("Unknown size for loading: %s %d\n",
                    AstTypeToString(type), type->size);
    }
}

char *AsmGetIntReg(AstType *type, char ch) {
    if (ch == 'a') {
        if (type->kind == AST_TYPE_CLASS && type->is_intrinsic) {
            return "rax";
        }
        switch (type->size) {
            case 1: return "al";
            case 2: return "ax";
            case 4: return "eax";
            case 8: return "rax";
            default:
                loggerPanic("Unknown size for reg prefix '%c': %s %d\n",
                        ch, AstTypeToString(type), type->size);
        }
    } else if (ch == 'c') {
        if (type->kind == AST_TYPE_CLASS && type->is_intrinsic) {
            return "rcx";
        }
        switch (type->size) {
            case 1: return "cl";
            case 2: return "cx";
            case 4: return "ecx";
            case 8: return "rcx";
            default:
         return "rcx";
                loggerPanic("Unknown size for reg prefix '%c': %s %d\n",
                        ch,
                        AstTypeToString(type), type->size);
        }
    } else {
        loggerPanic("unknown register prefix: %c\n", ch);
    }
}

void AsmToInt(aoStr *buf, AstType *type) {
    if (type->kind == AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf, "cvttsd2si  %%xmm0, %%rax\n\t");
    }
}

void AsmToFloat(aoStr *buf, AstType *type) {
    if (type->kind != AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf, "cvtsi2sd  %%rax, %%xmm0\n\t");
    }
}

void AsmPushXMM(aoStr *str, int reg) {
    aoStrCatPrintf(str, "sub   $8, %%rsp\n\t"
                        "movsd %%xmm%d, (%%rsp)\n\t", reg);
    stack_pointer += 8;
}

void AsmPopXMM(aoStr *str, int reg) {
    aoStrCatPrintf(str, "movsd  (%%rsp), %%xmm%d\n\t"
                        "add    $8, %%rsp\n\t", reg);
    stack_pointer -= 8;
    if (stack_pointer < 0) {
        loggerPanic("Stack underflow\n");
    }
}

void AsmPush(aoStr *buf, char *reg) {
    aoStrCatPrintf(buf,"push   %%%s\n\t", reg);
    stack_pointer += 8;
}

void AsmPop(aoStr *buf, char *reg) {
    aoStrCatPrintf(buf,"pop    %%%s\n\t", reg);
    stack_pointer -= 8;
    if (stack_pointer < 0) {
        loggerPanic("Int register stack underflow\n");
    }
}

void AsmCall(aoStr *buf, char *fname) {
    char *_fname = AsmNormaliseFunctionName(fname);
    aoStrCatPrintf(buf,"call   %s\n\t", _fname);
    free(_fname);
}

/* Save a global variable */
void AsmGSave(aoStr *buf, char *name, AstType *type, int offset) {
    assert(type->kind != AST_TYPE_ARRAY);
    char *reg;

    reg = AsmGetIntReg(type, 'a');
    if (offset) {
        aoStrCatPrintf(buf, "movq  %%%s, %s+%d(%%rip)\n\t", reg, name, offset);
    } else {
        aoStrCatPrintf(buf, "movq  %%%s, %s(%%rip)\n\t", reg, name);
    }
}

void AsmPlaceString(aoStr *buf, aoStr *str, int offset) {
    int i = 0;
    /* Place string on the stack character by character */
    for (; i < str->len; ++i) {
        aoStrCatPrintf(buf,"movb   $%d, %d(%%rbp)\n\t",
                (int)str->data[i], 
                -(offset - i));
    }
    /* Place '\0' */
    aoStrCatPrintf(buf, "movb   $0, %d(%%rbp)\n\t", 
            -(offset - i));
}

void AsmGLoad(aoStr *buf, AstType *type, aoStr *label, int offset) {
    char *reg = NULL;

    if (type->kind == AST_TYPE_ARRAY) {
        if (offset) {
            aoStrCatPrintf(buf, "lea    %s+%d(%%rip), %%rax\n\t",
                    label->data, offset);
        } else {
            aoStrCatPrintf(buf, "lea    %s(%%rip), %%rax\n\t",
                    label->data);
        }
        return;
    }

    reg = AsmGetIntReg(type, 'a');
    if (type->size < 4) {
        aoStrCatPrintf(buf, "movl  $0, %%eax\n\t");
    }
    if (offset) {
        aoStrCatPrintf(buf, "movq  %s+%d(%%rip), %%%s\n\t",
                label->data, offset, reg);
    } else {
        aoStrCatPrintf(buf, "movq  %s(%%rip), %%%s\n\t",
                label->data, reg);
    }
}

void AsmLLoad(aoStr *buf, AstType *type, int offset) {
    char *reg = NULL;
    
    switch (type->kind) {
        case AST_TYPE_ARRAY:
            aoStrCatPrintf(buf, "# LOAD LEAQ START: %s\n\t", AstKindToString(type->kind));
            aoStrCatPrintf(buf, "leaq   %d(%%rbp), %%rax\n\t", offset);
            aoStrCatPrintf(buf, "# LOAD LEAQ END: %s\n\t", AstKindToString(type->kind));
            return;
        case AST_TYPE_FLOAT:
            aoStrCatPrintf(buf, "movsd  %d(%%rbp), %%xmm0\n\t", offset);
            return;
        default:
            reg =  AsmGetLoadReg(type, 'a');
            aoStrCatPrintf(buf, "# LOAD %s %s START\n\t",reg,AstKindToString(type->kind));

            aoStrCatPrintf(buf, "%-4s  %d(%%rbp), %%%s\n\t",
                    AsmGetLoadMov(type), offset, reg);
            aoStrCatPrintf(buf, "# LOAD %s END\n\t",reg);
    }
}

void AsmLSave(aoStr *buf, AstType *type, int offset) {
    char *reg, *mov;
    switch (type->kind) {
        case AST_TYPE_FLOAT:
            aoStrCatPrintf(buf, "movsd   %%xmm0, %d(%%rbp)\n\t", offset);
            break;
        default:
            reg = AsmGetIntReg(type,'a');
            mov = AsmGetMov(type);
            aoStrCatPrintf(buf, "%-4s   %%%s, %d(%%rbp)\n\t",mov, reg, offset);
            break;
    }
}

void AstUpCastInt(aoStr *buf, AstType *type, int issigned) {
    switch(type->kind) {
    case AST_TYPE_CHAR:
        if (issigned) aoStrCatPrintf(buf, "movsbq   %%al, %%rax\n\t");
        else          aoStrCatPrintf(buf, "movzbq   %%al, %%rax\n\t");
        break;
    case AST_TYPE_INT:
        if (issigned) aoStrCatPrintf(buf, "cltq\n\t");
        else          aoStrCatPrintf(buf, "movslq   %%eax, %%rax #upcast :%d\n\t",type->size);
        break;
    }
}

void AsmDownCastInt(aoStr *buf, AstType *type, int issigned) {
    switch(type->kind) {
    case AST_TYPE_INT:
        if (issigned) aoStrCatPrintf(buf, "movsx  %%al, %%rax\n\t");
        else          aoStrCatPrintf(buf, "movzx  %%al, %%rax\n\t");
        break;
    }
}

void AsmFloatToInt(aoStr *buf, AstType *type) {
    if (type->kind == AST_TYPE_FLOAT)
        aoStrCatPrintf(buf, "cvttsd2si %%xmm0, %%eax\n\t");
}

void AsmCast(aoStr *buf, AstType *from, AstType *to) {
    if (AstIsIntType(from) && to->kind == AST_TYPE_FLOAT)
        aoStrCatPrintf(buf, "cvtsi2sd %%rax, %%xmm0\n\t");
    else if (AstIsIntType(from) && AstIsIntType(to)) {
        if (to->size > from->size) {
            if (to->is_intrinsic && from->size == 8) return;
            AstUpCastInt(buf, from, to->issigned);
        } else if (to->size < from->size) {
            AsmDownCastInt(buf, from, to->issigned);
        }
    }
    else if (AstIsIntType(to))
        AsmFloatToInt(buf, from);
}

void AsmTypeCast(Cctrl *cc, aoStr *buf, Ast *ast) {
    AsmExpression(cc,buf,ast->operand);
    AsmCast(buf,ast->operand->type,ast->type);
}

void AsmAssignDerefInternal(aoStr *buf, AstType *type, int offset) {
    char *reg,*mov;
    aoStrCatPrintf(buf, "# ASSIGN DREF INTERNAL START: %s\n\t",
            AstKindToString(type->kind));

    aoStrCatPrintf(buf, "movq   (%%rax), %%rcx #OK mov\n\t");
    AsmPop(buf, "rcx");

    reg = AsmGetIntReg(type, 'c');
    mov = AsmGetMov(type);

    if (type->kind == AST_TYPE_FLOAT) {
        if (offset) {
            aoStrCatPrintf(buf, "movsd  %%xmm0, %d(%%rax)\n\t", offset);
        } else {
            aoStrCatPrintf(buf, "movsd  %%xmm0, (%%rax)\n\t");
        }
    } else {
        if (offset) {
            aoStrCatPrintf(buf, "%s %%%s, %d(%%rax)\n\t",mov, reg, offset);
        } else {
            aoStrCatPrintf(buf, "%s  %%%s, (%%rax)\n\t",mov, reg);
        }
    }

    aoStrCatPrintf(buf, "# ASSIGN DREF INTERNAL end\n\t");
}

void AsmAssignDeref(Cctrl *cc,aoStr *buf, Ast *ast) {
    AsmPush(buf, REG_RAX);
    AsmExpression(cc,buf,ast->operand);
    AsmAssignDerefInternal(buf,ast->operand->type->ptr, 0);
}

void AsmLoadDeref(aoStr *buf, AstType *result, AstType *op_type, int off) {
    char *reg,*ptr_mov;
    if (op_type->kind == AST_TYPE_POINTER &&
            op_type->ptr->kind == AST_TYPE_ARRAY) {
        return;
    }
    aoStrCatPrintf(buf, "# LOAD DEREF: %s %s\n\t",
            AstKindToString(result->kind),
            AstKindToString(op_type->kind));

    ptr_mov = AsmGetPtrMove(op_type->kind);

    if (op_type->kind == AST_TYPE_FLOAT) {
        if (off) aoStrCatPrintf(buf, "%s   %d(%%rax), %%xmm1\n\t",  ptr_mov, off);
        else     aoStrCatPrintf(buf,"%s   (%%rax), %%xmm1\n\t", ptr_mov);
        aoStrCatPrintf(buf,"movsd   %%xmm1, %%xmm0\n\t");
    } else {
        reg = AsmGetIntReg(result,'c');
        if (result->size < 4) {
            aoStrCatPrintf(buf, "mov    $0, %%ecx\n\t");
        }

        if (off) aoStrCatPrintf(buf, "%s   %d(%%rax), %%%s\n\t",  ptr_mov, off, reg);
        else     aoStrCatPrintf(buf,"%s   (%%rax), %%%s\n\t", ptr_mov, reg);

        /* Move back for the next recursive call */
        aoStrCatPrintf(buf,"movq   %%%s, %%rax\n\t", reg);
    }
    aoStrCatPrintf(buf, "# LOAD DEREF END: %s %s\n\t",
            AstKindToString(result->kind),
            AstKindToString(op_type->kind));
}

void AsmPointerArithmetic(Cctrl *cc, aoStr *buf, long op, Ast *LHS, Ast *RHS) {
    //assert(LHS->type->kind == AST_TYPE_POINTER);
    int size;
    char *fn;

    aoStrCatPrintf(buf, "# Pointer Arithmetic start: %s %s\n\t",
            AstKindToString(LHS->kind), AstKindToString(RHS->kind));
    AsmExpression(cc,buf,LHS);
    aoStrCatPrintf(buf, "# Pointer Arithmetic MID\n\t");
    AsmPush(buf,REG_RAX);
    AsmExpression(cc,buf,RHS);

    /* XXX: What is going on */
    if (LHS->type->ptr->ptr != NULL) {
        if (LHS->type->ptr->kind == AST_TYPE_POINTER) {
            size = 8;
        } else {
            size = LHS->type->ptr->ptr->size; 
        }
    } else {
        size = LHS->type->ptr->size; 
    }

    switch (op) {
        case '+': fn = "addq"; break;
        case '-': fn = "subq"; break;
        case '>': fn = "setg"; break;
        case '<': fn = "setl"; break;
        case TK_GREATER_EQU: fn = "setge"; break;
        case TK_LESS_EQU: fn =  "setle"; break;
        case TK_EQU_EQU: fn = "sete"; break;
        case TK_NOT_EQU: fn = "setne"; break;
        default: loggerPanic("op: %c is not implemented for pointer arithmetic\n",(char)op);
    }

    if (op == '+' || op == '-') {
        if (size > 1) {
            aoStrCatPrintf(buf, "imul   $%d, %%rax\n\t", size);
        }
        AsmPop(buf,REG_RCX);
        aoStrCatPrintf(buf,
            "%-3s   %%rax, %%rcx\n\t"
            "movq   %%rcx, %%rax\n\t", fn);
    } else {
        AsmPop(buf,REG_RCX);
        aoStrCatPrintf(buf,
                "cmpq   %%rax, %%rcx\n\t"
                "%-3s   %%al\n\t"
                "movzb  %%al, %%rax\n\t", fn);
    }
    aoStrCatPrintf(buf, "# Pointer Arithmetic end\n\t");
}

void AsmAssignClassRef(Cctrl *cc, aoStr *buf, Ast *cls, AstType *field, int offset) {
    int total_offset;
    switch (cls->kind) {
        case AST_LVAR:
            total_offset = cls->loff + field->offset + offset;
            AsmLSave(buf,field,total_offset);
            break;

        case AST_GVAR:
            loggerPanic("Global variables unimplemented: %s\n", AstToString(cls));
           // total_offset = field->offset + offset;
           // AsmGSave(buf,cls->clsname->data,field,total_offset);
            break;

        case AST_CLASS_REF:
            total_offset = offset + cls->type->offset;
            AsmAssignClassRef(cc,buf,cls->cls, field, total_offset);
            break;

        case AST_DEREF:
            aoStrCatPrintf(buf, "\n# Problem >code START %s \n\t",
                    AstKindToString(field->kind));
            total_offset = field->offset+offset;

            switch (field->kind) {
                case AST_TYPE_FLOAT:{
                    AsmPushXMM(buf,0);
                    AsmExpression(cc,buf,cls->operand);
                    AsmPopXMM(buf,1);
                    if (total_offset > 0) {
                        aoStrCatPrintf(buf, "movsd   %%xmm1, %d(%%rax)\n\t",
                                field->offset+offset);
                    } else {
                        aoStrCatPrintf(buf, "movsd   %%xmm1, (%%rax)\n\t");
                    }
                    /* Whatever we were assigning we put back, allows for chaining
                     * assignments */
                    aoStrCatPrintf(buf,"movsd    %%xmm1, %%xmm0\n\t");
                    break;
                }
                default: {
                    AsmPush(buf, REG_RAX);
                    AsmExpression(cc,buf,cls->operand);
                    AsmPop(buf,REG_RCX);
                    if (total_offset > 0) {
                        aoStrCatPrintf(buf, "movq   %%rcx, %d(%%rax)\n\t",
                                field->offset+offset);
                    } else {
                        aoStrCatPrintf(buf, "movq   %%rcx, (%%rax)\n\t");
                    }
                    
                    /* Whatever we were assigning we put back, allows for chaining
                     * assignments */
                    aoStrCatPrintf(buf,"movq    %%rcx, %%rax\n\t");
                    break;
                }
            }
            
            aoStrCatPrintf(buf, "\n# Problem >code END \n\t");
            break;
        default:
            loggerPanic("Failed to create ASM for: %s\n",
                    AstToString(cls));
    }
}

void AsmLoadClassRef(Cctrl *cc, aoStr *buf, Ast *cls, AstType *field, int offset) {
    int total_offset;
    switch (cls->kind) {
        case AST_LVAR:
            total_offset = cls->loff + field->offset + offset;
            aoStrCatPrintf(buf, "# CLASS LOAD\n\t");
            AsmLLoad(buf, field, total_offset);
            break;
        
        case AST_GVAR:
            loggerPanic("Global variables unimplemented: %s\n", AstToString(cls));
            //total_offset = field->offset + offset;
            //AsmGLoad(buf,field,cls->clsname,total_offset);
            break;

        case AST_CLASS_REF:
            total_offset = offset + cls->type->offset;
            AsmLoadClassRef(cc,buf,cls->cls, field,total_offset);
            break;

        case AST_DEREF:
            total_offset = field->offset+offset;
            AsmExpression(cc,buf,cls->operand);
            AsmLoadDeref(buf,cls->cls->type,field,total_offset);
            break;
        
        default:
            loggerPanic("Failed to create ASM for: %s\n",
                    AstToString(cls));
    }
}

void AsmAssign(Cctrl *cc, aoStr *buf, Ast *variable) {
    if (variable->kind == AST_DEFAULT_PARAM) {
        variable = variable->declvar;
    }
    switch (variable->kind) {
    case AST_DEREF: 
        AsmAssignDeref(cc,buf, variable);
        break;

    case AST_CLASS_REF:
        AsmAssignClassRef(cc,buf,variable->cls,variable->type,0);
        break;

    case AST_FUNPTR:
    case AST_LVAR: 
        AsmLSave(buf,variable->type,variable->loff);
        break;

    case AST_GVAR: 
        AsmGSave(buf,variable->gname->data,variable->type,0);
        break;

    default:
        loggerPanic("Cannot assign: %s\n",
                AstToString(variable));
    }
}

void AsmCompare(Cctrl *cc, aoStr *buf, char *instruction, Ast *ast) {
    Ast *LHS,*RHS;
    LHS = ast->left;
    RHS = ast->right;
    
    if ((LHS->type->kind == AST_TYPE_FLOAT &&
                RHS->type->kind == AST_TYPE_FLOAT)) {
        AsmExpression(cc,buf,LHS);
        AsmToFloat(buf,LHS->type);
        AsmPushXMM(buf,0);
        AsmExpression(cc,buf,RHS);
        AsmToFloat(buf,LHS->type);
        AsmPopXMM(buf,1);

        switch (ast->kind) {
            case '<': {
                aoStrCatPrintf(buf,
                        "ucomisd  %%xmm1, %%xmm0\n\t"
                        "seta     %%al\n\t"
                        "andb     $1, %%al\n\t"
                        "movzbl   %%al, %%eax\n\t");
                break;
            }
            case TK_LESS_EQU: {
                aoStrCatPrintf(buf,
                        "ucomisd  %%xmm0, %%xmm1\n\t"
                        "setae    %%al\n\t"
                        "andb     $1, %%al\n\t"
                        "movzbl   %%al, %%eax\n\t");
                break;
            }
            case '>': {
                aoStrCatPrintf(buf,
                        "ucomisd  %%xmm0, %%xmm1\n\t"
                        "seta     %%al\n\t"
                        "andb     $1, %%al\n\t"
                        "movzbl   %%al, %%eax\n\t");
                break;
            }
            case TK_GREATER_EQU: {
                aoStrCatPrintf(buf,
                        "ucomisd  %%xmm0, %%xmm1\n\t"
                        "setae    %%al\n\t"
                        "andb     $1, %%al\n\t"
                        "movzbl   %%al, %%eax\n\t");
                break;
            }
            case TK_EQU_EQU: {
                aoStrCatPrintf(buf,
                        "ucomisd  %%xmm0, %%xmm1\n\t"
                        "sete     %%al\n\t"
                        "setnp    %%cl\n\t"
                        "andb     %%cl, %%al\n\t"
                        "andb     $1, %%al\n\t"
                        "movzbl   %%al, %%eax\n\t");
                break;
            }
            case TK_NOT_EQU: {
                aoStrCatPrintf(buf,
                        "ucomisd  %%xmm0, %%xmm1\n\t"
                        "setne    %%al\n\t"
                        "setnp    %%cl\n\t"
                        "andb     %%cl, %%al\n\t"
                        "andb     $1, %%al\n\t"
                        "movzbl   %%al, %%eax\n\t");
                break;
            }
        }
    } else {
        AsmExpression(cc,buf,LHS);
        AsmToInt(buf,LHS->type);
        AsmPush(buf,REG_RAX);
        AsmExpression(cc,buf,RHS);
        AsmToInt(buf,RHS->type);
        AsmPop(buf,REG_RCX);
        aoStrCatPrintf(buf,
                "cmp    %%rax, %%rcx\n\t"
                "%-3s   %%al\n\t"
                "movzb  %%al, %%eax\n\t", instruction);
    }
}

void AsmBinaryOpIntArithmetic(Cctrl *cc,aoStr *buf, Ast *ast, int reverse) {
    aoStrCatPrintf(buf, "# INt arithmetic START \n\t");
    char *op;
    Ast *LHS,*RHS;
    LHS = ast->left;
    RHS = ast->right;
    int is_unsinged = 0;
    if (!LHS->type->issigned || !RHS->type->issigned) {
       is_unsinged = 1; 
    }

    switch(ast->kind) {
    case '+':    op = "addq"; break;
    case '-':    op = "subq"; break;
    case '*':    op = "imulq"; break;
    case '^':    op = "xorq"; break;
    case '&':    op = "and"; break;
    case '|':    op = "or"; break;
    case TK_SHL: op = "salq"; break;
    case TK_SHR: op = "sarq"; break;
    case '/': case '%': break;
    default:
        loggerPanic("Invalid operator: '%s'\n",
                AstKindToString(ast->kind));
    }

    if (reverse == ASM_REVERSE_ARITHMETIC) {
        AsmExpression(cc,buf,RHS);
        AsmToInt(buf,RHS->type);
        aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");

        AsmExpression(cc,buf,LHS);
        AsmToInt(buf,LHS->type);
        AsmPush(buf,REG_RAX);
    } else {
        AsmExpression(cc,buf,LHS);
        AsmToInt(buf,LHS->type);
        AsmPush(buf,REG_RAX);

        AsmExpression(cc,buf,RHS);
        AsmToInt(buf,RHS->type);
        aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");
    }
    AsmPop(buf,REG_RAX);

    if (ast->kind == '/' || ast->kind == '%') {
        aoStrCatPrintf(buf, "movq   $0, %%rdx\n\t");
        aoStrCatPrintf(buf, "cqto\n\t");
        if (is_unsinged) aoStrCatPrintf(buf, "divq  %%rcx\n\t");
        else             aoStrCatPrintf(buf, "idivq  %%rcx\n\t");
        if (ast->kind == '%') {
            aoStrCatPrintf(buf, "movq   %%rdx, %%rax\n\t");
        }
    } else if (ast->kind == TK_SHL || ast->kind == TK_SHR) {
        aoStrCatPrintf(buf, "%s   %%cl, %%rax\n\t", op);
    } else {
        aoStrCatPrintf(buf, "%s   %%rcx, %%rax\n\t", op);
    }

    aoStrCatPrintf(buf, "# INt arithmetic END \n\t");
}

void AsmBinaryOpFloatArithmetic(Cctrl *cc, aoStr *buf, Ast *ast, int reverse) {
    char *op;
    Ast *LHS,*RHS;
    LHS = ast->left;
    RHS = ast->right;
    switch(ast->kind) {
    case '+': op = "addsd"; break;
    case '-': op = "subsd"; break;
    case '*': op = "mulsd"; break;
    case '/': op = "divsd"; break;
    default:
        loggerPanic("Invalid operator: '%c'\n", (char)ast->kind);
    }

    if (reverse == ASM_REVERSE_ARITHMETIC) {
        AsmExpression(cc,buf,RHS);
        AsmToFloat(buf,RHS->type);
        aoStrCatPrintf(buf, "movsd  %%xmm0, %%xmm1\n\t");

        AsmExpression(cc,buf,LHS);
        AsmToFloat(buf,LHS->type);
        AsmPushXMM(buf,0);
    } else {
        AsmExpression(cc,buf,LHS);
        AsmToFloat(buf,LHS->type);
        AsmPushXMM(buf,0);

        AsmExpression(cc,buf,RHS);
        AsmToFloat(buf,RHS->type);
        aoStrCatPrintf(buf, "movsd  %%xmm0, %%xmm1\n\t");
    }
    AsmPopXMM(buf,0);

    aoStrCatPrintf(buf, "%s     %%xmm1, %%xmm0\n\t", op);
}

void AsmLoadConvert(aoStr *buf, AstType *to, AstType *from) {
    if (to->kind == AST_TYPE_FLOAT) {
        AsmToFloat(buf,from);
    } else {
        AsmToInt(buf,from);
    }
}

void AsmSaveConvert(aoStr *buf, AstType *to, AstType *from) {
    if (AstIsIntType(from) && AstIsFloatType(to)) {
        aoStrCatPrintf(buf, "cvtsi2ss %%eax, %%xmm0\n\t");
    } else if (AstIsFloatType(from) && AstIsFloatType(to)) {
        return;
    } else {
        AsmLoadConvert(buf,to,from);
    }
}

static char *AsmGetCompartor(long op) {
    switch (op) {
    case '<': return "setl";
    case '>': return "setg";
    case TK_GREATER_EQU: return "setge";
    case TK_LESS_EQU: return "setle";
    default:
        loggerPanic("Operation: %ld -> %s is not supported for ranges\n",
                op, AstKindToString(op));
    }
}

/* For syntax like:
 * '0' <= ch <= '9'
 * '0' >= ch <= '9'
 * '0' >= ch >= '9'
 * '0' > ch < '9'
 * '0' < ch > '9'
 * Etc...
 * */
void AsmRangeOperation(Cctrl *cc, aoStr *buf, Ast *ast) {
    char *op2 = AsmGetCompartor(ast->kind);
    char *op1 = AsmGetCompartor(ast->left->kind);
    aoStr *label_end = AstMakeLabel();

    aoStrCatPrintf(buf, "# RANGE Start\n\t");
    AsmExpression(cc,buf,ast->left->left);
    AsmPush(buf,REG_RAX);
    AsmExpression(cc,buf,ast->left->right);
    AsmPop(buf,REG_RCX);
    aoStrCatPrintf(buf,
            "cmp    %%rax, %%rcx\n\t"
            "%-5s   %%al\n\t"
            "movzb  %%al, %%rax\n\t"
            "test   %%rax, %%rax\n\t"
            "movq   $0, %%rax\n\t"
            "je     %s\n\t", op1, label_end->data);
    AsmExpression(cc,buf,ast->left->right);
    AsmPush(buf,REG_RAX);
    AsmExpression(cc,buf,ast->right);
    AsmPop(buf,REG_RCX);
    aoStrCatPrintf(buf,
            "cmp    %%rax, %%rcx\n\t"
            "%-5s   %%al\n\t"
            "movzb  %%al, %%rax\n\t"
            "test   %%rax, %%rax\n\t"
            "movq   $0, %%rax\n\t"
            "je     %s\n\t"
            "movq   $1, %%rax\n"
            "%s:\n\t",op2, label_end->data, label_end->data);
    aoStrCatPrintf(buf, "# Range END \n\t\n\t");
}

int AsmOpIsCompoundAssign(Ast *ast) {
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

int AsmShouldReverseMaths(Ast *RHS) {
    return AsmOpIsCompoundAssign(RHS) && 
        RHS->right && (RHS->right->kind == AST_FUNCALL || 
         RHS->right->kind == AST_FUNPTR_CALL ||
         RHS->right->kind == AST_ASM_FUNCALL);
}

void AsmBinOpFunctionAssign(Cctrl *cc, aoStr *buf, Ast *fnptr, Ast *fn) {
    char *normalised;
    switch (fn->kind) {
        case AST_FUNC:
            normalised = AsmNormaliseFunctionName(fn->fname->data);
            aoStrCatPrintf(buf,
                    "leaq   %s(%%rip), %%rax\n\t"
                    "movq    %%rax, %d(%%rbp)\n\t",
                    normalised,
                    fnptr->loff);
            free(normalised);
            break;

        case AST_FUNPTR:
            aoStrCatPrintf(buf,
                    "movq    %d(%%rbp), %%rax\n\t"
                    "movq    %%rax, %d(%%rbp)\n\t",
                    fn->loff, fnptr->loff);
            break;

        case AST_ASM_FUNCDEF:
            aoStrCatPrintf(buf,
                    "leaq   %s(%%rip), %%rax\n\t"
                    "movq    %%rax, %d(%%rbp)\n\t",
                    fn->asmfname->data,
                    fnptr->loff);
            break;
        default:
            loggerPanic("Cannot assign: %s to a function pointer type\n",
                    AstKindToString(fn->kind));
    }
}

void AsmBinaryOp(Cctrl *cc, aoStr *buf, Ast *ast) {
    if (ast->kind == '=') {
        /* If it is compound and the value is being assigned to the return of 
         * a function we need to perform the integer arithmetic in reversed 
         * order */
        if (AsmShouldReverseMaths(ast->right)) {
            if (ast->type->kind == AST_TYPE_INT) { 
                AsmBinaryOpIntArithmetic(cc,buf,ast->right,
                        ASM_REVERSE_ARITHMETIC);
            } else if (ast->type->kind == AST_TYPE_FLOAT) {
                AsmBinaryOpFloatArithmetic(cc,buf,ast->right,
                        ASM_REVERSE_ARITHMETIC);
            }
        } else {
            AsmExpression(cc,buf,ast->right);
            // AsmLoadConvert(buf,ast->type,ast->right->type);
        }
        if (ast->right->kind == AST_DEREF && AstIsFloatType(ast->left->type)) {
            aoStrCatPrintf(buf, "movq    %%rax, %%xmm0\n\t");
            AsmAssign(cc,buf, ast->left);
        } else {
            AsmLoadConvert(buf,ast->type,ast->right->type);
            AsmAssign(cc,buf, ast->left);
        }
        return;
    }

    if (ast->type->kind == AST_TYPE_POINTER) {
        AsmPointerArithmetic(cc,buf,ast->kind,ast->left,ast->right);
        return;
    }

    /* This is pretty loose! */
    if (AstIsRangeOperator(ast->kind) && AstIsRangeOperator(ast->left->kind)) {
        AsmRangeOperation(cc,buf,ast);
        return;
    }

    switch (ast->kind) {
        case '<': AsmCompare(cc,buf,"setl",ast); return;
        case '>': AsmCompare(cc,buf,"setg",ast); return;
        case TK_EQU_EQU: AsmCompare(cc,buf,"sete",ast); return;
        case TK_GREATER_EQU: AsmCompare(cc,buf,"setge",ast); return;
        case TK_LESS_EQU: AsmCompare(cc,buf,"setle",ast); return;
        case TK_NOT_EQU: AsmCompare(cc,buf,"setne",ast); return;
    }

    if (ast->type->kind == AST_TYPE_INT || ast->type->kind == AST_TYPE_CHAR) {
        AsmBinaryOpIntArithmetic(cc,buf,ast,ASM_INORDER_ARITHMETIC);
    } else if (ast->type->kind == AST_TYPE_FLOAT) {
        AsmBinaryOpFloatArithmetic(cc,buf,ast,ASM_INORDER_ARITHMETIC);
    } else if (ast->type->kind == AST_TYPE_FUNC)  {
        char *fname = AsmNormaliseFunctionName(ast->fname->data);
        aoStrCatPrintf(buf,"leaq    %s(%%rip), %%rax\n\t",fname);
        free(fname);
    } else {
        loggerPanic("Cannot handle>: %s\n", AstToString(ast));
    }
}

void AsmPreIncrDecr(Cctrl *cc, aoStr *buf, Ast *ast, char *op) {
    AsmExpression(cc,buf,ast->operand);
    int size = 1;
    if (ast->operand->type->ptr) {
        size = ast->operand->type->ptr->size;
    }
    if (ast->operand->kind == AST_DEREF) {
        /* This is essentially pointer juggling like:
         * '*++ptr' or '*--ptr' */
        aoStrCatPrintf(buf, "%s   $%d, %%rax\n\t", op, size);
        AsmAssign(cc,buf,ast->operand);
        if (ast->operand->type->ptr) {
            aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");
        }
    } else {
        aoStrCatPrintf(buf, "%s   $%d, %%rax\n\t", op, size);
        AsmAssign(cc,buf,ast->operand);
        if (ast->operand->type->ptr) {
            aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");
        }
    }
}

void AsmIncrDecr(Cctrl *cc, aoStr *buf, Ast *ast, char *op) {
    AsmExpression(cc,buf,ast->operand);
    AsmPush(buf, REG_RAX);
    int size = 1;
    if (ast->operand->type->ptr) {
        size = ast->operand->type->ptr->size;
    }

    if (ast->operand->kind == AST_DEREF) {
        /* This is essentially pointer juggling like:
         * '*ptr++' or '*ptr--' */
        aoStrCatPrintf(buf, "%s   $%d, %%rax\n\t", op, size);
        AsmAssign(cc,buf,ast->operand);
    } else {
        aoStrCatPrintf(buf, "%s   $1, %%rax\n\t", op);
        AsmAssign(cc,buf,ast->operand);
    }

    AsmPop(buf,REG_RAX);
}

/* This function is seriouly bjorked */
void AsmAddr(Cctrl *cc, aoStr *buf, Ast *ast) {
    aoStrCatPrintf(buf, "# ADDR %s START %s \n\t", 
        AstKindToString(ast->operand->kind), 
        AstKindToString(ast->operand->type->kind));

    /* This is gross */
    switch (ast->operand->kind) {
        case AST_LVAR: {
            if (ast->operand->type->kind == AST_TYPE_POINTER) {
                /* XXX: this feels extremely hacky */
                aoStrCatPrintf(buf, "# ADDR of %s\n\t",
                        AstKindToString(ast->operand->type->ptr->kind));
                switch (ast->operand->type->ptr->kind) {
                    case AST_TYPE_ARRAY:
                    case AST_TYPE_CHAR:
                    case AST_TYPE_CLASS:
                        aoStrCatPrintf(buf, "leaq   %d(%%rbp), %%rax\n\t",
                                ast->operand->loff);
                        break;
                    default:
                        aoStrCatPrintf(buf, "movq   %d(%%rbp), %%rax\n\t",
                                ast->operand->loff);
                        break;
                }
            } else {
                aoStrCatPrintf(buf, "leaq   %d(%%rbp), %%rax\n\t",
                        ast->operand->loff);
            } 
            break;
        }
        /* XXX: This does not work */
        case AST_GVAR:
            aoStrCatPrintf(buf, "leaq   %s(%%rip), %%rax\n\t",
                    ast->operand->glabel->data);
            break;
        case AST_CLASS_REF: {
            if (ast->operand->operand->kind == AST_LVAR ||
                ast->operand->operand->kind == AST_DEREF)
            {
                aoStrCatPrintf(buf, "movq   %d(%%rbp), %%rax\n\t",
                        ast->cls->cls->operand->loff);
                aoStrCatPrintf(buf,"addq   $%d, %%rax\n\t",ast->cls->type->offset);
            } else {
                loggerPanic("Cannot produce ASM for: %s %s %s %s\n",
                    AstKindToString(ast->operand->operand->kind),
                    AstTypeToString(ast->type),
                    AstTypeToString(ast->operand->type),
                    AstTypeToString(ast->cls->type));
            }
            break;
        }
        case AST_DEREF: {
            /* That is the class */
            Ast *cls = ast->operand;
            AsmExpression(cc,buf,cls);
            break;
        }
        default:
            loggerPanic("Cannot turn Kind AST:%s %s into assembly\n",
                    AstKindToString(ast->kind),
                    AstToString(ast));
    }
    aoStrCatPrintf(buf, "# ADDR %s END %s \n\t", 
        AstKindToString(ast->operand->kind),
        AstKindToString(ast->operand->type->kind));
}

int AsmPlaceArgs(Cctrl *cc, aoStr *buf, List *argv, int reverse) {
    int r = 0;
    Ast *ast;
    if (reverse) {
        for (List *it = argv->prev; it != argv; it = it->prev) {
            ast = it->value;
            AsmExpression(cc,buf,ast);
            if (AstIsFloatType(ast->type)) AsmPushXMM(buf,0);
            else AsmPush(buf,REG_RAX);
            r += 8;
        }
    } else {
        for (List *it = argv->next; it != argv; it = it->next) {
            ast = it->value;
            AsmExpression(cc,buf,ast);
            if (AstIsFloatType(ast->type)) AsmPushXMM(buf,0);
            else AsmPush(buf,REG_RAX);
            r += 8;
        }
    }
    return align(r,8);
}

void AsmPopIntArgs(aoStr *buf, List *argv) {
    int count = ListCount(argv);
    for (int i = count - 1; i >= 0; --i) {
        AsmPop(buf, REGISTERS[i]);
    }
}

void AsmPopFloatArgs(aoStr *buf, List *argv) {
    int count = ListCount(argv);
    for (int i = count - 1; i >= 0; --i) {
        AsmPopXMM(buf,i);
    }
}

#define FUN_EXISTS 0x1
#define FUN_EXTERN 0x2
#define FUN_VARARG 0x4
void AsmPrepFuncCallArgs(Cctrl *cc, aoStr *buf, Ast *funcall) {
    int int_cnt,float_cnt,stack_cnt,stackoffset,needs_padding,var_arg_start,argc,
        flags;
    List *int_args, *float_args, *stack_args, 
         *funparam, *funarg, *params;
    Ast *tmp, *fun, *arg;

    flags = 0;
    fun = DictGetLen(cc->global_env,funcall->fname->data,funcall->fname->len);
    funarg = funparam = params = NULL;

    /* This should exist on the AST */
    if (fun) {
        flags |= FUN_EXISTS;
        if (fun->kind == AST_EXTERN_FUNC) {
            flags |= FUN_EXTERN;
        }
        if (fun->type->has_var_args) {
            flags |= FUN_VARARG;
        }
    }
    /* Extern functions with variable argument lengths need to be 
     * interoperable with c */

    var_arg_start = -1;
    argc = 0;
    if (flags & (FUN_EXISTS|FUN_VARARG) && !(flags & FUN_EXTERN)) {
        ListForEach(fun->params) {
            var_arg_start++;
            arg = it->value;
            if (arg->kind == AST_VAR_ARGS) {
                break;
            }
        }
        var_arg_start += 1;
    }

    /* Get the function parameters, either from the function call or the 
     * function definition. Try to get them from the definition first */
    if (flags & FUN_EXISTS) {
        if (fun->params != fun->params->next) {
            params = fun->params;
            funparam =  params->next;
        }
    } else {
        if (funcall->paramtypes && funcall->paramtypes != funcall->paramtypes->next) {
            params = funcall->paramtypes;
            funparam = params->next;
        }
    }
        
    if (funcall->args != funcall->args->next) {
        funarg = funcall->args->next;
    }

    int_cnt = float_cnt = stack_cnt = stackoffset = 0;
    
    int_args = ListNew();
    float_args = ListNew();
    stack_args = ListNew();

    while (1) {
        if (funarg != NULL) {
            tmp = funarg->value;
            if (tmp->kind == AST_PLACEHOLDER) {
                loggerDebug("place holder: %s\n",AstToString(funparam->value));
                tmp = ((Ast *)funparam->value)->declinit;
                if (tmp == NULL) {
                    loggerPanic("Default parameter not provided for function call: %s()\n",
                            funcall->fname->data);
                }
            }
        } else if (funparam != NULL) {
            /* Handling default function parameters, these can only come at
             * the end of a function call presently */
            tmp = ((Ast *)funparam->value)->declinit;
        } else {
            break;
        }

        if (tmp == NULL) break;

        /**
         * @SystemV - this below is incompatible
         * our var args are all placed on the stack after 'argc' */
        if (flags & FUN_VARARG && 
            !(flags & FUN_EXTERN) && 
            var_arg_start == argc)
        {
            ListAppend(stack_args,tmp);
            stack_cnt++;
        } else if (AstIsFloatType(tmp->type)) {
            argc++;
            if (float_cnt < 8) {
                ListAppend(float_args,tmp);
                float_cnt++;
            }
            else {
                ListAppend(stack_args,tmp);
                stack_cnt++;
            }
        } else {
            argc++;
            if (int_cnt < 6) {
                ListAppend(int_args,tmp);
                int_cnt++;
            }
            else {
                ListAppend(stack_args,tmp);
                stack_cnt++;
            }
        }

        /* Handling the case for either more arguments than parameters or
         * more parameters than arguments */
        if (funarg != NULL && funarg->next != funcall->args) {
            funarg = funarg->next;
        } else {
            funarg = NULL;
        }

        if (funparam != NULL && funparam->next != params) {
            funparam = funparam->next;
        } else {
            funparam = NULL;
        }
    }

    stack_cnt *= 8;
    aoStrCatPrintf(buf, "# %s(s: %d, i:%d f:%d) sp: %d\n\t",
            funcall->fname->data,stack_cnt,int_cnt*8,float_cnt*8,stack_pointer);
    needs_padding = (stack_cnt+stack_pointer) %16;

    if (needs_padding) {
        aoStrCatPrintf(buf, "subq   $%d, %%rsp #stack pad\n\t", 8);
        stack_pointer += 8;
    }

    if (funcall->kind == AST_FUNPTR_CALL) {
        switch (funcall->ref->kind) {
            case AST_CLASS_REF:
                AsmLoadClassRef(cc,buf,funcall->ref->cls,funcall->ref->type,0);
                break;

            case AST_FUNPTR:
            case AST_LVAR:
                AsmLLoad(buf,funcall->ref->type,funcall->ref->loff);
                break;

            case AST_GVAR:
                AsmGLoad(buf,funcall->ref->type,funcall->ref->gname,0);
                break;
        }
        aoStrCatPrintf(buf,"movq    %%rax,%%r11\n\t");
    }

    stackoffset = AsmPlaceArgs(cc,buf,stack_args,1);
    AsmPlaceArgs(cc,buf,int_args,0);
    AsmPlaceArgs(cc,buf,float_args,0);

    AsmPopFloatArgs(buf,float_args);
    AsmPopIntArgs(buf,int_args);

    if (float_cnt) {
        aoStrCatPrintf(buf, "movl   $%d, %%eax\n\t", float_cnt);
    }

    if (funcall->kind == AST_FUNPTR_CALL) {
        aoStrCatPrintf(buf, "call    *%%r11\n\t");
    } else {
        AsmCall(buf, funcall->fname->data);
    }

    if (float_cnt) {
        aoStrCatPrintf(buf, "movl   $%d, %%eax\n\t", float_cnt);
    }

    if (stack_cnt) {
        stack_pointer -= stack_cnt;
        if (needs_padding) {
            stack_cnt += 8;
            stack_pointer += 8;
        }
        aoStrCatPrintf(buf, "addq   $%d, %%rsp #rewind stack\n\t", stack_cnt);
    } else if (needs_padding) {
        aoStrCatPrintf(buf, "addq   $%d, %%rsp #stack add\n\t", 8);
        stack_pointer -= 8;
    }

    ListRelease(int_args,NULL);
    ListRelease(float_args,NULL);
    ListRelease(stack_args,NULL);
}

/* Function call for ASM, PTR and normal functions */
void AsmFunCall(Cctrl *cc, aoStr *buf, Ast *ast) {
    AsmPrepFuncCallArgs(cc,buf,ast);
}

void AsmExpression(Cctrl *cc, aoStr *buf, Ast *ast) {
    List *it;
    aoStr *label_begin, *label_end;

    switch (ast->kind) {
    case AST_LITERAL: {
        switch (ast->type->kind) {
        case AST_TYPE_CHAR:
        case AST_TYPE_INT:
            if (ast->type->issigned) {
                aoStrCatPrintf(buf, "movq   $%lld, %%rax\n\t", ast->i64);
            } else if (!ast->type->issigned) {
                aoStrCatPrintf(buf, "movq   $%lu, %%rax\n\t",
                        (unsigned long)ast->i64);
            }
            break;
        case AST_TYPE_FLOAT: {
            char *f64_label = NULL;
            if (ast->f64_label) {
                f64_label = ast->f64_label->data;
            }
            if (!f64_label) {
                ast->f64_label = AstMakeLabel();
                f64_label = ast->f64_label->data;
                double f = ast->f64;
                AsmRemovePreviousTab(buf);
                aoStrCatPrintf(buf, ".data\n %s:\n\t"
                                    ".quad 0x%lX #%.9f\n"
                                    ".text\n\t",f64_label,
                                    ieee754(f),f);
            }
            aoStrCatPrintf(buf, "movsd    %s(%%rip), %%xmm0\n\t", f64_label);
            break;
        }
        default:
            loggerPanic("Unknown literal type: %s\n",
                    AstKindToString(ast->kind));
        }
        break;
    }

    case AST_STRING:
        aoStrCatPrintf(buf, "leaq   %s(%%rip), %%rax\n\t", ast->slabel->data);
        break;
    
    case AST_LVAR:
        AsmLLoad(buf,ast->type,ast->loff);
        break;
    
    case AST_GVAR:
        AsmGLoad(buf,ast->type,ast->glabel, 0);
        break;

    case AST_FUNPTR_CALL:
    case AST_ASM_FUNCALL:
    case AST_FUNCALL: {
        AsmFunCall(cc,buf,ast);
        break;
    }

    case AST_DEFAULT_PARAM:
        AsmExpression(cc,buf,ast->declvar);
        break;
    
    case AST_DECL: {
        if (!ast->declinit) {
            return;
        }


        if (ast->declinit->kind == AST_ARRAY_INIT) {
            int offset = 0;
            Ast *tmp;
            it = ast->declinit->arrayinit->next;
            while (it != ast->declinit->arrayinit) {
                tmp = it->value;
                switch (tmp->type->kind) {
                case AST_TYPE_CHAR:
                case AST_TYPE_INT:
                    if (tmp->type->issigned) {
                        aoStrCatPrintf(buf, "movq   $%lld, %%rax\n\t", 
                                tmp->i64);
                    } else if (!tmp->type->issigned) {
                        aoStrCatPrintf(buf, "movq   $%lu, %%rax\n\t",
                                (unsigned long)tmp->i64);
                    }
                    break;
                // case AST_TYPE_FLOAT:
                default:
                    AsmExpression(cc,buf,it->value);
                    break;
                }


                AsmLSave(buf,ast->declvar->type->ptr,
                        ast->declvar->loff+offset);

                offset += ast->declvar->type->ptr->size;
                it = it->next;
            }
        } else if (ast->declvar->type->kind == AST_TYPE_ARRAY) {
            assert(ast->declinit->kind == AST_STRING);
            AsmPlaceString(buf,ast->declinit->sval,ast->declvar->loff);
        } else if (ast->declinit->kind == AST_STRING) {
            AsmGLoad(buf, ast->declinit->type, ast->declinit->slabel, 0);
            AsmLSave(buf, ast->declvar->type, ast->declvar->loff);
        } else if (ast->declinit->kind == AST_FUNC) {
            AsmBinOpFunctionAssign(cc,buf,ast->declvar,ast->declinit);
        } else {
            AsmExpression(cc,buf, ast->declinit);
            AsmLSave(buf, ast->declvar->type, ast->declvar->loff);
        }
        return;
    }

    case AST_FUNPTR:
        aoStrCatPrintf(buf, "movq    %d(%%rbp), %%rax\n\t", ast->loff);
        break;

    case AST_ADDR:
        AsmAddr(cc,buf,ast);
        break;

    case AST_CAST:
        AsmTypeCast(cc,buf,ast);
        break;

    case AST_DEREF: {
        assert(ast->operand->type->kind == AST_TYPE_POINTER);
        AsmExpression(cc,buf,ast->operand);

        AstType *op_type =  ast->operand->type;
        AstType *result = ast->type;
        char *reg;
    
        if (op_type->kind == AST_TYPE_POINTER &&
                op_type->ptr->kind == AST_TYPE_ARRAY) {
            return;
        }


        reg = "rcx"; // AsmGetIntReg(result,'c');

        aoStrCatPrintf(buf, "##__DEREF_START: %s\n\t", AstKindToString(result->kind));
        if (result->size < 4) {
            aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t"
                                "movzbl (%%%s), %%eax\n\t"
                                "movsbl  %%al, %%eax\n\t", reg); 
        } else {
            if (result->kind == AST_ADDR) {
                aoStrCatPrintf(buf,"movq   %%%s, %%rax\n\t", reg);
            } else if (result->kind == AST_TYPE_CLASS) {
                aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t"
                        "leaq (%%%s), %%rax\n\t", reg);
            } else if (result->kind != AST_TYPE_POINTER) {
                aoStrCatPrintf(buf,"# deref not ptr start: %s %s\n\t", AstKindToString(result->kind), AstKindToString(op_type->kind));
               // aoStrCatPrintf(buf,"movq   (%%%s), %%rax\n\t", reg);

                if (result->kind == AST_TYPE_FLOAT) {
                    aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t"
                                        "movq (%%%s), %%xmm0\n\t", reg);
                } else {
                    aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t"
                                        "movq (%%%s), %%rax\n\t", reg);
                }
                aoStrCatPrintf(buf,"# deref not ptr start\n\t");
            } else if (result->kind == AST_TYPE_POINTER) {
                reg = AsmGetIntReg(result,'a');
                char *mov = AsmGetMov(result);
                switch (result->ptr->kind) {
                    case AST_TYPE_CHAR:
                        aoStrCatPrintf(buf,"# deref char start\n\t");
                        aoStrCatPrintf(buf,"%s   (%%%s), %%rax\n\t",mov, reg);
                        aoStrCatPrintf(buf,"# deref char end\n\t");
                        break;
                    case AST_TYPE_INT:
                        aoStrCatPrintf(buf,"# deref int start\n\t");
                        aoStrCatPrintf(buf,"leaq   (%%%s), %%rax\n\t", reg);
                        aoStrCatPrintf(buf,"# deref int end\n\t");
                        break;
                    case AST_TYPE_POINTER:
                        aoStrCatPrintf(buf,"# deref ptr start\n\t");
                        aoStrCatPrintf(buf,"movq   %%%s, %%rax\n\t", reg);
                        aoStrCatPrintf(buf,"# deref ptr end\n\t");
                        break;
                    case AST_TYPE_CLASS:
                        aoStrCatPrintf(buf,"# deref class start\n\t");
                        aoStrCatPrintf(buf,"movq   (%%%s), %%rax\n\t", reg);
                        aoStrCatPrintf(buf,"# deref class end\n\t");
                        break;
                    default:
                        /*XXX: Does dereferencing a class make sense? 
                         * It seg faults usually */
                        if (ast->cls->cls->kind == AST_CLASS_REF) {
                            aoStrCatPrintf(buf,"# problematic class deref\n\t");
                            aoStrCatPrintf(buf,"movq   (%%%s), %%rax\n\t", reg);
                        }
                        break;
                }
            }
        }
        aoStrCatPrintf(buf, "##__DEREF_END\n\t");
        break;
    }

    case AST_IF: {
        AsmExpression(cc,buf,ast->cond);
        label_begin = AstMakeLabel();
        aoStrCatPrintf(buf,
                "test   %%rax, %%rax\n\t"
                "je     %s\n\t", label_begin->data);
        AsmExpression(cc,buf,ast->then);
        if (ast->els) {
            label_end = AstMakeLabel();
            aoStrCatPrintf(buf,
                    "jmp    %s\n"
                    "%s:\n\t"
                    ,label_end->data,
                    label_begin->data);
            AsmExpression(cc,buf,ast->els);
            AsmRemovePreviousTab(buf);
            aoStrCatPrintf(buf, "%s:\n\t", label_end->data);
        } else {
            AsmRemovePreviousTab(buf);
            aoStrCatPrintf(buf, "%s:\n\t", label_begin->data);
        }
        break;
    }

    case AST_JUMP:
         aoStrCatPrintf(buf, "jmp    %s\n\t", ast->jump_label->data);
         break;

    case AST_GOTO:
    case AST_BREAK:
    case AST_CONTINUE:
        aoStrCatPrintf(buf, "jmp    %s\n\t", ast->slabel->data);
        break;

    case AST_LABEL:
        AsmRemovePreviousTab(buf);
        if (ast->sval) {
            aoStrCatPrintf(buf, "%s:\n\t", ast->sval->data);
        } else {
            aoStrCatPrintf(buf, "%s:\n\t", ast->slabel->data);
        }
        break;

    case AST_DO_WHILE: {
        AsmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->while_begin->data);
        if (ast->whilebody) {
            AsmExpression(cc,buf, ast->whilebody);
        }
        AsmExpression(cc,buf,ast->whilecond);
        aoStrCatPrintf(buf,
                "test   %%rax, %%rax\n\t"
                "je     %s\n\t",
                ast->while_end->data);

        aoStrCatPrintf(buf,
                "jmp    %s\n"
                "%s:\n\t"
                ,ast->while_begin->data,
                ast->while_end->data);
        break;
    }

    case AST_WHILE: {
        AsmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->while_begin->data);
        AsmExpression(cc,buf,ast->whilecond);
        aoStrCatPrintf(buf,
                "test   %%rax, %%rax\n\t"
                "je     %s\n\t",
                ast->while_end->data);
        if (ast->whilebody) {
            AsmExpression(cc,buf, ast->whilebody);
        }
        aoStrCatPrintf(buf,
                "jmp    %s\n"
                "%s:\n\t"
                ,ast->while_begin->data,
                ast->while_end->data);
        break;
    }

    case AST_FOR: {
        /* Do not emit assembly if there is no body in the loop */
        if (!ast->forbody) {
            break;
        }
        if (ast->forinit) {
            AsmExpression(cc,buf,ast->forinit);
        }

        AsmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->for_begin->data);

        if (ast->forcond) {
            AsmExpression(cc,buf,ast->forcond);
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "je     %s\n\t",
                    ast->for_end->data);
        }

        AsmExpression(cc,buf, ast->forbody);

        /* To help with continue statements */
        AsmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->for_middle->data);

        if (ast->forstep) {
            AsmExpression(cc,buf,ast->forstep);
        }

        aoStrCatPrintf(buf,
                "jmp    %s\n"
                "%s:\n\t"
                ,ast->for_begin->data,
                ast->for_end->data);
        break;
    }
    
    case AST_RETURN:
        if (ast->retval == NULL) {
            aoStrCatPrintf(buf, "leave\n\tret\n");
            break;
        }
        AsmExpression(cc,buf, ast->retval);
        AsmSaveConvert(buf,ast->type,ast->retval->type);
        aoStrCatPrintf(buf, "leave\n\tret\n");
        break;

    case AST_COMPOUND_STMT:
        it = ast->stms->next;
        while (it != ast->stms) {
            AsmExpression(cc,buf,it->value);
            it = it->next;
        }
        break;

    case AST_CLASS_REF:
        AsmLoadClassRef(cc,buf,ast->cls,ast->type,0);
        break;

    case AST_CASE:
        AsmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->case_label->data);
        break;


    case TK_PRE_PLUS_PLUS: {
        if (ast->type->kind == AST_TYPE_FLOAT) {
            AsmExpression(cc,buf,ast->operand);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "addsd   %%xmm1, %%xmm0\n\t");
            AsmAssign(cc,buf,ast->operand);
        } else {
            AsmPreIncrDecr(cc,buf,ast,"addq");
        }
        break;
     }

    case TK_PLUS_PLUS: {
        if (ast->type->kind == AST_TYPE_FLOAT) {
            AsmExpression(cc,buf,ast->operand);
            AsmPushXMM(buf,0);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "addsd   %%xmm1, %%xmm0\n\t");
            AsmAssign(cc,buf,ast->operand);
            AsmPopXMM(buf,0);
        } else {
            AsmIncrDecr(cc,buf,ast,"addq");
        }
        break;
    }

    case TK_PRE_MINUS_MINUS:
        if (ast->type->kind == AST_TYPE_FLOAT) {
            AsmExpression(cc,buf,ast->operand);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "subsd   %%xmm1, %%xmm0\n\t");
            AsmAssign(cc,buf,ast->operand);
        } else {
            AsmPreIncrDecr(cc,buf,ast,"subq");
        }
        break;

    case TK_MINUS_MINUS:
        if (ast->type->kind == AST_TYPE_FLOAT) {
            AsmExpression(cc,buf,ast->operand);
            AsmPushXMM(buf,0);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "subsd   %%xmm1, %%xmm0\n\t");
            AsmAssign(cc,buf,ast->operand);
            AsmPopXMM(buf,0);
        } else {
            AsmIncrDecr(cc,buf,ast,"subq");
        }
        break;

    case '!': {
        AsmExpression(cc,buf, ast->operand);
        aoStrCatPrintf(buf,
                "cmp    $0, %%rax\n\t"
                "sete   %%al\n\t"
                "movzb  %%al, %%rax\n\t");
        break;
    }

    case '~': {
        AsmExpression(cc,buf, ast->operand);
        aoStrCatPrintf(buf, "not    %%rax\n\t");
        break;
    }

    case TK_OR_OR: {
        aoStrCat(buf,"# OR OR Start\n\t");
        label_end = AstMakeLabel();
        AsmExpression(cc,buf,ast->left);

        if (ast->type->kind == AST_TYPE_FLOAT) {
            AsmExpression(cc,buf,ast->left);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "movq    $1, %%rax\n\t"
                    "jne     %s\n\t", label_end->data);
            AsmExpression(cc,buf,ast->right);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "movq    $1, %%rax\n\t"
                    "jne     %s\n\t"
                    "movq    $0, %%rax\n\t"
                    "%s:\n\t", label_end->data, label_end->data);
        } else {
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "movq   $1, %%rax\n\t"
                    "jne    %s\n\t", label_end->data);
            AsmExpression(cc,buf,ast->right);
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "movq   $1, %%rax\n\t"
                    "jne    %s\n\t"
                    "movq   $0, %%rax\n\t"
                    "%s:\n\t", label_end->data, label_end->data);
        }
        aoStrCat(buf,"# OR OR End\n\t");
        break;
    }

    case TK_AND_AND: {
        aoStrCat(buf,"# AND AND Start\n\t");
        label_end = AstMakeLabel();

        if (ast->type->kind == AST_TYPE_FLOAT) {
            AsmExpression(cc,buf,ast->left);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "je    %s\n\t", label_end->data);
            AsmExpression(cc,buf,ast->right);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "movq    $0, %%rax\n\t"
                    "je     %s\n\t"
                    "movq    $1, %%rax\n\t"
                    "%s:\n\t", label_end->data, label_end->data);
        } else {
            AsmExpression(cc,buf,ast->left);
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "movq   $0, %%rax\n\t"
                    "je    %s\n\t", label_end->data);
            AsmExpression(cc,buf,ast->right);
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "movq   $0, %%rax\n\t"
                    "je     %s\n\t"
                    "movq   $1, %%rax\n\t"
                    "%s:\n\t", label_end->data, label_end->data);
        }
        aoStrCat(buf,"# AND AND End\n\t");
        break;
    }

    case '-': {
      if (ast->right == NULL) {
          AsmExpression(cc,buf, ast->left);
          if (ast->left->type->kind == AST_TYPE_FLOAT) {
              aoStrCatPrintf(buf,
                      "movsd   sign_bit(%%rip), %%xmm1\n\t"
                      "xorpd   %%xmm1, %%xmm0\n\t");
          } else {
              aoStrCatPrintf(buf,
                      "not    %%rax\n\t"
                      "addq    $1, %%rax\n\t");
          }
      } else {
          AsmBinaryOp(cc,buf,ast);
      }
      break;
    }

    default:
        AsmBinaryOp(cc,buf,ast);
        break;
    }
}

void AsmDataInternal(aoStr *buf, Ast *data) {
    assert(data->type->kind != AST_TYPE_ARRAY);
    switch (data->type->size) {
        case 1: aoStrCatPrintf(buf, ".byte %d\n", data->i64); break;
        case 4: aoStrCatPrintf(buf, ".long %d\n", data->i64); break;
        case 8: aoStrCatPrintf(buf, ".quad %d\n", data->i64); break;
        default:
            loggerPanic("Cannot create size information for: %s\n",
                    AstToString(data));
    }
}

void AsmGlobalVar(Dict *seen_globals, aoStr *buf, Ast* ast) {
    Ast *declvar = ast->declvar;
    Ast *declinit = ast->declinit;
    aoStr *varname = declvar->gname;

    if (DictGetLen(seen_globals,varname->data,varname->len)) {
        return;
    }

    DictSet(seen_globals,varname->data,ast);

    if (declinit &&
        (declinit->kind == AST_ARRAY_INIT || 
         declinit->kind == AST_LITERAL))
    {
        if (!declvar->type->is_static) {
            aoStrCatPrintf(buf,".global %s\n",varname->data);
        }
        aoStrCatPrintf(buf,"%s:\n",varname->data);

        if (declinit->kind == AST_ARRAY_INIT) {
            ListForEach(declinit->arrayinit) {
                AsmDataInternal(buf,it->value);
            }
            return;
        } else {
            assert(declinit->kind == AST_LITERAL && 
                    AstIsIntType(declinit->type));
            AsmDataInternal(buf,declinit);
        }
    } else {
        aoStrCatPrintf(buf, ".lcomm %s, %d\n\t", varname->data,
                declvar->type->size);
    }
}

void AsmDataSection(Cctrl *cc, aoStr *buf) {
    List *it;
    Ast *ast;
    char *label, *str, *ptr;
    aoStr *escaped;
    
    it = cc->strings->next;

    aoStrCatLen(buf, ".data\n", 6);
    aoStrCatPrintf(buf, "sign_bit:\n\t.quad 0x8000000000000000\n");
    aoStrCatPrintf(buf, "one_dbl:\n\t.double 1.0\n");

    while (it != cc->strings) {
        ast = (Ast *)it->value;
        assert(ast->kind == AST_STRING);

        label = ast->slabel->data;
        str = ast->sval->data;
        escaped = aoStrNew();

        ptr = str;
        while (*ptr) {
            switch (*ptr) {
                case '\\': aoStrCatPrintf(escaped,"\\"); break;
                case '\n': aoStrCatPrintf(escaped,"\\n"); break;
                case '\t': aoStrCatPrintf(escaped,"\\t"); break;
                case '\r': aoStrCatPrintf(escaped,"\\r"); break;
                case '\b': aoStrCatPrintf(escaped,"\\n"); break;
                case '\v': aoStrCatPrintf(escaped,"\\v"); break;
                default: aoStrPutChar(escaped,*ptr); break;
            }
            ptr++;
        }

        aoStrCatPrintf(buf,"%s:\n\t",label);
        aoStrCatPrintf(buf, ".string \"%s\\0\"\n", escaped->data);
        aoStrRelease(escaped);
        it = it->next;
    }
    aoStrPutChar(buf,'\t');
} 

void AsmStoreParam(aoStr *buf, int *_ireg, int *_arg, int offset) {
    int arg = *_arg, ireg = *_ireg;
    if (ireg == 6) {
        aoStrCatPrintf(buf,
                "movq   %d(%%rbp),%%rax\n\t"
                "movq    %%rax, %d(%%rbp)\n\t",
                arg++ * 8, -offset);
        *_arg = arg;
    } else {
        aoStrCatPrintf(buf, "movq   %%%s, %d(%%rbp)\n\t",
                REGISTERS[ireg++], -offset);
        *_ireg = ireg;
    }
}
void AsmStoreParamFloat(aoStr *buf, int *_freg, int *_arg, int offset) {
    int arg = *_arg, freg = *_freg;
    if (freg == 9) {
        aoStrCatPrintf(buf,
                "movq   %d(%%rbp),%%rax\n\t"
                "movq    %%rax, %d(%%rbp)\n\t",
                arg++ * 8, -offset);
        *_arg = arg;
    } else {
        aoStrCatPrintf(buf, "movq   %%%s, %d(%%rbp)\n\t",
                FLOAT_REGISTERS[freg++], -offset);
        *_freg = freg;
    }
}

void AsmGetRegisterCounts(List *params, int *ireg, int *freg) {
    Ast *param;
    ListForEach(params) {
        param = (Ast*)it->value;
        if (AstIsFloatType(param->type)) (*freg)++;
        else                             (*ireg)++;
    }
}

int AsmSaveRegisters(Cctrl *cc, aoStr *buf) {
    static const int save_size = 176;
    aoStrCatPrintf(buf,
            "subq    $%d, %%rsp\n\t"
            "movq    %%rdi, (%%rsp)\n\t"
            "movq    %%rsi, 8(%%rsp)\n\t"
            "movq    %%rdx, 16(%%rsp)\n\t"
            "movq    %%rcx, 24(%%rsp)\n\t"
            "movq    %%r8, 32(%%rsp)\n\t"
            "movq    %%r9, 40(%%rsp)\n\t"
            "movaps  %%xmm0, 48(%%rsp)\n\t"
            "movaps  %%xmm1, 64(%%rsp)\n\t"
            "movaps  %%xmm2, 80(%%rsp)\n\t"
            "movaps  %%xmm3, 96(%%rsp)\n\t"
            "movaps  %%xmm4, 112(%%rsp)\n\t"
            "movaps  %%xmm5, 128(%%rsp)\n\t"
            "movaps  %%xmm6, 144(%%rsp)\n\t"
            "movaps  %%xmm7, 160(%%rsp)\n\t",
            save_size);
    return save_size;
}

int AsmFunctionInit(Cctrl *cc, aoStr *buf, Ast *func) {
    int offset = 0;
    int ireg = 0, freg = 0, locals = 0, arg = 2;
    Ast *ast_tmp = NULL;
    char *fname = NULL;

    fname = AsmNormaliseFunctionName(func->fname->data);

    aoStrCatPrintf(buf, ".text"            "\n\t"
                        ".global %s\n"
                        "%s:\n\t"
                        "push   %%rbp"       "\n\t"
                        "movq   %%rsp, %%rbp" "\n\t",
                        fname, fname);


    int new_offset = 0;
    /* Now assign offsets */
    ListForEach(func->locals) {
        ast_tmp = it->value;
        /* Calculate how much stackspace is required for locals */
        locals += align(ast_tmp->type->size, 8);
        new_offset -= ast_tmp->type->size;
        switch (ast_tmp->kind) {
            case AST_DEFAULT_PARAM:
                ast_tmp->declvar->loff = new_offset;
                break;
            default:
                ast_tmp->loff = new_offset;
                break;
        }
    }

    ListForEach(func->params) {
        ast_tmp = it->value;
        switch (ast_tmp->kind) {
        case AST_DEFAULT_PARAM:
            locals += align(ast_tmp->declvar->type->size, 8);
            break;
        case AST_VAR_ARGS:
            locals += align(ast_tmp->argc->type->size, 8);
            locals += VAR_ARG_MAX*8+212; /* allocate a huge amount off the stack */
            break;
        default:
            locals += align(ast_tmp->type->size, 8);
            break;
        }
    }

    int stack_space = 0;
    if (locals) {
        stack_space = align(locals, 16);
        aoStrCatPrintf(buf, "subq   $%d, %%rsp #STACK LOCAL COUNT %d\n\t", 
                stack_space, ListCount(func->locals));
        stack_pointer = stack_space;
    }

    /* We can use register arguments */
    offset = stack_space;
    ListForEach(func->params) {
        ast_tmp = it->value;

        if (ast_tmp->kind != AST_VAR_ARGS && AstIsFloatType(ast_tmp->type)) {
            AsmStoreParamFloat(buf,&freg,&arg,offset);
        } else {
            AsmStoreParam(buf,&ireg,&arg,offset);
        }
        switch (ast_tmp->kind) {
            case AST_DEFAULT_PARAM:
                ast_tmp->declvar->loff = -offset;
                offset -= align(ast_tmp->declvar->type->size, 8);
                break;

            case AST_VAR_ARGS:
                ast_tmp->argc->loff = -offset;
                offset -= align(ast_tmp->argc->type->size, 8);
                /* XXX: varargs limited to 4 */
                ast_tmp->argv->loff = align(16,8);
                for (int i = ireg; i < 6; ++i) {
                    AsmStoreParam(buf,&ireg,&arg,offset);
                    offset -= 8;
                }
                for (int i = freg; i < 8; ++i) {
                    AsmStoreParamFloat(buf,&freg,&arg,offset);
                    offset -= 8;
                }
                break;

            default:
                ast_tmp->loff = -offset;
                offset -= align(ast_tmp->type->size, 8);
                break;
        }
    }
    offset = stack_space;
    return stack_space;
}

void AsmFunctionLeave(aoStr *buf) {
    aoStrCatPrintf(buf,"leave\n\tret\n");
}

int AsmHasRet(aoStr *buf) {
    static char *match = "leave\n\tret\n";
    int len = 11;
    int curlen = buf->len-1;
    int j = len-1;

    for (int i = 0; i < len; ++i) {
        if (buf->data[curlen] != match[j]) {
            return 0;
        }
        --j;
        --curlen;
    }
    return 1;
}

void AsmFunction(Cctrl *cc, aoStr *buf, Ast *func) {
    assert(func->kind == AST_FUNC);
    cc->stack_local_space = AsmFunctionInit(cc,buf,func);
    AsmExpression(cc,buf,func->body);
    if (!AsmHasRet(buf)) {
        AsmFunctionLeave(buf);
    }
}

void AsmPasteAsmBlocks(aoStr *buf, Cctrl *cc) {
    List *it = cc->asm_blocks->next;
    List *func_it;
    Ast *asm_block, *asm_func;

    while (it != cc->asm_blocks) {
        asm_block = (Ast *)it->value;

        func_it = asm_block->funcs->next;

        while (func_it != asm_block->funcs) {
            asm_func = func_it->value;
                aoStrCatPrintf(buf, ".global %s\n",
                         asm_func->asmfname->data);

                aoStrCatPrintf(buf,"%s:\n",asm_func->asmfname->data);
                aoStrCatLen(buf,
                        asm_func->body->asm_stmt->data,
                        asm_func->body->asm_stmt->len);
                aoStrPutChar(buf, '\n');


            func_it = func_it->next;
        }
        it = it->next;
    }
}

void AsmInitaliser(Cctrl *cc, aoStr *buf) {
    if (ListEmpty(cc->initalisers)) return;
    char *fname = NULL;
    int locals = 0, stack_space;

#ifdef IS_BSD
    fname = "_main";
#else
    fname = "main";
#endif
    aoStrCatPrintf(buf, ".text"            "\n\t"
                        ".global %s\n"
                        "%s:\n\t"
                        "push   %%rbp"       "\n\t"
                        "movq   %%rsp, %%rbp" "\n\t",
                        fname, fname);
    /* Calculate how much stackspace is required for locals */
    ListForEach(cc->initaliser_locals) {
        Ast *ast_tmp = it->value;
        locals += align(ast_tmp->type->size, 8);
    }

    int new_offset = 0;
    /* Now take chunks out of the total size */
    ListForEach(cc->initaliser_locals) {
        Ast *ast_tmp = it->value;
        new_offset -= ast_tmp->type->size;
        switch (ast_tmp->kind) {
        default:
            ast_tmp->loff = new_offset;
            break;
        }
    }

    stack_space = align(locals, 16);

    /* This handles command line arguments */
    aoStrCatPrintf(buf,
            "subq   $%d, %%rsp\n\t"
            "movq   %%rdi, argc(%%rip)\n\t"
            "movq   %%rsi, argv(%%rip)\n\t",
            stack_space);
    stack_pointer = stack_space;

    ListForEach(cc->initalisers) {
        AsmExpression(cc,buf,it->value);
    }
    aoStrCatPrintf(buf, "leave\n\tret\n");
}

/* Create assembly */
aoStr *AsmGenerate(Cctrl *cc) {
    Ast *ast;
    Dict *seen_globals = DictNew(&default_table_type);
    aoStr *asmbuf = aoStrAlloc(1<<10);
 
    if (!ListEmpty(cc->initalisers)) {
        has_initialisers = 1;
    }
    AsmPasteAsmBlocks(asmbuf,cc);
    AsmDataSection(cc,asmbuf);


    ListForEach(cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            AsmFunction(cc,asmbuf,ast);
        } else if (ast->kind == AST_DECL || ast->kind == AST_GVAR) {
            AsmGlobalVar(seen_globals,asmbuf,ast);
        } else if (ast->kind == AST_ASM_STMT) {
           // aoStrCatLen(asmbuf, ast->asm_stmt->data, ast->asm_stmt->len);
           // loggerDebug("%s\n", ast->asm_stmt->data);
        } else if (ast->kind == AST_ASM_FUNCDEF || ast->kind == AST_ASM_FUNC_BIND || ast->kind == AST_EXTERN_FUNC) {
           // aoStrCatPrintf(asmbuf, ".text\n\t.global %s\n\t", 
           //         ast->asmfname->data);
        } else {
            loggerPanic("Cannot handle: %s\n", AstToString(ast));
        }
    }
    if (!ListEmpty(cc->initalisers)) {
        AsmInitaliser(cc,asmbuf);
    }
    return asmbuf;
}
