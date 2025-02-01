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
#include "lexer.h"
#include "list.h"
#include "map.h"
#include "prsutil.h"
#include "util.h"
#include "version.h"

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

void asmExpression(Cctrl *cc, aoStr *buf, Ast *ast);

#define asmGetGlabel(gvar) \
    gvar->is_static ? gvar->glabel->data : gvar->gname->data

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
char *asmNormaliseFunctionName(char *fname) {
    aoStr *newfn = astNormaliseFunctionName(fname);
    if (!strncasecmp(fname, "Main", 4)) {
        if (has_initialisers) {
            aoStrCatPrintf(newfn,"Fn");
        } else {
            aoStrToLowerCase(newfn);
        }
    }
    return aoStrMove(newfn);
}

void asmRemovePreviousTab(aoStr *buf) {
    if (buf->data[buf->len-1] == '\t') {
        buf->len--;
    }
}

char *asmGetMov(AstType *type) {
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

char *asmGetMovWithSign(AstType *type) {
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

char *asmGetPtrMove(AstType *type) {
    switch (type->kind) {
        case AST_TYPE_ARRAY: return "leaq";
        default:             return "mov";
    }
}

char *asmGetLoadReg(AstType *type, char ch) {
    (void)type;
    if (ch == 'a') return "rax";
    else           return "rcx";
}

char *asmGetLoadMov(AstType *type) {
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
                    astTypeToString(type), type->size);
    }
}

char *asmGetIntReg(AstType *type, char ch) {
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
                        ch, astTypeToString(type), type->size);
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
                        astTypeToString(type), type->size);
        }
    } else {
        loggerPanic("unknown register prefix: %c\n", ch);
    }
}

void asmToInt(aoStr *buf, AstType *type) {
    if (type->kind == AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf, "cvttsd2si  %%xmm0, %%rax\n\t");
    }
}

void asmToFloat(aoStr *buf, AstType *type) {
    if (type->kind != AST_TYPE_FLOAT) {
        if (type->has_var_args) {
            aoStrCatPrintf(buf, "movq    %%rax, %%xmm0\n\t");
        } else {
            aoStrCatPrintf(buf, "cvtsi2sd  %%rax, %%xmm0\n\t");
        }
    }
}

void asmPushXMM(aoStr *str, int reg) {
    aoStrCatPrintf(str, "sub   $8, %%rsp\n\t"
                        "movsd %%xmm%d, (%%rsp)\n\t", reg);
    stack_pointer += 8;
}

void asmPopXMM(aoStr *str, int reg) {
    aoStrCatPrintf(str, "movsd  (%%rsp), %%xmm%d\n\t"
                        "add    $8, %%rsp\n\t", reg);
    stack_pointer -= 8;
    if (stack_pointer < 0) {
        loggerPanic("Stack underflow\n");
    }
}

void asmPush(aoStr *buf, char *reg) {
    aoStrCatPrintf(buf,"push   %%%s\n\t", reg);
    stack_pointer += 8;
}

void asmPop(aoStr *buf, char *reg) {
    aoStrCatPrintf(buf,"pop    %%%s\n\t", reg);
    stack_pointer -= 8;
    if (stack_pointer < 0) {
        loggerPanic("Int register stack underflow\n");
    }
}

void asmCall(aoStr *buf, char *fname) {
    char *_fname = asmNormaliseFunctionName(fname);
    aoStrCatPrintf(buf,"call   %s\n\t", _fname);
    free(_fname);
}

/* Save a global variable */
void asmGSave(aoStr *buf, char *name, AstType *type, int offset) {
    assert(type->kind != AST_TYPE_ARRAY);
    char *reg;

    if (type->kind == AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf,"movq   %%xmm0, %%rax\n\t");
    }
    reg = asmGetIntReg(type, 'a');
    if (offset) {
        aoStrCatPrintf(buf, "movq  %%%s, %s+%d(%%rip)\n\t", reg, name, offset);
    } else {
        aoStrCatPrintf(buf, "movq  %%%s, %s(%%rip)\n\t", reg, name);
    }
}

void asmPlaceString(aoStr *buf, aoStr *str, int offset) {
    int i = 0;
    /* Place string on the stack character by character */
    for (; i < (int)str->len; ++i) {
        aoStrCatPrintf(buf,"movb   $%d, %d(%%rbp)\n\t",
                (int)str->data[i], 
                -(offset - i));
    }
    /* Place '\0' */
    aoStrCatPrintf(buf, "movb   $0, %d(%%rbp)\n\t", 
            -(offset - i));
}

void asmGLoad(aoStr *buf, AstType *type, aoStr *label, int offset) {
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

    reg = asmGetIntReg(type, 'a');
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
    if (type->kind == AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf,"movq   %%rax, %%xmm0\n\t");
    }
}

void asmLLoad(aoStr *buf, AstType *type, int offset) {
    char *reg = NULL;

    switch (type->kind) {
        case AST_TYPE_ARRAY:
            aoStrCatPrintf(buf, "# LOAD LEAQ START: %s\n\t", astKindToString(type->kind));
            aoStrCatPrintf(buf, "leaq   %d(%%rbp), %%rax\n\t", offset);
            aoStrCatPrintf(buf, "# LOAD LEAQ END: %s\n\t", astKindToString(type->kind));
            return;
        case AST_TYPE_FLOAT:
            aoStrCatPrintf(buf, "movsd  %d(%%rbp), %%xmm0\n\t", offset);
            return;
        default:
            reg =  asmGetLoadReg(type, 'a');
            aoStrCatPrintf(buf, "# LOAD %s %s START\n\t",reg,astKindToString(type->kind));

            aoStrCatPrintf(buf, "%-4s  %d(%%rbp), %%%s\n\t",
                    asmGetLoadMov(type), offset, reg);
            aoStrCatPrintf(buf, "# LOAD %s END\n\t",reg);
    }
}

void asmLSave(aoStr *buf, AstType *type, int offset) {
    char *reg, *mov;
    switch (type->kind) {
        case AST_TYPE_FLOAT:
            aoStrCatPrintf(buf, "movsd   %%xmm0, %d(%%rbp)\n\t", offset);
            break;
        default:
            reg = asmGetIntReg(type,'a');
            mov = asmGetMov(type);
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

void asmDownCastInt(aoStr *buf, AstType *type, int issigned) {
    switch(type->kind) {
    case AST_TYPE_INT:
        if (issigned) aoStrCatPrintf(buf, "movsx  %%al, %%rax\n\t");
        else          aoStrCatPrintf(buf, "movzx  %%al, %%rax\n\t");
        break;
    }
}

void asmFloatToInt(aoStr *buf, AstType *type) {
    if (type->kind == AST_TYPE_FLOAT)
        aoStrCatPrintf(buf, "cvttsd2si %%xmm0, %%eax\n\t");
}

void asmCast(aoStr *buf, AstType *from, AstType *to) {
    if (from->kind == to->kind && from->size == to->size) return;
    if (astIsIntType(from) && to->kind == AST_TYPE_FLOAT) {
        asmToFloat(buf, from);
    } else if (astIsIntType(from) && astIsIntType(to)) {
        if (to->size > from->size) {
            if (to->is_intrinsic && from->size == 8) return;
            AstUpCastInt(buf, from, to->issigned);
        } else if (to->size < from->size) {
            asmDownCastInt(buf, from, to->issigned);
        }
    }
    else if (astIsIntType(to))
        asmFloatToInt(buf, from);
}

void asmTypeCast(Cctrl *cc, aoStr *buf, Ast *ast) {
    asmExpression(cc,buf,ast->operand);
    asmCast(buf,ast->operand->type,ast->type);
}

void asmAssignDerefInternal(aoStr *buf, AstType *type, int offset) {
    char *reg,*mov;
    aoStrCatPrintf(buf, "# ASSIGN DREF INTERNAL START: %s\n\t",
            astKindToString(type->kind));

    aoStrCatPrintf(buf, "movq   (%%rax), %%rcx #OK mov\n\t");
    asmPop(buf, "rcx");

    reg = asmGetIntReg(type, 'c');
    mov = asmGetMov(type);

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

void asmAssignDeref(Cctrl *cc,aoStr *buf, Ast *ast) {
    asmPush(buf, REG_RAX);
    asmExpression(cc,buf,ast->operand);
    asmAssignDerefInternal(buf,ast->operand->type->ptr, 0);
}

void asmLoadDeref(aoStr *buf, AstType *result, AstType *op_type, int off) {
    char *reg,*ptr_mov;
    if (op_type->kind == AST_TYPE_POINTER &&
            op_type->ptr->kind == AST_TYPE_ARRAY) {
        return;
    }
    aoStrCatPrintf(buf, "# LOAD DEREF: %s %s\n\t",
            astKindToString(result->kind),
            astKindToString(op_type->kind));

    ptr_mov = asmGetPtrMove(op_type);

    if (op_type->kind == AST_TYPE_FLOAT) {
        if (off) aoStrCatPrintf(buf, "movq   %d(%%rax), %%xmm1\n\t", off);
        else     aoStrCatPrintf(buf,"movq   (%%rax), %%xmm1\n\t");
        aoStrCatPrintf(buf,"movsd   %%xmm1, %%xmm0\n\t");
    } else {
        reg = asmGetIntReg(result,'c');
        if (result->size < 4) {
            aoStrCatPrintf(buf, "mov    $0, %%ecx\n\t");
        }

        switch (op_type->kind) {
            case AST_TYPE_CHAR:
                if (off) aoStrCatPrintf(buf,"movzbl   %d(%%rax), %%ecx\n\t", off);
                else     aoStrCatPrintf(buf,"movzbl   (%%rax), %%ecx\n\t");
                aoStrCatPrintf(buf,"movzbl   %%cl, %%ecx\n\t");
                break;
            default:
                if (off) aoStrCatPrintf(buf,"%s   %d(%%rax), %%%s\n\t",  ptr_mov, off, reg);
                else     aoStrCatPrintf(buf,"%s   (%%rax), %%%s\n\t", ptr_mov, reg);
        }


        /* Move back for the next recursive call */
        aoStrCatPrintf(buf,"movq   %%%s, %%rax\n\t", reg);
    }
    aoStrCatPrintf(buf, "# LOAD DEREF END: %s %s\n\t",
            astKindToString(result->kind),
            astKindToString(op_type->kind));
}

void asmPointerArithmetic(Cctrl *cc, aoStr *buf, long op, Ast *LHS, Ast *RHS) {
    //assert(LHS->type->kind == AST_TYPE_POINTER);
    int size;
    char *fn;

    aoStrCatPrintf(buf, "# Pointer Arithmetic start: %s %s\n\t",
            astKindToString(LHS->kind), astKindToString(RHS->kind));
    asmExpression(cc,buf,LHS);
    aoStrCatPrintf(buf, "# Pointer Arithmetic MID\n\t");
    asmPush(buf,REG_RAX);
    asmExpression(cc,buf,RHS);

    /* XXX: What is going on */
    if (LHS->type->ptr->ptr != NULL) {
        if (LHS->type->ptr->kind == AST_TYPE_POINTER) {
            size = 8;
        } else {
            size = LHS->type->ptr->size; 
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
        asmPop(buf,REG_RCX);
        aoStrCatPrintf(buf,
            "%-3s   %%rax, %%rcx\n\t"
            "movq   %%rcx, %%rax\n\t", fn);
    } else {
        asmPop(buf,REG_RCX);
        aoStrCatPrintf(buf,
                "cmpq   %%rax, %%rcx\n\t"
                "%-3s   %%al\n\t"
                "movzb  %%al, %%rax\n\t", fn);
    }
    aoStrCatPrintf(buf, "# Pointer Arithmetic end\n\t");
}

void asmAssignClassRef(Cctrl *cc, aoStr *buf, Ast *cls, AstType *field, int offset) {
    int total_offset;
    switch (cls->kind) {
        case AST_LVAR:
            total_offset = cls->loff + field->offset + offset;
            asmLSave(buf,field,total_offset);
            break;

        case AST_GVAR:
            loggerPanic("Global variables unimplemented: %s\n", astToString(cls));
           // total_offset = field->offset + offset;
           // asmGSave(buf,cls->clsname->data,field,total_offset);
            break;

        case AST_CLASS_REF:
            total_offset = offset + cls->type->offset;
            asmAssignClassRef(cc,buf,cls->cls, field, total_offset);
            break;

        case AST_DEREF:
            aoStrCatPrintf(buf, "\n# Problem >code START %s \n\t",
                    astKindToString(field->kind));
            total_offset = field->offset+offset;

            switch (field->kind) {
                case AST_TYPE_FLOAT:{
                    asmPushXMM(buf,0);
                    asmExpression(cc,buf,cls->operand);
                    asmPopXMM(buf,1);
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
                    asmPush(buf, REG_RAX);
                    asmExpression(cc,buf,cls->operand);
                    asmPop(buf,REG_RCX);
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
                    astToString(cls));
    }
}

void asmLoadClassRef(Cctrl *cc, aoStr *buf, Ast *cls, AstType *field, int offset) {
    int total_offset;
    switch (cls->kind) {
        case AST_LVAR:
            total_offset = cls->loff + field->offset + offset;
            aoStrCatPrintf(buf, "# CLASS LOAD\n\t");
            asmLLoad(buf, field, total_offset);
            break;
        
        case AST_GVAR:
            loggerPanic("Global variables unimplemented: %s\n", astToString(cls));
            //total_offset = field->offset + offset;
            //AsmGLoad(buf,field,cls->clsname,total_offset);
            break;

        case AST_CLASS_REF:
            total_offset = offset + cls->type->offset;
            asmLoadClassRef(cc,buf,cls->cls, field,total_offset);
            break;

        case AST_DEREF:
            total_offset = field->offset+offset;
            asmExpression(cc,buf,cls->operand);
            asmLoadDeref(buf,cls->cls->type,field,total_offset);
            break;
        
        default:
            loggerPanic("Failed to create ASM for: %s\n",
                    astToString(cls));
    }
}

void asmAssign(Cctrl *cc, aoStr *buf, Ast *variable) {
    if (variable->kind == AST_DEFAULT_PARAM) {
        variable = variable->declvar;
    }
    switch (variable->kind) {
    case AST_CAST:
        asmCast(buf,variable->operand->type,variable->type);
        variable->operand->type = astTypeCopy(variable->type);
        asmAssign(cc,buf,variable->operand);
        break;

    case AST_DEREF: 
        asmAssignDeref(cc,buf, variable);
        break;

    case AST_CLASS_REF:
        asmAssignClassRef(cc,buf,variable->cls,variable->type,0);
        break;

    case AST_FUNPTR:
    case AST_LVAR: 
        asmLSave(buf,variable->type,variable->loff);
        break;

    case AST_GVAR: {
        char *label = asmGetGlabel(variable);
        asmGSave(buf,label,variable->type,0);
        break;
    }
    default:
        loggerPanic("Cannot assign: %s\n",
                astToString(variable));
    }
}

void asmCompare(Cctrl *cc, aoStr *buf, char *instruction, Ast *ast) {
    Ast *LHS,*RHS;
    LHS = ast->left;
    RHS = ast->right;
    
    if ((LHS->type->kind == AST_TYPE_FLOAT &&
                RHS->type->kind == AST_TYPE_FLOAT)) {
        asmExpression(cc,buf,LHS);
        asmToFloat(buf,LHS->type);
        asmPushXMM(buf,0);
        asmExpression(cc,buf,RHS);
        asmToFloat(buf,LHS->type);
        asmPopXMM(buf,1);

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
        asmExpression(cc,buf,LHS);
        asmToInt(buf,LHS->type);
        asmPush(buf,REG_RAX);
        asmExpression(cc,buf,RHS);
        asmToInt(buf,RHS->type);
        asmPop(buf,REG_RCX);

        switch (LHS->type->size) {
            case 1: aoStrCatPrintf(buf,"cmp    %%al, %%cl\n\t");break;
            case 2:
            case 4: aoStrCatPrintf(buf,"cmp    %%eax, %%ecx\n\t"); break;
            case 8: aoStrCatPrintf(buf,"cmp    %%rax, %%rcx\n\t"); break;
            default:
                loggerPanic("Cannot compare type with size: %d\n", LHS->type->size);
        }
        aoStrCatPrintf(buf,
                "%-3s   %%al\n\t"
                "movzb  %%al, %%eax\n\t", instruction);
    }
}

void asmBinaryOpIntArithmetic(Cctrl *cc,aoStr *buf, Ast *ast, int reverse) {
    aoStrCatPrintf(buf, "# INt arithmetic START \n\t");
    char *op = NULL;
    Ast *LHS,*RHS;
    LHS = ast->left;
    RHS = ast->right;
    int is_unsinged = 0;
    if (!LHS->type->issigned || !RHS->type->issigned) {
       is_unsinged = 1; 
    }

    int ok = 1;
    ssize_t result = evalIntArithmeticOrErr(ast,&ok);
    if (!ok) {
        ok = 1;
        result = evalOneIntExprOrErr(LHS,RHS,ast->kind,&ok);
    }

    if (ok) {
        aoStrCatPrintf(buf, "movq   $%lld, %%rax\n\t",result);
        aoStrCatPrintf(buf, "# INt arithmetic END, folded value \n\t");
        return;
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
                astKindToString(ast->kind));
    }

    if (reverse == ASM_REVERSE_ARITHMETIC) {
        asmExpression(cc,buf,RHS);
        asmToInt(buf,RHS->type);
        aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");

        asmExpression(cc,buf,LHS);
        asmToInt(buf,LHS->type);
        asmPush(buf,REG_RAX);
    } else {
        asmExpression(cc,buf,LHS);
        asmToInt(buf,LHS->type);
        asmPush(buf,REG_RAX);

        asmExpression(cc,buf,RHS);
        asmToInt(buf,RHS->type);
        aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");
    }
    asmPop(buf,REG_RAX);

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

void asmBinaryOpFloatArithmetic(Cctrl *cc, aoStr *buf, Ast *ast, int reverse) {
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
        asmExpression(cc,buf,RHS);
        asmToFloat(buf,RHS->type);
        aoStrCatPrintf(buf, "movsd  %%xmm0, %%xmm1\n\t");

        asmExpression(cc,buf,LHS);
        asmToFloat(buf,LHS->type);
        asmPushXMM(buf,0);
    } else {
        asmExpression(cc,buf,LHS);
        asmToFloat(buf,LHS->type);
        asmPushXMM(buf,0);

        asmExpression(cc,buf,RHS);
        asmToFloat(buf,RHS->type);
        aoStrCatPrintf(buf, "movsd  %%xmm0, %%xmm1\n\t");
    }
    asmPopXMM(buf,0);

    aoStrCatPrintf(buf, "%s     %%xmm1, %%xmm0\n\t", op);
}

void asmLoadConvert(aoStr *buf, AstType *to, AstType *from) {
    (void)from;
    if (to->kind == AST_TYPE_FLOAT) {
        asmToFloat(buf,to);
    } else {
        asmToInt(buf,to);
    }
}

void asmSaveConvert(aoStr *buf, AstType *to, AstType *from) {
    if (astIsIntType(from) && astIsFloatType(to)) {
        aoStrCatPrintf(buf, "cvtsi2ss %%eax, %%xmm0\n\t");
    } else if (astIsFloatType(from) && astIsFloatType(to)) {
        return;
    } else {
        asmLoadConvert(buf,to,from);
    }
}

static char *asmGetCompartor(long op) {
    switch (op) {
    case '<': return "setl";
    case '>': return "setg";
    case TK_GREATER_EQU: return "setge";
    case TK_LESS_EQU: return "setle";
    default:
        loggerPanic("Operation: %ld -> %s is not supported for ranges\n",
                op, astKindToString(op));
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
void asmRangeOperation(Cctrl *cc, aoStr *buf, Ast *ast) {
    char *op2 = asmGetCompartor(ast->kind);
    char *op1 = asmGetCompartor(ast->left->kind);
    aoStr *label_end = astMakeLabel();

    aoStrCatPrintf(buf, "# RANGE Start\n\t");
    asmExpression(cc,buf,ast->left->left);
    asmPush(buf,REG_RAX);
    asmExpression(cc,buf,ast->left->right);
    asmPop(buf,REG_RCX);
    aoStrCatPrintf(buf,
            "cmp    %%rax, %%rcx\n\t"
            "%-5s   %%al\n\t"
            "movzb  %%al, %%rax\n\t"
            "test   %%rax, %%rax\n\t"
            "movq   $0, %%rax\n\t"
            "je     %s\n\t", op1, label_end->data);
    asmExpression(cc,buf,ast->left->right);
    asmPush(buf,REG_RAX);
    asmExpression(cc,buf,ast->right);
    asmPop(buf,REG_RCX);
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

int asmOpIsCompoundAssign(Ast *ast) {
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

int asmShouldReverseMaths(Ast *RHS) {
    return asmOpIsCompoundAssign(RHS) && 
        RHS->right && (RHS->right->kind == AST_FUNCALL || 
         RHS->right->kind == AST_FUNPTR_CALL ||
         RHS->right->kind == AST_ASM_FUNCALL);
}

void asmBinOpFunctionAssign(Cctrl *cc, aoStr *buf, Ast *fnptr, Ast *fn) {
    (void)cc;
    switch (fn->kind) {
        case AST_FUNC: {
            char *normalised = asmNormaliseFunctionName(fn->fname->data);
            aoStrCatPrintf(buf,
                    "leaq   %s(%%rip), %%rax\n\t"
                    "movq    %%rax, %d(%%rbp)\n\t",
                    normalised,
                    fnptr->loff);
            free(normalised);
            break;
        }

        case AST_FUNPTR:
            aoStrCatPrintf(buf,
                    "movq    %d(%%rbp), %%rax\n\t"
                    "movq    %%rax, %d(%%rbp)\n\t",
                    fn->loff, fnptr->loff);
            break;

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND: {
            aoStrCatPrintf(buf,
                    "leaq   %s(%%rip), %%rax\n\t"
                    "movq    %%rax, %d(%%rbp)\n\t",
                    fn->asmfname->data,
                    fnptr->loff);
            break;
        }

        default:
            loggerPanic("Cannot assign: %s to a function pointer type\n",
                    astKindToString(fn->kind));
    }
}

void asmBinaryOp(Cctrl *cc, aoStr *buf, Ast *ast) {
    if (ast->kind == '=') {
        /* If it is compound and the value is being assigned to the return of 
         * a function we need to perform the integer arithmetic in reversed 
         * order */
        if (asmShouldReverseMaths(ast->right)) {
            if (ast->type->kind == AST_TYPE_INT) { 
                asmBinaryOpIntArithmetic(cc,buf,ast->right,
                        ASM_REVERSE_ARITHMETIC);
            } else if (ast->type->kind == AST_TYPE_FLOAT) {
                asmBinaryOpFloatArithmetic(cc,buf,ast->right,
                        ASM_REVERSE_ARITHMETIC);
            }
        } else {
            asmExpression(cc,buf,ast->right);
        }
        asmLoadConvert(buf,ast->type,ast->right->type);
        asmAssign(cc,buf, ast->left);
        return;
    }

    if (ast->type->kind == AST_TYPE_POINTER) {
        asmPointerArithmetic(cc,buf,ast->kind,ast->left,ast->right);
        return;
    }

    /* This is pretty loose! */
    if (astIsRangeOperator(ast->kind) && astIsRangeOperator(ast->left->kind)) {
        asmRangeOperation(cc,buf,ast);
        return;
    }

    switch (ast->kind) {
        case '<': asmCompare(cc,buf,"setl",ast); return;
        case '>': asmCompare(cc,buf,"setg",ast); return;
        case TK_EQU_EQU: asmCompare(cc,buf,"sete",ast); return;
        case TK_GREATER_EQU: asmCompare(cc,buf,"setge",ast); return;
        case TK_LESS_EQU: asmCompare(cc,buf,"setle",ast); return;
        case TK_NOT_EQU: asmCompare(cc,buf,"setne",ast); return;
    }

    if (ast->type->kind == AST_TYPE_INT || ast->type->kind == AST_TYPE_CHAR) {
        asmBinaryOpIntArithmetic(cc,buf,ast,ASM_INORDER_ARITHMETIC);
    } else if (ast->type->kind == AST_TYPE_FLOAT) {
        asmBinaryOpFloatArithmetic(cc,buf,ast,ASM_INORDER_ARITHMETIC);
    } else if (ast->type->kind == AST_TYPE_FUNC) {
        char *fname;
        if (ast->kind == AST_ASM_FUNC_BIND) {
            fname = strndup(ast->asmfname->data,ast->asmfname->len);
        } else {
            fname = asmNormaliseFunctionName(ast->fname->data);
        }
        aoStrCatPrintf(buf,"leaq    %s(%%rip), %%rax\n\t",fname);
        free(fname);
    } else {
        loggerPanic("Cannot handle>: %s\n", astToString(ast));
    }
}

void asmPreIncrDecr(Cctrl *cc, aoStr *buf, Ast *ast, char *op) {
    asmExpression(cc,buf,ast->operand);
    int size = 1;
    if (ast->operand->type->ptr) {
        size = ast->operand->type->ptr->size;
    }
    if (ast->operand->kind == AST_DEREF) {
        /* This is essentially pointer juggling like:
         * '*++ptr' or '*--ptr' */
        aoStrCatPrintf(buf, "%s   $%d, %%rax\n\t", op, size);
        asmAssign(cc,buf,ast->operand);
        if (ast->operand->type->ptr) {
            aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");
        }
    } else {
        aoStrCatPrintf(buf, "%s   $%d, %%rax\n\t", op, size);
        asmAssign(cc,buf,ast->operand);
        if (ast->operand->type->ptr) {
            aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t");
        }
    }
}

void asmIncrDecr(Cctrl *cc, aoStr *buf, Ast *ast, char *op) {
    asmExpression(cc,buf,ast->operand);
    asmPush(buf, REG_RAX);
    int size = 1;
    if (ast->operand->type->ptr) {
        size = ast->operand->type->ptr->size;
    }

    if (ast->operand->kind == AST_DEREF) {
        /* This is essentially pointer juggling like:
         * '*ptr++' or '*ptr--' */
        aoStrCatPrintf(buf, "%s   $%d, %%rax\n\t", op, size);
        asmAssign(cc,buf,ast->operand);
    } else {
        aoStrCatPrintf(buf, "%s   $1, %%rax\n\t", op);
        asmAssign(cc,buf,ast->operand);
    }

    asmPop(buf,REG_RAX);
}

/* This function is seriouly bjorked */
void asmAddr(Cctrl *cc, aoStr *buf, Ast *ast) {
    aoStrCatPrintf(buf, "# ADDR %s START %s \n\t", 
        astKindToString(ast->operand->kind), 
        astKindToString(ast->operand->type->kind));

    /* This is gross */
    switch (ast->operand->kind) {
        case AST_LVAR: {
            if (ast->operand->type->kind == AST_TYPE_POINTER) {
                /* XXX: this feels extremely hacky */
                aoStrCatPrintf(buf, "# ADDR of %s\n\t",
                        astKindToString(ast->operand->type->ptr->kind));
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
                    astKindToString(ast->operand->operand->kind),
                    astTypeToString(ast->type),
                    astTypeToString(ast->operand->type),
                    astTypeToString(ast->cls->type));
            }
            break;
        }
        case AST_DEREF: {
            /* That is the class */
            Ast *cls = ast->operand;
            asmExpression(cc,buf,cls);
            break;
        }
        default:
            loggerPanic("Cannot turn Kind AST:%s %s into assembly\n",
                    astKindToString(ast->kind),
                    astToString(ast));
    }
    aoStrCatPrintf(buf, "# ADDR %s END %s \n\t", 
        astKindToString(ast->operand->kind),
        astKindToString(ast->operand->type->kind));
}

int asmPlaceArgs(Cctrl *cc, aoStr *buf, List *argv, int reverse) {
    int r = 0;
    Ast *ast;
    if (reverse) {
        for (List *it = argv->prev; it != argv; it = it->prev) {
            ast = (Ast *)it->value;
            asmExpression(cc,buf,ast);
            if (astIsFloatType(ast->type)) asmPushXMM(buf,0);
            else asmPush(buf,REG_RAX);
            r += 8;
        }
    } else {
        for (List *it = argv->next; it != argv; it = it->next) {
            ast = (Ast *)it->value;
            asmExpression(cc,buf,ast);
            if (astIsFloatType(ast->type)) asmPushXMM(buf,0);
            else asmPush(buf,REG_RAX);
            r += 8;
        }
    }
    return align(r,8);
}

void asmPopIntArgs(aoStr *buf, List *argv) {
    int count = listCount(argv);
    for (int i = count - 1; i >= 0; --i) {
        asmPop(buf, REGISTERS[i]);
    }
}

void asmPopFloatArgs(aoStr *buf, List *argv) {
    int count = listCount(argv);
    for (int i = count - 1; i >= 0; --i) {
        asmPopXMM(buf,i);
    }
}

/* For default arguments this should be an int vector and we just look up
 * the index, having 2 linked lists is very tricky to manage */
#define FUN_EXISTS 0x1
#define FUN_EXTERN 0x2
#define FUN_VARARG 0x4
void asmPrepFuncCallArgs(Cctrl *cc, aoStr *buf, Ast *funcall) {
    int int_cnt,float_cnt,stack_cnt,stackoffset,needs_padding,var_arg_start,argc,
        flags;
    List *int_args, *float_args, *stack_args;
    Ast *funarg; 
    Ast *tmp, *fun, *arg, *funparam;

    flags = 0;
    fun = strMapGetLen(cc->global_env,funcall->fname->data,funcall->fname->len);
    funarg = funparam = NULL;

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
        for (ssize_t i = 0; i < fun->params->size; ++i) {
            var_arg_start++;
            arg = fun->params->entries[i];
            if (arg->kind == AST_VAR_ARGS) {
                break;
            }
        }
        var_arg_start += 1;
    }

    int_cnt = float_cnt = stack_cnt = stackoffset = 0;
    
    int_args = listNew();
    float_args = listNew();
    stack_args = listNew();

    funarg = NULL;
    ssize_t i = 0;
    while (1) {
        /* Handling the case for either more arguments than parameters or
         * more parameters than arguments */
        if (fun && fun->params) {
            funparam = vecGetInBounds(fun->params,i);
        }
        funarg = vecGetInBounds(funcall->args,i);
        i++;

        if (funarg != NULL) {
            tmp = funarg;
            if (tmp->kind == AST_PLACEHOLDER) {
                loggerDebug("place holder: %s\n",astToString(funparam));
                tmp = funparam->declinit;
                if (tmp == NULL) {
                    loggerPanic("Default parameter not provided for function call: %s()\n",
                            funcall->fname->data);
                }
            }
        } else if (funparam) {
            /* Handling default function parameters, these can only come at
             * the end of a function call presently */
            /* For function pointers the value is stored elsewhere */
            if (funparam->kind == AST_FUNPTR) {
                tmp = funparam->default_fn->declinit;
            } else {
                tmp = funparam->declinit;
            }
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
            listAppend(stack_args,tmp);
            stack_cnt++;
        } else if (astIsFloatType(tmp->type)) {
            argc++;
            if (float_cnt < 8) {
                listAppend(float_args,tmp);
                float_cnt++;
            }
            else {
                listAppend(stack_args,tmp);
                stack_cnt++;
            }
        } else {
            argc++;
            if (int_cnt < 6) {
                listAppend(int_args,tmp);
                int_cnt++;
            }
            else {
                listAppend(stack_args,tmp);
                stack_cnt++;
            }
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
                asmLoadClassRef(cc,buf,funcall->ref->cls,funcall->ref->type,0);
                break;

            case AST_FUNPTR:
            case AST_LVAR:
                asmLLoad(buf,funcall->ref->type,funcall->ref->loff);
                break;

            case AST_GVAR:
                asmGLoad(buf,funcall->ref->type,funcall->ref->gname,0);
                break;
        }
        aoStrCatPrintf(buf,"movq    %%rax,%%r11\n\t");
    }

    stackoffset = asmPlaceArgs(cc,buf,stack_args,1);
    asmPlaceArgs(cc,buf,int_args,0);
    asmPlaceArgs(cc,buf,float_args,0);

    asmPopFloatArgs(buf,float_args);
    asmPopIntArgs(buf,int_args);

    if (float_cnt) {
        aoStrCatPrintf(buf, "movl   $%d, %%eax\n\t", float_cnt);
    }

    if (funcall->kind == AST_FUNPTR_CALL) {
        aoStrCatPrintf(buf, "call    *%%r11\n\t");
    } else {
        asmCall(buf, funcall->fname->data);
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

    listRelease(int_args,NULL);
    listRelease(float_args,NULL);
    listRelease(stack_args,NULL);
}

/* Function call for ASM, PTR and normal functions */
void asmFunCall(Cctrl *cc, aoStr *buf, Ast *ast) {
    asmPrepFuncCallArgs(cc,buf,ast);
}

/* This is also used for initalising classes as they are treated much in the 
 * same way as arrays */
void asmArrayInit(Cctrl *cc, aoStr *buf, Ast *ast, AstType *type, int offset) {
    listForEach(ast->arrayinit) {
        Ast *tmp = (Ast *)it->value;
        if (tmp->kind == AST_ARRAY_INIT) {
            asmArrayInit(cc,buf,tmp,type->ptr,offset);
            offset += type->ptr->size;
            continue;
        }

        if (tmp->kind == AST_STRING) {
            aoStrCatPrintf(buf,"leaq    %s(%%rip), %%rax\n\t",tmp->slabel->data);
            aoStrCatPrintf(buf,"movq    %%rax, %d(%%rbp)\n\t",offset);
            offset += 8;
        } else {
            asmExpression(cc, buf, tmp);
            if (type->ptr) {
                asmLSave(buf,type->ptr,offset);
                offset += type->ptr->size;
            } else {
                asmLSave(buf,tmp->type,offset);
                offset += tmp->type->size;
            }
        }
    }
}

void asmHandleSwitch(Cctrl *cc, aoStr *buf, Ast *ast) {
    /* create jump table */
    PtrVec *cases = ast->cases;
    Ast **jump_table = ast->jump_table_order;
    int jump_table_size = cases->size;
    Ast *case_ast_min = jump_table[0];
    Ast *case_ast_max = jump_table[jump_table_size - 1];

    int case_min = case_ast_min->case_begin;
    int case_max = case_ast_max->case_end;
    int case_normalised_end = case_max - case_min;

    /* we need to work off the assumption that RAX is containing the evaluated 
     * expression from cond */
    asmExpression(cc,buf,ast->switch_cond);

    aoStr *end_label;
    aoStr *jump_table_start = astMakeLabel();

    if (ast->case_default) end_label = ast->case_default->case_label;
    else                   end_label = ast->case_end_label;

    /* Normalise RAX */
    aoStrCatPrintf(buf,
            "# Switch \n\t"
            "sub   $%d, %%rax\n\t",case_min);

    /* Preample for jump table */
    if (ast->switch_bounds_checked) {
        aoStrCatPrintf(buf,
                "# bounds check \n\t"
                "cmp   $%d, %%rax\n\t"
                "ja    %s\n\t",
                case_normalised_end,
                end_label->data);
    }
    aoStrCatPrintf(
            buf,
            "leaq  %s(%%rip), %%rdx\n\t"
            "movl  (%%rdx,%%rax,4), %%eax\n\t"
            "add   %%rdx, %%rax\n\t"
            "jmp   *%%rax\n"
            "\n\t.p2align 2\n"
            "%s:\n\t"
            "# Jump Table\n\t",
            jump_table_start->data, jump_table_start->data);

    int cur_case = 0;
    for (int i = 0; i <= case_normalised_end; ++i) {
        Ast *_case = jump_table[cur_case];
        int case_begin_normalised     = _case->case_begin - case_min;
        int case_normalised_range_end = _case->case_end - case_min;
        int diff                      = case_normalised_range_end - case_begin_normalised;

        /* pad out the table */
        for (; i < case_begin_normalised; ++i) {
            aoStrCatPrintf(buf, ".long %s-%s\n\t",
                    end_label->data,
                    jump_table_start->data);
        }

        for (int j = case_begin_normalised; j <= case_normalised_range_end; ++j) {
            aoStrCatPrintf(buf, ".long %s-%s\n\t",
                    _case->case_label->data,
                    jump_table_start->data);
        }

        i += diff;
        cur_case++;
    }
    aoStrCatPrintf(buf,".long %s-%s\n\t",
            end_label->data,
            jump_table_start->data);


    for (int i = 0; i < cases->size; ++i) {
        Ast *_case = cases->entries[i];
        asmExpression(cc,buf,_case);
    }

    if (ast->case_default) {
        asmExpression(cc,buf,ast->case_default);
    }

    asmRemovePreviousTab(buf);
    aoStrCatPrintf(buf, "%s:\n\t", ast->case_end_label->data);
}

void asmExpression(Cctrl *cc, aoStr *buf, Ast *ast) {
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
                ast->f64_label = astMakeLabel();
                f64_label = ast->f64_label->data;
                double f = ast->f64;
                asmRemovePreviousTab(buf);
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
                    astKindToString(ast->kind));
        }
        break;
    }

    case AST_STRING:
        aoStrCatPrintf(buf, "leaq   %s(%%rip), %%rax\n\t", ast->slabel->data);
        break;
    
    case AST_LVAR:
        asmLLoad(buf,ast->type,ast->loff);
        break;
    
    case AST_GVAR:
        asmGLoad(buf,ast->type,ast->glabel,0);
        break;

    case AST_FUNPTR_CALL:
    case AST_ASM_FUNCALL:
    case AST_FUNCALL: {
        asmFunCall(cc,buf,ast);
        break;
    }

    case AST_DEFAULT_PARAM:
        asmExpression(cc,buf,ast->declvar);
        break;
    
    case AST_DECL: {
        if (!ast->declinit) {
            return;
        }

        if (ast->declinit->kind == AST_ARRAY_INIT) {
            asmArrayInit(cc,buf,ast->declinit,ast->declvar->type,ast->declvar->loff);
        } else if (ast->declvar->type->kind == AST_TYPE_ARRAY) {
            assert(ast->declinit->kind == AST_STRING);
            asmPlaceString(buf,ast->declinit->sval,ast->declvar->loff);
        } else if (ast->declinit->kind == AST_STRING) {
            asmGLoad(buf, ast->declinit->type, ast->declinit->slabel, 0);
            asmLSave(buf, ast->declvar->type, ast->declvar->loff);
        } else if (ast->declinit->kind == AST_FUNC) {
            asmBinOpFunctionAssign(cc,buf,ast->declvar,ast->declinit);
        } else {
            asmExpression(cc,buf, ast->declinit);
            asmCast(buf,ast->declinit->type, ast->declvar->type);
            asmLSave(buf, ast->declvar->type, ast->declvar->loff);
        }
        return;
    }

    case AST_FUNPTR:
        aoStrCatPrintf(buf, "movq    %d(%%rbp), %%rax\n\t", ast->loff);
        break;

    case AST_ADDR:
        asmAddr(cc,buf,ast);
        break;

    case AST_CAST:
        asmTypeCast(cc,buf,ast);
        break;

    case AST_DEREF: {
        assert(ast->operand->type->kind == AST_TYPE_POINTER);
        asmExpression(cc,buf,ast->operand);

        AstType *op_type =  ast->operand->type;
        AstType *result = ast->type;
        char *reg;
    
        if (op_type->kind == AST_TYPE_POINTER &&
                op_type->ptr->kind == AST_TYPE_ARRAY) {
            return;
        }


        reg = "rcx"; // asmGetIntReg(result,'c');

        aoStrCatPrintf(buf, "##__DEREF_START: %s\n\t", astKindToString(result->kind));
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
                aoStrCatPrintf(buf,"# deref not ptr start: %s %s\n\t",
                        astKindToString(result->kind), astKindToString(op_type->kind));
               // aoStrCatPrintf(buf,"movq   (%%%s), %%rax\n\t", reg);
               

                if (result->kind == AST_TYPE_FLOAT) {
                    aoStrCatPrintf(buf, "movq   (%%rax), %%xmm0\n\t");
                } else {
                    // reg = asmGetIntReg(result,'c');
                    char *mov = asmGetPtrMove(result);
                    aoStrCatPrintf(buf, "movq   %%rax, %%rcx\n\t"
                                        "%s   (%%%s), %%rax\n\t",mov, reg);
                }
                aoStrCatPrintf(buf,"# deref not ptr start\n\t");
            } else if (result->kind == AST_TYPE_POINTER) {
                reg = asmGetIntReg(result,'a');
                char *mov = asmGetMov(result);
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
                    /* We want to derefernce a 'U0**' ptr */
                    case AST_TYPE_VOID:
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
        asmExpression(cc,buf,ast->cond);
        label_begin = astMakeLabel();
        aoStrCatPrintf(buf,
                "test   %%rax, %%rax\n\t"
                "je     %s\n\t", label_begin->data);
        asmExpression(cc,buf,ast->then);
        if (ast->els) {
            label_end = astMakeLabel();
            aoStrCatPrintf(buf,
                    "jmp    %s\n"
                    "%s:\n\t"
                    ,label_end->data,
                    label_begin->data);
            asmExpression(cc,buf,ast->els);
            asmRemovePreviousTab(buf);
            aoStrCatPrintf(buf, "%s:\n\t", label_end->data);
        } else {
            asmRemovePreviousTab(buf);
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
        asmRemovePreviousTab(buf);
        if (ast->sval) {
            aoStrCatPrintf(buf, "%s:\n\t", ast->sval->data);
        } else {
            aoStrCatPrintf(buf, "%s:\n\t", ast->slabel->data);
        }
        break;

    case AST_DO_WHILE: {
        asmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->while_begin->data);
        if (ast->whilebody) {
            asmExpression(cc,buf, ast->whilebody);
        }
        asmExpression(cc,buf,ast->whilecond);
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
        asmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->while_begin->data);
        asmExpression(cc,buf,ast->whilecond);
        aoStrCatPrintf(buf,
                "test   %%rax, %%rax\n\t"
                "je     %s\n\t",
                ast->while_end->data);
        if (ast->whilebody) {
            asmExpression(cc,buf, ast->whilebody);
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
            asmExpression(cc,buf,ast->forinit);
        }

        asmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->for_begin->data);

        if (ast->forcond) {
            asmExpression(cc,buf,ast->forcond);
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "je     %s\n\t",
                    ast->for_end->data);
        }

        asmExpression(cc,buf, ast->forbody);

        /* To help with continue statements */
        asmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s:\n\t", ast->for_middle->data);

        if (ast->forstep) {
            asmExpression(cc,buf,ast->forstep);
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
        asmExpression(cc,buf, ast->retval);
        asmSaveConvert(buf,ast->type,ast->retval->type);
        aoStrCatPrintf(buf, "leave\n\tret\n");
        break;

    case AST_COMPOUND_STMT:
        listForEach(ast->stms) {
            asmExpression(cc,buf,(Ast *)it->value);
        }
        break;

    case AST_CLASS_REF:
        asmLoadClassRef(cc,buf,ast->cls,ast->type,0);
        break;

    case AST_DEFAULT:
        asmRemovePreviousTab(buf);
        aoStrCatPrintf(buf, "%s: #default_label \n\t", ast->case_label->data);
        listForEach(ast->case_asts) {
            asmExpression(cc,buf,(Ast *)it->value);
        }
        break;

    case AST_CASE:
        asmRemovePreviousTab(buf);

        if (listEmpty(ast->case_asts) || ast->case_asts == NULL) return;
        aoStrCatPrintf(buf, "%s: #case_label \n\t", ast->case_label->data);

        listForEach(ast->case_asts) {
            asmExpression(cc,buf,(Ast *)it->value);
        }
        break;

    case AST_SWITCH:
        asmHandleSwitch(cc,buf,ast);
        break;

    case TK_PRE_PLUS_PLUS: {
        if (ast->type->kind == AST_TYPE_FLOAT) {
            asmExpression(cc,buf,ast->operand);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "addsd   %%xmm1, %%xmm0\n\t");
            asmAssign(cc,buf,ast->operand);
        } else {
            asmPreIncrDecr(cc,buf,ast,"addq");
        }
        break;
     }

    case TK_PLUS_PLUS: {
        if (ast->type->kind == AST_TYPE_FLOAT) {
            asmExpression(cc,buf,ast->operand);
            asmPushXMM(buf,0);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "addsd   %%xmm1, %%xmm0\n\t");
            asmAssign(cc,buf,ast->operand);
            asmPopXMM(buf,0);
        } else {
            asmIncrDecr(cc,buf,ast,"addq");
        }
        break;
    }

    case TK_PRE_MINUS_MINUS:
        if (ast->type->kind == AST_TYPE_FLOAT) {
            asmExpression(cc,buf,ast->operand);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "subsd   %%xmm1, %%xmm0\n\t");
            asmAssign(cc,buf,ast->operand);
        } else {
            asmPreIncrDecr(cc,buf,ast,"subq");
        }
        break;

    case TK_MINUS_MINUS:
        if (ast->type->kind == AST_TYPE_FLOAT) {
            asmExpression(cc,buf,ast->operand);
            asmPushXMM(buf,0);
            aoStrCatPrintf(buf,
                    "movsd   one_dbl(%%rip), %%xmm1\n\t"
                    "subsd   %%xmm1, %%xmm0\n\t");
            asmAssign(cc,buf,ast->operand);
            asmPopXMM(buf,0);
        } else {
            asmIncrDecr(cc,buf,ast,"subq");
        }
        break;

    case '!': {
        asmExpression(cc,buf, ast->operand);
        aoStrCatPrintf(buf,
                "cmp    $0, %%rax\n\t"
                "sete   %%al\n\t"
                "movzb  %%al, %%rax\n\t");
        break;
    }

    case '~': {
        asmExpression(cc,buf, ast->operand);
        aoStrCatPrintf(buf, "not    %%rax\n\t");
        break;
    }

    case TK_OR_OR: {
        aoStrCat(buf,"# OR OR Start\n\t");
        label_end = astMakeLabel();
        asmExpression(cc,buf,ast->left);

        if (ast->type->kind == AST_TYPE_FLOAT) {
            asmExpression(cc,buf,ast->left);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "movq    $1, %%rax\n\t"
                    "jne     %s\n\t", label_end->data);
            asmExpression(cc,buf,ast->right);
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
            asmExpression(cc,buf,ast->right);
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
        label_end = astMakeLabel();

        if (ast->type->kind == AST_TYPE_FLOAT) {
            asmExpression(cc,buf,ast->left);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "je    %s\n\t", label_end->data);
            asmExpression(cc,buf,ast->right);
            aoStrCatPrintf(buf,
                    "xorpd   %%xmm1, %%xmm1\n\t"
                    "ucomisd %%xmm1, %%xmm0\n\t"
                    "movq    $0, %%rax\n\t"
                    "je     %s\n\t"
                    "movq    $1, %%rax\n\t"
                    "%s:\n\t", label_end->data, label_end->data);
        } else {
            asmExpression(cc,buf,ast->left);
            aoStrCatPrintf(buf,
                    "test   %%rax, %%rax\n\t"
                    "movq   $0, %%rax\n\t"
                    "je    %s\n\t", label_end->data);
            asmExpression(cc,buf,ast->right);
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
          asmExpression(cc,buf, ast->left);
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
          asmBinaryOp(cc,buf,ast);
      }
      break;
    }

    default:
        asmBinaryOp(cc,buf,ast);
        break;
    }
}

void asmDataInternal(aoStr *buf, Ast *data) {
    if (data->kind == AST_STRING) {
        aoStrCatPrintf(buf,".quad %s\n\t",data->slabel->data);
        return;
    }

    /* If we have something like:
     * ```
     * I64 arr[][2] = {
     *   {1,2},
     *   {3,4},
     * };
     * ```
     * We need to call this function again.
     * */
    if (data->kind == AST_ARRAY_INIT) {
        listForEach(data->arrayinit) {
            asmDataInternal(buf,(Ast *)it->value);
        }
        return;
    }

    if (data->type->kind == AST_TYPE_FLOAT) {
        aoStrCatPrintf(buf,".quad 0x%lX #%.9f\n\t",
                ieee754(data->f64),
                data->f64);
        return;
    }

    switch (data->type->size) {
        case 1: aoStrCatPrintf(buf, ".byte %d\n\t", data->i64); break;
        case 4: aoStrCatPrintf(buf, ".long %d\n\t", data->i64); break;
        case 8: aoStrCatPrintf(buf, ".quad %d\n\t", data->i64); break;
        default:
            loggerPanic("Cannot create size information for: %s\n",
                    astToString(data));
    }
}

void asmGlobalVar(StrMap *seen_globals, aoStr *buf, Ast* ast) {
    Ast *declvar = ast->declvar;
    Ast *declinit = ast->declinit;
    aoStr *varname = declvar->gname;
    char *label = asmGetGlabel(declvar);

    if (strMapGetLen(seen_globals,varname->data,varname->len)) {
        return;
    }

    if (buf->data[buf->len-1] == '\t') {
        buf->len--;
    }

    strMapAdd(seen_globals,varname->data,ast);

    if (declinit &&
        (declinit->kind == AST_ARRAY_INIT || 
         declinit->kind == AST_LITERAL || declinit->kind == AST_STRING))
    {
        if (!ast->is_static) {
            aoStrCatPrintf(buf,".global %s\n",label);
            aoStrCatPrintf(buf,".data\n");
        } else {
            aoStrCatPrintf(buf,".data\n");
        }
        if (declinit->kind == AST_ARRAY_INIT) {
            Ast *head = (Ast *)declinit->arrayinit->next->value;
            if (head->kind == AST_STRING) {
                aoStrCatPrintf(buf,".align 4\n");
            }
        }

        aoStrCatPrintf(buf,"%s:\n\t",label);

        if (declinit->kind == AST_ARRAY_INIT) {
            listForEach(declinit->arrayinit) {
                asmDataInternal(buf,(Ast *)it->value);
            }
            return;
        } else {
            asmDataInternal(buf,declinit);
        }
    } else {
        aoStrCatPrintf(buf,".lcomm %s, %d\n\t",label,declvar->type->size);
    }
}

void asmDataSection(Cctrl *cc, aoStr *buf) {
    aoStrCatPrintf(buf, "sign_bit:\n\t.quad 0x8000000000000000\n");
    aoStrCatPrintf(buf, "one_dbl:\n\t.double 1.0\n");

    StrMapIterator *it = strMapIteratorNew(cc->strs);
    StrMapNode *n = NULL;
    while ((n = strMapNext(it)) != NULL) {
        Ast *ast = (Ast *)n->value;
        assert(ast->kind == AST_STRING);

        char *label = ast->slabel->data;
        char *str = ast->sval->data;
        aoStr *escaped = aoStrAlloc(1<<10);

        char *ptr = str;
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
        aoStrCatPrintf(buf,
                ".string \"%s\\0\"\n\t"
                ".data\n\t"
                ".align 4\n"
                , escaped->data);
        aoStrRelease(escaped);
    }
    aoStrPutChar(buf,'\t');
    strMapIteratorRelease(it);
} 

void asmStoreParam(aoStr *buf, int *_ireg, int *_arg, int offset) {
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
void asmStoreParamFloat(aoStr *buf, int *_freg, int *_arg, int offset) {
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

void asmGetRegisterCounts(List *params, int *ireg, int *freg) {
    listForEach(params) {
        Ast *param = (Ast *)it->value;
        if (astIsFloatType(param->type)) (*freg)++;
        else                             (*ireg)++;
    }
}

int asmFunctionInit(Cctrl *cc, aoStr *buf, Ast *func) {
    (void)cc;
    int offset = 0;
    int ireg = 0, freg = 0, locals = 0, arg = 2;
    Ast *ast_tmp = NULL;

    char *fname = asmNormaliseFunctionName(func->fname->data);

    aoStrCatPrintf(buf, ".text"            "\n\t"
                        ".global %s\n"
                        "%s:\n\t"
                        "push   %%rbp"       "\n\t"
                        "movq   %%rsp, %%rbp" "\n\t",
                        fname,
                        fname);

    int new_offset = 0, alignment = 0;
    /* Now assign offsets */
    listForEach(func->locals) {
        ast_tmp = (Ast *)it->value;
        /* Calculate how much stackspace is required for locals */
        alignment = align(ast_tmp->type->size, 8);
        locals += alignment;
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

    for (ssize_t i = 0; i < func->params->size; ++i) {
        ast_tmp = vecGet(Ast *, func->params, i);
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
                stack_space, listCount(func->locals));
        stack_pointer = stack_space;
    }

    /* We can use register arguments */
    offset = stack_space;
    for (ssize_t i = 0; i < func->params->size; ++i) {
        ast_tmp = vecGet(Ast *, func->params, i);

        if (ast_tmp->kind != AST_VAR_ARGS && astIsFloatType(ast_tmp->type)) {
            asmStoreParamFloat(buf,&freg,&arg,offset);
        } else {
            asmStoreParam(buf,&ireg,&arg,offset);
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
                    asmStoreParam(buf,&ireg,&arg,offset);
                    offset -= 8;
                }
                for (int i = freg; i < 8; ++i) {
                    asmStoreParamFloat(buf,&freg,&arg,offset);
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

void asmFunctionLeave(aoStr *buf) {
    aoStrCatPrintf(buf,"leave\n\tret\n");
}

int asmHasRet(aoStr *buf) {
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

void asmFunction(Cctrl *cc, aoStr *buf, Ast *func) {
    assert(func->kind == AST_FUNC);
    cc->stack_local_space = asmFunctionInit(cc,buf,func);
    asmExpression(cc,buf,func->body);
    if (!asmHasRet(buf)) {
        asmFunctionLeave(buf);
    }
}

void asmPasteAsmBlocks(aoStr *buf, Cctrl *cc) {
    List *it = cc->asm_blocks->next;
    List *func_it;
    Ast *asm_block, *asm_func;

    while (it != cc->asm_blocks) {
        asm_block = (Ast *)it->value;

        func_it = asm_block->funcs->next;

        while (func_it != asm_block->funcs) {
            asm_func = (Ast *)func_it->value;
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

void asmInitaliser(Cctrl *cc, aoStr *buf) {
    if (listEmpty(cc->initalisers)) return;
    char *fname = NULL;
    int locals = 0, stack_space;

#if IS_BSD
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
    listForEach(cc->initaliser_locals) {
        Ast *ast_tmp = (Ast *)it->value;
        locals += align(ast_tmp->type->size, 8);
    }

    int new_offset = 0;
    /* Now take chunks out of the total size */
    listForEach(cc->initaliser_locals) {
        Ast *ast_tmp = (Ast *)it->value;
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

    listForEach(cc->initalisers) {
        asmExpression(cc,buf,(Ast *)it->value);
    }
    aoStrCatPrintf(buf, "leave\n\tret\n");
}

/* Create assembly */
aoStr *asmGenerate(Cctrl *cc) {
    Ast *ast;
    StrMap *seen_globals = strMapNew(32);
    aoStr *asmbuf = aoStrAlloc(1<<10);
 
    if (!listEmpty(cc->initalisers)) {
        has_initialisers = 1;
    }
    asmPasteAsmBlocks(asmbuf,cc);
    asmDataSection(cc,asmbuf);


    listForEach(cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            /* We do not emit inline functions */
            if (ast->flags & AST_FLAG_INLINE) {
                continue;
            }
            asmFunction(cc,asmbuf,ast);
        } else if (ast->kind == AST_DECL || ast->kind == AST_GVAR) {
            asmGlobalVar(seen_globals,asmbuf,ast);
        } else if (ast->kind == AST_ASM_STMT) {
           // aoStrCatLen(asmbuf, ast->asm_stmt->data, ast->asm_stmt->len);
           // loggerDebug("%s\n", ast->asm_stmt->data);
        } else if (ast->kind == AST_ASM_FUNCDEF || ast->kind == AST_ASM_FUNC_BIND || ast->kind == AST_EXTERN_FUNC) {
           // aoStrCatPrintf(asmbuf, ".text\n\t.global %s\n\t", 
           //         ast->asmfname->data);
        } else {
            loggerPanic("Cannot handle: %s\n", astToString(ast));
        }
    }
    if (!listEmpty(cc->initalisers)) {
        asmInitaliser(cc,asmbuf);
    }
    aoStrCatFmt(asmbuf,".LFE0:\n\t");
#if IS_LINUX
    aoStrCatFmt(asmbuf, ".section    .note.GNU-stack,\"\",@progbits\n\t");
#endif
    aoStrCatFmt(asmbuf,".ident      \"hcc: %s %s %s\"\n",
            OS_STR, ARCH_STR, cctrlGetVersion());
    return asmbuf;
}
