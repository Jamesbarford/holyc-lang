/**
 * This is the intermediate representation of our AST getting it prepped for
 * codegen.
 *
 * @TODO:
 * - default function parameters; perhaps this is a function of the parser 
 *   as it could potentially be done at the code generation phase?
 */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#include "aostr.h"
#include "ast.h"
#include "arena.h"
#include "cctrl.h"
#include "ir.h"
#include "map.h"
#include "util.h"

static Arena ir_memory_arena;
static int ir_memory_arena_init = 0;

/* The memory arena for the IR */

void irArenaInit(unsigned int capacity) {
    if (!ir_memory_arena_init) {
        arenaInit(&ir_memory_arena, capacity);
        ir_memory_arena_init = 1;
    }
}

void irMemoryRelease(void) {
    if (ir_memory_arena_init) {
        ir_memory_arena_init = 0;
        arenaClear(&ir_memory_arena);
    }
}

void irMemoryStats(void) {
    printf("IR Arena:\n");
    arenaPrintStats(&ir_memory_arena);
}

static void *irArenaAlloc(unsigned int size) {
    return arenaAlloc(&ir_memory_arena,size);
}


/*==================== IR HELPERS =========================================== */
/* Is what we are looking at a comparison? */
int irOpIsCmp(IrOpcode opcode) {
    if (opcode == IR_OP_ICMP || opcode == IR_OP_FCMP) {
        return 1;
    }
    return 0;
}

/*==================== IR STRING REPRESENTATIONS ============================ */
const char *irValueTypeToString(IrValueType ir_value_type) {
    switch (ir_value_type) {
        case IR_TYPE_VOID:     return "void";
        case IR_TYPE_I8:       return "i8";
        case IR_TYPE_I16:      return "i16";
        case IR_TYPE_I32:      return "i32";
        case IR_TYPE_I64:      return "i64";
        case IR_TYPE_F64:      return "f64";
        case IR_TYPE_PTR:      return "ptr";
        case IR_TYPE_ARRAY:    return "array";
        case IR_TYPE_STRUCT:   return "struct";
        case IR_TYPE_FUNCTION: return "function";
        case IR_TYPE_LABEL:    return "";
        default: loggerPanic("Unhandled IrValueType: %d\n", ir_value_type);
    }
}

aoStr *irValueToString(IrValue *ir_value) {
    aoStr *buf = aoStrNew();

    switch (ir_value->kind) {
        case IR_VALUE_CONST_INT:   aoStrCatFmt(buf, "%I", ir_value->i64); break;
        case IR_VALUE_CONST_FLOAT: aoStrCatFmt(buf, "%f", ir_value->f64); break;
        case IR_VALUE_CONST_STR:   aoStrCatFmt(buf, "%S", ir_value->name); break;
        case IR_VALUE_GLOBAL:      aoStrCatFmt(buf, "%S", ir_value->name); break;
        case IR_VALUE_PARAM:       aoStrCatFmt(buf, "%S", ir_value->name); break;
        case IR_VALUE_LOCAL:       aoStrCatFmt(buf, "%S", ir_value->name); break;
        case IR_VALUE_TEMP:        aoStrCatFmt(buf, "%S", ir_value->name); break;
        case IR_VALUE_PHI:         aoStrCatFmt(buf, "phi"); break;
        case IR_VALUE_LABEL:       aoStrCatFmt(buf, "%S", ir_value->name); break;
        case IR_VALUE_UNDEFINED:   aoStrCatFmt(buf, "undefined"); break;
        default: loggerPanic("Unhandled IrValueKind: %d\n", ir_value->kind);
    }

    const char *ir_value_type_str = irValueTypeToString(ir_value->type);
    aoStrCatFmt(buf," %s",ir_value_type_str);

    return buf;
}

const char *irOpcodeToString(IrOpcode opcode) {
    switch (opcode) {
        case IR_OP_ALLOCA:   return "alloca";
        case IR_OP_LOAD:     return "load";
        case IR_OP_STORE:    return "store";
        case IR_OP_GEP:      return "gep";
        case IR_OP_ADD:      return "add";
        case IR_OP_SUB:      return "sub";
        case IR_OP_MUL:      return "mul";
        case IR_OP_DIV:      return "div";
        case IR_OP_REM:      return "rem";
        case IR_OP_NEG:      return "neg";
        case IR_OP_AND:      return "and";
        case IR_OP_OR:       return "or";
        case IR_OP_XOR:      return "xor";
        case IR_OP_SHL:      return "shl";
        case IR_OP_SHR:      return "shr";
        case IR_OP_SAR:      return "sar";
        case IR_OP_NOT:      return "not";
        case IR_OP_ICMP:     return "icmp";
        case IR_OP_FCMP:     return "fcmp"; /* Floating point compare */
        case IR_OP_TRUNC:    return "trunc"; /* Int to smaller int i.e i32 -> i16 */
        case IR_OP_ZEXT:     return "zext"; /* zero extend */
        case IR_OP_SEXT:     return "sext"; /* sign extend */
        case IR_OP_FPTRUNC:  return "fptrunc"; /* float truncate f32 -> f64 */
        case IR_OP_FPEXT:    return "fpext";   /* f32 -> f64 */
        case IR_OP_FPTOUI:   return "fptoui";
        case IR_OP_FPTOSI:   return "fptosi";
        case IR_OP_UITOFP:   return "uitofp";
        case IR_OP_SITOFP:   return "sitofp";
        case IR_OP_PTRTOINT: return "ptrtoint";
        case IR_OP_INTTOPTR: return "inttoptr";
        case IR_OP_BITCAST:  return "bitcast";
        case IR_OP_RET:      return "ret";
        case IR_OP_BR:       return "br";
        case IR_OP_JMP:      return "jmp";
        case IR_OP_SWITCH:   return "switch";
        case IR_OP_CALL:     return "call";
        case IR_OP_PHI:      return "phi";
        case IR_OP_SELECT:   return "select";
        case IR_OP_VA_ARG:   return "va_arg";
        case IR_OP_VA_START: return "va_start";
        case IR_OP_VA_END:   return "va_end";
        default: loggerPanic("Unhandled opcode: %d\n", opcode);
    }
}

const char *irCmpKindToString(IrCmpKind ir_cmp_kind) {
    switch (ir_cmp_kind) {
        case IR_CMP_EQ:  return "cmp_eq";
        case IR_CMP_NE:  return "cmp_ne";
        case IR_CMP_LT:  return "cmp_lt";
        case IR_CMP_LE:  return "cmp_le";
        case IR_CMP_GT:  return "cmp_gt";
        case IR_CMP_GE:  return "cmp_ge";
        case IR_CMP_ULT: return "cmp_ult";
        case IR_CMP_ULE: return "cmp_ule";
        case IR_CMP_UGT: return "cmp_ugt";
        case IR_CMP_UGE: return "cmp_uge";
        case IR_CMP_OEQ: return "cmp_oeq";
        case IR_CMP_ONE: return "cmp_one";
        case IR_CMP_OLT: return "cmp_olt";
        case IR_CMP_OLE: return "cmp_ole";
        case IR_CMP_OGT: return "cmp_ogt";
        case IR_CMP_OGE: return "cmp_oge";
        case IR_CMP_UNO: return "cmp_uno";
        case IR_CMP_ORD: return "cmp_ord";
        default: loggerPanic("Unhandled comparison kind: %d\n", ir_cmp_kind);
    }
}

/* Convert an instruction to a string. */
aoStr *irInstrToString(IrInstr *ir_instr) {
    IrValue *ir_values[4] = {
        ir_instr->result,
        ir_instr->op1,
        ir_instr->op2,
        ir_instr->op3,
    };
    aoStr *buf = aoStrNew();
    const char *op = irOpcodeToString(ir_instr->opcode);
    aoStrCatFmt(buf, "%s", op);

    for (int i = 0; i < (int)static_size(ir_values); i++) {
        IrValue *ir_value = ir_values[i];
        if (!ir_value) continue;
        aoStr *ir_value_str = irValueToString(ir_value);
        aoStrCatFmt(buf, " %S,", ir_value_str);
        aoStrRelease(ir_value_str);
    }
    if (buf->data[buf->len-1] == ',') {
        buf->len--; /* remove trailing ',' */
        buf->data[buf->len] = '\0';
    }
    return buf;
}


/* Convert a basic block to a string
 * <block_name>:
 *   <instructions...>
 * */
aoStr *irBlockToString(IrBlock *ir_block) {
    aoStr *buf = aoStrNew();
    aoStrCatFmt(buf, "  %S:\n", ir_block->label);
    for (int j = 0; j < ir_block->instructions->size; ++j) {
        IrInstr *ir_instr = vecGet(IrInstr *, ir_block->instructions, j);
        aoStr *ir_instr_str = irInstrToString(ir_instr);
        aoStrCatFmt(buf, "    %S\n", ir_instr_str);
        aoStrRelease(ir_instr_str);
    }
    return buf;
}

aoStr *irParamsToString(PtrVec *ir_value_vector) {
    aoStr *buf = aoStrNew();
    if (ir_value_vector->size == 0) {
        aoStrCatLen(buf,str_lit("void"));
    } else {
        for (int i = 0; i < ir_value_vector->size; ++i) {
            IrValue *ir_value = vecGet(IrValue *, ir_value_vector, i);
            aoStr *ir_value_str = irValueToString(ir_value);
            aoStrCatFmt(buf,"%S",ir_value_str);
            if (i + 1 != ir_value_vector->size) {
                aoStrCatLen(buf,str_lit(", "));
            }
            aoStrRelease(ir_value_str);
        }
    }
    return buf;
}

aoStr *irFunctionToString(IrFunction *ir_func) {
    aoStr *buf = aoStrNew();
    /* This is not as high fidelity as LLVM's ir */
    const char *ir_return_type_str = irValueTypeToString(ir_func->return_type->type);
    aoStrCatFmt(buf, "%s %S(", ir_return_type_str, ir_func->name);
    aoStr *params_str = irParamsToString(ir_func->params);
    aoStrCatFmt(buf, "%S) {\n", params_str);
    aoStrRelease(params_str);

    aoStr *ir_entry_block_str = irBlockToString(ir_func->entry_block);

    aoStrCatFmt(buf, "%S",ir_entry_block_str);
    aoStrRelease(ir_entry_block_str);

    for (int i = 0; i < ir_func->blocks->size; ++i) {
        IrBlock *ir_block = vecGet(IrBlock *, ir_func->blocks, i);
        aoStr *ir_block_str = irBlockToString(ir_block);
        aoStrCatFmt(buf, "%S\n", ir_block_str);
        aoStrRelease(ir_block_str);

        if (i + 1 != ir_func->blocks->size) {
            aoStrPutChar(buf, '\n');
        } 
    }

    aoStrCatLen(buf, str_lit("}\n"));
    return buf;
}

/*==================== IR CONSTRUCTORS ====================================== */
IrBlock *irBlockNew(aoStr *label) {
    IrBlock *ir_block = (IrBlock *)irArenaAlloc(sizeof(IrBlock));
    ir_block->label = label;
    ir_block->instructions = ptrVecNew();
    ir_block->predecessors = ptrVecNew();
    ir_block->successors = ptrVecNew();
    ir_block->sealed = 0;
    ir_block->ssa_values = strMapNew(8);
    return ir_block;
}

IrValue *irValueNew(IrValueType ir_type, IrValueKind ir_kind) {
    IrValue *ir_value = (IrValue *)irArenaAlloc(sizeof(IrValue));
    ir_value->type = ir_type;
    ir_value->kind = ir_kind;
    ir_value->version = 1;
    return ir_value;
}

IrInstr *irInstrNew(IrOpcode opcode) {
    IrInstr *ir_instr = (IrInstr *)irArenaAlloc(sizeof(IrInstr));
    ir_instr->opcode = opcode;
    ir_instr->result = NULL;
    ir_instr->op1 = NULL;
    ir_instr->op2 = NULL;
    ir_instr->op3 = NULL;
    ir_instr->target_block = NULL;
    ir_instr->fallthrough_block = NULL;
    return ir_instr;
}

/* We will reset this after each function has been created */
static int ir_tmp_variable_count = 0;

IrValue *irTmpVariable(void) {
    IrValue *ir_value = irValueNew(IR_TYPE_LABEL, IR_VALUE_TEMP);
    ir_value->name = aoStrPrintf("%%tmp%d",ir_tmp_variable_count++);
    return ir_value;
}

IrValue *irGetAllocaVar(IrInstr *ir_alloca) {
    assert(ir_alloca->opcode == IR_OP_ALLOCA);
    return ir_alloca->op1;
}

IrValue *irConstInt(long i64) {
    IrValue *ir_value = irValueNew(IR_TYPE_I64, IR_VALUE_CONST_INT);
    ir_value->i64 = i64;
    return ir_value;
}


/* Where `ir_value` is always a constant int so we know how much stack space
 * we require. We _may_ want type info for of the thing we are storing? */
IrInstr *irAlloca(long size) {
    IrInstr *ir_instr = irInstrNew(IR_OP_ALLOCA);
    IrValue *ir_alloca_size = irConstInt(size);
    IrValue *ir_temporary_variable = irTmpVariable();
    ir_instr->op1 = ir_temporary_variable;
    ir_instr->op2 = ir_alloca_size;
    return ir_instr;
}

/* op1 is where we are storing something and op2 is the thing we are storing 
 * I think op1 could/shoule have an offset as it is either going to be the 
 * stack or it is going to be a struct/pointer offset? */
IrInstr *irStore(IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_instr = irInstrNew(IR_OP_STORE);
    ir_instr->op1 = ir_dest;
    ir_instr->op2 = ir_value;
    return ir_instr;
}

IrFunction *irFunctionNew(aoStr *name) {
    IrFunction *ir_function = (IrFunction *)irArenaAlloc(sizeof(IrFunction));
    ir_function->name = name;
    ir_function->params = ptrVecNew();
    ir_function->blocks = ptrVecNew();
    ir_function->entry_block = NULL;
    ir_function->exit_block = NULL;
    ir_function->variables = strMapNew(16);
    return ir_function;
}

void irLower(Cctrl *cc, IrFunction *ir_function, Ast *ast) {
    return;
}

IrValueType irConvertType(Cctrl *cc, AstType *type) {
    switch (type->kind) {
        case AST_TYPE_VOID: return IR_TYPE_VOID;
        case AST_TYPE_INT: {
            switch (type->size) {
                case 1: return IR_TYPE_I8;
                case 2: return IR_TYPE_I16;
                case 4: return IR_TYPE_I32;
                case 8: return IR_TYPE_I64;
                default:
                    cctrlIce(cc,"Invalid integer size `%d` for type\n",
                            astTypeToString(type),
                            type->size);
            }
        }
        case AST_TYPE_FLOAT:   return IR_TYPE_F64;
        case AST_TYPE_CHAR:    return IR_TYPE_I8;
        case AST_TYPE_ARRAY:   return IR_TYPE_ARRAY;
        case AST_TYPE_POINTER: return IR_TYPE_PTR;
        case AST_TYPE_FUNC:    return IR_TYPE_FUNCTION;
        case AST_TYPE_CLASS:   return IR_TYPE_STRUCT;
        case AST_TYPE_UNION:   return IR_TYPE_STRUCT;
        case AST_TYPE_VIS_MODIFIER:
            cctrlIce(cc, "Type visibility modifier is not a type!\n");
        case AST_TYPE_INLINE:
            cctrlIce(cc, "Type `inline` is not a type!\n");
        case AST_TYPE_AUTO:
            cctrlIce(cc, "Type `auto` failed to infer it's runtime type\n");
        default:
            cctrlIce(cc, "Type `%s` unhandled\n",astTypeToString(type));
    }
}

/*==================== IR LOWERING ========================================== */


/* Allocate stack space and store the ir_value */
static void irBlockStore(IrBlock *ir_block, IrValue *ir_value, long size) {
    /* Allocate on the stack an amount of space big enough to hold the value 
     * and get the location for where we are storing something. */
    IrInstr *ir_alloca = irAlloca(size);
    IrValue *ir_tmp_var = irGetAllocaVar(ir_alloca);
    /* Now create the instruction to store */
    IrInstr *ir_store = irStore(ir_tmp_var, ir_value);

    /* Push the allocation and the store in the instructions vector */
    ptrVecPush(ir_block->instructions, ir_alloca);
    ptrVecPush(ir_block->instructions, ir_store);
}

/* Function parameters can only be a one of a few different types, thus this is
 * fairly reasonable to have separate */
IrValue *irConvertAstFuncParam(Cctrl *cc, Ast *ast_param) {
    IrValueType ir_type = irConvertType(cc, ast_param->type);
    IrValue *ir_value = irValueNew(ir_type, IR_VALUE_PARAM);
    if (ast_param->kind == AST_LVAR) {
        ir_value->name = ast_param->lname;
    } else if (ast_param->kind == AST_FUNPTR) {
        ir_value->name = ast_param->fname;
    } else {
        /* We need to handle all cases, such as default arguments which 
         * change things considerably */
        loggerPanic("Unhandled Ast: %s\n", astToString(ast_param));
    }
    return ir_value;
}

IrFunction *irLowerFunction(Cctrl *cc, Ast *ast_function) {
    Ast *body = ast_function->body;
    IrFunction *ir_function = irFunctionNew(ast_function->fname);
    IrBlock *ir_entry_block = irBlockNew(aoStrDupRaw(str_lit("entry")));

    IrValueType ir_type = irConvertType(cc, ast_function->type->rettype);
    IrValue *ir_value = irValueNew(ir_type, IR_VALUE_UNDEFINED);
    ir_function->return_type = ir_value;

    for (int i = 0; i < ast_function->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *,ast_function->params,i);
        /* Add to the current basic block as a store 
         * this is 2 instructions;
         * 1) Allocate space on the stack for the function parameter
         * 2) Store the function parameter at this location.
         * */
        IrValue *ir_param = irConvertAstFuncParam(cc, ast_param);
        ptrVecPush(ir_function->params,ir_param);
        irBlockStore(ir_entry_block,ir_param,ast_param->type->size);
    }

    ir_function->entry_block = ir_entry_block;

    return ir_function;
}

void irLowerAst(Cctrl *cc) {
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            IrFunction *ir_func = irLowerFunction(cc, ast);
            aoStr *ir_func_str = irFunctionToString(ir_func);
            printf("%s\n",ir_func_str->data);

        } else if (ast->kind == AST_ASM_FUNC_BIND) {
           // loggerWarning("Asm function bind cannot lowered by ir - \n");
        } else {
       //     loggerWarning("Cannot lower AST to IR: %s\n",
       //             astToString(ast));
        }
    }
}

