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
#include "lexer.h"
#include "map.h"
#include "memory.h"
#include "transpiler.h"
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

int irIsFloat(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_F64;
}

int irIsInt(IrValueType ir_value_type) {
    return ir_value_type == IR_TYPE_I8 ||
           ir_value_type == IR_TYPE_I16 ||
           ir_value_type == IR_TYPE_I32 ||
           ir_value_type == IR_TYPE_I64;
}

int irAreCompatibleCmpTypes(IrValueType t1, IrValueType t2) {
    if (t1 == t2) return 1;
    if (irIsInt(t1) & irIsInt(t2)) return 1;
    if (irIsFloat(t1) & irIsFloat(t2)) return 1;

    if ((t1 == IR_TYPE_PTR && irIsInt(t2)) || (t2 == IR_TYPE_PTR && irIsInt(t1)))
        return 1;

    return 0;
}

int irBlockHasBlock(PtrVec *blocks_vector, IrBlock *needle) {
    for (int i = 0; i < blocks_vector->size; ++i) {
        IrBlock *block = vecGet(IrBlock *,blocks_vector,i);
        if (aoStrCmp(block->label,needle->label)) {
            return 1;
        }
    }
    return 0;
}

IrValueType irConvertType(AstType *type) {
    switch (type->kind) {
        case AST_TYPE_VOID: return IR_TYPE_VOID;
        case AST_TYPE_INT: {
            switch (type->size) {
                case 1: return IR_TYPE_I8;
                case 2: return IR_TYPE_I16;
                case 4: return IR_TYPE_I32;
                case 8: return IR_TYPE_I64;
                default:
                    loggerPanic("Invalid integer size `%d` for type %s\n",
                            type->size,
                            astTypeToString(type));
            }
        }
        case AST_TYPE_CHAR:    return IR_TYPE_I8;
        case AST_TYPE_FLOAT:   return IR_TYPE_F64;
        case AST_TYPE_ARRAY:   return IR_TYPE_ARRAY;
        case AST_TYPE_POINTER: return IR_TYPE_PTR;
        case AST_TYPE_FUNC:    return IR_TYPE_FUNCTION;
        case AST_TYPE_CLASS:   return IR_TYPE_STRUCT;
        case AST_TYPE_UNION:   return IR_TYPE_STRUCT;
        case AST_TYPE_VIS_MODIFIER:
            loggerPanic("Type visibility modifier is not a type!\n");
        case AST_TYPE_INLINE:
            loggerPanic( "Type `inline` is not a type!\n");
        case AST_TYPE_AUTO:
            loggerPanic("Type `auto` failed to infer it's runtime type\n");
        default:
            loggerPanic("Type `%s` unhandled\n",astTypeToString(type));
    }
}


/*==================== IR STRING REPRESENTATIONS ============================ */
const char *irValueTypeToString(IrValueType ir_value_type) {
    const char *type_str = NULL;
    switch (ir_value_type) {
        case IR_TYPE_VOID:     type_str = "void"; break;
        case IR_TYPE_I8:       type_str = "i8"; break;
        case IR_TYPE_I16:      type_str = "i16"; break;
        case IR_TYPE_I32:      type_str = "i32"; break;
        case IR_TYPE_I64:      type_str = "i64"; break;
        case IR_TYPE_F64:      type_str = "f64"; break;
        case IR_TYPE_PTR:      type_str = "ptr"; break;
        case IR_TYPE_ARRAY:    type_str = "array"; break;
        case IR_TYPE_STRUCT:   type_str = "struct"; break;
        case IR_TYPE_FUNCTION: type_str = "function"; break;
        case IR_TYPE_LABEL:    return "";
        default: loggerPanic("Unhandled IrValueType: %d\n", ir_value_type);
    }
    if (is_terminal) {
        return mprintf(ESC_WHITE"%s"ESC_RESET,type_str);
    } else {
        return mprintf("%s",type_str);
    }
}

aoStr *irValueToString(IrValue *ir_value) {
    aoStr *buf = aoStrNew();

    switch (ir_value->kind) {
        case IR_VALUE_CONST_INT:   aoStrCatFmt(buf, "%I", ir_value->i64); break;
        case IR_VALUE_CONST_FLOAT: aoStrCatFmt(buf, "%f", ir_value->f64); break;
        case IR_VALUE_CONST_STR:   aoStrCatFmt(buf, "\"%S\"", ir_value->name); break;
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
    aoStrPutChar(buf,' ');
    aoStrCatFmt(buf,"%s",ir_value_type_str);

    return buf;
}

const char *irCmpKindToString(IrCmpKind ir_cmp_kind) {
    const char *ir_cmp_str = NULL;
    switch (ir_cmp_kind) {
        case IR_CMP_EQ:  ir_cmp_str = "cmp_eq"; break;
        case IR_CMP_NE:  ir_cmp_str = "cmp_ne"; break;
        case IR_CMP_LT:  ir_cmp_str = "cmp_lt"; break;
        case IR_CMP_LE:  ir_cmp_str = "cmp_le"; break;
        case IR_CMP_GT:  ir_cmp_str = "cmp_gt"; break;
        case IR_CMP_GE:  ir_cmp_str = "cmp_ge"; break;
        case IR_CMP_ULT: ir_cmp_str = "cmp_ult"; break;
        case IR_CMP_ULE: ir_cmp_str = "cmp_ule"; break;
        case IR_CMP_UGT: ir_cmp_str = "cmp_ugt"; break;
        case IR_CMP_UGE: ir_cmp_str = "cmp_uge"; break;
        case IR_CMP_OEQ: ir_cmp_str = "cmp_oeq"; break;
        case IR_CMP_ONE: ir_cmp_str = "cmp_one"; break;
        case IR_CMP_OLT: ir_cmp_str = "cmp_olt"; break;
        case IR_CMP_OLE: ir_cmp_str = "cmp_ole"; break;
        case IR_CMP_OGT: ir_cmp_str = "cmp_ogt"; break;
        case IR_CMP_OGE: ir_cmp_str = "cmp_oge"; break;
        case IR_CMP_UNO: ir_cmp_str = "cmp_uno"; break;
        case IR_CMP_ORD: ir_cmp_str = "cmp_ord"; break;
        default: loggerPanic("Unhandled comparison kind: %d\n", ir_cmp_kind);
    }
    if (is_terminal) {
        return mprintf(ESC_BLUE"%-8s"ESC_RESET,ir_cmp_str);
    } else {
        return mprintf("%-8s",ir_cmp_str);
    }
}

const char *irOpcodeToString(IrInstr *ir_instr) {
    IrOpcode opcode = ir_instr->opcode;
    const char *ir_op_str;
    switch (opcode) {
        case IR_OP_ALLOCA:   ir_op_str = "alloca"; break;
        case IR_OP_LOAD:     ir_op_str = "load"; break;
        case IR_OP_STORE:    ir_op_str = "store"; break;
        case IR_OP_GEP:      ir_op_str = "gep"; break;

        case IR_OP_IADD:      ir_op_str = "iadd"; break;
        case IR_OP_ISUB:      ir_op_str = "isub"; break;
        case IR_OP_IMUL:      ir_op_str = "imul"; break;
        case IR_OP_IDIV:      ir_op_str = "idiv"; break;
        case IR_OP_UDIV:      ir_op_str = "udiv"; break;
        case IR_OP_IREM:      ir_op_str = "irem"; break;
        case IR_OP_UREM:      ir_op_str = "urem"; break;
        case IR_OP_INEG:      ir_op_str = "ineg"; break;

        case IR_OP_FADD:      ir_op_str = "fadd"; break;
        case IR_OP_FSUB:      ir_op_str = "fsub"; break;
        case IR_OP_FMUL:      ir_op_str = "fmul"; break;
        case IR_OP_FDIV:      ir_op_str = "fdiv"; break;
        case IR_OP_FNEG:      ir_op_str = "fneg"; break;

        case IR_OP_AND:      ir_op_str = "and"; break;
        case IR_OP_OR:       ir_op_str = "or"; break;
        case IR_OP_XOR:      ir_op_str = "xor"; break;
        case IR_OP_SHL:      ir_op_str = "shl"; break;
        case IR_OP_SHR:      ir_op_str = "shr"; break;
        case IR_OP_SAR:      ir_op_str = "sar"; break;
        case IR_OP_NOT:      ir_op_str = "not"; break;
        case IR_OP_ICMP:
        case IR_OP_FCMP:
            return irCmpKindToString(ir_instr->extra.cmp_kind);
        case IR_OP_TRUNC:    ir_op_str = "trunc"; /* Int to smaller int i.e i32 -> i16 */ break;
        case IR_OP_ZEXT:     ir_op_str = "zext"; /* zero extend */ break;
        case IR_OP_SEXT:     ir_op_str = "sext"; /* sign extend */ break;
        case IR_OP_FPTRUNC:  ir_op_str = "fptrunc"; /* float truncate f32 -> f64 */ break;
        case IR_OP_FPEXT:    ir_op_str = "fpext";   /* f32 -> f64 */ break;
        case IR_OP_FPTOUI:   ir_op_str = "fptoui"; break;
        case IR_OP_FPTOSI:   ir_op_str = "fptosi"; break;
        case IR_OP_UITOFP:   ir_op_str = "uitofp"; break;
        case IR_OP_SITOFP:   ir_op_str = "sitofp"; break;
        case IR_OP_PTRTOINT: ir_op_str = "ptrtoint"; break;
        case IR_OP_INTTOPTR: ir_op_str = "inttoptr"; break;
        case IR_OP_BITCAST:  ir_op_str = "bitcast"; break;
        case IR_OP_RET:      ir_op_str = "ret"; break;
        case IR_OP_BR:       ir_op_str = "br"; break;
        case IR_OP_JMP:      ir_op_str = "jmp"; break;
        case IR_OP_LOOP:     ir_op_str = "loop"; break;
        case IR_OP_SWITCH:   ir_op_str = "switch"; break;
        case IR_OP_CALL:     ir_op_str = "call"; break;
        case IR_OP_PHI:      ir_op_str = "phi"; break;
        case IR_OP_SELECT:   ir_op_str = "select"; break;
        case IR_OP_VA_ARG:   ir_op_str = "va_arg"; break;
        case IR_OP_VA_START: ir_op_str = "va_start"; break;
        case IR_OP_VA_END:   ir_op_str = "va_end"; break;
        default: loggerPanic("Unhandled opcode: %d\n", opcode);
    }
    if (is_terminal) {
        return mprintf(ESC_BLUE"%-8s"ESC_RESET,ir_op_str);
    } else {
        return mprintf("%-8s",ir_op_str);
    }
}

aoStr *irBlockToString(IrBlock *ir_block);
void irPhiPairToString(aoStr *buf, IrPhiPair *ir_phi_pair) {
    aoStr *ir_value_str = irValueToString(ir_phi_pair->ir_value);
    /* <Value, block> */
    aoStrCatFmt(buf, "[ %S, %S ]", ir_value_str, ir_phi_pair->ir_block->label);
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
    const char *op = irOpcodeToString(ir_instr);

    switch (ir_instr->opcode) {
        case IR_OP_CALL: {
            PtrVec *fn_args =  ir_instr->extra.fn_args;
            if (fn_args && fn_args->size > 0) {
                for (int i = 0; i < fn_args->size; ++i) {
                    IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                    aoStr *ir_value_str = irValueToString(ir_value);
                    if (i != 0) {
                        aoStrCatLen(buf,str_lit("    "));
                    }
                    if (is_terminal) {
                        aoStrCatFmt(buf, ESC_BLUE"%-8s "ESC_RESET"%S\n",
                                    "arg", ir_value_str);
                    } else {
                        aoStrCatFmt(buf, "%-8s %S\n", "arg", ir_value_str);
                    }
                    aoStrRelease(ir_value_str);
                }
                aoStrCatLen(buf,str_lit("    "));
            }
            aoStrCatFmt(buf, "%s %S", op, ir_instr->result->name);
            break;
        }

        case IR_OP_BR: {
            aoStr *ir_value_str = irValueToString(ir_instr->op1);
            aoStrCatFmt(buf, "%s %S, %S, %S", op, ir_value_str,
                        ir_instr->target_block->label,
                        ir_instr->fallthrough_block->label);
            break;
        }

        case IR_OP_LOOP:
        case IR_OP_JMP: {
            aoStrCatFmt(buf, "%s %S", op, ir_instr->target_block->label);
            break;
        }

        case IR_OP_PHI: {
            aoStr *phi_pairs_str = aoStrNew();
            for (int i = 0; i < ir_instr->extra.phi.pairs->size; ++i) {
                IrPhiPair *ir_phi_pair = vecGet(IrPhiPair *,
                                                ir_instr->extra.phi.pairs, i);
                irPhiPairToString(phi_pairs_str, ir_phi_pair);
                if (i + 1 != ir_instr->extra.phi.pairs->size) {
                    aoStrPutChar(phi_pairs_str, ' ');;
                }
            }
            aoStrCatFmt(buf,"%s %S",op, phi_pairs_str);
            break;
        }

        default: {
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
            break;
        }
    }
    
    return buf;
}

aoStr *irBlockConnectionsToString(PtrVec *connections) {
    aoStr *buf = aoStrNew();
    aoStrPutChar(buf, '{');
    for (int i = 0; i < connections->size; ++i) {
        IrBlock *ir_connection = vecGet(IrBlock *,connections,i);
        aoStrCatFmt(buf, "%S", ir_connection->label);
        if (i + 1 != connections->size) {
            aoStrCatLen(buf, str_lit(", "));
        }
    }
    aoStrPutChar(buf, '}');
    return buf;
}

/* Convert a basic block to a string
 * <block_name>:
 *   <instructions...>
 * */
aoStr *irBlockToString(IrBlock *ir_block) {
    aoStr *buf = aoStrNew();
    aoStr *predecessors_str = irBlockConnectionsToString(ir_block->predecessors);
    aoStr *successors_str = irBlockConnectionsToString(ir_block->successors);

    if (is_terminal) {
        aoStrCatFmt(buf, ESC_BOLD"  %S"ESC_CLEAR_BOLD":", ir_block->label);
    } else {
        aoStrCatFmt(buf, "  %S:", ir_block->label);
    }

    aoStrCatFmt(buf, "  predecessors: %S"
                     "  successors: %S\n",predecessors_str, successors_str);

    for (int j = 0; j < ir_block->instructions->size; ++j) {
        IrInstr *ir_instr = vecGet(IrInstr *, ir_block->instructions, j);
        aoStr *ir_instr_str = irInstrToString(ir_instr);
        aoStrCatFmt(buf, "    %S\n", ir_instr_str);
        aoStrRelease(ir_instr_str);
    }

    aoStrRelease(predecessors_str);
    aoStrRelease(successors_str);
    return buf;
}

void irPrintBlock(IrBlock *ir_block) {
    aoStr *ir_block_str = irBlockToString(ir_block);
    printf("%s\n",ir_block_str->data);
    aoStrRelease(ir_block_str);
}

aoStr *irParamsToString(PtrVec *ir_value_vector) {
    aoStr *buf = aoStrNew();
    if (ir_value_vector->size != 0) {
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
    const char *ir_return_type_str = irValueTypeToString(ir_func->return_value->type);
    aoStrCatFmt(buf, "%s %S(", ir_return_type_str, ir_func->name);
    aoStr *params_str = irParamsToString(ir_func->params);
    aoStrCatFmt(buf, "%S) {\n", params_str);
    aoStrRelease(params_str);

    aoStr *ir_entry_block_str = irBlockToString(ir_func->entry_block);

    aoStrCatFmt(buf, "%S\n",ir_entry_block_str);
    aoStrRelease(ir_entry_block_str);

    for (int i = 0; i < ir_func->blocks->size; ++i) {
        IrBlock *ir_block = vecGet(IrBlock *, ir_func->blocks, i);
        aoStr *ir_block_str = irBlockToString(ir_block);
        aoStrCatFmt(buf, "%S\n", ir_block_str);
        aoStrRelease(ir_block_str);
    }

    buf->len--;

    aoStrCatLen(buf, str_lit("}\n"));
    return buf;
}

void irPrintFunction(IrFunction *ir_function) {
    aoStr *ir_function_str = irFunctionToString(ir_function);
    printf("%s\n",ir_function_str->data);
    aoStrRelease(ir_function_str);
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

IrProgram *irProgramNew(void) {
    IrProgram *program = (IrProgram *)irArenaAlloc(sizeof(IrProgram));
    program->functions = ptrVecNew();
    program->global_variables = strMapNew(16);
    program->strings = strMapNew(16);
    program->types = strMapNew(16);
    return program;
}

IrValue *irValueNew(IrValueType ir_type, IrValueKind ir_kind) {
    IrValue *ir_value = (IrValue *)irArenaAlloc(sizeof(IrValue));
    ir_value->type = ir_type;
    ir_value->kind = ir_kind;
    ir_value->version = 1;
    return ir_value;
}

void irCtxReset(IrCtx *ctx) {
    ctx->flags = 0;
    ctx->current_block = NULL;
    ctx->exit_block = NULL;
    ctx->cond_end_block = NULL;
    ctx->loop_end_block = NULL;
    ctx->loop_head_block = NULL;
    ctx->switch_end_block = NULL;
}

IrCtx *irCtxNew(void) {
    IrCtx *ctx = (IrCtx *)xmalloc(sizeof(IrCtx));
    irCtxReset(ctx);
    return ctx;
}

void irCtxRelease(IrCtx *ctx) {
    xfree(ctx);
}

static void irCtxSetCurrentBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->current_block = ir_block;
}

static void irCtxSetExitBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->exit_block = ir_block;
}

static void irCtxSetCondEndBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->cond_end_block = ir_block;
}

static void irCtxSetLoopEndBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->loop_end_block = ir_block;
}

static void irCtxSetLoopHeadBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->loop_head_block = ir_block;
}

static void irCtxSetSwitchEndBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->switch_end_block = ir_block;
}

/* We will reset this after each function has been created */
static int ir_tmp_variable_count = 1;

IrValue *irTmpVariable(IrValueType ir_value_type) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_TEMP);
    ir_value->name = aoStrPrintf("%%t%d",ir_tmp_variable_count++);
    return ir_value;
}

IrValue *irGetAllocaVar(IrInstr *ir_alloca) {
    assert(ir_alloca->opcode == IR_OP_ALLOCA);
    return ir_alloca->op1;
}

IrValue *irConstInt(IrValueType ir_value_type, long i64) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_CONST_INT);
    ir_value->i64 = i64;
    return ir_value;
}

IrValue *irConstFloat(IrValueType ir_value_type, double f64) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_CONST_FLOAT);
    ir_value->f64 = f64;
    return ir_value;
}

IrValue *irGlobalString(IrProgram *ir_program, Ast *ast) {
    IrValue *ir_value = strMapGetAoStr(ir_program->strings, ast->slabel);
    if (!ir_value) {
        /* A string is an array of characters */
        ir_value = irValueNew(IR_TYPE_ARRAY, IR_VALUE_CONST_STR);
        ir_value->str = ast->sval;
        ir_value->str_real_len = ast->real_len;
        strMapAdd(ir_program->strings, ast->slabel->data, ir_value);
    }
    return ir_value;
}

IrValue *irFunctionGetLocal(IrFunction *ir_function, Ast *ast) {
    assert(ast->kind == AST_LVAR);
    IrValue *ir_local_var = strMapGetAoStr(ir_function->variables,
                                           ast->tmp_var_name);
    if (!ir_local_var) {
        loggerPanic("Local variable with original name `%s` not found\n",
                ast->lname->data);
    }
    return ir_local_var;
}

IrValue *irFunctionGetGlobal(IrFunction *ir_function, Ast *ast) {
    assert(ast->kind == AST_GVAR);
    IrProgram *ir_program = ir_function->program;
    IrValue *ir_global_var = strMapGetAoStr(ir_program->global_variables,
                                            ast->gname);
    if (!ir_global_var) {
        loggerPanic("Global variable with name `%s` not found\n",
                ast->gname->data);
    }
    return ir_global_var;
}

IrValue *irFunctionGetLocalFnPtr(IrFunction *ir_function, Ast *ast) {
    assert(ast->kind == AST_FUNPTR || ast->kind == AST_FUNPTR_CALL);
    IrValue *ir_fnptr = strMapGetAoStr(ir_function->variables,
                                       ast->tmp_fnptr_name);
    if (!ir_fnptr) {
        loggerPanic("Local function pointer with original name `%s` not found\n",
                ast->fname->data);
    }
    return ir_fnptr;
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

IrPhiPair *irPhiPair(IrBlock *ir_block, IrValue *ir_value) {
    IrPhiPair *ir_phi_pair = (IrPhiPair *)irArenaAlloc(sizeof(IrPhiPair));
    ir_phi_pair->ir_value = ir_value;
    ir_phi_pair->ir_block = ir_block;
    return ir_phi_pair;
}

IrInstr *irPhi(IrBlock *block, IrValue *result) {
    IrInstr *ir_phi_instr = irInstrNew(IR_OP_PHI);
    ir_phi_instr->result = result;
    ir_phi_instr->extra.phi.pairs = ptrVecNew();

    ptrVecPush(block->instructions, ir_phi_instr);
    return ir_phi_instr;
}

void irAddPhiIncoming(IrInstr *ir_phi_instr,
                      IrValue *ir_value, 
                      IrBlock *ir_block)
{
    IrPhiPair *ir_phi_pair = irPhiPair(ir_block, ir_value);
    ptrVecPush(ir_phi_instr->extra.phi.pairs, ir_phi_pair);
}

static int ir_block_count = 0;
void irBlockCountReset(void) {
    ir_block_count = 0;
}

aoStr *irBlockName(void) {
    return aoStrPrintf("bb%d", ir_block_count++);
}

/* Where `ir_value` is always a constant int so we know how much stack space
 * we require. We _may_ want type info for of the thing we are storing? */
IrInstr *irAlloca(IrBlock *ir_block, AstType *ast_type) {
    IrInstr *ir_alloca_instr = irInstrNew(IR_OP_ALLOCA);
    IrValueType ir_value_type = irConvertType(ast_type);
    IrValue *ir_alloca_size = irConstInt(ir_value_type, ast_type->size);
    IrValue *ir_temporary_variable = irTmpVariable(ir_value_type);
    ir_alloca_instr->op1 = ir_temporary_variable;
    ir_alloca_instr->op2 = ir_alloca_size;
    ptrVecPush(ir_block->instructions, ir_alloca_instr);
    return ir_alloca_instr;
}

/* op1 is where we are storing something and op2 is the thing we are storing 
 * I think op1 could/shoule have an offset as it is either going to be the 
 * stack or it is going to be a struct/pointer offset? */
IrInstr *irStore(IrBlock *ir_block, IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_store_instr = irInstrNew(IR_OP_STORE);
    ir_store_instr->op1 = ir_dest;
    ir_store_instr->op2 = ir_value;
    ptrVecPush(ir_block->instructions, ir_store_instr);
    return ir_store_instr;
}

IrInstr *irLoad(IrBlock *ir_block, IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_load_instr = irInstrNew(IR_OP_LOAD);
    ir_load_instr->op1 = ir_dest;
    ir_load_instr->op2 = ir_value;
    ptrVecPush(ir_block->instructions, ir_load_instr);
    return ir_load_instr;
}

IrInstr *irRet(IrBlock *ir_block, IrValue *ir_value) {
    IrInstr *ir_return_instr = irInstrNew(IR_OP_RET);
    ir_return_instr->op1 = ir_value;
    ptrVecPush(ir_block->instructions, ir_return_instr);
    return ir_return_instr;
}

IrInstr *irGetElementPointer(IrBlock *ir_block,
                             IrValue *ir_dest,
                             IrValue *ir_value)
{
    IrInstr *ir_getelementptr_instr = irInstrNew(IR_OP_GEP);
    ir_getelementptr_instr->op1 = ir_dest;
    ir_getelementptr_instr->op2 = ir_value;
    ptrVecPush(ir_block->instructions, ir_getelementptr_instr);
    return ir_getelementptr_instr;
}

IrInstr *irGetAtIdx(IrBlock *ir_block,
                    IrValue *ir_dest,
                    IrValue *ir_value,
                    long offset)
{
    IrInstr *ir_getelementptr_instr = irInstrNew(IR_OP_GEP);
    IrValue *ir_offset = irConstInt(IR_TYPE_I64, offset);
    ir_getelementptr_instr->op1 = ir_dest;
    ir_getelementptr_instr->op2 = ir_value;
    ir_getelementptr_instr->op3 = ir_offset;
    ptrVecPush(ir_block->instructions, ir_getelementptr_instr);
    return ir_getelementptr_instr;
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

/*==================== IR AST PARSING ======================================= */
IrInstr *irCmp(IrBlock *block, IrValue *result, IrValue *op1, IrValue *op2,
               IrCmpKind kind);
IrInstr *irICmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irFCmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irJump(IrBlock *block, IrBlock *target);
IrInstr *irBranch(IrBlock *block, IrValue *cond, IrBlock *true_block,
              IrBlock *false_block);
IrValue *irLoadClassRef(IrCtx *ctx,
                        IrFunction *func,
                        Ast *cls,
                        AstType *field,
                        int offset);

IrValue *irExpression(IrCtx *ctx, IrFunction *func, Ast *ast);
void irStatement(IrCtx *ctx, IrFunction *func, Ast *ast);

IrInstr *irCmp(IrBlock *block, IrValue *result, IrValue *op1, IrValue *op2,
               IrCmpKind kind)
{
    if (result->type != IR_TYPE_I8) {
        loggerPanic("Result can only be a boolean type\n");
    }

    int is_float_cmp = irIsFloat(op1->type) || irIsFloat(op2->type);
    IrInstr *instr  = irInstrNew(is_float_cmp ? IR_OP_FCMP : IR_OP_ICMP);

    if (is_float_cmp) {
        if (op1->type != op2->type) {
            loggerPanic("Operand types do not match `op1%s` and `op2%s`",
                    irValueTypeToString(op1->type),
                    irValueTypeToString(op2->type));
        }

        switch (kind) {
            case IR_CMP_EQ: kind = IR_CMP_OEQ; break; /* ordered equal */
            case IR_CMP_NE: kind = IR_CMP_ONE; break; /* ordered not equal */
            case IR_CMP_LT: kind = IR_CMP_OLT; break; /* ordered less than */
            case IR_CMP_LE: kind = IR_CMP_OLE; break; /* ordered less or equal */
            case IR_CMP_GT: kind = IR_CMP_OGT; break; /* ordered greater than */
            case IR_CMP_GE: kind = IR_CMP_OGE; break; /* ordered greater or equal */
            default: break;
        }
    } else {
        /* Ensure that we can compare these types */
        if (!irAreCompatibleCmpTypes(op1->type, op2->type)) {
            loggerPanic("Operand types cannot be compared `op1%s` and `op2%s`",
                    irValueTypeToString(op1->type),
                    irValueTypeToString(op2->type));
        }
    }

    instr->result = result;
    instr->op1 = op1;
    instr->op2 = op2;
    instr->extra.cmp_kind = kind;
    ptrVecPush(block->instructions, instr);

    return instr;
}

IrInstr *irICmp(IrBlock *block,
                IrValue *result,
                IrCmpKind kind,
                IrValue *op1, 
                IrValue *op2)
{
    if (op1->type == IR_TYPE_PTR || op2->type == IR_TYPE_PTR) {
        if      (kind == IR_CMP_LT) kind = IR_CMP_ULT;
        else if (kind == IR_CMP_LE) kind = IR_CMP_ULE;
        else if (kind == IR_CMP_GT) kind = IR_CMP_UGT;
        else if (kind == IR_CMP_GE) kind = IR_CMP_UGE;
    }

    IrInstr *instr = irInstrNew(IR_OP_ICMP);
    instr->result = result;
    instr->op1 = op1;
    instr->op2 = op2;
    instr->extra.cmp_kind = kind;

    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irFCmp(IrBlock *block,
                IrValue *result,
                IrCmpKind kind,
                IrValue *op1, 
                IrValue *op2)
{
    IrInstr *instr = irInstrNew(IR_OP_FCMP);
    instr->result = result;
    instr->op1 = op1;
    instr->op2 = op2;
    instr->extra.cmp_kind = kind;

    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irFAdd(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_FADD);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;

    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irIAdd(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_IADD);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irFNeg(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    IrInstr *instr = irInstrNew(IR_OP_FNEG);
    instr->result = ir_result;
    instr->op1 = ir_expr;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irINeg(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    IrInstr *instr = irInstrNew(IR_OP_INEG);
    instr->result = ir_result;
    instr->op1 = ir_expr;
    ptrVecPush(block->instructions, instr);
    return instr;
}

/*==================== IR MATHS ============================================= */
// Reasonably these could be refactored to one function with an opcode

IrInstr *irFSub(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_FSUB);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irISub(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_ISUB);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irFMul(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_FMUL);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irIMul(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_IMUL);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irFDiv(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_FDIV);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irSDiv(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_IDIV);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irUDiv(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_UDIV);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irSRem(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_IREM);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irURem(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_UREM);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irBitAnd(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_AND);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irBitOr(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_OR);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irBitNot(IrBlock *block,
                  IrValue *ir_result,
                  IrValue *left)
{
    IrInstr *instr = irInstrNew(IR_OP_NOT);
    instr->result = ir_result;
    instr->op1 = left;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irXor(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_XOR);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irSHL(IrBlock *block,
                IrValue *ir_result,
                IrValue *left,
                IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_SHL);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}

IrInstr *irSHR(IrBlock *block,
               IrValue *ir_result,
               IrValue *left,
               IrValue *right)
{
    IrInstr *instr = irInstrNew(IR_OP_SHR);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    ptrVecPush(block->instructions, instr);
    return instr;
}
        
IrInstr *irBranch(IrBlock *block,
                  IrValue *cond,
                  IrBlock *true_block,
                  IrBlock *false_block)
{
    if (!block || !cond || !true_block || !false_block) {
        loggerPanic("irBranch: NULL parameter provided\n");
    }

    if (cond->type != IR_TYPE_I8) {
        IrValue *zero = irConstInt(IR_TYPE_I8, 0);
        IrValue *bool_cond = irTmpVariable(IR_TYPE_I8);
        irICmp(block, bool_cond, IR_CMP_NE, cond, zero);
        cond = bool_cond;
    }

    IrInstr *instr = irInstrNew(IR_OP_BR);
    instr->op1 = cond;
    instr->target_block = true_block;
    instr->fallthrough_block = false_block;

    ptrVecPush(block->instructions, instr);
    block->sealed = 1;

    if (!irBlockHasBlock(true_block->predecessors, block)) {
        ptrVecPush(true_block->predecessors, block);
    }

    if (!irBlockHasBlock(false_block->predecessors, block)) {
        ptrVecPush(false_block->predecessors, block);
    }

    if (!irBlockHasBlock(block->successors, true_block)) {
        ptrVecPush(block->successors, true_block);
    }

    if (!irBlockHasBlock(block->successors, false_block)) {
        ptrVecPush(block->successors, false_block);
    }

    return instr;
}

IrInstr *irJumpInternal(IrBlock *block, IrBlock *target, IrOpcode opcode) {
    if (!block || !target) {
        loggerPanic("NULL param\n");
    }

    if (block->sealed) {
        loggerWarning("Tried to add a jump to a sealed block: %s\n",
                block->label->data);
        return NULL;
    }

    IrInstr *instr = irInstrNew(opcode);
    instr->target_block = target;
    instr->fallthrough_block = NULL;

    /* Add to the current blocks instructions */
    ptrVecPush(block->instructions, instr);

    /* This block is done */
    block->sealed = 1;

    /* Now update the control flow graph */
    if (!irBlockHasBlock(block->successors, target)) {
        ptrVecPush(block->successors, target);
    }

    if (!irBlockHasBlock(target->predecessors, block)) {
        ptrVecPush(target->predecessors, block);
    }

    return instr;
}

IrInstr *irJump(IrBlock *block, IrBlock *target) {
    return irJumpInternal(block,target,IR_OP_JMP);
}

IrInstr *irLoop(IrBlock *block, IrBlock *target) {
    return irJumpInternal(block,target,IR_OP_LOOP);
}

/* Get Element Pointer */
IrValue *irLoadAddr(IrCtx *ctx, IrFunction *func, Ast *ast) {
    Ast *operand = ast->operand;
    IrValue *ir_dest = irTmpVariable(IR_TYPE_PTR);

    switch (operand->kind) {
        case AST_LVAR: {
            IrValue *ir_value = irFunctionGetLocal(func, operand);
            if (operand->type->kind == AST_TYPE_POINTER) {
                /* XXX: this feels extremely hacky */
                switch (operand->type->ptr->kind) {
                    case AST_TYPE_CLASS:
                        return irLoadClassRef(ctx, func, operand->cls, operand->type, 0);
             //           break;
                    case AST_TYPE_ARRAY:
                    case AST_TYPE_CHAR:
                        irGetElementPointer(ctx->current_block, ir_dest, ir_value);
                        break;
                    default:
                        irGetElementPointer(ctx->current_block, ir_dest, ir_value);
                        break;
                }
            } else {
                irGetElementPointer(ctx->current_block, ir_dest, ir_value);
            }
            break;
        }

        case AST_GVAR: {
            IrValue *ir_value = irFunctionGetGlobal(func, operand);
            irGetElementPointer(ctx->current_block, ir_dest, ir_value);
            break;
        }

        case AST_CLASS_REF: {
            loggerWarning("AST_CLASS_REF -> ir bugged\n");
            return irLoadClassRef(ctx, func, operand->cls, operand->type, 0);
            // return irExpression(ctx, func, operand->operand);
            if (operand->operand->kind == AST_LVAR ||
                operand->operand->kind == AST_DEREF)
            {
                //aoStrCatPrintf(buf, "movq   %d(%%rbp), %%rax\n\t", ast->cls->cls->operand->loff);
                //aoStrCatPrintf(buf, "addq   $%d, %%rax\n\t", ast->cls->type->offset);
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
            return irExpression(ctx, func, cls);
        }
        default:
            loggerPanic("Cannot turn Kind AST:%s %s into ir\n",
                    astKindToString(ast->kind),
                    astToString(ast));
    }

    return ir_dest;
}

IrValue *irAssignClassRef(IrCtx *ctx, IrFunction *func, Ast *cls, AstType *field, IrValue *rhs, int offset) {
    switch (cls->kind) {
        case AST_LVAR: {
            /* Load the offset into a variable and then assign */
            IrValue *ir_local = irFunctionGetLocal(func, cls);
            IrValue *ir_dest = irTmpVariable(IR_TYPE_PTR);
            (void)irGetAtIdx(ctx->current_block,
                             ir_dest,
                             ir_local,
                             field->offset + offset);
            irStore(ctx->current_block,ir_dest,rhs);
            return ir_dest;
        }

        case AST_GVAR:
            // loggerPanic("Global variables unimplemented: %s\n", astToString(cls));
            // total_offset = field->offset + offset;
            // asmGSave(buf,cls->clsname->data,field,total_offset);
            break;

        case AST_CLASS_REF:
            break;

        case AST_DEREF: {
            IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_dest = irTmpVariable(IR_TYPE_PTR);
            (void)irGetAtIdx(ctx->current_block,
                             ir_dest,
                             ir_expr,
                             field->offset + offset);
            irStore(ctx->current_block,ir_dest,rhs);
            return ir_dest;
        }
        default:
            loggerPanic("Failed to create ASM for: %s\n",
                    astToString(cls));
    }
    loggerPanic("Unimplemented %s\n", astKindToString(cls->kind));
    return NULL;
}

IrValue *irAssign(IrCtx *ctx, IrFunction *func, Ast *ast) {
    IrValue *rhs = irExpression(ctx, func, ast->right);

    if (ast->left->kind == AST_LVAR) {
        IrValue *ir_local = irFunctionGetLocal(func, ast->left);
        irStore(ctx->current_block, ir_local, rhs);
        /* Assignments return the value */
        return rhs;
    } else if (ast->left->kind == AST_GVAR) {
        IrValue *ir_global = irFunctionGetGlobal(func, ast->left);
        irStore(ctx->current_block, ir_global, rhs);
        return rhs; 
    } else if (ast->left->kind == AST_DEREF) {
        IrValue *ptr = irExpression(ctx, func, ast->left->left);
        irStore(ctx->current_block, ptr, rhs);
        return rhs;
    } else if (ast->left->kind == AST_CLASS_REF) {
        return irAssignClassRef(ctx, func, ast->left->cls, ast->left->type, rhs, 0);
    } else {
        loggerPanic("Unsupported LHS assignment %s\n",
                astToString(ast->left));
    }
}

IrValue *irLoadClassRef(IrCtx *ctx,
                        IrFunction *func,
                        Ast *cls,
                        AstType *field,
                        int offset)
{
    switch (cls->kind) {
        case AST_LVAR: {
            IrValueType ir_dest_type = irConvertType(cls->type->ptr);
            IrValue *ir_dest = irTmpVariable(ir_dest_type);
            IrValue *ir_local = irFunctionGetLocal(func, cls->left);
            (void)irGetAtIdx(ctx->current_block,
                             ir_dest,
                             ir_local,
                             field->offset + offset);
            return ir_dest;
        }

        /* Call function again, this would be a nested struct or a union */
        case AST_CLASS_REF:
            return irLoadClassRef(ctx,
                                 func,
                                 cls->cls,
                                 field,
                                 offset + field->offset);

        /* Load the `->` dereference */
        case AST_DEREF: {
            IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_dest = irTmpVariable(irConvertType(field));
            (void)irGetAtIdx(ctx->current_block,
                             ir_dest,
                             ir_expr,
                             field->offset + offset);
            return ir_dest;
        }

        case AST_GVAR:
        default:
            loggerPanic("Failed to create IR for: %s\n", astKindToString(cls->kind));

    }
}

IrValue *irExpression(IrCtx *ctx, IrFunction *func, Ast *ast) {
    IrBlock *ir_block = ctx->current_block;

    switch (ast->kind) {
        case AST_LITERAL: {
            switch (ast->type->kind) {
                case AST_TYPE_INT:
                case AST_TYPE_CHAR:
                    return irConstInt(irConvertType(ast->type), ast->i64);
                case AST_TYPE_FLOAT:
                    return irConstFloat(IR_TYPE_F64, ast->f64);
                default:
                    loggerPanic("Unknown literal: %s\n",
                             astKindToString(ast->type->kind));
            }
            break;
        }
        
        case AST_STRING: {
            return irGlobalString(func->program, ast);
        }

        case AST_LVAR: {
            IrValue *local_var = irFunctionGetLocal(func, ast);
            if (!local_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_var);
            return ir_load_dest;
        }

        /* @TODO: Check classes and arrays */
        case AST_DEREF: {
            IrValue *ir_ptr = irExpression(ctx, func, ast->operand);

            if (ir_ptr->type != IR_TYPE_PTR) {
                loggerPanic("Attempted to dereference a non-pointer: %s\n",
                            astToString(ast->left));
            }

            /* Dereferencing an Ast node is `type->ptr`, to get what the type
             * is pointing to */
            IrValueType ir_deref_type = irConvertType(ast->operand->type->ptr);
            IrValue *ir_tmp_var = irTmpVariable(ir_deref_type);
            irLoad(ir_block, ir_tmp_var, ir_ptr);
            return ir_tmp_var;
        }
        
        case AST_CLASS_REF: {
            IrValue *ir_value = irLoadClassRef(ctx, func, ast->cls, ast->type, 0); 
            return ir_value;
        }

        case AST_ADDR: {
            return irLoadAddr(ctx, func, ast);
        }

        case AST_FUNPTR: {
            IrValue *local_fnptr = irFunctionGetLocalFnPtr(func, ast);

            if (!local_fnptr) {
                char *keys = strMapKeysToString(func->variables);
                loggerPanic("func %s Variable %s not found keys = %s\n",
                            func->name->data,
                            ast->tmp_fnptr_name->data, keys);
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_fnptr);
            /* @Tracking Update where the variable is? */
            strMapAddAoStr(func->variables,ast->tmp_fnptr_name,ir_load_dest);
            return ir_load_dest;
        }

        case '=': {
            return irAssign(ctx, func, ast);
        }

        case TK_MOD_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            if (irIsFloat(irConvertType(ast->type))) {
                irFDiv(ir_block, ir_result, lhs, rhs);
            } else {
                if (ast->type->issigned) {
                    irSRem(ctx->current_block, ir_result, lhs, rhs);
                } else {
                    irURem(ctx->current_block, ir_result, lhs, rhs);
                }
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_MUL_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            if (irIsFloat(irConvertType(ast->type))) {
                irFMul(ir_block, ir_result, lhs, rhs);
            } else {
                irIMul(ir_block, ir_result, lhs, rhs);
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_DIV_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            if (irIsFloat(irConvertType(ast->type))) {
                irFDiv(ir_block, ir_result, lhs, rhs);
            } else {
                if (ast->type->issigned) {
                    irSDiv(ctx->current_block, ir_result, lhs, rhs);
                } else {
                    irUDiv(ctx->current_block, ir_result, lhs, rhs);
                }
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_SUB_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (irIsFloat(irConvertType(ast->type))) {
                irFSub(ir_block, ir_result, lhs, rhs);
            } else {
                irISub(ir_block, ir_result, lhs, rhs);
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_ADD_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (irIsFloat(irConvertType(ast->type))) {
                irFAdd(ir_block, ir_result, lhs, rhs);
            } else {
                irIAdd(ir_block, ir_result, lhs, rhs);
            }
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_XOR_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irXor(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_AND_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irBitAnd(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_OR_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irBitOr(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_SHR_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irSHR(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        case TK_SHL_EQU: {
            IrValue *lhs = irExpression(ctx, func, ast->left);
            IrValue *rhs = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            irSHL(ir_block, ir_result, lhs, rhs);
            irAssign(ctx, func, ast);
            return ir_result;
        }

        /* @TODO: Pre and post increment/decrement
         * Not sure what the difference in IR is yet and this is functional
         * for both. This should be split out when I've figured that out */
        case TK_PLUS_PLUS:
        case TK_PRE_PLUS_PLUS:
        case TK_MINUS_MINUS:
        case TK_PRE_MINUS_MINUS: {
            IrValue *ir_value =  NULL;
            if (ast->operand->kind == AST_LVAR) {
                ir_value = irFunctionGetLocal(func, ast->operand);
            } else if (ast->operand->kind == AST_GVAR) {
                ir_value = irFunctionGetGlobal(func, ast->operand);
            } else if (ast->operand->kind == AST_DEREF ||
                       ast->operand->kind == AST_CLASS_REF) {
                /* This would be the result of derefencing, for example, a 
                 * struct member, pointer ...*/
                ir_value = irExpression(ctx, func, ast->operand);
            } else {
                loggerPanic("Unsupported LHS assignment %s\n",
                        astKindToString(ast->operand->kind));
            }

            IrValueType ir_value_type = irConvertType(ast->operand->type);
            /* Function pointer which allows us to merge both implementations, 
             * PRE should minimally be split from POST increment/decrement */
            IrInstr *(*operator)(IrBlock *block, IrValue *ir_result,
                                 IrValue *left, IrValue *right) = NULL;
            int is_sub = ast->kind == TK_MINUS_MINUS ||
                         ast->kind == TK_PRE_MINUS_MINUS;

            IrValue *ir_size = NULL;
            if (irIsInt(ir_value_type)) {
                ir_size = irConstInt(ir_value_type, 1);
                if (is_sub) operator = &irISub;
                else        operator = &irIAdd;
            } else if (irIsFloat(ir_value_type)) {
                ir_size = irConstFloat(ir_value_type, 1.0);
                if (is_sub) operator = &irFSub;
                else        operator = &irFAdd;
            } else {
                ir_size = irConstInt(ir_value_type, ast->operand->type->size);
                if (is_sub) operator = &irISub;
                else        operator = &irIAdd;
            }

            IrValue *ir_result = irTmpVariable(ir_value_type);
            operator(ctx->current_block, ir_result,ir_value,ir_size);
            irStore(ctx->current_block, ir_value, ir_result);
            return ir_result;
        }

        case '+': {
            if (ast->right == NULL) {
                /* Unary plus - just return the operand */
                return irExpression(ctx, func, ast->left);
            } else {
                /* Binary addition */
                IrValue *left = irExpression(ctx, func, ast->left);
                IrValue *right = irExpression(ctx, func, ast->right);
    
                IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
                if (ast->type->kind == AST_TYPE_FLOAT) {
                    irFAdd(ctx->current_block, ir_result, left, right);
                } else {
                    irIAdd(ctx->current_block, ir_result, left, right);
                }
                return ir_result;
            }
        }

        case '-': {
            if (ast->right == NULL) {
                /* Unary negation */
                IrValue *ir_expr = irExpression(ctx, func, ast->left);
                IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

                if (ast->left->type->kind == AST_TYPE_FLOAT) {
                    irFNeg(ctx->current_block, ir_result, ir_expr);
                } else {
                    irINeg(ctx->current_block, ir_result, ir_expr);
                }
                return ir_result;
            } else {
                /* Binary subtraction */
                IrValue *left = irExpression(ctx, func, ast->left);
                IrValue *right = irExpression(ctx, func, ast->right);
                
                IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
                if (ast->type->kind == AST_TYPE_FLOAT) {
                    irFSub(ctx->current_block, ir_result, left, right);
                } else {
                    irISub(ctx->current_block, ir_result, left, right);
                }
                return ir_result;
            }
        }

        case '*': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (ast->type->kind == AST_TYPE_FLOAT) {
                irFMul(ctx->current_block, ir_result, left, right);
            } else {
                irIMul(ctx->current_block, ir_result, left, right);
            }
            return ir_result;
        }

        case '/': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);

            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (ast->type->kind == AST_TYPE_FLOAT) {
                irFDiv(ctx->current_block, ir_result, left, right);
            } else if (ast->type->issigned) {
                irSDiv(ctx->current_block, ir_result, left, right);
            } else {
                irUDiv(ctx->current_block, ir_result, left, right);
            }
            return ir_result;
        }
        
        case '%': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));
            if (ast->type->issigned) {
                irSRem(ctx->current_block, ir_result, left, right);
            } else {
                irURem(ctx->current_block, ir_result, left, right);
            }
            return ir_result;
        }

        case '^': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irXor(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case '~': {
            IrValue *ir_expr = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irBitNot(ctx->current_block, ir_result, ir_expr);
            return ir_result;
        }

        case '&': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irBitAnd(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case '|': {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irBitOr(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case TK_SHL: {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irSHL(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case TK_SHR: {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            IrValue *ir_result = irTmpVariable(irConvertType(ast->type));

            irSHR(ctx->current_block, ir_result, left, right);
            return ir_result;
        }

        case TK_AND_AND: {
            IrBlock *ir_block = ctx->current_block;
            IrBlock *ir_right_block = irBlockNew(irBlockName());
            IrBlock *ir_end_block = irBlockNew(irBlockName());

            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            irBranch(ir_block, left, ir_right_block, ir_end_block);
            ptrVecPush(func->blocks, ir_right_block);
            irCtxSetCurrentBlock(ctx, ir_right_block);

            IrValue *right = irExpression(ctx, func, ast->right);

            irJump(ctx->current_block, ir_end_block);
            irCtxSetCurrentBlock(ctx, ir_end_block);
            ptrVecPush(func->blocks, ir_end_block);

            IrInstr *phi_instr = irPhi(ir_end_block, ir_result);
            irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
            irAddPhiIncoming(phi_instr, right, ir_right_block);
            return ir_result;
        }

        case TK_OR_OR: {
            IrBlock *ir_block = ctx->current_block;
            IrBlock *ir_right_block = irBlockNew(irBlockName());
            IrBlock *ir_end_block = irBlockNew(irBlockName());

            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            irBranch(ir_block, left, ir_end_block, ir_right_block);
            ptrVecPush(func->blocks, ir_right_block);
            irCtxSetCurrentBlock(ctx, ir_right_block);

            IrValue *right = irExpression(ctx, func, ast->right);

            irJump(ctx->current_block, ir_end_block);
            irCtxSetCurrentBlock(ctx, ir_end_block);
            ptrVecPush(func->blocks, ir_end_block);

            IrInstr *phi_instr = irPhi(ir_end_block, ir_result);
            irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 1), ir_block);
            irAddPhiIncoming(phi_instr, right, ir_right_block);
            return ir_result;
        }

        case '!': {
            IrValue *ir_expr = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            /* Create NOT operation - a comparison against 0 */
            IrValue *zero = irConstInt(ir_expr->type, 0);
            irICmp(ctx->current_block, ir_result, IR_CMP_EQ, ir_expr, zero);

            return ir_result;
        }

        case '<': 
        case '>': 
        case TK_LESS_EQU:
        case TK_GREATER_EQU: 
        case TK_EQU_EQU: 
        case TK_NOT_EQU: {
            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *right = irExpression(ctx, func, ast->right);
            
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);
            IrCmpKind cmp_kind;
            switch (ast->kind) {
                case '<': {
                    cmp_kind = ast->type->issigned ? IR_CMP_LT : IR_CMP_ULT ;
                    break;
                }
                case '>': {
                    cmp_kind = ast->type->issigned ? IR_CMP_GT : IR_CMP_UGT;
                    break;
                }
                case TK_LESS_EQU: {
                    cmp_kind = ast->type->issigned ? IR_CMP_LE : IR_CMP_ULE;
                    break;
                }
                case TK_GREATER_EQU: {
                    cmp_kind = ast->type->issigned ? IR_CMP_GE : IR_CMP_UGE;
                    break;
                }
                case TK_EQU_EQU: {
                    cmp_kind = IR_CMP_EQ;
                    break;
                }
                case TK_NOT_EQU: {
                    cmp_kind = IR_CMP_NE;
                    break;
                }
                /* Should not be reached */
                default: {
                    cmp_kind = IR_CMP_EQ;
                    break;
                }
            }
            
            if (ast->left->type->kind == AST_TYPE_FLOAT) {
                irFCmp(ctx->current_block, ir_result, cmp_kind, left, right);
            } else {
                irICmp(ctx->current_block, ir_result, cmp_kind, left, right);
            }
            return ir_result;
        }

        case AST_ASM_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_FUNCALL: {
            IrValue *ir_fn_call = NULL;

            if (ast->kind == AST_FUNPTR_CALL) {
                ir_fn_call = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_LOCAL);
                IrValue *fn_ptr_var = irFunctionGetLocalFnPtr(func, ast);
                ir_fn_call->name = fn_ptr_var->name;
            } else if (ast->kind == AST_FUNCALL) {
                ir_fn_call = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
                ir_fn_call->name = ast->fname;
            } else if (ast->kind == AST_ASM_FUNCALL) {
                ir_fn_call = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
                ir_fn_call->name = ast->fname;
            }

            IrInstr *ir_call = irInstrNew(IR_OP_CALL);
            ir_call->result = ir_fn_call;
            if (ast->args) {
                ir_call->extra.fn_args = ptrVecNew();
                for (int i = 0; i < ast->args->size; ++i) {
                    Ast *ast_arg = vecGet(Ast *, ast->args, i);
                    IrValue *ir_arg = irExpression(ctx, func, ast_arg);
                    ptrVecPush(ir_call->extra.fn_args, ir_arg);
                } 
            } else {
                ir_call->extra.fn_args = NULL;
            }
            ptrVecPush(ctx->current_block->instructions, ir_call);
            break;
        }

        default:
            loggerPanic("Unhandled Ast kind: %s\n", astKindToString(ast->kind));
    }

    return NULL;
}

void irStatement(IrCtx *ctx, IrFunction *func, Ast *ast) {
    if (!ast) return;

    switch (ast->kind) {
        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                irStatement(ctx, func, next);
                if (ctx->current_block->sealed) break;
            }
            break;
        }

        case AST_IF: {
            /* This creates a compare and then a branch */
            IrValue *ir_cond = irExpression(ctx, func, ast->cond);
            IrBlock *ir_then = irBlockNew(irBlockName());
            IrBlock *ir_else = ast->els ? irBlockNew(irBlockName()) : NULL;
            IrBlock *ir_end_block = irBlockNew(irBlockName());
            IrBlock *ir_previous_end_block = ctx->cond_end_block;

            irCtxSetCondEndBlock(ctx, ir_end_block);
            ptrVecPush(func->blocks, ir_then);

            if (ir_else) {
                irBranch(ctx->current_block, ir_cond, ir_then, ir_else);
            } else {
                irBranch(ctx->current_block, ir_cond, ir_then, ir_end_block);
            }

            irCtxSetCurrentBlock(ctx, ir_then);

            irStatement(ctx, func, ast->then);
            if (!ctx->current_block->sealed) {
                irJump(ctx->current_block, ir_end_block);
            }

            if (ir_else) {
                ptrVecPush(func->blocks, ir_else);
                irCtxSetCurrentBlock(ctx, ir_else);
                irStatement(ctx, func, ast->els);
                if (!ctx->current_block->sealed) {
                    irJump(ctx->current_block, ir_end_block);
                }
            }

            ptrVecPush(func->blocks, ir_end_block);

            /* Update the current block we are pointing to */
            irCtxSetCurrentBlock(ctx, ir_end_block);
            irCtxSetCondEndBlock(ctx, ir_previous_end_block);
            break;
        }

        case AST_WHILE: {
            IrBlock *ir_while_cond = irBlockNew(irBlockName());
            IrBlock *ir_while_body = irBlockNew(irBlockName());
            IrBlock *ir_while_end = irBlockNew(irBlockName());
            IrBlock *ir_previous_end_block = ctx->loop_end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;
            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* Update the current end block */
            irCtxSetLoopHeadBlock(ctx, ir_while_cond);
            irCtxSetLoopEndBlock(ctx, ir_while_end);

            irJump(ctx->current_block, ir_while_cond);

            irCtxSetCurrentBlock(ctx, ir_while_cond);
            ptrVecPush(func->blocks, ir_while_cond);

            IrValue *ir_while_cond_expr = irExpression(ctx, func, ast->whilecond);
            irBranch(ctx->current_block, 
                     ir_while_cond_expr,
                     ir_while_body, 
                     ir_while_end);

            irCtxSetCurrentBlock(ctx, ir_while_body);

            ptrVecPush(func->blocks, ir_while_body);
            irStatement(ctx, func, ast->whilebody);

            if (!ctx->current_block->sealed) {
                irLoop(ctx->current_block, ir_while_cond);
            }
            ptrVecPush(func->blocks, ir_while_end);
            irCtxSetCurrentBlock(ctx, ir_while_end);
            ctx->flags = ctx_prev_flags;
            /* We are now out of the scope of the while loop so reset the 
             * end block to what it was previously */
            irCtxSetLoopEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }

        case AST_DO_WHILE: {
            /**
             * jump to body
             * Body block
             * 
             * jump to condition
             * Condition block
             * jump to body or end;
             *
             * End block
             */
            IrBlock *ir_dowhile_body = irBlockNew(irBlockName());
            IrBlock *ir_dowhile_cond = irBlockNew(irBlockName());
            IrBlock *ir_dowhile_end = irBlockNew(irBlockName());

            IrBlock *ir_previous_end_block = ctx->loop_end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;

            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* this seems weird as a continue would not hit the condition 
             * which is what would happen with a for loop. Though this is 
             * "correct" it simply seems a bit odd */
            irCtxSetLoopHeadBlock(ctx, ir_dowhile_body);
            irCtxSetLoopEndBlock(ctx, ir_dowhile_end);

            /* Jump into the body and prepare IR for the body */
            irJump(ctx->current_block, ir_dowhile_body);
            irCtxSetCurrentBlock(ctx, ir_dowhile_body);
            ptrVecPush(func->blocks, ir_dowhile_body);
            irStatement(ctx, func, ast->whilebody);

            /* Jump into the conditio and prepare IR for the branch */
            irJump(ctx->current_block, ir_dowhile_cond);
            irCtxSetCurrentBlock(ctx, ir_dowhile_cond);
            ptrVecPush(func->blocks, ir_dowhile_cond);
            IrValue *ir_dowhile_cond_expr = irExpression(ctx, func,
                                                         ast->whilecond);
            irBranch(ctx->current_block,
                     ir_dowhile_cond_expr,
                     ir_dowhile_body,
                     ir_dowhile_end);
            irLoop(ctx->current_block, ir_dowhile_body);

            ptrVecPush(func->blocks, ir_dowhile_end);
            irCtxSetCurrentBlock(ctx, ir_dowhile_end);

            /* Restore previous state */
            ctx->flags = ctx_prev_flags;
            irCtxSetLoopEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }

        case AST_FOR: {
            IrBlock *ir_for_cond = ast->forcond ? irBlockNew(irBlockName()) : NULL;
            IrBlock *ir_for_body = irBlockNew(irBlockName());
            IrBlock *ir_for_step = ast->forstep ? irBlockNew(irBlockName()) : NULL;
            IrBlock *ir_for_head = ir_for_cond ? ir_for_cond : ir_for_body;
            IrBlock *ir_for_end = irBlockNew(irBlockName());

            IrBlock *ir_previous_end_block = ctx->loop_end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;

            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* Initaliser belongs to the current block */
            if (ast->forinit) {
                irStatement(ctx, func, ast->forinit);
            }

            /* Update the current end block */
            irCtxSetLoopHeadBlock(ctx, ir_for_cond ? ir_for_cond : ir_for_body);
            irCtxSetLoopEndBlock(ctx, ir_for_end);

            if (ir_for_cond) {
                irJump(ctx->current_block, ir_for_cond);
                irCtxSetCurrentBlock(ctx, ir_for_cond);
                ptrVecPush(func->blocks, ir_for_cond);

                IrValue *ir_for_cond_expr = irExpression(ctx, func, ast->forcond);
                irBranch(ctx->current_block, 
                         ir_for_cond_expr,
                         ir_for_body, 
                         ir_for_end);
            }

            irCtxSetCurrentBlock(ctx, ir_for_body);

            ptrVecPush(func->blocks, ir_for_body);
            irStatement(ctx, func, ast->forbody);

            /* If there is a step we want to add it to the current block */
            if (ir_for_step) {
                irJump(ctx->current_block, ir_for_step);
                irCtxSetCurrentBlock(ctx, ir_for_step);
                ptrVecPush(func->blocks, ir_for_step);
                irExpression(ctx, func, ast->forstep);

                irLoop(ctx->current_block, ir_for_head);
            } else {
                if (!ctx->current_block->sealed) {
                    irLoop(ctx->current_block, ir_for_head);
                }
            }

            ptrVecPush(func->blocks, ir_for_end);
            irCtxSetCurrentBlock(ctx, ir_for_end);

            /* Restore */
            ctx->flags = ctx_prev_flags;
            irCtxSetLoopEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }

        case AST_CONTINUE: {
            if (ctx->flags & IR_CTX_FLAG_IN_LOOP) {
                irJump(ctx->current_block, ctx->loop_head_block);
            } else {
                loggerPanic("Continue found outside of loop\n");
            }
            break;
        }

        case AST_BREAK: {
            if (ctx->flags & IR_CTX_FLAG_IN_LOOP) {
                irJump(ctx->current_block, ctx->loop_end_block);
            } else {
                loggerPanic("IR SWITCH BREAKS NOT HANDLED\n");
            }
            break;
        }

        case AST_DECL: {
            IrInstr *ir_local = irAlloca(ctx->current_block, ast->declvar->type);
            IrValue *ir_local_var = irGetAllocaVar(ir_local);

            int ok = strMapAddAoStr(func->variables,
                                    ast->declvar->tmp_var_name, 
                                    ir_local_var);

            if (!ok) {
                loggerPanic("Failed to set function parameter variable with name %s already exists!\n",
                        ir_local_var->name->data);
            }
            if (ast->declinit) {
                IrValue *ir_init = irExpression(ctx, func, ast->declinit);
                irStore(ctx->current_block,ir_local_var,ir_init);  
            }
            break;
        }

        /* We want to avoid having multiple return statements in a function 
         * it is more cannoincal to move the value you want to return to a
         * specified stack variable and then jump to the end of the function. */
        case AST_RETURN: {
            IrValue *ir_return_val = irExpression(ctx, func, ast->retval);
            irStore(ctx->current_block,func->return_value,ir_return_val);
            irJump(ctx->current_block, func->exit_block);
            break;
        }

        default:
            irExpression(ctx, func, ast);
            break;
    }
}

/*==================== IR LOWERING ========================================== */

/* Function parameters can only be a one of a few different types, thus this is
 * fairly reasonable to have separate */
IrValue *irConvertAstFuncParam(Ast *ast_param) {
    IrValueType ir_type = irConvertType(ast_param->type);
    IrValue *ir_value = irValueNew(ir_type, IR_VALUE_PARAM);
    if (ast_param->kind == AST_LVAR) {
        ir_value->name = ast_param->tmp_var_name;
    } else if (ast_param->kind == AST_FUNPTR) {
        ir_value->name = ast_param->tmp_fnptr_name;
    } else {
        /* We need to handle all cases, such as default arguments which 
         * change things considerably */
        loggerPanic("Unhandled Ast: %s\n", astToString(ast_param));
    }
    return ir_value;
}

IrFunction *irLowerFunction(IrCtx *ctx, 
                            IrProgram *program, 
                            Ast *ast_function)
{
    irBlockCountReset();
    IrFunction *ir_function = irFunctionNew(ast_function->fname);
    IrBlock *ir_entry_block = irBlockNew(aoStrDupRaw(str_lit("entry")));
    IrBlock *ir_exit_block = irBlockNew(aoStrDupRaw(str_lit("exit")));

    ir_function->program = program;

    IrInstr *ir_return_space = irAlloca(ir_entry_block,
                                        ast_function->type->rettype);
    IrValue *ir_return_var = irGetAllocaVar(ir_return_space);

    /* I'm not sure if this is needed */
    ir_function->return_value = ir_return_var;

    for (int i = 0; i < ast_function->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *,ast_function->params,i);
        /* Add to the current basic block as a store 
         * this is 2 instructions;
         * 1) Allocate space on the stack for the function parameter
         * 2) Store the function parameter at this location.
         * */
        IrValue *ir_param = irConvertAstFuncParam(ast_param);
        ptrVecPush(ir_function->params, ir_param);

        aoStr *key = NULL;
        if (ast_param->kind == AST_LVAR) {
            key = ast_param->tmp_var_name;
        } else if (ast_param->kind == AST_FUNPTR) {
            key = ast_param->tmp_fnptr_name;
        } else {
            loggerPanic("Unhandled key kind: %s\n",
                    astKindToString(ast_param->kind));
        }

        IrInstr *ir_alloca = irAlloca(ir_entry_block, ast_param->type);
        IrValue *ir_tmp_var = irGetAllocaVar(ir_alloca);
        int ok = strMapAddAoStrOrErr(ir_function->variables,
                                     key,
                                     ir_tmp_var);
        if (!ok) {
            loggerPanic("Failed to set function parameter variable with name %s already exists!\n",
                    ir_tmp_var->name->data);
        }
        /* Now create the instruction to store */
        irStore(ir_entry_block, ir_tmp_var, ir_param);
    }

    ir_function->entry_block = ir_entry_block;
    ir_function->exit_block = ir_exit_block;
    
    irCtxSetCurrentBlock(ctx, ir_entry_block);
    irCtxSetExitBlock(ctx, ir_exit_block);

    irStatement(ctx, ir_function, ast_function->body);

    ptrVecPush(ir_function->blocks, ir_exit_block);
    irRet(ir_function->exit_block, ir_function->return_value);
    return ir_function;
}

void irLowerAst(Cctrl *cc) {
    IrProgram *ir_program = irProgramNew();
    IrCtx *ctx = irCtxNew();

    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            irCtxReset(ctx);
            IrFunction *ir_func = irLowerFunction(ctx, ir_program, ast);
            aoStr *ir_func_str = irFunctionToString(ir_func);
            printf("%s\n",ir_func_str->data);

        } else if (ast->kind == AST_ASM_FUNC_BIND) {
           // loggerWarning("Asm function bind cannot lowered by ir - \n");
        } else {
       //     loggerWarning("Cannot lower AST to IR: %s\n",
       //             astToString(ast));
        }
    }
    irCtxRelease(ctx);
}
