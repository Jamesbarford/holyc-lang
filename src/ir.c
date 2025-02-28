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

const char *irOpcodeToString(IrInstr *ir_instr) {
    IrOpcode opcode = ir_instr->opcode;
    switch (opcode) {
        case IR_OP_ALLOCA:   return "alloca";
        case IR_OP_LOAD:     return "load";
        case IR_OP_STORE:    return "store";
        case IR_OP_GEP:      return "gep";

        case IR_OP_IADD:      return "iadd";
        case IR_OP_ISUB:      return "isub";
        case IR_OP_IMUL:      return "imul";
        case IR_OP_IDIV:      return "idiv";
        case IR_OP_UDIV:      return "udiv";
        case IR_OP_IREM:      return "irem";
        case IR_OP_UREM:      return "urem";
        case IR_OP_INEG:      return "ineg";

        case IR_OP_FADD:      return "fadd";
        case IR_OP_FSUB:      return "fsub";
        case IR_OP_FMUL:      return "fmul";
        case IR_OP_FDIV:      return "fdiv";
        case IR_OP_FNEG:      return "fneg";

        case IR_OP_AND:      return "and";
        case IR_OP_OR:       return "or";
        case IR_OP_XOR:      return "xor";
        case IR_OP_SHL:      return "shl";
        case IR_OP_SHR:      return "shr";
        case IR_OP_SAR:      return "sar";
        case IR_OP_NOT:      return "not";
        case IR_OP_ICMP:
        case IR_OP_FCMP:
            return irCmpKindToString(ir_instr->extra.cmp_kind);
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
                    aoStrCatFmt(buf, "%-8s %S\n", "arg", ir_value_str);
                    aoStrRelease(ir_value_str);
                }
                aoStrCatLen(buf,str_lit("    "));
            }
            aoStrCatFmt(buf, "%-8s %S", op, ir_instr->result->name);
            break;
        }

        case IR_OP_BR: {
            aoStr *ir_value_str = irValueToString(ir_instr->op1);
            aoStrCatFmt(buf, "%-8s %S, %S, %S", op, ir_value_str,
                        ir_instr->target_block->label,
                        ir_instr->fallthrough_block->label);
            break;
        }

        case IR_OP_JMP: {
            aoStrCatFmt(buf, "%-8s %S", op, ir_instr->target_block->label);
            break;
        }

        default: {
            aoStrCatFmt(buf, "%-8s", op);
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

    aoStrCatFmt(buf, "%S\n",ir_entry_block_str);
    aoStrRelease(ir_entry_block_str);

    for (int i = 0; i < ir_func->blocks->size; ++i) {
        IrBlock *ir_block = vecGet(IrBlock *, ir_func->blocks, i);
        aoStr *ir_block_str = irBlockToString(ir_block);
        aoStrCatFmt(buf, "%S\n", ir_block_str);
        aoStrRelease(ir_block_str);

        //if (i + 1 != ir_func->blocks->size) {
        //    aoStrPutChar(buf, '\n');
        //} 
    }

    buf->len--;

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

/* We will reset this after each function has been created */
static int ir_tmp_variable_count = 0;

IrValue *irTmpVariable(IrValueType ir_value_type) {
    IrValue *ir_value = irValueNew(ir_value_type, IR_VALUE_TEMP);
    ir_value->name = aoStrPrintf("%%tmp%03d",ir_tmp_variable_count++);
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

static int ir_block_count = 0;
void irBlockCountReset(void) {
    ir_block_count = 0;
}

aoStr *irBlockName(void) {
    return aoStrPrintf("bb%03d", ir_block_count++);
}

/* Where `ir_value` is always a constant int so we know how much stack space
 * we require. We _may_ want type info for of the thing we are storing? */
IrInstr *irAlloca(IrBlock *ir_block, long size) {
    IrInstr *ir_alloca_instr = irInstrNew(IR_OP_ALLOCA);
    IrValue *ir_alloca_size = irConstInt(IR_TYPE_I64, size);
    IrValue *ir_temporary_variable = irTmpVariable(IR_TYPE_I64);
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
IrInstr *irJump(IrBlock *block, IrBlock *target);
IrInstr *irCmp(IrBlock *block, IrValue *result, IrValue *op1, IrValue *op2,
               IrCmpKind kind);
IrInstr *irICmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irFCmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irJump(IrBlock *block, IrBlock *target);
IrInstr *irBranch(IrBlock *block, IrValue *cond, IrBlock *true_block,
              IrBlock *false_block);

IrValue *irExpression(Cctrl *cc, IrFunction *func, IrBlock **current_block, Ast *ast);
IrValue *irStatememt(Cctrl *cc, IrFunction *func, IrBlock **current_block, Ast *ast);

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

IrInstr *irJump(IrBlock *block, IrBlock *target) {
    if (!block || !target) {
        loggerPanic("NULL param\n");
    }

    if (block->sealed) {
        loggerWarning("Tried to add a jump to a sealed block: %s\n",
                block->label->data);
        return NULL;
    }

    IrInstr *instr = irInstrNew(IR_OP_JMP);
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

IrValue *irExpression(Cctrl *cc, IrFunction *func, IrBlock **current_block, Ast *ast) {
    IrBlock *ir_block = *current_block;

    switch (ast->kind) {
        case AST_LITERAL: {
            switch (ast->type->kind) {
                case AST_TYPE_INT:
                case AST_TYPE_CHAR:
                    return irConstInt(irConvertType(cc, ast->type), ast->i64);
                case AST_TYPE_FLOAT:
                    return irConstFloat(IR_TYPE_F64, ast->f64);
                default:
                    cctrlIce(cc, "Unknown literal: %s\n",
                             astKindToString(ast->type->kind));
            }
            break;
        }
        
        case AST_STRING: {
            return irGlobalString(func->program, ast);
        }

        case AST_LVAR: {
            IrValue *local_var = strMapGetAoStr(func->variables,
                                                ast->tmp_var_name);
            if (!local_var) {
                cctrlIce(cc, "Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(cc, ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_var);
            return ir_load_dest;
        }

        case AST_FUNPTR: {
            IrValue *local_fnptr = strMapGetAoStr(func->variables,
                                                  ast->tmp_fnptr_name);
            if (!local_fnptr) {

                char *keys = strMapKeysToString(func->variables);
                cctrlIce(cc, "func %s Variable %s not found keys = %s\n",
                              func->name->data,
                              ast->tmp_fnptr_name->data, keys);
            }

            IrValueType ir_value_type = irConvertType(cc,ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_fnptr);
            /* Update where the variable is? */
            strMapAddAoStr(func->variables,ast->tmp_fnptr_name,ir_load_dest);
            return ir_load_dest;
        }

        case AST_DEREF: {
            IrValue *ir_ptr = irExpression(cc, func, current_block, ast->left);

            if (ir_ptr->type != IR_TYPE_PTR) {
                cctrlIce(cc, "Attempted to dereference a non-pointer: %s\n",
                        astToString(ast->left));
            }

            IrValue *ir_tmp_var = irTmpVariable(ir_ptr->type);
            irLoad(ir_block, ir_tmp_var, ir_ptr);
            return ir_tmp_var;
        }

        case '=': {
            IrValue *rhs = irExpression(cc, func, current_block, ast->right);

            if (ast->left->kind == AST_LVAR) {
                IrValue *ir_local = strMapGetAoStr(func->variables,
                                                   ast->left->tmp_var_name);
                irStore(ir_block, ir_local, rhs);
                /* Assignments return the value */
                return rhs;
            } else if (ast->left->kind == AST_GVAR) {
                IrValue *ir_global = strMapGetAoStr(func->program->global_variables,
                                                    ast->left->gname);

                irStore(ir_block, ir_global, rhs);
                return rhs; 
            } else if (ast->left->kind == AST_DEREF) {
                IrValue *ptr = irExpression(cc, func, current_block, ast->left->left);
                irStore(ir_block, ptr, rhs);
                return rhs;
            } else {
                cctrlIce(cc, "Unsupported LHS assignment %s\n",
                        astToString(ast->left));
            }
        }

        case '+': {
            if (ast->right == NULL) {
                /* Unary plus - just return the operand */
                return irExpression(cc, func, current_block, ast->left);
            } else {
                /* Binary addition */
                IrValue *left = irExpression(cc, func, current_block, ast->left);
                IrValue *right = irExpression(cc, func, current_block, ast->right);
    
                IrValue *ir_result = irTmpVariable(irConvertType(cc, ast->type));
                if (ast->type->kind == AST_TYPE_FLOAT) {
                    irFAdd(*current_block, ir_result, left, right);
                } else {
                    irIAdd(*current_block, ir_result, left, right);
                }
                return ir_result;
            }
        }

        case '-': {
            if (ast->right == NULL) {
                /* Unary negation */
                IrValue *ir_expr = irExpression(cc, func, current_block, ast->left);
                IrValue *ir_result = irTmpVariable(irConvertType(cc, ast->type));

                if (ast->left->type->kind == AST_TYPE_FLOAT) {
                    irFNeg(*current_block, ir_result, ir_expr);
                } else {
                    irINeg(*current_block, ir_result, ir_expr);
                }
                return ir_result;
            } else {
                /* Binary subtraction */
                IrValue *left = irExpression(cc, func, current_block, ast->left);
                IrValue *right = irExpression(cc, func, current_block, ast->right);
                
                IrValue *ir_result = irTmpVariable(irConvertType(cc, ast->type));
                if (ast->type->kind == AST_TYPE_FLOAT) {
                    irFSub(*current_block, ir_result, left, right);
                } else {
                    irISub(*current_block, ir_result, left, right);
                }
                return ir_result;
            }
        }

        case '*': {
            IrValue *left = irExpression(cc, func, current_block, ast->left);
            IrValue *right = irExpression(cc, func, current_block, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(cc, ast->type));
            if (ast->type->kind == AST_TYPE_FLOAT) {
                irFMul(*current_block, ir_result, left, right);
            } else {
                irIMul(*current_block, ir_result, left, right);
            }
            return ir_result;
        }

        case '/': {
            IrValue *left = irExpression(cc, func, current_block, ast->left);
            IrValue *right = irExpression(cc, func, current_block, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(cc, ast->type));
            if (ast->type->kind == AST_TYPE_FLOAT) {
                irFDiv(*current_block, ir_result, left, right);
            } else if (ast->type->issigned) {
                irSDiv(*current_block, ir_result, left, right);
            } else {
                irUDiv(*current_block, ir_result, left, right);
            }
            return ir_result;
        }
        
        case '%': {
            IrValue *left = irExpression(cc, func, current_block, ast->left);
            IrValue *right = irExpression(cc, func, current_block, ast->right);
            
            IrValue *ir_result = irTmpVariable(irConvertType(cc, ast->type));
            if (ast->type->issigned) {
                irSRem(*current_block, ir_result, left, right);
            } else {
                irURem(*current_block, ir_result, left, right);
            }
            return ir_result;
        }

        case '<': 
        case '>': 
        case TK_LESS_EQU:
        case TK_GREATER_EQU: 
        case TK_EQU_EQU: 
        case TK_NOT_EQU: {
            IrValue *left = irExpression(cc, func, current_block, ast->left);
            IrValue *right = irExpression(cc, func, current_block, ast->right);
            
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
                irFCmp(*current_block, ir_result, cmp_kind, left, right);
            } else {
                irICmp(*current_block, ir_result, cmp_kind, left, right);
            }
            return ir_result;
        }

        case AST_FUNPTR_CALL:
        case AST_FUNCALL: {
            IrValue *ir_fn = NULL;
            IrValue *ir_fn_call = NULL;

            if (ast->kind == AST_FUNPTR_CALL) {
                ir_fn_call = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_LOCAL);
                IrValue *fn_ptr_var = strMapGetAoStr(func->variables,
                                                     ast->tmp_fnptr_name);
                ir_fn_call->name = fn_ptr_var->name;
            } else {
                ir_fn_call = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
                ir_fn_call->name = ast->fname;
            }

            IrInstr *ir_call = irInstrNew(IR_OP_CALL);
            ir_call->result = ir_fn_call;
            if (ast->args) {
                ir_call->extra.fn_args = ptrVecNew();
                for (int i = 0; i < ast->args->size; ++i) {
                    Ast *ast_arg = vecGet(Ast *, ast->args, i);
                    IrValue *ir_arg = irExpression(cc,func,current_block,ast_arg);
                    ptrVecPush(ir_call->extra.fn_args, ir_arg);
                } 
            } else {
                ir_call->extra.fn_args = NULL;
            }
            ptrVecPush((*current_block)->instructions,ir_call);
            break;
        }
        
        default:
            loggerPanic("Unhandled Ast kind: %s\n", astKindToString(ast->kind));
    }

    return NULL;
}

void irStatement(Cctrl *cc, IrFunction *func, IrBlock **current_block, Ast *ast) {
    if (!ast) return;

    switch (ast->kind) {
        case AST_COMPOUND_STMT: {
            listForEach(ast->stms) {
                Ast *next = (Ast *)it->value;
                irStatement(cc, func, current_block, next);
                if ((*current_block)->sealed) break;
            }
            break;
        }

        case AST_IF: {
            /* This creates a compare and then a branch */
            IrValue *ir_cond = irExpression(cc, func, current_block, ast->cond);
            IrBlock *ir_then = irBlockNew(irBlockName());
            IrBlock *ir_else = ast->els ? irBlockNew(irBlockName()) : NULL;
            IrBlock *ir_end_block = irBlockNew(irBlockName());

            ptrVecPush(func->blocks, ir_then);

            if (ir_else) {
                irBranch(*current_block, ir_cond, ir_then, ir_else);
            } else {
                irBranch(*current_block, ir_cond, ir_then, ir_end_block);
            }

            *current_block = ir_then;

            irStatement(cc, func, current_block, ast->then);
            if (!(*current_block)->sealed) {
                irJump(*current_block, ir_end_block);
            }

            if (ir_else) {
                ptrVecPush(func->blocks, ir_else);
                *current_block = ir_else;
                irStatement(cc, func, current_block, ast->els);
                if (!(*current_block)->sealed) {
                    irJump(*current_block, ir_end_block);
                }
            }


            ptrVecPush(func->blocks, ir_end_block);

            /* Update the current block we are pointing to */
            *current_block = ir_end_block;
            break;
        }

        case AST_DECL: {
            IrInstr *ir_local = irAlloca(*current_block, ast->declvar->type->size);
            IrValue *ir_local_var = irGetAllocaVar(ir_local);

            int ok = strMapAddAoStr(func->variables,
                                    ast->declvar->tmp_var_name, 
                                    ir_local_var);

            if (!ok) {
                cctrlIce(cc, "Failed to set function parameter variable with name %s already exists!\n",
                        ir_local_var->name->data);
            }
            if (ast->declinit) {
                IrValue *ir_init = irExpression(cc,func,current_block,
                                                ast->declinit);
                irStore(*current_block,ir_local_var,ir_init);  
                // ptrVecPush((*current_block)->instructions, ir_init);
            }
            break;
        }

        case AST_FUNPTR_CALL:
        case AST_FUNCALL:
            irExpression(cc, func, current_block, ast);
            break;

        default:
            loggerWarning("Unhandled AST_KIND = %s\n", astKindToString(ast->kind));
            break;
    }
}

/*==================== IR LOWERING ========================================== */

/* Function parameters can only be a one of a few different types, thus this is
 * fairly reasonable to have separate */
IrValue *irConvertAstFuncParam(Cctrl *cc, Ast *ast_param) {
    IrValueType ir_type = irConvertType(cc, ast_param->type);
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

IrFunction *irLowerFunction(Cctrl *cc, IrProgram *program, Ast *ast_function) {
    irBlockCountReset();
    IrFunction *ir_function = irFunctionNew(ast_function->fname);
    IrBlock *ir_entry_block = irBlockNew(aoStrDupRaw(str_lit("entry")));

    ir_function->program = program;

    IrValueType ir_type = irConvertType(cc, ast_function->type->rettype);
    IrValue *ir_value = irValueNew(ir_type, IR_VALUE_UNDEFINED);
    /* I'm not sure if this is needed */
    ir_function->return_type = ir_value;

    for (int i = 0; i < ast_function->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *,ast_function->params,i);
        /* Add to the current basic block as a store 
         * this is 2 instructions;
         * 1) Allocate space on the stack for the function parameter
         * 2) Store the function parameter at this location.
         * */
        IrValue *ir_param = irConvertAstFuncParam(cc, ast_param);
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

        IrInstr *ir_alloca = irAlloca(ir_entry_block, ast_param->type->size);
        IrValue *ir_tmp_var = irGetAllocaVar(ir_alloca);
        int ok = strMapAddAoStrOrErr(ir_function->variables,
                                     key,
                                     ir_tmp_var);
        if (!ok) {
            cctrlIce(cc, "Failed to set function parameter variable with name %s already exists!\n",
                    ir_tmp_var->name->data);
        }
        /* Now create the instruction to store */
        irStore(ir_entry_block, ir_tmp_var, ir_param);
    }

    ir_function->entry_block = ir_entry_block;
    irStatement(cc, ir_function, &ir_entry_block, ast_function->body);

    return ir_function;
}

void irLowerAst(Cctrl *cc) {
    IrProgram *ir_program = irProgramNew();
    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            IrFunction *ir_func = irLowerFunction(cc, ir_program, ast);
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

