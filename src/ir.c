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
#include "ir-interp.h"
#include "ir-types.h"
#include "list.h"
#include "lexer.h"
#include "map.h"
#include "memory.h"
#include "transpiler.h"
#include "uniq-list.h"
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

/*==================== IR PROTOTYPES ======================================== */
IrUnresolvedBlock *irUnresolvedGotoNew(List *list_node, IrBlock *ir_source_block);
IrUnresolvedBlock *irUnresolvedLabelNew(List *list_node, IrBlock *ir_destination_block);
IrInstr *irCmp(IrBlock *block, IrValue *result, IrValue *op1, IrValue *op2,
               IrCmpKind kind);
IrInstr *irICmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irFCmp(IrBlock *block, IrValue *result, IrCmpKind kind, IrValue *op1, 
                IrValue *op2);
IrInstr *irJump(IrFunction *ir_function, IrBlock *block, IrBlock *target);
IrInstr *irBranch(IrFunction *func,
                  IrBlock *block,
                  IrValue *cond,
                  IrBlock *true_block,
                  IrBlock *false_block);
IrValue *irLoadClassRef(IrCtx *ctx,
                        IrFunction *func,
                        Ast *cls,
                        AstType *field,
                        int offset);
aoStr *irBlockToString(IrFunction *func, IrBlock *ir_block);

IrValue *irGlobalExpression(IrCtx *ctx, Ast *ast);
IrValue *irExpression(IrCtx *ctx, IrFunction *func, Ast *ast);
void irStatement(IrCtx *ctx, IrFunction *func, Ast *ast);

IrValue *irFunctionGetLocalFnPtr(IrFunction *ir_function, Ast *ast);
IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block);
IntMap *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block);
IntMap *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block);

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
        case IR_TYPE_ASM_FUNCTION: type_str = "asm"; break;
        case IR_TYPE_LABEL:    return "";
        default: loggerPanic("Unhandled IrValueType: %d\n", ir_value_type);
    }
    if (is_terminal) {
        return mprintf(ESC_WHITE"%s"ESC_RESET,type_str);
    } else {
        return mprintf("%s",type_str);
    }
}

/**
 * @Bug - Non breaking as in principle the array will still work.
 * Because we flatten something like: 
 * 
 * `I64 array[][2][2] = {{{1,1}, {2,2}}, {{3, 3}, {4, 4}}};`
 *
 * To; `i64 [1, 1, 2, 2, 3, 3, 4, 4]`
 *
 * We can't represent;
 * `[i64 x 2 [i64 x 2 [i64 x 2]]]`
 *
 * We just have; `[i64 x 8]`
 */
void irArrayInitToString(aoStr *buf, IrValue *ir_value) {
    aoStrCatFmt(buf,"[%i x ", ir_value->array_.values->size);
    if (ir_value->array_.nesting == 1) {
        IrValue *head = (IrValue *)ir_value->array_.values->entries[0];
        aoStrCatFmt(buf,"%s]", irValueTypeToString(head->type));
    } else {
        PtrVec *ir_array = ir_value->array_.values;
        IrValue *actual_value = NULL;
        int bracket_count = 0;
        for (int i = 0; i < ir_array->size; ++i) {
            IrValue *tmp = (IrValue *)ir_value->array_.values->entries[i];
            if (tmp->type == IR_TYPE_ARRAY_INIT) {
                ir_array = tmp->array_.values;
            } else {
                actual_value = tmp;
                bracket_count = i;
                break;
            }
        }
        bracket_count++;

        aoStrCatFmt(buf,"%s",irValueTypeToString(actual_value->type));
        aoStrCatRepeat(buf,"]",bracket_count);
    }
}

aoStr *irValueToString(IrValue *ir_value) {
    aoStr *buf = aoStrNew();

    if (ir_value->type == IR_TYPE_ARRAY_INIT) {
        irArrayInitToString(buf,ir_value);
    } else {
        switch (ir_value->kind) {
            case IR_VALUE_CONST_INT:   aoStrCatFmt(buf, "%I", ir_value->i64); break;
            case IR_VALUE_CONST_FLOAT: aoStrCatFmt(buf, "%f", ir_value->f64); break;
            case IR_VALUE_CONST_STR:   aoStrCatFmt(buf, "\"%S\"", ir_value->str); break;
            case IR_VALUE_GLOBAL: {
                if (is_terminal) {
                    aoStrCatFmt(buf, ESC_WHITE"global "ESC_RESET"%S", ir_value->name);
                } else {
                    aoStrCatFmt(buf, "global %S", ir_value->name);
                }

                if (ir_value->global.value) {
                    aoStr *init_buf = irValueToString(ir_value->global.value);
                    aoStrCatFmt(buf, " %S ", init_buf);
                    aoStrRelease(init_buf);
                }
                break;
            }
            case IR_VALUE_PARAM:       aoStrCatFmt(buf, "%S", ir_value->name); break;
            case IR_VALUE_LOCAL:       aoStrCatFmt(buf, "%S", ir_value->name); break;
            case IR_VALUE_TEMP:        aoStrCatFmt(buf, "%S", ir_value->name); break;
            case IR_VALUE_PHI:         aoStrCatFmt(buf, "phi"); break;
            case IR_VALUE_LABEL:       aoStrCatFmt(buf, "%S", ir_value->name); break;
            case IR_VALUE_UNDEFINED:   aoStrCatFmt(buf, "undefined"); break;
            case IR_VALUE_UNRESOLVED:  aoStrCatFmt(buf, "unresolved"); break;
            default: loggerPanic("Unhandled IrValueKind: %d\n", ir_value->kind);
        }

        const char *ir_value_type_str = irValueTypeToString(ir_value->type);
        aoStrPutChar(buf,' ');
        aoStrCatFmt(buf,"%s",ir_value_type_str);
    }
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
        case IR_OP_LABEL:    ir_op_str = "label"; break;
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

void irPairToString(aoStr *buf, IrPair *ir_phi_pair) {
    aoStr *ir_value_str = irValueToString(ir_phi_pair->ir_value);
    /* <Value, block> */
    aoStrCatFmt(buf, "[ %S, bb%i ]", ir_value_str, ir_phi_pair->ir_block->id);
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
            PtrVec *fn_args = ir_instr->op1->array_.values;
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

            aoStr *ir_ret_var = irValueToString(ir_instr->result);
            aoStrCatFmt(buf, "%s %S, %S", op, ir_ret_var, ir_instr->op1->name);
            aoStrRelease(ir_ret_var);
            break;
        }

        case IR_OP_BR: {
            aoStr *ir_value_str = irValueToString(ir_instr->op1);
            aoStrCatFmt(buf, "%s %S, bb%i, bb%i",
                        op,
                        ir_value_str,
                        ir_instr->target_block->id,
                        ir_instr->fallthrough_block->id);
            aoStrRelease(ir_value_str);
            break;
        }

        case IR_OP_LOOP:
        case IR_OP_JMP: {
            /* This is for unresolved gotos */
            if (ir_instr->target_block == NULL) {
                aoStrCatFmt(buf, "%s ???", op);
            } else {
                aoStrCatFmt(buf, "%s bb%I", op, ir_instr->target_block->id);
            }
            break;
        }

        case IR_OP_PHI: {
            aoStr *phi_pairs_str = aoStrNew();
            for (int i = 0; i < ir_instr->extra.phi.pairs->size; ++i) {
                IrPair *ir_phi_pair = vecGet(IrPair *,
                                                ir_instr->extra.phi.pairs, i);
                irPairToString(phi_pairs_str, ir_phi_pair);
                if (i + 1 != ir_instr->extra.phi.pairs->size) {
                    aoStrPutChar(phi_pairs_str, ' ');;
                }
            }
            aoStrCatFmt(buf,"%s %S",op, phi_pairs_str);
            aoStrRelease(phi_pairs_str);
            break;
        }

        case IR_OP_SWITCH: {
            aoStr *pairs_str = aoStrNew();
            aoStr *ir_value_str = irValueToString(ir_instr->op1);
            aoStrCatFmt(buf, "%s %S, label bb%i [\n",
                        op,
                        ir_value_str,
                        ir_instr->target_block->id);

            listForEach(ir_instr->extra.cases) {
                IrPair *ir_pair = listValue(IrPair *, it);
                aoStr *ir_value_str = irValueToString(ir_pair->ir_value);
                aoStrCatFmt(buf, "      %S, bb%i\n", ir_value_str, ir_pair->ir_block->id);
            }
            aoStrCatLen(buf, str_lit("    ]"));

            aoStrRelease(ir_value_str);
            aoStrRelease(pairs_str);
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

/* Convert a basic block to a string
 * <block_name>:
 *   <instructions...>
 * */
aoStr *irBlockToString(IrFunction *func, IrBlock *ir_block) {
    aoStr *buf = aoStrNew();

    IntMap *successors = irFunctionGetSuccessors(func, ir_block);
    IntMap *predecessors = irFunctionGetPredecessors(func, ir_block);

    if (is_terminal) {
        aoStrCatFmt(buf, ESC_BOLD"  bb%i"ESC_CLEAR_BOLD" -> ", ir_block->id);
    } else {
        aoStrCatFmt(buf, "  bb%i -> ", ir_block->id);
    }

    if (predecessors) {
        aoStr *predecessors_str = intMapKeysToString(predecessors);
        aoStrCatFmt(buf, "predecessors: %S  ", predecessors_str);
        aoStrRelease(predecessors_str);
    } else {
        aoStrCatFmt(buf, "predecessors: {}  ");
    }

    if (successors) {
        aoStr *successors_str = intMapKeysToString(successors);
        aoStrCatFmt(buf, "successors: %S", successors_str);
        aoStrRelease(successors_str);
    } else {
        aoStrCatFmt(buf, "successors: {}");
    }

    aoStrPutChar(buf, '\n');
    if (!listEmpty(ir_block->instructions)) {
        listForEach(ir_block->instructions) {
            IrInstr *ir_instr = (IrInstr *)it->value;
            aoStr *ir_instr_str = irInstrToString(ir_instr);
            aoStrCatFmt(buf, "    %S\n", ir_instr_str);
            aoStrRelease(ir_instr_str);
        }
    }

    return buf;
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

    aoStr *ir_entry_block_str = irBlockToString(ir_func, ir_func->entry_block);

    aoStrCatFmt(buf, "%S\n",ir_entry_block_str);
    aoStrRelease(ir_entry_block_str);

    listForEach(ir_func->blocks) {
        IrBlock *ir_block = (IrBlock *)it->value;
        aoStr *ir_block_str = irBlockToString(ir_func, ir_block);
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
IrBlock *irBlockNew(int id) {
    IrBlock *ir_block = (IrBlock *)irArenaAlloc(sizeof(IrBlock));
    ir_block->id = id;
    ir_block->instructions = listNew();
    ir_block->sealed = 0;
    ir_block->ssa_values = strMapNew(8);
    return ir_block;
}

IrUnresolvedBlock *irUnresolvedGotoNew(List *list_node,
                                        IrBlock *ir_source_block)
{
    IrUnresolvedBlock *ir_unresolved = (IrUnresolvedBlock *)irArenaAlloc(
                                                     sizeof(IrUnresolvedBlock));
    ir_unresolved->goto_.list_node = list_node;
    ir_unresolved->goto_.ir_block = ir_source_block;
    return ir_unresolved;
}

static int ir_block_count = 0;
void irBlockCountReset(void) {
    ir_block_count = 0;
}

int irBlockId(void) {
    return ir_block_count++;
}

aoStr *irBlockName(void) {
    return aoStrPrintf("bb%d", ir_block_count++);
}


static int ir_array_count = 0;
aoStr *irArrayName(IrFunction *func) {
    return aoStrPrintf("array%d.%s",ir_array_count++,func->name->data);
}

void irArrayCountReset(void) {
    ir_array_count = 0;
}

/* We will reset this after each function has been created */
static int ir_tmp_variable_count = 1;
void irTmpVariableCountReset(void) {
    ir_tmp_variable_count = 1;
}

void irBlockAddInstruction(IrBlock *block, IrInstr *ir_instr) {
    listAppend(block->instructions, ir_instr);
}

/* Check to see if the instruction is the last one in the list */
int irBlockIsLastInstruction(IrBlock *block, IrInstr *ir_instr) {
    List *last = block->instructions->prev;
    IrInstr *ir_last_instr = (IrInstr *)last->value;
    return ir_last_instr == ir_instr;
}

IrUnresolvedBlock *irUnresolvedLabelNew(List *list_node,
                                        IrBlock *ir_destination_block)
{
    IrUnresolvedBlock *ir_unresolved = (IrUnresolvedBlock *)irArenaAlloc(
                                                     sizeof(IrUnresolvedBlock));
    ir_unresolved->label_.list_node = list_node;
    ir_unresolved->label_.ir_block = ir_destination_block;
    return ir_unresolved;
}

IrBlockMapping *irBlockMappingNew(int id) {
    IrBlockMapping *mapping = (IrBlockMapping *)irArenaAlloc(sizeof(IrBlockMapping));
    mapping->id = id;
    mapping->successors = intMapNew(8);
    mapping->predecessors = intMapNew(8);
    return mapping;
}

IrProgram *irProgramNew(void) {
    IrProgram *program = (IrProgram *)irArenaAlloc(sizeof(IrProgram));
    program->functions = ptrVecNew();
    program->asm_functions = ptrVecNew();
    program->global_variables = strMapNew(16);
    program->strings = strMapNew(16);
    program->types = strMapNew(16);
    program->arrays = strMapNew(16);
    return program;
}

IrValue *irValueNew(IrValueType ir_type, IrValueKind ir_kind) {
    IrValue *ir_value = (IrValue *)irArenaAlloc(sizeof(IrValue));
    memset(ir_value,0,sizeof(IrValue));
    ir_value->type = ir_type;
    ir_value->kind = ir_kind;
    ir_value->version = 1;
    return ir_value;
}

void irCtxResetArray(IrCtx *ctx) {
    memset(&ctx->array_,0,sizeof(IrArrayCtx));
}

void irCtxReset(IrCtx *ctx) {
    ctx->flags = 0;
    ctx->current_block = NULL;
    ctx->end_block = NULL;
    ctx->loop_head_block = NULL;
    irCtxResetArray(ctx);
    irBlockCountReset();
    irTmpVariableCountReset();

    if (ctx->unresolved_gotos) {
        ptrVecClear(ctx->unresolved_gotos, NULL);
    }
    if (ctx->unresolved_labels) {
        strMapClear(ctx->unresolved_labels);
    }
}

IrCtx *irCtxNew(void) {
    IrCtx *ctx = (IrCtx *)xmalloc(sizeof(IrCtx));
    ctx->unresolved_gotos = NULL;
    ctx->unresolved_labels = NULL;
    irCtxReset(ctx);
    ctx->unresolved_gotos = ptrVecNew();
    ctx->unresolved_labels = strMapNew(8);
    return ctx;
}

void irCtxRelease(IrCtx *ctx) {
    xfree(ctx);
}

static void irCtxSetCurrentBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->current_block = ir_block;
}

static void irCtxSetEndBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->end_block = ir_block;
}

static void irCtxSetLoopHeadBlock(IrCtx *ctx, IrBlock *ir_block) {
    ctx->loop_head_block = ir_block;
}

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
    assert(ast->kind == AST_FUNPTR || ast->kind == AST_FUNPTR_CALL || 
           ast->kind == AST_FUNC);

    IrValue *ir_fnptr = NULL;
    if (ast->tmp_fnptr_name) {
        ir_fnptr = strMapGetAoStr(ir_function->variables,
                ast->tmp_fnptr_name);
    } else {
        ir_fnptr = strMapGetAoStr(ir_function->variables,
                ast->tmp_var_name);
    }

    if (!ir_fnptr) {
        loggerPanic("Local function pointer with original name `%s` not found\n",
                    ast->fname->data);
    }
    return ir_fnptr;
}

/* This is very easy to inline */
IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)intMapGet(func->cfg,
                                                                   ir_block->id);
    return ir_block_mapping;
}

IntMap *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->successors;
    }
    return NULL;
}

IntMap *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->predecessors;
    }
    return NULL;
}
/* Pass in the whole block to abstract away that we area using an interal 
 * datastructure to keep track of things. I'm trying a few different ones out */
void irFunctionAddSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)intMapGet(func->cfg, src->id);
    if (!ir_block_mapping) {
        ir_block_mapping = irBlockMappingNew(src->id);
        intMapAdd(func->cfg, src->id, ir_block_mapping);
    }
    intMapAdd(ir_block_mapping->successors, dest->id, dest);
}

void irFunctionAddPredecessor(IrFunction *func, IrBlock *src, IrBlock *prev) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)intMapGet(func->cfg, src->id);
    if (!ir_block_mapping) {
        ir_block_mapping = irBlockMappingNew(src->id);
        intMapAdd(func->cfg, src->id, ir_block_mapping);
    }
    intMapAdd(ir_block_mapping->predecessors, prev->id, prev);
}

void irFunctionRemoveSuccessor(IrFunction *func, IrBlock *src, IrBlock *dest) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)intMapGet(func->cfg, src->id);
    if (ir_block_mapping) {
        intMapDelete(ir_block_mapping->successors, dest->id);
    }
}

void irFunctionRemovePredecessor(IrFunction *func, IrBlock *src, IrBlock *prev) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)intMapGet(func->cfg, src->id);
    if (ir_block_mapping) {
        intMapDelete(ir_block_mapping->predecessors, prev->id);
    }
}

/* Add `dest` to the `src`'s successor set AND add `src` to `dest`'s 
 * predecessor set */
void irFunctionAddMapping(IrFunction *func, IrBlock *src, IrBlock *dest) {
    irFunctionAddSuccessor(func, src, dest);
    irFunctionAddPredecessor(func, dest, src);
}

void irFunctionRemoveMapping(IrFunction *func, IrBlock *src, IrBlock *dest) {
    irFunctionRemoveSuccessor(func, src, dest);
    irFunctionRemovePredecessor(func, dest, src);
}

void irFunctionAddBlock(IrFunction *func, IrBlock *block) {
    listAppend(func->blocks, block);
}

IrBlock *irFunctionFindBlock(IrFunction *func, int id) {
    listForEach(func->blocks) {
        IrBlock *ir_block = (IrBlock *)it->value;
        if (ir_block->id == id) {
            return ir_block;
        }
    }
    return NULL;
}

/* This makes no sense to use in the wild as it is specifically formatted to be
 * used with the `intMapToString(...)` function. That isn't to say this cannot
 * be used... Just that it will look a bit our of place */
aoStr *irBlockMappingToStringCallback(void *_ir_block_mapping) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)_ir_block_mapping;
    aoStr *buf = aoStrNew();
    aoStr *successor_str = intMapKeysToString(ir_block_mapping->successors);
    aoStr *predecessor_str = intMapKeysToString(ir_block_mapping->predecessors);
    aoStrCatFmt(buf, "{\n"
                "    P: %S\n"
                "    S: %S\n  }",
                predecessor_str,
                successor_str);
    aoStrRelease(successor_str);
    aoStrRelease(predecessor_str);
    return buf;
}

aoStr *irFunctionCFGToString(IrFunction *func) {
    aoStr *buf = aoStrNew();
    /* `char *` has been allocated for an aoStr arena. DO NOT FREE */
    aoStr *map_str = intMapToString(func->cfg, ",\n  ", irBlockMappingToStringCallback);
    aoStrCatFmt(buf, "CFG {\n  %S\n}",map_str);
    return buf;
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

void irInstrRelease(IrInstr *ir_instr) {
    /* haven't figured out what to do with these... They are all from the area
     * so I guess we could 'recycle' unused instructions by keeping a free list
     * of them and checking that first which should lead to less memory
     * wastage */
    (void)ir_instr;
}

IrPair *irPairNew(IrBlock *ir_block, IrValue *ir_value) {
    IrPair *ir_phi_pair = (IrPair *)irArenaAlloc(sizeof(IrPair));
    ir_phi_pair->ir_value = ir_value;
    ir_phi_pair->ir_block = ir_block;
    return ir_phi_pair;
}

IrInstr *irPhi(IrBlock *block, IrValue *result) {
    IrInstr *ir_phi_instr = irInstrNew(IR_OP_PHI);
    ir_phi_instr->result = result;
    ir_phi_instr->extra.phi.pairs = ptrVecNew();

    irBlockAddInstruction(block, ir_phi_instr);
    return ir_phi_instr;
}

void irAddPhiIncoming(IrInstr *ir_phi_instr,
                      IrValue *ir_value, 
                      IrBlock *ir_block)
{
    IrPair *ir_phi_pair = irPairNew(ir_block, ir_value);
    ptrVecPush(ir_phi_instr->extra.phi.pairs, ir_phi_pair);
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
    irBlockAddInstruction(ir_block, ir_alloca_instr);
    return ir_alloca_instr;
}

/* op1 is where we are storing something and op2 is the thing we are storing 
 * I think op1 could/shoule have an offset as it is either going to be the 
 * stack or it is going to be a struct/pointer offset? */
IrInstr *irStore(IrBlock *ir_block, IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_store_instr = irInstrNew(IR_OP_STORE);
    ir_store_instr->op1 = ir_dest;
    ir_store_instr->op2 = ir_value;
    irBlockAddInstruction(ir_block, ir_store_instr);
    return ir_store_instr;
}

IrInstr *irLoad(IrBlock *ir_block, IrValue *ir_dest, IrValue *ir_value) {
    IrInstr *ir_load_instr = irInstrNew(IR_OP_LOAD);
    ir_load_instr->op1 = ir_dest;
    ir_load_instr->op2 = ir_value;
    irBlockAddInstruction(ir_block, ir_load_instr);
    return ir_load_instr;
}

IrInstr *irRet(IrBlock *ir_block, IrValue *ir_value) {
    IrInstr *ir_return_instr = irInstrNew(IR_OP_RET);
    ir_return_instr->op1 = ir_value;
    irBlockAddInstruction(ir_block, ir_return_instr);
    return ir_return_instr;
}

IrInstr *irGetElementPointer(IrBlock *ir_block,
                             IrValue *ir_dest,
                             IrValue *ir_value)
{
    IrInstr *ir_getelementptr_instr = irInstrNew(IR_OP_GEP);
    ir_getelementptr_instr->op1 = ir_dest;
    ir_getelementptr_instr->op2 = ir_value;
    /* @GEP
     * For clarity; op3 is the index, if it is NULL, there is no index */
    ir_getelementptr_instr->op3 = NULL;
    irBlockAddInstruction(ir_block, ir_getelementptr_instr);
    return ir_getelementptr_instr;
}

IrInstr *irGetElementPointerAtIdx(IrBlock *ir_block,
                                  IrValue *ir_dest,
                                  IrValue *ir_value,
                                  long offset)
{
    IrInstr *ir_getelementptr_instr = irInstrNew(IR_OP_GEP);
    IrValue *ir_offset = irConstInt(IR_TYPE_I64, offset);
    ir_getelementptr_instr->op1 = ir_dest;
    ir_getelementptr_instr->op2 = ir_value;
    ir_getelementptr_instr->op3 = ir_offset;
    irBlockAddInstruction(ir_block, ir_getelementptr_instr);
    return ir_getelementptr_instr;
}

IrFunction *irFunctionNew(aoStr *name) {
    IrFunction *ir_function = (IrFunction *)irArenaAlloc(sizeof(IrFunction));
    ir_function->name = name;
    ir_function->params = ptrVecNew();
    ir_function->blocks = listNew();
    ir_function->entry_block = NULL;
    ir_function->exit_block = NULL;
    ir_function->variables = strMapNew(16);
    ir_function->cfg = intMapNew(16);
    ir_function->has_var_args = 0;
    return ir_function;
}


/*==================== IR AST PARSING ======================================= */

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
    irBlockAddInstruction(block, instr);

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

    irBlockAddInstruction(block, instr);
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

    irBlockAddInstruction(block, instr);
    return instr;
}

__inline IrInstr *irUnary(IrBlock *block,
                          IrOpcode opcode,
                          IrValue *ir_result,
                          IrValue *ir_expr)
{
    IrInstr *instr = irInstrNew(opcode);
    instr->result = ir_result;
    instr->op1 = ir_expr;
    irBlockAddInstruction(block, instr);
    return instr;
}

IrInstr *irFNeg(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FNEG, ir_result, ir_expr);
}

IrInstr *irINeg(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_INEG, ir_result, ir_expr);
}


IrInstr *irSExt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_SEXT, ir_result, ir_expr);
}

IrInstr *irZExt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_ZEXT, ir_result, ir_expr);
}

IrInstr *irTrunc(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_TRUNC, ir_result, ir_expr);
}

IrInstr *irBitCast(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_BITCAST, ir_result, ir_expr);
}

IrInstr *irFPExt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPEXT, ir_result, ir_expr);
}

IrInstr *irFPTrunc(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPTRUNC, ir_result, ir_expr);
}

IrInstr *irSIToFP(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_SITOFP, ir_result, ir_expr);
}

IrInstr *irUIToFP(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_UITOFP, ir_result, ir_expr);
}

IrInstr *irFPToSI(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPTOSI, ir_result, ir_expr);
}

IrInstr *irFPToUI(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_FPTOUI, ir_result, ir_expr);
}

IrInstr *irPtrToInt(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_PTRTOINT, ir_result, ir_expr);
}

IrInstr *irIntToPtr(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_INTTOPTR, ir_result, ir_expr);
}

/*==================== IR MATHS ============================================= */
// Reasonably these could be refactored to one function with an opcode

__inline IrInstr *irMathsOp(IrBlock *block,
                   IrOpcode opcode,
                   IrValue *ir_result,
                   IrValue *left,
                   IrValue *right)
{
    IrInstr *instr = irInstrNew(opcode);
    instr->result = ir_result;
    instr->op1 = left;
    instr->op2 = right;
    irBlockAddInstruction(block, instr);
    return instr;
}

IrInstr *irFAdd(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FADD, ir_result, left, right);
}

IrInstr *irIAdd(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IADD, ir_result, left, right);
}

IrInstr *irFSub(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FSUB, ir_result, left, right);
}

IrInstr *irISub(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_ISUB, ir_result, left, right);
}

IrInstr *irFMul(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FMUL, ir_result, left, right);
}

IrInstr *irIMul(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IMUL, ir_result, left, right);
}

IrInstr *irFDiv(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_FDIV, ir_result, left, right);
}

IrInstr *irSDiv(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IDIV, ir_result, left, right);
}

IrInstr *irUDiv(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_UDIV, ir_result, left, right);
}

IrInstr *irSRem(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_IREM, ir_result, left, right);
}

IrInstr *irURem(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_UREM, ir_result, left, right);
}

IrInstr *irBitAnd(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_AND, ir_result, left, right);
}

IrInstr *irBitOr(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_OR, ir_result, left, right);
}

IrInstr *irBitNot(IrBlock *block, IrValue *ir_result, IrValue *ir_expr) {
    return irUnary(block, IR_OP_NOT, ir_result, ir_expr);
}

IrInstr *irXor(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_XOR, ir_result, left, right);
}

IrInstr *irSHL(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_SHL, ir_result, left, right);
}

IrInstr *irSHR(IrBlock *block, IrValue *ir_result, IrValue *left, IrValue *right) {
    return irMathsOp(block, IR_OP_SHR, ir_result, left, right);
}

/* Compare two values that are integers and branch return a `List<IrInstr *>` */
void irBuildIntCompareAndBranch(IrFunction *func,
                                IrBlock *block,
                                IrValue *var,
                                IrValue *var2,
                                IrCmpKind cmp_kind,
                                IrBlock *true_block,
                                IrBlock *false_block)
{
    IrValue *result = irTmpVariable(IR_TYPE_I8);
    IrInstr *ir_cmp_instr = irInstrNew(IR_OP_ICMP);
    ir_cmp_instr->result = result;
    ir_cmp_instr->op1 = var;
    ir_cmp_instr->op2 = var2;
    ir_cmp_instr->extra.cmp_kind = cmp_kind;

    IrInstr *ir_branch = irInstrNew(IR_OP_BR);
    ir_branch->op1 = ir_cmp_instr->result;
    ir_branch->target_block = true_block;
    ir_branch->fallthrough_block = false_block;


    irBlockAddInstruction(block, ir_cmp_instr);
    irBlockAddInstruction(block, ir_branch);
    irFunctionAddMapping(func, block, true_block);
    irFunctionAddMapping(func, block, false_block);
}
        
IrInstr *irBranch(IrFunction *func,
                  IrBlock *block,
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

    irBlockAddInstruction(block, instr);
    block->sealed = 1;

    irFunctionAddMapping(func, block, true_block);
    irFunctionAddMapping(func, block, false_block);

    return instr;
}

IrInstr *irJumpInternal(IrFunction *func,
                        IrBlock *block,
                        IrBlock *target,
                        IrOpcode opcode)
{
    if (!block || !target) {
        loggerPanic("NULL param\n");
    }

    if (block->sealed) {
        loggerWarning("Tried to add a jump to a sealed block: %d\n",
                block->id);
        return NULL;
    }

    IrInstr *instr = irInstrNew(opcode);
    instr->target_block = target;
    instr->fallthrough_block = NULL;

    /* Add to the current blocks instructions */
    irBlockAddInstruction(block, instr);

    /* This block is done */
    block->sealed = 1;

    /* Now update the control flow graph */
    irFunctionAddMapping(func, block, target);

    return instr;
}

IrInstr *irJump(IrFunction *func, IrBlock *block, IrBlock *target) {
    return irJumpInternal(func, block,target,IR_OP_JMP);
}

IrInstr *irLoop(IrFunction *func, IrBlock *block, IrBlock *target) {
    return irJumpInternal(func, block,target,IR_OP_LOOP);
}

IrInstr *irTmpGoto(IrCtx *ctx, aoStr *label) {
    IrInstr *ir_instr = irInstrNew(IR_OP_JMP);
    ir_instr->target_block = NULL;
    ir_instr->extra.unresolved_label = label;

    irBlockAddInstruction(ctx->current_block, ir_instr);

    List *list_node = ctx->current_block->instructions->prev;

    IrUnresolvedBlock *ir_unresolved_goto = irUnresolvedGotoNew(list_node,
                                                            ctx->current_block);
    /* There can be many different goto / source block combinations so 
     * we do not want a set. */
    ptrVecPush(ctx->unresolved_gotos, ir_unresolved_goto);

    return ir_instr;
}

IrInstr *irTmpGotoLabel(IrCtx *ctx, aoStr *label) {
    IrInstr *ir_instr = irInstrNew(IR_OP_LABEL);
    ir_instr->result = irValueNew(IR_TYPE_LABEL, IR_VALUE_UNRESOLVED);
    ir_instr->result->name = label;
    ir_instr->target_block = NULL;
    ir_instr->extra.unresolved_label = label;

    irBlockAddInstruction(ctx->current_block, ir_instr);
    List *list_node = ctx->current_block->instructions->prev;

    IrUnresolvedBlock *ir_unresolved_label = irUnresolvedLabelNew(list_node,
                                                            ctx->current_block);
    int ok = strMapAddAoStrOrErr(ctx->unresolved_labels,
                                 label,
                                 ir_unresolved_label);
    if (!ok) {
        loggerPanic("Label %s has already been seen in IR block %d\n", 
                label->data, ctx->current_block->id);
    }

    return ir_instr;
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
                        return irLoadClassRef(ctx, func, operand, operand->type, 0);
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
        }

        case AST_DEREF: {
            /* That is the class */
            Ast *cls = ast->operand;
            return irExpression(ctx, func, cls);
        }
        //if (ast->operand->kind == '+') {
        //    Ast *left = ast->operand->left;
        //    Ast *right = ast->operand->right;
        //    transpileAstInternal(left,ctx,indent);
        //    aoStrPutChar(buf, '[');
        //    transpileAstInternal(right,ctx,indent);
        //    aoStrPutChar(buf, ']');
        //} else {
        //    /* As `->` is a dereference we need to be able to distinguish 
        //     * between a class dereference and a general pointer dereference */
        //    if (ast->deref_symbol != TK_ARROW) {
        //        aoStrCatFmt(buf, "*");
        //    }
        //    transpileAstInternal(ast->operand,ctx,indent);
        //}
        //break;

        default:
            loggerPanic("Cannot turn Kind AST:%s %s into ir\n",
                    astKindToString(operand->kind),
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
            (void)irGetElementPointerAtIdx(ctx->current_block,
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

        case AST_CLASS_REF: {
            IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_dest = irTmpVariable(ir_expr->type);
            (void)irGetElementPointerAtIdx(ctx->current_block,
                             ir_dest,
                             ir_expr,
                             field->offset + offset);
            irStore(ctx->current_block,ir_dest,rhs);
            return ir_dest;
        }

        case AST_DEREF: {
            IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_dest = irTmpVariable(IR_TYPE_PTR);
            (void)irGetElementPointerAtIdx(ctx->current_block,
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

    switch (ast->left->kind) {
        case AST_LVAR: {
            IrValue *ir_local = irFunctionGetLocal(func, ast->left);
            irStore(ctx->current_block, ir_local, rhs);
            /* Assignments return the value */
            return rhs;
        }

        /* @Bug store the declvar in the parser not the whole AST for a 
         * default param */
        case AST_DEFAULT_PARAM: {
            IrValue *ir_local = irFunctionGetLocal(func, ast->left->declvar);
            irStore(ctx->current_block, ir_local, rhs);
            /* Assignments return the value */
            return rhs;
        }

        case AST_GVAR: {
            IrValue *ir_global = irFunctionGetGlobal(func, ast->left);
            irStore(ctx->current_block, ir_global, rhs);
            return rhs; 
        }

        case AST_DEREF: {
            IrValue *ptr = irExpression(ctx, func, ast->left->left);
            irStore(ctx->current_block, ptr, rhs);
            return rhs;
        }

        case AST_CLASS_REF: {
            return irAssignClassRef(ctx, func, ast->left->cls,
                                    ast->left->type, rhs, 0);
        }

        case AST_FUNPTR: {
            IrValue *ir_local = irFunctionGetLocalFnPtr(func, ast->left);
            irStore(ctx->current_block, ir_local, rhs);
            /* Assignments return the value */
            return rhs;
        }

        default: {
            loggerPanic("Unsupported LHS assignment %s %s\n",
                    astKindToString(ast->left->kind),
                    astToString(ast->left));

        }
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
            /*AstType *cls_type = cls->type;
            *if (cls_type->kind == AST_TYPE_POINTER) {
            *    cls_type = cls->type->ptr;
            */
            IrValueType ir_dest_type = irConvertType(field);
            IrValue *ir_dest = irTmpVariable(ir_dest_type);
            /* @Unknown
             * Why did this code used to be:
             * `IrValue *ir_local = irFunctionGetLocal(func, cls->left);` */
            IrValue *ir_local = irFunctionGetLocal(func, cls);
            (void)irGetElementPointerAtIdx(ctx->current_block,
                             ir_dest,
                             ir_local,
                             field->offset + offset);
            return ir_dest;
        }

        /* Call function again, this would be a nested struct or a union */
        case AST_CLASS_REF:
            return irLoadClassRef(ctx,
                                 func,
                                 cls->operand,
                                 field,
                                 offset + field->offset);

        /* Load the `->` dereference */
        case AST_DEREF: {
            IrValue *ir_expr = irExpression(ctx, func, cls->operand);
            IrValue *ir_dest = irTmpVariable(irConvertType(field));
            (void)irGetElementPointerAtIdx(ctx->current_block,
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

/* Actually... we should push all values to one vector and then have the 
 * shape of the vector as different properties on the struct */
void irArrayInit(IrCtx *ctx, IrFunction *func, Ast *ast) {
    int dimension_size = ast->arrayinit->size;
    Ast *array_value = NULL;

    /* Eventually that will be correct */
    ctx->array_.length_per_array = dimension_size;

    for (int i = 0; i < ast->arrayinit->size; ++i) {
        array_value = (Ast *)ast->arrayinit->entries[i];
        IrValue *ir_value = irExpression(ctx, func, array_value);
        if (ir_value) {
            ctx->array_.type = ir_value->type;
            ptrVecPush(ctx->array_.init, ir_value);
        }
    }
}

IrValue *irExpression(IrCtx *ctx, IrFunction *func, Ast *ast) {
    IrBlock *ir_block = ctx->current_block;
    if (!ast) return NULL;

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

        case AST_DEFAULT_PARAM: {
            Ast *declvar = ast->declvar;
            IrValue *local_var = irFunctionGetLocal(func, declvar);
            if (!local_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(declvar->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, local_var);
            return ir_load_dest;
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

        case AST_GVAR: {
            IrValue *global_var = irFunctionGetGlobal(func, ast);
            if (!global_var) {
                loggerPanic("Variable %s not found\n", astToString(ast));
            }

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, global_var);
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

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND: {
            IrValue *fn = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
            fn->name = ast->asmfname;

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, fn);
            /* @Tracking Update where the variable is? */
            strMapAddAoStr(func->variables,ast->fname,ir_load_dest);
            return ir_load_dest;
        }
    
        /* We hit this when we load function arguments. NOT for anything else */
        case AST_EXTERN_FUNC:
        case AST_FUNC: {
            IrValue *fn = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);  // irFunctionGetLocalFnPtr(func, ast);
            fn->name = ast->fname;

            IrValueType ir_value_type = irConvertType(ast->type);
            IrValue *ir_load_dest = irTmpVariable(ir_value_type);
            irLoad(ir_block, ir_load_dest, fn);
            /* @Tracking Update where the variable is? */
            strMapAddAoStr(func->variables,ast->fname,ir_load_dest);
            return ir_load_dest;
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
            } else if (ast->operand->kind == AST_DEFAULT_PARAM) {
                /* @Bug
                 * The parser should just save the declvar not the whole 
                 * Ast. */
                ir_value = irFunctionGetLocal(func, ast->operand->declvar);
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
            IrBlock *ir_right_block = irBlockNew(irBlockId());
            IrBlock *ir_end_block = irBlockNew(irBlockId());

            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            irBranch(func, ir_block, left, ir_right_block, ir_end_block);
            irFunctionAddBlock(func, ir_right_block);
            irCtxSetCurrentBlock(ctx, ir_right_block);

            IrValue *right = irExpression(ctx, func, ast->right);

            irJump(func, ctx->current_block, ir_end_block);
            irCtxSetCurrentBlock(ctx, ir_end_block);
            irFunctionAddBlock(func, ir_end_block);

            IrInstr *phi_instr = irPhi(ir_end_block, ir_result);
            irAddPhiIncoming(phi_instr, irConstInt(IR_TYPE_I8, 0), ir_block);
            irAddPhiIncoming(phi_instr, right, ir_right_block);
            return ir_result;
        }

        case TK_OR_OR: {
            IrBlock *ir_block = ctx->current_block;
            IrBlock *ir_right_block = irBlockNew(irBlockId());
            IrBlock *ir_end_block = irBlockNew(irBlockId());

            IrValue *left = irExpression(ctx, func, ast->left);
            IrValue *ir_result = irTmpVariable(IR_TYPE_I8);

            irBranch(func, ir_block, left, ir_end_block, ir_right_block);
            irFunctionAddBlock(func, ir_right_block);
            irCtxSetCurrentBlock(ctx, ir_right_block);

            IrValue *right = irExpression(ctx, func, ast->right);

            irJump(func, ctx->current_block, ir_end_block);
            irCtxSetCurrentBlock(ctx, ir_end_block);
            irFunctionAddBlock(func, ir_end_block);

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
            IrValueType fn_ret_type = irConvertType(ast->type);
            IrValue *ir_fn_ret = irTmpVariable(fn_ret_type);
            IrValue *ir_call_args = irValueNew(IR_TYPE_ARRAY, IR_VALUE_PARAM);
            IrInstr *ir_call_intr = irInstrNew(IR_OP_CALL);

            if (ast->kind == AST_FUNPTR_CALL) {
                /* This needs to be an offset load */
                if (ast->ref && ast->ref->kind == AST_CLASS_REF) {
                    /* This will load the function pointer from the class */
                    IrValue *ir_expr = irExpression(ctx, func, ast->ref);
                    ir_call_args->name = ir_expr->name;
                } else {
                    IrValue *fn_ptr_var = irFunctionGetLocalFnPtr(func, ast);
                    ir_call_args->name = fn_ptr_var->name;
                }
            } else if (ast->kind == AST_FUNCALL) {
                ir_call_args->name = ast->fname;
            } else if (ast->kind == AST_ASM_FUNCALL) {
                ir_call_args->name = ast->fname;
            }

            PtrVec *call_args = ptrVecNew();
            ir_call_intr->result = ir_fn_ret;
            ir_call_intr->op1 = ir_call_args;
            ir_call_args->array_.values = call_args;

            if (ast->args) {
                for (int i = 0; i < ast->args->size; ++i) {
                    Ast *ast_arg = vecGet(Ast *, ast->args, i);
                    IrValue *ir_arg = irExpression(ctx, func, ast_arg);
                    ptrVecPush(call_args, ir_arg);
                } 
            }

            listAppend(ctx->current_block->instructions, ir_call_intr);
            /* This is the return value of the function */
            return ir_fn_ret;
        }

        /* Start a basic block */
        case AST_LABEL: {
            IrBlock *ir_block = irBlockNew(irBlockId());
            irJump(func, ctx->current_block, ir_block);
            irCtxSetCurrentBlock(ctx, ir_block);
            irFunctionAddBlock(func, ir_block);
            IrInstr *ir_instr = irTmpGotoLabel(ctx, ast->slabel);
            return ir_instr->result;
        }

        /* Ends a basic block */
        case AST_GOTO: {
            /* If the block is completely empty then we can just reuse it 
             * else we should add a new block */
            irTmpGoto(ctx, ast->slabel);
            IrBlock *ir_block = irBlockNew(irBlockId());
            irCtxSetCurrentBlock(ctx, ir_block);
            irFunctionAddBlock(func, ir_block);
            return NULL;
        }

        case AST_ARRAY_INIT: {
            /* If we reach here we are nesting an array */
            ctx->array_.nesting++;
            irArrayInit(ctx, func, ast);
            break;
        }

        case AST_CAST: {
            /* Skip no-op casts if types are the same */
            AstType *from_type = ast->operand->type;
            AstType *to_type = ast->type;
            IrValue *expr = irExpression(ctx, func, ast->operand);


            if (to_type->kind == from_type->kind) {
                return expr;
            }

            IrValueType ir_to_type = irConvertType(to_type);
            IrValue *result = irTmpVariable(ir_to_type);
            /* @Bug
             * We only use classes as 'intrinsics' that are integers at the 
             * moment however this should be expanded to accept floats */
            int types_are_ints = (astIsIntType(from_type) || 
                                  from_type->is_intrinsic) &&
                                 (astIsIntType(to_type) || 
                                  to_type->is_intrinsic);

            if (types_are_ints) {
                if (from_type->size < to_type->size) {
                    if (to_type->issigned) {
                        irSExt(ctx->current_block, result, expr);  /* Sign extend */
                    } else {
                        irZExt(ctx->current_block, result, expr);  /* Zero extend */
                    }
                } else if (from_type->size > to_type->size) {
                    irTrunc(ctx->current_block, result, expr); /* Larger to smaller int */
                } else {
                    irBitCast(ctx->current_block, result, expr); /* Change signedness */
                }
            }
            /* Floating points casts */
            else if (astIsFloatType(from_type) && astIsFloatType(to_type)) {
                /* HolyC only has F64, though this is for completeness */
                if (from_type->size < to_type->size) {
                    /* floating point extend */
                    irFPExt(ctx->current_block, result, expr);
                } else {
                    /* floating point truncate */
                    irFPTrunc(ctx->current_block, result, expr);
                }
            }
            /* Integer-to-float casts */
            else if (astIsIntType(from_type) && astIsFloatType(to_type)) {
                if (from_type->issigned) {
                    /* Signed int to float */
                    irSIToFP(ctx->current_block, result, expr);
                } else {
                    /* unsigned int to float */
                    irUIToFP(ctx->current_block, result, expr);
                }
            }
            /* Floating point to integer casts */
            else if (astIsFloatType(from_type) && astIsIntType(to_type)) {
                if (to_type->issigned) {
                    /* Float to signed int */
                    irFPToSI(ctx->current_block, result, expr);
                } else {
                    /* Float to usigned int */
                    irFPToUI(ctx->current_block, result, expr);
                }
            }
            /* Pointer casts */
            else if (from_type->kind == to_type->kind) { /* @Unreached? */
                /* Pointer to pointer... no op? */
                irBitCast(ctx->current_block, result, expr);
            }
            else if (from_type->kind == AST_TYPE_POINTER && astIsIntType(to_type)) {
                /* Pointer to integer */
                irPtrToInt(ctx->current_block, result, expr);
            }
            else if (astIsIntType(from_type) && to_type->kind == AST_TYPE_POINTER) {
                /* Integer to pointer  */
                irIntToPtr(ctx->current_block, result, expr);
            } else {
                loggerPanic("Unsupported cast: %s to %s", 
                        astTypeToString(from_type),
                        astTypeToString(to_type));
            }

            return result;
        }


        default:
            loggerPanic("Error with function '%s' Unhandled Ast kind: %s %s\n",
                    func->name->data,
                    astKindToString(ast->kind),
                    astToString(ast));
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
                /* We cannot do this because of unresolved gotos that may need 
                 * to jump to this block vvvvv */
                //if (ctx->current_block->sealed) break;
            }
            break;
        }

        case AST_IF: {
            /* This creates a compare and then a branch */
            IrValue *ir_cond = irExpression(ctx, func, ast->cond);
            IrBlock *ir_then = irBlockNew(irBlockId());
            IrBlock *ir_else = ast->els ? irBlockNew(irBlockId()) : NULL;
            IrBlock *ir_end_block = irBlockNew(irBlockId());

            irFunctionAddBlock(func, ir_then);

            if (ir_else) {
                irBranch(func, ctx->current_block, ir_cond, ir_then, ir_else);
            } else {
                irBranch(func, ctx->current_block, ir_cond, ir_then, ir_end_block);
            }

            irCtxSetCurrentBlock(ctx, ir_then);

            irStatement(ctx, func, ast->then);
            if (!ctx->current_block->sealed) {
                irJump(func, ctx->current_block, ir_end_block);
            }

            if (ir_else) {
                irFunctionAddBlock(func, ir_else);
                irCtxSetCurrentBlock(ctx, ir_else);
                irStatement(ctx, func, ast->els);
                if (!ctx->current_block->sealed) {
                    irJump(func, ctx->current_block, ir_end_block);
                }
            }

            irFunctionAddBlock(func, ir_end_block);

            /* Update the current block we are pointing to */
            irCtxSetCurrentBlock(ctx, ir_end_block);
            break;
        }

        /* LOOP IR start =====================================================*/
        case AST_WHILE: {
            IrBlock *ir_while_cond = irBlockNew(irBlockId());
            IrBlock *ir_while_body = irBlockNew(irBlockId());
            IrBlock *ir_while_end = irBlockNew(irBlockId());
            IrBlock *ir_previous_end_block = ctx->end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;
            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* Update the current end block */
            irCtxSetLoopHeadBlock(ctx, ir_while_cond);
            irCtxSetEndBlock(ctx, ir_while_end);

            irJump(func, ctx->current_block, ir_while_cond);

            irCtxSetCurrentBlock(ctx, ir_while_cond);
            irFunctionAddBlock(func, ir_while_cond);

            IrValue *ir_while_cond_expr = irExpression(ctx, func, ast->whilecond);
            irBranch(func,
                     ctx->current_block, 
                     ir_while_cond_expr,
                     ir_while_body, 
                     ir_while_end);

            irCtxSetCurrentBlock(ctx, ir_while_body);

            irFunctionAddBlock(func, ir_while_body);
            irStatement(ctx, func, ast->whilebody);

            if (!ctx->current_block->sealed) {
                irLoop(func, ctx->current_block, ir_while_cond);
            }
            irFunctionAddBlock(func, ir_while_end);
            irCtxSetCurrentBlock(ctx, ir_while_end);
            ctx->flags = ctx_prev_flags;
            /* We are now out of the scope of the while loop so reset the 
             * end block to what it was previously */
            irCtxSetEndBlock(ctx, ir_previous_end_block);
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
            IrBlock *ir_dowhile_body = irBlockNew(irBlockId());
            IrBlock *ir_dowhile_cond = irBlockNew(irBlockId());
            IrBlock *ir_dowhile_end = irBlockNew(irBlockId());

            IrBlock *ir_previous_end_block = ctx->end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;

            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* this seems weird as a continue would not hit the condition 
             * which is what would happen with a for loop. Though this is 
             * "correct" it simply seems a bit odd */
            irCtxSetLoopHeadBlock(ctx, ir_dowhile_body);
            irCtxSetEndBlock(ctx, ir_dowhile_end);

            /* Jump into the body and prepare IR for the body */
            irJump(func, ctx->current_block, ir_dowhile_body);
            irCtxSetCurrentBlock(ctx, ir_dowhile_body);
            irFunctionAddBlock(func, ir_dowhile_body);
            irStatement(ctx, func, ast->whilebody);

            /* Jump into the conditio and prepare IR for the branch */
            irJump(func, ctx->current_block, ir_dowhile_cond);
            irCtxSetCurrentBlock(ctx, ir_dowhile_cond);
            irFunctionAddBlock(func, ir_dowhile_cond);
            IrValue *ir_dowhile_cond_expr = irExpression(ctx, func,
                                                         ast->whilecond);
            irBranch(func,
                     ctx->current_block,
                     ir_dowhile_cond_expr,
                     ir_dowhile_body,
                     ir_dowhile_end);
            irLoop(func, ctx->current_block, ir_dowhile_body);

            irFunctionAddBlock(func, ir_dowhile_end);
            irCtxSetCurrentBlock(ctx, ir_dowhile_end);

            /* Restore previous state */
            ctx->flags = ctx_prev_flags;
            irCtxSetEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }

        case AST_FOR: {
            IrBlock *ir_for_cond = ast->forcond ? irBlockNew(irBlockId()) : NULL;
            IrBlock *ir_for_body = irBlockNew(irBlockId());
            IrBlock *ir_for_step = ast->forstep ? irBlockNew(irBlockId()) : NULL;
            IrBlock *ir_for_head = ir_for_cond ? ir_for_cond : ir_for_body;
            IrBlock *ir_for_end = irBlockNew(irBlockId());

            IrBlock *ir_previous_end_block = ctx->end_block;
            IrBlock *ir_previous_head_block = ctx->loop_head_block;

            unsigned long ctx_prev_flags = ctx->flags;
            ctx->flags |= IR_CTX_FLAG_IN_LOOP;

            /* Initaliser belongs to the current block */
            if (ast->forinit) {
                irStatement(ctx, func, ast->forinit);
            }

            /* Update the current end block */
            irCtxSetLoopHeadBlock(ctx, ir_for_cond ? ir_for_cond : ir_for_body);
            irCtxSetEndBlock(ctx, ir_for_end);

            if (ir_for_cond) {
                irJump(func, ctx->current_block, ir_for_cond);
                irCtxSetCurrentBlock(ctx, ir_for_cond);
                irFunctionAddBlock(func, ir_for_cond);

                IrValue *ir_for_cond_expr = irExpression(ctx, func, ast->forcond);
                irBranch(func,
                         ctx->current_block, 
                         ir_for_cond_expr,
                         ir_for_body, 
                         ir_for_end);
            }

            irCtxSetCurrentBlock(ctx, ir_for_body);

            irFunctionAddBlock(func, ir_for_body);
            irStatement(ctx, func, ast->forbody);

            /* If there is a step we want to add it to the current block */
            if (ir_for_step) {
                irJump(func, ctx->current_block, ir_for_step);
                irCtxSetCurrentBlock(ctx, ir_for_step);
                irFunctionAddBlock(func, ir_for_step);
                irExpression(ctx, func, ast->forstep);

                irLoop(func, ctx->current_block, ir_for_head);
            } else {
                if (!ctx->current_block->sealed) {
                    irLoop(func, ctx->current_block, ir_for_head);
                }
            }

            irFunctionAddBlock(func, ir_for_end);
            irCtxSetCurrentBlock(ctx, ir_for_end);

            /* Restore */
            ctx->flags = ctx_prev_flags;
            irCtxSetEndBlock(ctx, ir_previous_end_block);
            irCtxSetLoopHeadBlock(ctx, ir_previous_head_block);
            break;
        }
        /* LOOP IR end =======================================================*/

        case AST_CONTINUE: {
            if (ctx->flags & IR_CTX_FLAG_IN_LOOP) {
                irJump(func, ctx->current_block, ctx->loop_head_block);
            } else {
                loggerPanic("Continue found outside of loop\n");
            }
            break;
        }

        case AST_SWITCH: {
            unsigned long prev_flags = ctx->flags;
            IrBlock *ir_prev_end_block = ctx->end_block;

            ctx->flags |= IR_CTX_FLAG_IN_SWITCH;

            /* Evaluate the switch condition `switch(<condition>)`, as we may 
             * need to do a bounds check we create the instruction after 
             * the possible bounds check but need the Value ahead of time */
            IrValue *switch_var = irExpression(ctx, func, ast->switch_cond);
            /* The block passsed the switch */
            IrBlock *end_block = irBlockNew(irBlockId());
            IrBlock *default_block = ast->case_default ? irBlockNew(irBlockId()) : NULL;
            IrBlock *out_of_bounds_block = default_block ? default_block : end_block;

            /* We want to use the orderd jump table */
            Ast **jump_table = ast->jump_table_order;
            int jump_table_size = ast->cases->size;

            /* If we have bounds checking we want to make sure that we are in
             * range and either jump to the `default` case if there is one or 
             * jump passed the switch */
            if (ast->switch_bounds_checked) {
                Ast *case_ast_min = jump_table[0];
                Ast *case_ast_max = jump_table[jump_table_size - 1];
                /* Create some temporaries for the minimum and maximum values of 
                 * the jump table */
                IrValue *min_var = irConstInt(switch_var->type, case_ast_min->case_begin);
                IrValue *max_var = irConstInt(switch_var->type, case_ast_max->case_end);

                IrBlock *max_block = irBlockNew(irBlockId());
                IrBlock *switch_jump_table = irBlockNew(irBlockId());

                /* Compare the minimum and jump to the max check, or to the 
                 * out of bounds */
                irBuildIntCompareAndBranch(func, ctx->current_block, switch_var,
                        min_var, IR_CMP_LT, max_block, out_of_bounds_block);

                irFunctionAddBlock(func, max_block);
                irCtxSetCurrentBlock(ctx, max_block);

                /* Compare maximum and jump to the jump table condition or 
                 * to out of bounds */
                irBuildIntCompareAndBranch(func, ctx->current_block, switch_var, max_var,
                        IR_CMP_GT, switch_jump_table, out_of_bounds_block);

                irFunctionAddBlock(func, switch_jump_table);
                irCtxSetCurrentBlock(ctx, switch_jump_table);
            }

            IrInstr *ir_switch_head = irInstrNew(IR_OP_SWITCH);
            /* Setup the switch condition instruction */
            ir_switch_head->op1 = switch_var;
            ir_switch_head->target_block = end_block;
            ir_switch_head->extra.cases = listNew();
            irBlockAddInstruction(ctx->current_block, ir_switch_head);

            IrBlock *switch_start = ctx->current_block;

            /* Reserve the basic blocks for the case labels so we can do fall 
             * throughs if needed. */
            PtrVec *case_blocks = ptrVecNew();
            for (int i = 0; i < jump_table_size; ++i) {
                ptrVecPush(case_blocks, irBlockNew(irBlockId()));
            }

            for (int i = 0; i < jump_table_size; ++i) {
                /* The next block is either the block after or the out of bounds 
                 * block. This is for when the last instruction is not a break,
                 * thus we create a jump to the next block. */
                IrBlock *next_block = i+1 >= jump_table_size ?
                                             out_of_bounds_block :
                                             vecGet(IrBlock *,case_blocks,i+1);

                Ast *case_ = jump_table[i];
                IrBlock *ir_case_block = vecGet(IrBlock *,case_blocks,i);

                irCtxSetCurrentBlock(ctx, ir_case_block);
                /* @Redundant?
                 * Consistently re-set the end block as it is possible inside 
                 * the case there could be something that sets the end block 
                 * that we are not intrested in. */
                irCtxSetEndBlock(ctx, end_block);

                /* Expand out the case ranges to multiple labels that point to 
                 * the same block */
                for (int case_number = case_->case_begin; case_number <= case_->case_end; ++case_number) {
                    IrValue *ir_case_value = irConstInt(IR_TYPE_I64, case_number);
                    IrPair *ir_pair = irPairNew(ir_case_block, ir_case_value);
                    listAppend(ir_switch_head->extra.cases, ir_pair);
                }

                irFunctionAddMapping(func, switch_start, ir_case_block);
                irFunctionAddBlock(func,ir_case_block);
    
                /* Evaluate the body of the case */
                listForEach(case_->case_asts) {
                    Ast *stmt = listValue(Ast *, it);
                    irStatement(ctx, func, stmt);
                }

                IrInstr *last_instruction = irBlockLastInstr(ir_case_block);
                if (!last_instruction || (last_instruction && last_instruction->opcode != IR_OP_JMP)) { 
                    /* if the last operation in the block is not a jump then 
                     * we need to add a jump from the case block to the next 
                     * block. Or if we have stacked cases
                     * */
                    irJump(func, ir_case_block, next_block);
                }
            }

            /* If we have a default block then evaluate the block */
            if (default_block) {
                irCtxSetEndBlock(ctx, end_block);
                irCtxSetCurrentBlock(ctx, default_block);
                irFunctionAddBlock(func,default_block);

                listForEach(ast->case_default->case_asts) {
                    Ast *stmt = listValue(Ast *, it);
                    irStatement(ctx, func, stmt);
                }
            }

            ptrVecRelease(case_blocks);
            ctx->flags = prev_flags;
            irFunctionAddBlock(func,end_block);
            irCtxSetEndBlock(ctx, ir_prev_end_block);
            irCtxSetCurrentBlock(ctx, end_block);
            break;
        }

        case AST_BREAK: {
            assert(ctx->end_block);
            irJump(func, ctx->current_block, ctx->end_block);
            break;
        }

        case AST_DECL: {
            int ast_kind = ast->declvar->type->kind;
            IrInstr *ir_local = irAlloca(ctx->current_block, ast->declvar->type);
            IrValue *ir_local_var = irGetAllocaVar(ir_local);
            int ok = 0;

            if (ast_kind == AST_TYPE_FUNC) {
                ok = strMapAddAoStr(func->variables,
                                    ast->declvar->tmp_fnptr_name, 
                                    ir_local_var);
            } else {
                ok = strMapAddAoStr(func->variables,
                                    ast->declvar->tmp_var_name, 
                                    ir_local_var);
            }

            if (!ok) {
                loggerPanic("Failed to set function parameter variable with name %s already exists!\n",
                        ir_local_var->name->data);
            }

            if (ast->declinit) {
                IrValue *ir_init = NULL;
                switch (ast->declinit->kind) {
                    case AST_ARRAY_INIT: {
                        irCtxResetArray(ctx);
                        PtrVec *ir_array = ptrVecNew();
                        ctx->array_.init = ir_array;
                        irArrayInit(ctx, func, ast->declinit);

                        IrValue *head = (IrValue *)ir_array->entries[0];
                        ir_init = irValueNew(IR_TYPE_ARRAY_INIT, head->kind);
                        ir_init->array_.values = ir_array;
                        ir_init->array_.nesting = ctx->array_.nesting;
                        ir_init->array_.length_per_array = ctx->array_.length_per_array;
                        ir_init->array_.label = irArrayName(func);
                        strMapAddAoStr(ctx->ir_program->arrays, ir_init->array_.label, ir_init);
                        break;
                    }

                    case AST_FUN_PROTO:
                    case AST_FUNC:
                    case AST_EXTERN_FUNC:
                    case AST_ASM_FUNCDEF:
                    case AST_ASM_FUNC_BIND: {
                        ir_init = irGlobalExpression(ctx, ast->declinit);
                        break;
                    }

                    default: {
                        ir_init = irExpression(ctx, func, ast->declinit);
                        break;
                    }    
                }

                irStore(ctx->current_block,ir_local_var,ir_init);  
            }
            break;
        }

        /* We want to avoid having multiple return statements in a function 
         * it is more cannoincal to move the value you want to return to a
         * specified stack variable and then jump to the end of the function. 
         * Moving the stack variable into the return register. */
        case AST_RETURN: {
            IrValue *ir_return_val = irExpression(ctx, func, ast->retval);
            irStore(ctx->current_block,func->return_value,ir_return_val);
            irJump(func, ctx->current_block, func->exit_block);
            break;
        }

        default:
            irExpression(ctx, func, ast);
            break;
    }
}

void irResolveGotos(IrCtx *ctx, IrFunction *func) {
    PtrVec *ir_goto_vec = ctx->unresolved_gotos;
    for (int i = 0; i < ir_goto_vec->size; ++i) {
        IrUnresolvedBlock *ir_ugoto = vecGet(IrUnresolvedBlock *,ir_goto_vec,i);
        List *goto_list_node = ir_ugoto->goto_.list_node;
        IrInstr *goto_instr = (IrInstr *)goto_list_node->value;
        aoStr *goto_label = (aoStr *)goto_instr->extra.unresolved_label;

        IrUnresolvedBlock *ir_ulabel = strMapGetAoStr(ctx->unresolved_labels,
                                                      goto_label);

        if (!ir_ulabel) {
            loggerPanic("Goto points to label `%s` which does not exist\n",
                    goto_label->data);
        }

        IrBlock *ir_src = ir_ugoto->goto_.ir_block;
        IrBlock *ir_dest = ir_ulabel->label_.ir_block;

        /* We remove the temporary label, do we even need to add it in the 
         * first place? While developing this, it is nice to see it being 
         * printed but this can be removed once the feature is built. */
        listDeque(ir_dest->instructions);
        goto_instr->target_block = ir_dest;
        irFunctionAddMapping(func, ir_src, ir_dest);
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
    } else if (ast_param->kind == AST_DEFAULT_PARAM) {
        ir_value->name = ast_param->declvar->tmp_var_name;
    } else {
        loggerPanic("Unhandled Ast: %s\n", astToString(ast_param));
    }
    return ir_value;
}

/* Remove `delete_block` from all predecessor and successor maps */
void irFunctionDelinkBlock(IrFunction *func, IrBlock *delete_block) {
    List *needle = NULL;
    listForEach(func->blocks) {
        IrBlock *block = listValue(IrBlock *, it);
        if (block->id == delete_block->id) {
            needle = it;
        }
        irFunctionRemoveSuccessor(func, block, delete_block);
        irFunctionRemovePredecessor(func, block, delete_block);
    }

    intMapDelete(func->cfg, delete_block->id);
    if (needle) {
        listUnlink(func->blocks, needle);
    }
}

/* If the fallthrough id is the destination id and the destination target is 
 * the same as the target, then we can remove the branch */
int irBranchFallsbackToSameBlock(IrInstr *src, InInstr *destination, IrBlock *block) {
    if (src->opcode == IR_OP_BR) {
        return src->fallthrough_block->id == block->id &&
               src->target_block->id == destination->target_block->id;
    }
    return 0;
}

/* Merge target to the end of block */
void irBlockMerge(IrFunction *func, IrBlock *block, IrBlock *target) {
    /* We now also need to unlink `target` which is kind of tricky as we use a 
     * linked list... */
    irFunctionRemoveSuccessor(func, block, target);
    irFunctionRemovePredecessor(func, block, target);
    listMergeAppend(block->instructions, target->instructions);
    /* A `NULL` list of instructions means it can be deleted */
    target->instructions = NULL;
}

int irBlocksHaveSingleLink(IrFunction *func, IrBlock *next, IrBlock *prev) {
    IrBlockMapping *prev_mapping = intMapGet(func->cfg, prev->id);
    IrBlockMapping *next_mapping = intMapGet(func->cfg, next->id);
    /* This should not happen */
    if (!prev_mapping || !next_mapping) {
        return 0;
    }

    if (prev_mapping->successors->size == 1 && next_mapping->predecessors->size == 1) {
        IrBlock *previous_next = intMapGetFirst(prev_mapping->successors);

        if (previous_next->id == next->id && next->id == previous_next->id) {
            return 1;
        }
    }
    return 0;
}

/* This function takes a `IrBlock *` node and returns the id */
int irBlockListIdAccessor(void *_ir_block) {
    return ((IrBlock *)_ir_block)->id;
}

/* Can the entry and exit be joined together? This is a function with no 
 * jumps or branches and the start block has no joining blocks and the 
 * end block similarly has no joining blocks */
int irBlockEntryCanMergeExit(IrFunction *func) {
    /* The nodes do not point to anything */
    if (!irBlockHasSuccessors(func, func->entry_block) &&
        !irBlockHasPredecessors(func, func->entry_block) &&
        !irBlockHasSuccessors(func, func->exit_block) &&
        !irBlockHasPredecessors(func, func->exit_block))
    {
        return 1;
    }
    return 0;
}

void irSimplifyBlocks(IrFunction *func) {
    UniqList *work_queue = uniqListNew(&irBlockListIdAccessor);
    IntSet *blocks_to_delete = intSetNew(16);

    /* 2 passses seems to eliminate a lot of issues however is not based on 
     * anything other than trial and error, this is primarily done to run 
     * `irFunctionDelinkBlock(...)` which scans all blocks removing items from 
     * the set of successors and predecessors */
    int i = 2;
    while (i--) {
        /** 
         * @THINK
         * We append the list nodes themeselves as opposed to the `IrBlock *` as this
         * allows for easier delinking from the actual `IrFunction *`'s blocks. Though
         * it is _slightly_ annoying having to cast the value, seems like a reasonable
         * compromise.
         *
         * Maybe we keep a hashtable of nodes that we need to delete from the blocks
         * and do it in one pass. Doing it as we go seems extremely problematic as
         * you'd keep doing passes of the list. */
        listForEach(func->blocks) {
            uniqListAppend(work_queue, listValue(IrBlock *, it));
        }

        while (!uniqListEmpty(work_queue)) {
            /* We kind of have a list of lists */
            IrBlock *block = (IrBlock *)uniqListDequeue(work_queue);

            if (block == func->exit_block) continue;
            if (intSetHas(blocks_to_delete, block->id)) continue;

            /* Currently only a few simple cases are handled for the block 
             * simplification but it is enough to work with and have some simple 
             * optimisations. */
            
            if (irBlockIsRedundant(func, block)) {
                intSetAdd(blocks_to_delete, block->id);
            } else if (irBlockIsConstCompareAndBranch(block)) {
                IrInstr *ir_cmp= listValue(IrInstr *, block->instructions->next);
                IrInstr *ir_branch = listValue(IrInstr *, block->instructions->next->next);
                IrBlock *jump_target = irInstrEvalConstBranch(ir_cmp, ir_branch);

                /* Clear the list, removing the cmp and the branch */
                irInstrRelease(listPop(block->instructions));
                irInstrRelease(listPop(block->instructions));

                /* Jump from the source to the destiation without doing the 
                 * comarison! */
                block->sealed = 0;

                irFunctionRemoveSuccessor(func, block, ir_branch->fallthrough_block);
                irFunctionRemovePredecessor(func, ir_branch->fallthrough_block, block);
                if (!intSetHas(blocks_to_delete, jump_target->id)) {
                    irJump(func, block, jump_target);
                }
            } else if (irBlockIsRedundantJump(func, block)) {
                IrInstr *ir_jmp = irBlockLastInstr(block);
                irFunctionRemoveSuccessor(func, block, ir_jmp->target_block);
                irFunctionRemovePredecessor(func, ir_jmp->target_block, block);
                listPop(block->instructions);
                if (!intSetHas(blocks_to_delete, ir_jmp->target_block->id)) {
                    irBlockMerge(func, block, ir_jmp->target_block);
                }
                intSetAdd(blocks_to_delete, ir_jmp->target_block->id);
            }
        }

        listForEach(func->blocks) {
            IrBlock *block = it->value;

            if (irLastInstructionIsJumpLike(block)) {
                IrInstr *last_instr = irBlockLastInstr(block);
                switch (last_instr->opcode) {
                    case IR_OP_JMP:
                    case IR_OP_LOOP:
                        if (intSetHas(blocks_to_delete, last_instr->target_block->id)) {
                            loggerWarning("case 1 bb%d\n", block->id);
                            last_instr->target_block = func->exit_block;
                            irFunctionAddMapping(func, block, func->exit_block);
                        }
                        break;
                    case IR_OP_BR: {
                        if (intSetHas(blocks_to_delete, last_instr->target_block->id)) {
                            loggerWarning("case 2 bb%d\n", block->id);
                            last_instr->target_block = func->exit_block;
                            irFunctionAddMapping(func, block, func->exit_block);
                        }

                        if (intSetHas(blocks_to_delete, last_instr->fallthrough_block->id)) {
                            loggerWarning("case 3 bb%d\n", block->id);
                            last_instr->fallthrough_block = func->exit_block;
                            irFunctionAddMapping(func, block, func->exit_block);
                        }
                        break;
                    }
                    default:
                        break;
                }
            }

            if (intSetHas(blocks_to_delete, block->id)) {
                /* This means that nothing links to it. However... Things can 
                 * reference this node so we need to traverse all blocks removing 
                 * it. Doing this is extremely slow */
                irFunctionDelinkBlock(func, block);
            }

            if (!irBlockHasPredecessors(func, block) && block != func->exit_block) {
                loggerWarning("No predecessors for block %d\n", block->id);
                irFunctionDelinkBlock(func, block);
            }

            //if (mapping && mapping->successors->size == 0 && block != func->exit_block) {
            //    printf("Creating jump from %d to %d\n", block->id, func->exit_block->id);
            //    irJump(func, block, func->exit_block);
            //}
        }
    } /* While end */

    if (irBlockEntryCanMergeExit(func) || irBlocksPointToEachOther(func, func->entry_block, func->exit_block)) {
        irBlockMerge(func, func->entry_block, func->exit_block);
        irFunctionDelinkBlock(func, func->exit_block);
    }

    uniqListRelease(work_queue);
    intSetRelease(blocks_to_delete);
}

IrFunction *irLowerFunction(IrCtx *ctx, IrProgram *program, Ast *ast_function) {
    IrFunction *ir_function = irFunctionNew(ast_function->fname);
    IrBlock *ir_entry_block = irBlockNew(irBlockId());
    IrBlock *ir_exit_block = irBlockNew(irBlockId());

    ir_function->program = program;

    if (ast_function->type->has_var_args) {
        ir_function->has_var_args = 1;
    }

    IrInstr *ir_return_space = irAlloca(ir_entry_block,
                                        ast_function->type->rettype);
    IrValue *ir_return_var = irGetAllocaVar(ir_return_space);
    IrBlockMapping *ir_exit_block_mapping = irBlockMappingNew(ir_exit_block->id);
    intMapAdd(ir_function->cfg, ir_exit_block_mapping->id, ir_exit_block_mapping);

    /* I'm not sure if this is needed */
    ir_function->return_value = ir_return_var;

    Ast *ast_var_args = NULL;
    for (int i = 0; i < ast_function->params->size; ++i) {
        Ast *ast_param = vecGet(Ast *,ast_function->params,i);

        if (ast_param->kind == AST_VAR_ARGS) {
            assert(ir_function->has_var_args);
            ast_var_args = ast_param;
            break;
        }

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
        } else if (ast_param->kind == AST_DEFAULT_PARAM) {
            key = ast_param->declvar->tmp_var_name;
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

    if (ir_function->has_var_args) {
        assert(ast_var_args != NULL);
        if (ast_var_args) {
            // Create argc local variable
            IrInstr *argc_alloca = irAlloca(ir_entry_block, ast_var_args->argc->type);
            IrValue *argc_var = irGetAllocaVar(argc_alloca);
            argc_var->name = ast_var_args->argc->tmp_var_name;
            strMapAddAoStr(ir_function->variables, argc_var->name, argc_var);
            
            // Create a special parameter for argc
            IrValue *argc_param = irValueNew(irConvertType(ast_var_args->argc->type), IR_VALUE_PARAM);
            argc_param->name = aoStrPrintf("argc");
            ptrVecPush(ir_function->params, argc_param);
            
            // Store the parameter value to the local
            irStore(ir_entry_block, argc_var, argc_param);
            
            // Create argv local variable
            IrInstr *argv_alloca = irAlloca(ir_entry_block, ast_var_args->argv->type);
            IrValue *argv_var = irGetAllocaVar(argv_alloca);
            argv_var->name = ast_var_args->argv->tmp_var_name;
            strMapAddAoStr(ir_function->variables, argv_var->name, argv_var);
            
            // Create a special parameter for argv
            IrValue *argv_param = irValueNew(irConvertType(ast_var_args->argv->type), IR_VALUE_PARAM);
            argv_param->name = aoStrPrintf("argv");
            ptrVecPush(ir_function->params, argv_param);
            
            // Store the parameter value to the local
            irStore(ir_entry_block, argv_var, argv_param);
        }
    }

    ir_function->entry_block = ir_entry_block;
    ir_function->exit_block = ir_exit_block;
    
    irCtxSetCurrentBlock(ctx, ir_entry_block);

    /* The function now has everything lowered to IR */
    irStatement(ctx, ir_function, ast_function->body);

    irFunctionAddBlock(ir_function, ir_exit_block);
    irRet(ir_function->exit_block, ir_function->return_value);

    if (ctx->unresolved_gotos->size) {
        irResolveGotos(ctx, ir_function);
    }

    /* @Broken */
    printf("irSimplifyBlocks(%s);\n", ir_function->name->data);
    irSimplifyBlocks(ir_function);

    return ir_function;
}

void irFlattenGlobalAstArray(IrCtx *ctx, Ast *ast);

IrValue *irGlobalExpression(IrCtx *ctx, Ast *ast) {
    if (!ast) return NULL;

    // Handle the allowed types of global initializers
    switch (ast->kind) {
        case AST_LITERAL: {
            IrValueType type = irConvertType(ast->type);
            IrValue *value = irValueNew(type, type == IR_TYPE_F64 ? IR_VALUE_CONST_FLOAT : IR_VALUE_CONST_INT);
            
            if (type == IR_TYPE_F64) {
                value->f64 = ast->f64;
            } else {
                value->i64 = ast->i64;
            }
            return value;
        }
        
        case AST_STRING: {
            IrValue *value = irValueNew(IR_TYPE_ARRAY, IR_VALUE_CONST_STR);
            value->str = ast->sval;
            value->str_real_len = ast->real_len;
            return value;
        }

        /**
         * @Error AST_FUN_PROTO
         * This should perhaps be an error as it implies that the function 
         * prototype has not been bound to a function definition.
         */
        case AST_FUN_PROTO:
        case AST_FUNC:
        case AST_EXTERN_FUNC: {
            IrValue *value = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
            value->name = ast->fname;
            return value;
        }

        case AST_ASM_FUNCDEF:
        case AST_ASM_FUNC_BIND: {
            IrValue *value = irValueNew(IR_TYPE_FUNCTION, IR_VALUE_GLOBAL);
            value->name = ast->asmfname;
            return value;
        }

        case AST_ARRAY_INIT: {
            // This case should be handled by the calling function (irFlattenAstArray)
            // since it needs to maintain array nesting context
            irFlattenGlobalAstArray(ctx,ast);
            return NULL;
        }
        
        case AST_ADDR: {
            // Handle address-of operator for globals
            if (ast->operand->kind == AST_GVAR) {
                IrValue *value = irValueNew(IR_TYPE_PTR, IR_VALUE_GLOBAL);
                value->name = ast->operand->gname;
                return value;
            }
            loggerPanic("Cannot handle address for; %s\n", astToString(ast));
        }

        default:
            loggerPanic("Error: Non-constant expression in global initializer\n");
    }
}

void irFlattenGlobalAstArray(IrCtx *ctx, Ast *ast) {
    // Base case: if this isn't an array initialization, just add the value
    if (ast->kind != AST_ARRAY_INIT) {
        IrValue *ir_value = irGlobalExpression(ctx, ast);
        if (ir_value) {
            ctx->array_.type = ir_value->type;
            ptrVecPush(ctx->array_.init, ir_value);
        }
        return;
    }

    int dimension_size = ast->arrayinit->size;
    
    // If this is the first nesting level, initialize the tracking info
    if (ctx->array_.nesting == 0) {
        ctx->array_.length_per_array = dimension_size;
    }
    
    // Increment nesting for recursive calls
    ctx->array_.nesting++;
    
    // Process each element in the array
    for (int i = 0; i < dimension_size; ++i) {
        Ast *array_element = (Ast *)ast->arrayinit->entries[i];
        
        // Recursively process nested arrays
        if (array_element->kind == AST_ARRAY_INIT) {
            irFlattenGlobalAstArray(ctx, array_element);
        } else {
            // Convert and add the element
            IrValue *ir_value = irGlobalExpression(ctx, array_element);
            if (ir_value) {
                ctx->array_.type = ir_value->type;
                ptrVecPush(ctx->array_.init, ir_value);
            }
        }
    }
    
    // Decrement nesting as we return from recursion
    ctx->array_.nesting--;
}

IrValue *irLowerGlobal(IrCtx *ctx, Ast *global_variable) {
    Ast *global_decl = global_variable->declvar;
    Ast *global_init = global_variable->declinit;
    IrValueType type = irConvertType(global_decl->type);
    IrValue *value = irValueNew(type, IR_VALUE_GLOBAL);
    value->name = global_decl->gname;

    /* At this point in time in the compiler we know this is either going to 
     * be an integer, float or string. This means we really want global 
     * initialisers to be a Vector of some sort? */
    if (global_init) {
        /* As the parser is guaranteed to give us a literal, either string, int 
         * or float is trivial to create a literal value. */
        IrValue *ir_init = NULL;
        
        // Handle different types of initializers
        if (global_init->kind == AST_STRING) {
            ir_init = irGlobalExpression(ctx, global_init);
            // Add to program's string table if needed
            if (ir_init) {
                strMapAddAoStr(ctx->ir_program->strings, value->name, ir_init);
            }
        } else if (global_init->kind == AST_LITERAL) {
            ir_init = irGlobalExpression(ctx, global_init);
        } else if (global_decl->type->kind == AST_TYPE_ARRAY) {
            // Reset array context for new array processing
            ctx->array_.nesting = 0;
            ctx->array_.length_per_array = 0;
            ctx->array_.init = ptrVecNew();

            irFlattenGlobalAstArray(ctx, global_init);
            
            ir_init = irValueNew(IR_TYPE_ARRAY_INIT, IR_VALUE_GLOBAL);
            ir_init->array_.label = value->name;
            ir_init->array_.nesting = ctx->array_.nesting;
            ir_init->array_.length_per_array = ctx->array_.length_per_array;
            ir_init->array_.values = ctx->array_.init;

            // Add to program's array table
            strMapAddAoStr(ctx->ir_program->arrays, value->name, ir_init);
        } else {
            loggerWarning("Trying to create global IR for : %s\n",
                    astToString(global_init));
            // Try to handle any other type of constant expression
            ir_init = irGlobalExpression(ctx, global_init);
        }

        if (ir_init) {
            value->global.value = ir_init;
        }
    }

    // Always add the global variable definition
    return value;
}

void irLowerAst(Cctrl *cc) {
    IrProgram *ir_program = irProgramNew();
    IrCtx *ctx = irCtxNew();
    ctx->ir_program = ir_program;

    /* Handle asm blocks */
    if (!listEmpty(cc->asm_blocks)) {
        listForEach(cc->asm_blocks) {
            /*
             * this is one:
             *
             * ```
             * asm {
             * _FUNC1::
             *
             * _FUNC2::
             * 
             * }
             * ```
             * Thus has potentially many functions.
             * */
            Ast *asm_block = listValue(Ast *, it);
            List *func_it = asm_block->funcs->next;

            /* For each function in the block, create and save the assembly function
             * but in IR. This is essentially a cut and paste, we provide no 
             * safe guards what so ever. */
            while (func_it != asm_block->funcs) {
                IrValue *ir_asm_func = irValueNew(IR_TYPE_ASM_FUNCTION, IR_VALUE_GLOBAL);
                Ast *asm_func = (Ast *)func_it->value;
                ir_asm_func->name = asm_func->asmfname;
                ir_asm_func->str = asm_func->body->asm_stmt;

                ptrVecPush(ir_program->asm_functions, ir_asm_func);
                func_it = func_it->next;
            }
        }
    }

    listForEach(cc->ast_list) {
        Ast *ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            irCtxReset(ctx);
            IrFunction *ir_func = irLowerFunction(ctx, ir_program, ast);
            ptrVecPush(ir_program->functions, ir_func);

        } else if (ast->kind == AST_DECL) { 
            IrValue *global_value = irLowerGlobal(ctx, ast);
            strMapAddAoStr(ctx->ir_program->global_variables, global_value->name, global_value);
        } else if (ast->kind == AST_ASM_FUNC_BIND) {

        } else {
       //     loggerWarning("Cannot lower AST to IR: %s\n",
       //             astToString(ast));
        }
    }

    printf("target = %s-%s\n", ARCH_STR, OS_STR);
    if (ctx->ir_program->global_variables->size) {
        StrMapIterator *it = strMapIteratorNew(ctx->ir_program->global_variables);
        StrMapNode *n = NULL;
        while ((n = strMapNext(it)) != NULL) {
            IrValue *ir_value = (IrValue *)n->value;
            aoStr *ir_value_str = irValueToString(ir_value);
            printf("%s\n",ir_value_str->data);
            aoStrRelease(ir_value_str);
        }
        strMapIteratorRelease(it);
        printf("\n");
    }

    if (ir_program->asm_functions->size > 0) {
        for (int i = 0; i < ir_program->asm_functions->size; ++i) {
            IrValue *asm_function = vecGet(IrValue *, ir_program->asm_functions, i);
            const char *type_str = irValueTypeToString(asm_function->type);
            printf("%s fn %s\n",type_str, asm_function->name->data);
        }
        printf("\n");
    }

    for (int i = 0; i < ir_program->functions->size; ++i) {
        IrFunction *ir_func = vecGet(IrFunction *, ir_program->functions, i);
        aoStr *ir_func_str = irFunctionToString(ir_func);
        printf("%s\n",ir_func_str->data);
        aoStr *ir_func_cfg = irFunctionCFGToString(ir_func);
        printf("%s\n",ir_func_cfg->data);
    }

    irCtxRelease(ctx);
}
