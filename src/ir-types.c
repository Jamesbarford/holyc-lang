#include <assert.h>

#include "aostr.h"
#include "arena.h"
#include "map.h"
#include "list.h"
#include "util.h"
#include "ir-types.h"
#include "ir-interp.h"

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

void *irArenaAlloc(unsigned int size) {
    return arenaAlloc(&ir_memory_arena,size);
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

const char *irValueKindToString(IrValueKind ir_value_kind) {
    const char *kind_str = NULL;
    switch (ir_value_kind) {
        case IR_VALUE_CONST_INT: kind_str = "IR_VALUE_CONST_INT"; break;
        case IR_VALUE_CONST_FLOAT: kind_str = "IR_VALUE_CONST_FLOAT"; break;
        case IR_VALUE_CONST_STR: kind_str = "IR_VALUE_CONST_STR"; break;
        case IR_VALUE_GLOBAL: kind_str = "IR_VALUE_GLOBAL"; break;
        case IR_VALUE_PARAM: kind_str = "IR_VALUE_PARAM"; break;
        case IR_VALUE_LOCAL: kind_str = "IR_VALUE_LOCAL"; break;
        case IR_VALUE_TEMP: kind_str = "IR_VALUE_TEMP"; break;
        case IR_VALUE_PHI: kind_str = "IR_VALUE_PHI"; break;
        case IR_VALUE_LABEL: kind_str = "IR_VALUE_LABEL"; break;
        case IR_VALUE_UNDEFINED: kind_str = "IR_VALUE_UNDEFINED"; break;
        case IR_VALUE_UNRESOLVED: kind_str = "IR_VALUE_UNRESOLVED"; break;
        default: loggerPanic("Unhandled IrValueKind: %d\n", ir_value_kind);
    }
    return kind_str;
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

            if (ir_instr->result) {
                aoStr *ir_ret_var = irValueToString(ir_instr->result);
                aoStrCatFmt(buf, "%s %S, %S", op, ir_ret_var, ir_instr->op1->name);
                aoStrRelease(ir_ret_var);
            } else {
                aoStrCatFmt(buf, "%s %S", op, ir_instr->op1->name);
            }
            break;
        }

        case IR_OP_BR: {
            aoStr *ir_value_str = irValueToString(ir_instr->result);
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
            aoStr *ir_value_str = irValueToString(ir_instr->result);
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

    Map *successors = irFunctionGetSuccessors(func, ir_block);
    Map *predecessors = irFunctionGetPredecessors(func, ir_block);

    if (is_terminal) {
        aoStrCatFmt(buf, ESC_BOLD"  bb%i"ESC_CLEAR_BOLD" -> ", ir_block->id);
    } else {
        aoStrCatFmt(buf, "  bb%i -> ", ir_block->id);
    }

    if (predecessors) {
        aoStr *predecessors_str = mapKeysToString(predecessors);
        aoStrCatFmt(buf, "predecessors: %S  ", predecessors_str);
        aoStrRelease(predecessors_str);
    } else {
        aoStrCatFmt(buf, "predecessors: {}  ");
    }

    if (successors) {
        aoStr *successors_str = mapKeysToString(successors);
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

static aoStr *irBlockToStringSimplified(IrBlock *ir_block) {
    aoStr *buf = aoStrNew();
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


/* This makes no sense to use in the wild as it is specifically formatted to be
 * used with the `mapToString(...)` function. That isn't to say this cannot
 * be used... Just that it will look a bit our of place */
aoStr *irBlockMappingToStringCallback(void *_ir_block_mapping) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)_ir_block_mapping;
    aoStr *buf = aoStrNew();
    aoStr *successor_str = mapKeysToString(ir_block_mapping->successors);
    aoStr *predecessor_str = mapKeysToString(ir_block_mapping->predecessors);
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
    aoStr *map_str = mapToString(func->cfg, ",\n");
    aoStrCatFmt(buf, "CFG {\n%S\n}",map_str);
    return buf;
}

/*==================== DATA STRUCTURE SPECIALISATION =========================*/

unsigned long irValueHash(IrValue *value) {
   // assert(value->name);
    return aoStrHashFunction(value->name);
}

long irValueKeyLen(IrValue *value) {
    assert(value->name);
    return (long)value->name->len;
}

aoStr *irValueKeyStringify(IrValue *value) {
    return value->name;
}

/* `Set<IrValue *>` */
static SetType ir_value_set = {
    .match = (setValueMatch *)irValuesEq,
    .stringify = (setValueToString *)irValueKeyStringify,
    .hash = (setValueHash *)irValueHash,
    .value_release = NULL, /* The set does not own the memory */
    .type = "IrValue *",
};

/* `Set<IrValue *>` */
Set *irValueSetNew(void) {
    return setNew(16, &ir_value_set);
}

/* create a `Map<aoStr *, IrValue *>` */
static MapType str_irvalue_map_type = {
    .match = (mapKeyMatch *)aoStrEq,
    .hash = (mapKeyHash *)aoStrHashFunction,
    .get_key_len = (mapKeyLen *)aoStrGetLen,
    .key_to_string = (mapKeyToString *)aoStrIdentity,
    .value_to_string = (mapValueToString *)&irValueToString,
    .value_release = NULL,
    .value_type = "IrValue *",
    .key_type = "aoStr *",
};

/* create a `Map<long, IrValue *>` */
static MapType int_irvalue_map_type = {
    .match = (mapKeyMatch *)intMapKeyMatch,
    .hash = (mapKeyHash *)intMapKeyHash,
    .get_key_len = (mapKeyLen *)intMapKeyLen,
    .key_to_string = (mapKeyToString *)intMapKeyToString,
    .value_to_string = (mapValueToString *)&irValueToString,
    .value_release = NULL,
    .value_type = "IrValue *",
    .key_type = "long",
};

/* create a `Map<long, IrBlock *>` */
static MapType int_irblock_map_type = {
    .match = (mapKeyMatch *)intMapKeyMatch,
    .hash = (mapKeyHash *)intMapKeyHash,
    .get_key_len = (mapKeyLen *)intMapKeyLen,
    .key_to_string = (mapKeyToString *)intMapKeyToString,
    .value_to_string = (mapValueToString *)&irBlockToStringSimplified,
    .value_release = NULL,
    .value_type = "IrBlock *",
    .key_type = "long",
};

/* create a `Map<aoStr *, IrInstr *>` */
static MapType str_irinstr_map_type = {
    .match = (mapKeyMatch *)aoStrEq,
    .hash = (mapKeyHash *)aoStrHashFunction,
    .get_key_len = (mapKeyLen *)aoStrGetLen,
    .key_to_string = (mapKeyToString *)aoStrIdentity,
    .value_to_string = (mapValueToString *)&irInstrToString,
    .value_release = NULL,
    .value_type = "IrInstr *",
    .key_type = "long",
};

/* create a `Map<long, IrBlock *>` */
static MapType int_blockmapping_map_type = {
    .match = (mapKeyMatch *)intMapKeyMatch,
    .hash = (mapKeyHash *)intMapKeyHash,
    .get_key_len = (mapKeyLen *)intMapKeyLen,
    .key_to_string = (mapKeyToString *)intMapKeyToString,
    .value_to_string = (mapValueToString *)&irBlockMappingToStringCallback,
    .value_release = NULL,
    .value_type = "IrBlockMapping *",
    .key_type = "long",
};

/* create a `Map<aoStr *, IrValue *>` */
Map *irStrValueMapNew(void) {
    return mapNew(32, &str_irvalue_map_type);
}

Map *irStrInstrMapNew(void) {
    return mapNew(32, &str_irinstr_map_type);
}

/* create a `Map<long, IrValue *>` */
Map *irIntValueMapNew(void) {
    return mapNew(32, &int_irvalue_map_type);
}

/* create a `Map<long, IrBlock*>` */
Map *irIntBlockMapNew(void) {
    return mapNew(8, &int_irblock_map_type);
}

Map *irIntBlockMappingMapNew(void) {
    return mapNew(16, &int_blockmapping_map_type);
}


/*==================== TYPE CONSTRUCTORS =====================================*/
IrBlockMapping *irBlockMappingNew(int id) {
    IrBlockMapping *mapping = (IrBlockMapping *)irArenaAlloc(sizeof(IrBlockMapping));
    mapping->id = id;
    mapping->successors = irIntBlockMapNew();
    mapping->predecessors = irIntBlockMapNew();
    return mapping;
}

IrProgram *irProgramNew(void) {
    IrProgram *program = (IrProgram *)irArenaAlloc(sizeof(IrProgram));
    program->functions = ptrVecNew();
    program->asm_functions = ptrVecNew();
    program->global_variables = irStrValueMapNew();
    program->strings = strMapNew(16);
    program->arrays = irStrValueMapNew();
    program->floats = strMapNew(16);
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

/* This is very easy to inline */
IrBlockMapping *irFunctionGetBlockMapping(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)mapGet(func->cfg,
                                                                   ir_block->id);
    return ir_block_mapping;
}

Map *irFunctionGetSuccessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->successors;
    }
    return NULL;
}

Map *irFunctionGetPredecessors(IrFunction *func, IrBlock *ir_block) {
    IrBlockMapping *ir_block_mapping = irFunctionGetBlockMapping(func, ir_block);
    if (ir_block_mapping) {
        return ir_block_mapping->predecessors;
    }
    return NULL;
}
