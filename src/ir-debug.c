#include <stdio.h>
#include <stdlib.h>

#include "aostr.h"
#include "ir-types.h"
#include "list.h"
#include "util.h"

AoStr *debugColourInt(long value) {
    if (is_terminal) {
        return aoStrPrintf(ESC_YELLOW"%ld"ESC_RESET, value);
    } else {
        return aoStrPrintf("%ld", value);
    }
}

AoStr *debugColourIntAsPtr(void *value) {
    return debugColourInt((long)value);
}

AoStr *debugColourFloat(double value) {
    if (is_terminal) {
        return aoStrPrintf(ESC_YELLOW"%f"ESC_RESET, value);
    } else {
        return aoStrPrintf("%f", value);
    }
}

AoStr *debugColourAoStr(AoStr *str) {
    if (is_terminal) {
        return aoStrPrintf(ESC_GREEN"\"%s\""ESC_RESET, str->data);
    } else {
        return aoStrPrintf("\"%s\"", str->data);
    }
}

void debugVectorAoStrStringify(AoStr *buf, AoStr *value) {
    if (is_terminal) {
        aoStrCatFmt(buf, ESC_GREEN"\"%S\""ESC_RESET, value);
    } else {
        aoStrCatFmt(buf, "\"%S\"", value);
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
        case IR_VAL_CONST_INT: kind_str = "IR_VAL_CONST_INT"; break;
        case IR_VAL_CONST_FLOAT: kind_str = "IR_VAL_CONST_FLOAT"; break;
        case IR_VAL_CONST_STR: kind_str = "IR_VAL_CONST_STR"; break;
        case IR_VAL_GLOBAL: kind_str = "IR_VAL_GLOBAL"; break;
        case IR_VAL_PARAM: kind_str = "IR_VAL_PARAM"; break;
        case IR_VAL_LOCAL: kind_str = "IR_VAL_LOCAL"; break;
        case IR_VAL_TMP: kind_str = "IR_VAL_TMP"; break;
        case IR_VAL_PHI: kind_str = "IR_VAL_PHI"; break;
        case IR_VAL_LABEL: kind_str = "IR_VAL_LABEL"; break;
        case IR_VAL_UNDEFINED: kind_str = "IR_VAL_UNDEFINED"; break;
        case IR_VAL_UNRESOLVED: kind_str = "IR_VAL_UNRESOLVED"; break;
        default: loggerPanic("Unhandled IrValueKind: %d\n", ir_value_kind);
    }
    return kind_str;
}

const char *irValueKindToPrettyString(IrValueKind ir_value_kind) {
    const char *kind_str = NULL;
    switch (ir_value_kind) {
        case IR_VAL_CONST_INT: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"const int"ESC_RESET;
            } else {
                kind_str = "const int";
            }
            break;
        }

        case IR_VAL_CONST_FLOAT: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"const float"ESC_RESET;
            } else {
                kind_str = "const float";
            }
            break;
        }

        case IR_VAL_CONST_STR: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"const str"ESC_RESET;
            } else {
                kind_str = "const str";
            }
            break;
        }

        case IR_VAL_GLOBAL: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"global"ESC_RESET;
            } else {
                kind_str = "global";
            }
            break;
        }

        case IR_VAL_PARAM: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"param"ESC_RESET;
            } else {
                kind_str = "param";
            }
            break;
        }

        case IR_VAL_LOCAL: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"local"ESC_RESET;
            } else {
                kind_str = "local";
            }
            break;
        }

        case IR_VAL_TMP: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"tmp"ESC_RESET;
            } else {
                kind_str = "tmp";
            }
            break;
        }

        case IR_VAL_PHI: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"phi"ESC_RESET;
            } else {
                kind_str = "phi";
            }
            break;
        }

        case IR_VAL_LABEL: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"label"ESC_RESET;
            } else {
                kind_str = "label";
            }
            break;
        }

        case IR_VAL_UNDEFINED: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"undefined"ESC_RESET;
            } else {
                kind_str = "undefined";
            }
            break;
        }

        case IR_VAL_UNRESOLVED: {
            if (is_terminal) {
                kind_str = ESC_PURPLE"unresolved"ESC_RESET;
            } else {
                kind_str = "unresolved";
            }
            break;
        }
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
void irArrayInitToString(AoStr *buf, IrValue *ir_value) {
    aoStrCatFmt(buf,"[%i x ", ir_value->as.array.values->size);
    if (ir_value->as.array.nesting == 1) {
        IrValue *head = (IrValue *)ir_value->as.array.values->entries[0];
        aoStrCatFmt(buf,"%s]", irValueTypeToString(head->type));
    } else {
        Vec *ir_array = ir_value->as.array.values;
        IrValue *actual_value = NULL;
        int bracket_count = 0;
        for (u64 i = 0; i < ir_array->size; ++i) {
            IrValue *tmp = (IrValue *)ir_value->as.array.values->entries[i];
            if (tmp->type == IR_TYPE_ARRAY_INIT) {
                ir_array = tmp->as.array.values;
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

AoStr *irValueToString(IrValue *ir_value) {
    AoStr *buf = aoStrNew();
    AoStr *flags_str = NULL;
    
    if (ir_value->flags != 0) {
        flags_str = aoStrPrintf(ESC_BLUE"0b"ESC_RESET);
        for (int i = 4; i >= 0; --i) {
            int bit = (ir_value->flags >> i) & 1;
            aoStrCatFmt(flags_str, ESC_BLUE"%i"ESC_RESET, bit);
        }
    }

    if (ir_value->type == IR_TYPE_ARRAY_INIT) {
        irArrayInitToString(buf,ir_value);
    } else {
        switch (ir_value->kind) {
            case IR_VAL_CONST_INT: {
                AoStr *colour_int = debugColourInt(ir_value->as._i64);
                aoStrCatAoStr(buf, colour_int);
                aoStrRelease(colour_int);
                break;
            }

            case IR_VAL_CONST_FLOAT: {
                AoStr *colour_float = debugColourFloat(ir_value->as._f64);
                aoStrCatAoStr(buf, colour_float);
                aoStrRelease(colour_float);
                break;
            }

            case IR_VAL_CONST_STR: {
                AoStr *colour_str = debugColourAoStr(ir_value->as.str.str);
                aoStrCatAoStr(buf, colour_str);
                aoStrRelease(colour_str);
                break;
            }

            case IR_VAL_GLOBAL: {
                if (is_terminal) {
                    aoStrCatFmt(buf, ESC_WHITE"global "ESC_RESET"%S", ir_value->as.global.name);
                } else {
                    aoStrCatFmt(buf, "global %S", ir_value->as.global.name);
                }

                if (ir_value->as.global.value) {
                    AoStr *init_buf = irValueToString(ir_value->as.global.value);
                    aoStrCatFmt(buf, " %S ", init_buf);
                    aoStrRelease(init_buf);
                }
                break;
            }
            /* @TODO: I am not sure these are correct */
            case IR_VAL_PARAM:       aoStrCatFmt(buf, "%S", ir_value->as.name); break;
            case IR_VAL_LOCAL:       aoStrCatFmt(buf, "%S", ir_value->as.name); break;
            case IR_VAL_LABEL:       aoStrCatFmt(buf, "%S", ir_value->as.name); break;

            case IR_VAL_TMP:         aoStrCatFmt(buf, "%%t%u", ir_value->as.var); break;
            case IR_VAL_PHI:         aoStrCatFmt(buf, "phi"); break;
            case IR_VAL_UNDEFINED:   aoStrCatFmt(buf, "undefined"); break;
            case IR_VAL_UNRESOLVED:  aoStrCatFmt(buf, "unresolved"); break;
            default: loggerPanic("Unhandled IrValueKind: %d\n", ir_value->kind);
        }

        const char *ir_value_kind_str = irValueKindToPrettyString(ir_value->kind);
        const char *ir_value_type_str = irValueTypeToString(ir_value->type);
        if (flags_str) {
            aoStrCatFmt(buf," %s %S %s",ir_value_type_str, flags_str, ir_value_kind_str);
            aoStrRelease(flags_str);
        } else {
            aoStrCatFmt(buf," %s %s",ir_value_type_str, ir_value_kind_str);
        }

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
    IrOp opcode = ir_instr->op;
    const char *ir_op_str;
    switch (opcode) {
        case IR_NOP:      ir_op_str = "nop"; break;
        case IR_ALLOCA:   ir_op_str = "alloca"; break;
        case IR_LOAD:     ir_op_str = "load"; break;
        case IR_STORE:    ir_op_str = "store"; break;
        case IR_GEP:      ir_op_str = "gep"; break;

        case IR_IADD:      ir_op_str = "iadd"; break;
        case IR_ISUB:      ir_op_str = "isub"; break;
        case IR_IMUL:      ir_op_str = "imul"; break;
        case IR_IDIV:      ir_op_str = "idiv"; break;
        case IR_UDIV:      ir_op_str = "udiv"; break;
        case IR_IREM:      ir_op_str = "irem"; break;
        case IR_UREM:      ir_op_str = "urem"; break;
        case IR_INEG:      ir_op_str = "ineg"; break;

        case IR_FADD:      ir_op_str = "fadd"; break;
        case IR_FSUB:      ir_op_str = "fsub"; break;
        case IR_FMUL:      ir_op_str = "fmul"; break;
        case IR_FDIV:      ir_op_str = "fdiv"; break;
        case IR_FNEG:      ir_op_str = "fneg"; break;

        case IR_AND:      ir_op_str = "and"; break;
        case IR_OR:       ir_op_str = "or"; break;
        case IR_XOR:      ir_op_str = "xor"; break;
        case IR_SHL:      ir_op_str = "shl"; break;
        case IR_SHR:      ir_op_str = "shr"; break;
        case IR_SAR:      ir_op_str = "sar"; break;
        case IR_NOT:      ir_op_str = "not"; break;
        case IR_ICMP:
        case IR_FCMP:
            return irCmpKindToString(ir_instr->extra.cmp_kind);
        case IR_TRUNC:    ir_op_str = "trunc"; /* Int to smaller int i.e i32 -> i16 */ break;
        case IR_ZEXT:     ir_op_str = "zext"; /* zero extend */ break;
        case IR_SEXT:     ir_op_str = "sext"; /* sign extend */ break;
        case IR_FPTRUNC:  ir_op_str = "fptrunc"; /* float truncate f32 -> f64 */ break;
        case IR_FPEXT:    ir_op_str = "fpext";   /* f32 -> f64 */ break;
        case IR_FPTOUI:   ir_op_str = "fptoui"; break;
        case IR_FPTOSI:   ir_op_str = "fptosi"; break;
        case IR_UITOFP:   ir_op_str = "uitofp"; break;
        case IR_SITOFP:   ir_op_str = "sitofp"; break;
        case IR_PTRTOINT: ir_op_str = "ptrtoint"; break;
        case IR_INTTOPTR: ir_op_str = "inttoptr"; break;
        case IR_BITCAST:  ir_op_str = "bitcast"; break;
        case IR_RET:      ir_op_str = "ret"; break;
        case IR_BR:       ir_op_str = "br"; break;
        case IR_JMP:      ir_op_str = "jmp"; break;
        case IR_LOOP:     ir_op_str = "loop"; break;
        case IR_SWITCH:   ir_op_str = "switch"; break;
        case IR_CALL:     ir_op_str = "call"; break;
        case IR_PHI:      ir_op_str = "phi"; break;
        case IR_LABEL:    ir_op_str = "label"; break;
        case IR_SELECT:   ir_op_str = "select"; break;
        case IR_VA_ARG:   ir_op_str = "va_arg"; break;
        case IR_VA_START: ir_op_str = "va_start"; break;
        case IR_VA_END:   ir_op_str = "va_end"; break;
        default: loggerPanic("Unhandled opcode: %d\n", opcode);
    }
    if (is_terminal) {
        return mprintf(ESC_BLUE"%-8s"ESC_RESET,ir_op_str);
    } else {
        return mprintf("%-8s",ir_op_str);
    }
}

void irPairToString(AoStr *buf, IrPair *ir_phi_pair) {
    AoStr *ir_value_str = irValueToString(ir_phi_pair->ir_value);
    /* <Value, block> */
    aoStrCatFmt(buf, "[ %S, bb%i ]", ir_value_str, ir_phi_pair->ir_block->id);
}

/* Convert an instruction to a string. */
AoStr *irInstrToString(IrInstr *ir_instr) {
    IrValue *ir_values[4] = {
        ir_instr->dst,
        ir_instr->r1,
        ir_instr->r2,
    };
    AoStr *buf = aoStrNew();
    const char *op = irOpcodeToString(ir_instr);

    switch (ir_instr->op) {
        case IR_CALL: {
            Vec *fn_args = ir_instr->r1->as.array.values;
            if (fn_args && fn_args->size > 0) {
                for (u64 i = 0; i < fn_args->size; ++i) {
                    IrValue *ir_value = vecGet(IrValue *,fn_args,i);
                    AoStr *ir_value_str = irValueToString(ir_value);
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

            if (ir_instr->dst) {
                AoStr *ir_ret_var = irValueToString(ir_instr->dst);
                aoStrCatFmt(buf, "%s %S, %S", op, ir_ret_var, ir_instr->r1->as.name);
                aoStrRelease(ir_ret_var);
            } else {
                aoStrCatFmt(buf, "%s %S", op, ir_instr->r1->as.name);
            }
            break;
        }

        case IR_BR: {
            AoStr *ir_value_str = irValueToString(ir_instr->dst);
            aoStrCatFmt(buf, "%s %S, bb%i, bb%i",
                        op,
                        ir_value_str,
                        ir_instr->extra.blocks.target_block->id,
                        ir_instr->extra.blocks.fallthrough_block->id);
            aoStrRelease(ir_value_str);
            break;
        }

        case IR_LOOP:
        case IR_JMP: {
            /* This is for unresolved gotos */
            if (ir_instr->extra.blocks.target_block == NULL) {
                aoStrCatFmt(buf, "%s ???", op);
            } else {
                aoStrCatFmt(buf, "%s bb%I", op, ir_instr->extra.blocks.target_block->id);
            }
            break;
        }

        case IR_PHI: {
            AoStr *phi_pairs_str = aoStrNew();
            for (u64 i = 0; i < ir_instr->extra.phi_pairs->size; ++i) {
                IrPair *ir_phi_pair = vecGet(IrPair *,
                                                ir_instr->extra.phi_pairs, i);
                irPairToString(phi_pairs_str, ir_phi_pair);
                if (i + 1 != ir_instr->extra.phi_pairs->size) {
                    aoStrPutChar(phi_pairs_str, ' ');;
                }
            }
            aoStrCatFmt(buf,"%s %S",op, phi_pairs_str);
            aoStrRelease(phi_pairs_str);
            break;
        }

        case IR_SWITCH: {
            AoStr *pairs_str = aoStrNew();
            AoStr *ir_value_str = irValueToString(ir_instr->dst);
            aoStrCatFmt(buf, "%s %S, label bb%i [\n",
                        op,
                        ir_value_str,
                        ir_instr->extra.blocks.target_block->id);

            listForEach(ir_instr->extra.cases) {
                IrPair *ir_pair = (IrPair *)it->value;
                AoStr *ir_value_str = irValueToString(ir_pair->ir_value);
                aoStrCatFmt(buf, "      %S, bb%i\n", ir_value_str, ir_pair->ir_block->id);
            }
            aoStrCatLen(buf, str_lit("    ]"));

            aoStrRelease(ir_value_str);
            aoStrRelease(pairs_str);
            break;
        }

        case IR_RET: {
            if (ir_instr->dst) {
                AoStr *ir_value_str = irValueToString(ir_instr->dst);
                aoStrCatFmt(buf, "%s %S", op, ir_value_str);
                aoStrRelease(ir_value_str);
            } else {
                aoStrCatFmt(buf, "%s", op);
            }
            break;
        }

        default: {
            aoStrCatFmt(buf, "%s", op);
            for (int i = 0; i < (int)static_size(ir_values); i++) {
                IrValue *ir_value = ir_values[i];
                if (!ir_value) continue;
                AoStr *ir_value_str = irValueToString(ir_value);
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
AoStr *irBlockToString(IrFunction *func, IrBlock *ir_block) {
    AoStr *buf = aoStrNew();

    Map *successors = irFunctionGetSuccessors(func, ir_block);
    Map *predecessors = irFunctionGetPredecessors(func, ir_block);

    if (is_terminal) {
        aoStrCatFmt(buf, ESC_BOLD"  bb%i"ESC_CLEAR_BOLD" -> ", ir_block->id);
    } else {
        aoStrCatFmt(buf, "  bb%i -> ", ir_block->id);
    }

    if (predecessors) {
        AoStr *predecessors_str = mapKeysToString(predecessors);
        aoStrCatFmt(buf, "predecessors: %S  ", predecessors_str);
        aoStrRelease(predecessors_str);
    } else {
        aoStrCatFmt(buf, "predecessors: {}  ");
    }

    if (successors) {
        AoStr *successors_str = mapKeysToString(successors);
        aoStrCatFmt(buf, "successors: %S", successors_str);
        aoStrRelease(successors_str);
    } else {
        aoStrCatFmt(buf, "successors: {}");
    }

    aoStrPutChar(buf, '\n');
    if (!listEmpty(ir_block->instructions)) {
        listForEach(ir_block->instructions) {
            IrInstr *ir_instr = (IrInstr *)it->value;
            AoStr *ir_instr_str = irInstrToString(ir_instr);
            aoStrCatFmt(buf, "    %S\n", ir_instr_str);
            aoStrRelease(ir_instr_str);
        }
    }

    return buf;
}

AoStr *irParamsToString(Vec *ir_value_vector) {
    AoStr *buf = aoStrNew();
    if (ir_value_vector->size != 0) {
        for (u64 i = 0; i < ir_value_vector->size; ++i) {
            IrValue *ir_value = vecGet(IrValue *, ir_value_vector, i);
            AoStr *ir_value_str = irValueToString(ir_value);
            aoStrCatFmt(buf,"%S",ir_value_str);
            if (i + 1 != ir_value_vector->size) {
                aoStrCatLen(buf,str_lit(", "));
            }
            aoStrRelease(ir_value_str);
        }
    }
    return buf;
}

AoStr *irFunctionToString(IrFunction *ir_func) {
    AoStr *buf = aoStrNew();
    /* This is not as high fidelity as LLVM's ir */
    const char *ir_return_type_str = irValueTypeToString(ir_func->return_value->type);
    aoStrCatFmt(buf, "%s %S(", ir_return_type_str, ir_func->name);
    AoStr *params_str = irParamsToString(ir_func->params);
    aoStrCatFmt(buf, "%S) {\n", params_str);
    aoStrRelease(params_str);

    listForEach(ir_func->blocks) {
        IrBlock *ir_block = (IrBlock *)it->value;
        AoStr *ir_block_str = irBlockToString(ir_func, ir_block);
        aoStrCatFmt(buf, "%S\n", ir_block_str);
        aoStrRelease(ir_block_str);
    }

    buf->len--;

    aoStrCatLen(buf, str_lit("}\n"));
    return buf;
}

void irPrintFunction(IrFunction *ir_function) {
    AoStr *ir_function_str = irFunctionToString(ir_function);
    printf("%s\n",ir_function_str->data);
    aoStrRelease(ir_function_str);
}

AoStr *irBlockToStringSimplified(IrBlock *ir_block) {
    AoStr *buf = aoStrNew();
    aoStrPutChar(buf, '\n');
    if (!listEmpty(ir_block->instructions)) {
        listForEach(ir_block->instructions) {
            IrInstr *ir_instr = (IrInstr *)it->value;
            AoStr *ir_instr_str = irInstrToString(ir_instr);
            aoStrCatFmt(buf, "    %S\n", ir_instr_str);
            aoStrRelease(ir_instr_str);
        }
    }
    return buf;
}


/* This makes no sense to use in the wild as it is specifically formatted to be
 * used with the `mapToString(...)` function. That isn't to say this cannot
 * be used... Just that it will look a bit our of place */
AoStr *irBlockMappingToStringCallback(void *_ir_block_mapping) {
    IrBlockMapping *ir_block_mapping = (IrBlockMapping *)_ir_block_mapping;
    AoStr *buf = aoStrNew();
    AoStr *successor_str = mapKeysToString(ir_block_mapping->successors);
    AoStr *predecessor_str = mapKeysToString(ir_block_mapping->predecessors);
    aoStrCatFmt(buf, "{\n"
                "    P: %S\n"
                "    S: %S\n  }",
                predecessor_str,
                successor_str);
    aoStrRelease(successor_str);
    aoStrRelease(predecessor_str);
    return buf;
}

AoStr *irFunctionCFGToString(IrFunction *func) {
    AoStr *buf = aoStrNew();
    AoStr *map_str = mapToString(func->cfg, ",\n");
    aoStrCatFmt(buf, "CFG {\n%S\n}",map_str);
    return buf;
}

void irValuePrint(IrValue *ir_value) {
    AoStr *str = irValueToString(ir_value);
    AoStr *buf = aoStrNew();
    if (is_terminal) {
        aoStrCatFmt(buf, ESC_GREEN"IrValue "ESC_RESET"%S", str);
    } else {
        aoStrCatFmt(buf, "IrValue %S", str);
    }
    printf("%s\n",str->data);
    aoStrRelease(str);
    aoStrRelease(buf);
}

void irInstrPrint(IrInstr *ir_instr) {
    AoStr *buf = aoStrNew();
    AoStr *str = irInstrToString(ir_instr);
    if (is_terminal) {
        aoStrCatFmt(buf, ESC_GREEN"IrInstr "ESC_RESET"%S", str);
    } else {
        aoStrCatFmt(buf, "IrInstr %S", str);
    }
    printf("%s\n",buf->data);
    aoStrRelease(str);
    aoStrRelease(buf);
}
