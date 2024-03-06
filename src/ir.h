#ifndef IR_H__
#define IR_H__

#include "aostr.h"
#include "ast.h"
#include "list.h"

#define IR_LOAD     200
#define IR_SAVE     201
#define IR_FUNCTION 202

#define IR_FLAGS_UNSIGNED (1 << 0)

typedef struct IrOperand {
    int kind;
    int offset;
    int size;
    unsigned long flags;

    union {
        long i64;

        /* float */
        struct {
            double f64;
            aoStr *f64_label;
        };

        /* string */
        struct {
            aoStr *sval;
            aoStr *slabel;
        };

        /* global var */
        struct {
            aoStr *gname;
        };
    };
} IrOperand;

typedef struct IrInstruction {
    int op;
    union {
        /* function */
        struct {
            aoStr *fname;
            List *body;
        };

        /* function call */
        struct {
            aoStr *call_name;
            List *args;
            int has_var_args;
        };

        /* classic */
        struct {
            IrOperand *arg1;
            IrOperand *arg2;
            IrOperand *arg3;
        };
    };
} IrInstruction;

#endif
