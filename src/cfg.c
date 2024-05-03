#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "aostr.h"
#include "ast.h"
#include "cctrl.h"
#include "cfg.h"
#include "dict.h"
#include "lexer.h"
#include "list.h"
#include "util.h"

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast);

static BasicBlock *cfgBuilderAllocBasicBlock(CFGBuilder *builder, int type) {
    if (builder->bb_pos + 1 >= builder->bb_cap) {
        int new_cap = builder->bb_cap * 2;
        BasicBlock *bb_pool = cast(BasicBlock *,
                realloc(builder->bb_pool,new_cap * sizeof(BasicBlock)));
        if (bb_pool == NULL) {
            loggerPanic("Failed to reallocate memory for basic block pool\n");
        }
        builder->bb_pool = bb_pool;
        builder->bb_cap = new_cap;
    }
    BasicBlock *bb = &builder->bb_pool[builder->bb_pos++];
    bb->type = type;
    bb->prev_cnt = 0;
    bb->block_no = builder->bb_count++;
    /* @Leak
     * This should probably be a memory pool as well */
    bb->ast_array = AstArrayNew(16);
    bb->next = bb->_else = bb->_if = NULL;
    return bb;
}

void cfgBuilderRelease(CFGBuilder *builder, int free_builder) {
    if (builder) {
        free(builder->bb_pool);
        if (free_builder) {
            free(builder);
        }
    }
}

static void cfgBuilderInit(CFGBuilder *builder, Cctrl *cc) {
    BasicBlock *bb_pool = cast(BasicBlock *, malloc(sizeof(BasicBlock)*64));
    builder->cc = cc;
    builder->bb_count = 1;
    builder->bb_cap = 64;
    builder->bb_pool = bb_pool;
    builder->bb_pos = 0;
    builder->flags = 0;
}

static void cfgBuilderSetCFG(CFGBuilder *builder, CFG *cfg) {
    builder->cfg = cfg;
}

static void cfgBuilderSetBasicBlock(CFGBuilder *builder, BasicBlock *bb) {
    builder->bb = bb;
}

static CFG *cfgNew(aoStr *fname, BasicBlock *head_block) {
    CFG *cfg = (CFG *)malloc(sizeof(CFG));
    cfg->head = head_block;
    cfg->ref_fname = fname;
    return cfg;
}

static void cgfHandleIfBlock(CFGBuilder *builder, Ast *ast) {
    /**
     * c-code:
     * +--------------------------+                                           
     * | I64 x = 10;              |                                           
     * | I64 y;                   |
     * | y = x * 89 + 1;          |
     * | if (x == 10) {           |                                           
     * |    y = 42;               |                                           
     * | } else if (x == 21) {    |                                            
     * |    y = 99;               |                                           
     * | } else {                 |                                           
     * |    y = 69;               |                                           
     * |    if (y == 69) {        |                                           
     * |       printf("69\n");    |                                           
     * |    }                     |                                           
     * | }                        |                                           
     * | printf("%ld\n",y);       |                                           
     * | if (x == 12) {           |                                           
     * |    printf("kowabunga\n");|
     * | }                       | 
     * | return 10;               |                                           
     * +--------------------------+                                           
     */

    BasicBlock *bb = builder->bb;
    bb->type = CFG_BRANCH_BLOCK;
    AstArrayPush(bb->ast_array,ast->cond);

    BasicBlock *else_body = cfgBuilderAllocBasicBlock(builder,
                                                      CFG_CONTROL_BLOCK);
    BasicBlock *if_body = cfgBuilderAllocBasicBlock(builder,
                                                    CFG_CONTROL_BLOCK);
    BasicBlock *new_block;


    if_body->prev   = bb;
    else_body->prev = bb;

    bb->_if = if_body;
    bb->_else = else_body;

    /* Start of a new basic block */
    cfgBuilderSetBasicBlock(builder,if_body);
    cfgHandleAstNode(builder,ast->then);

    if (ast->els) {
        cfgBuilderSetBasicBlock(builder,else_body);
        cfgHandleAstNode(builder,ast->els);
        /* This means the node has changed, and thus the next block needs 
         * to be this branch */
        if (builder->bb != else_body) {
            /* Join */
            bb->_if->next = builder->bb;
        } else {
            new_block = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
            bb->_else->next = new_block;
            bb->_if->next = new_block;
            cfgBuilderSetBasicBlock(builder,new_block);
        }
    } else {
        bb->_if->next = else_body;
        cfgBuilderSetBasicBlock(builder,else_body);
    }
}

static void cfgHandleForLoop(CFGBuilder *builder, Ast *ast) {
    Ast *init = ast->forinit;
    Ast *cond = ast->forcond;
    Ast *body = ast->forbody;
    Ast *step = ast->forstep;

    BasicBlock *bb = builder->bb;
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         CFG_CONTROL_BLOCK);
    BasicBlock *bb_cond = cfgBuilderAllocBasicBlock(builder,CFG_BRANCH_BLOCK);
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,CFG_LOOP_BLOCK);

    if (init) AstArrayPush(bb->ast_array,init);

    bb->next = bb_cond;
    bb_cond->prev = bb;
    /* Jump into loop body if condition is met */
    bb_cond->_if = bb_body;
    /* Else move past loop body */
    bb_cond->_else = bb_cond_else;
    /* Past the loop body, set for the fix function */
    bb_cond->next = bb_cond_else;
    /* And the previously met block was the condition */
    bb_cond_else->prev = bb_cond;

    /* Forms a loop, but the type is also set as a CFG_LOOP_BLOCK so it
     * should be identifiable without (bb->next == bb->prev) */
    bb_body->prev = bb_cond;
    bb_body->next = bb_cond;

    cfgBuilderSetBasicBlock(builder,bb_cond);
    cfgHandleAstNode(builder,cond);
    /* Body exists within a loop 
     *
     * This c code:
     * +--------------------------------+
     * | I64 y = 420;                   |
     * | for (I64 i = 0; i < 10; ++i) { |
     * |    printf("%d\n",y);           |
     * | }                              |
     * | return 0;                      |
     * +--------------------------------+
     *
     *
     * Becomes this control flow graph:
     * +--------------------+
     * | BB1:               |
     * +--------------------+
     * | y = 420;           |
     * | i = 0;             |<- forinit.
     * +--------------------+
     *           |          
     *           V          
     * +--------------------+
     * | BB2:               |<- forcond is this whole block.
     * +--------------------+
     * | if i < 10          |         
     * |    goto BB3;       |--------+
     * | else               |        |
     * |    goto BB4;       |--------+
     * +--------------------+        |
     *                               |
     * +--------------------+        |
     * | BB3:               |<-------+
     * +--------------------+        |
     * | printf("%d\n", y); |        |<- forbody forms the base of this block.
     * | ++i                |  loop  |<- forstep is merged in with forbody.
     * | goto BB2           |=======>+<- unconditional jump to the condition
     * +--------------------+  BB2   |   block.
     *                               |
     *                               |
     * +--------------------+        |
     * | BB4:               |<-------+
     * +--------------------+
     * | return 0;          |
     * +--------------------+
     **/

    cfgBuilderSetBasicBlock(builder,bb_body);
    cfgHandleAstNode(builder,body);
    cfgHandleAstNode(builder,step);
    cfgBuilderSetBasicBlock(builder,bb_cond_else);
}

static void cfgHandleReturn(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_return;

    if (bb->ast_array->count == 0) {
        bb_return = bb;
    } else {
        bb_return = cfgBuilderAllocBasicBlock(builder,CFG_RETURN_BLOCK);
        bb->next = bb_return;
        bb_return->prev = bb;
    }
    bb_return->next = NULL;
    AstArrayPush(bb_return->ast_array,ast);
    cfgBuilderSetBasicBlock(builder,bb_return);
}

static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast) {
    assert(ast != NULL);
    int kind = ast->kind;
    BasicBlock *bb = builder->bb;
    List *stmts;

    /* We are only interested in the AST nodes that are the start of a new 
     * basic block or a top level `I64 x = 10;` type declaration or assignment 
     * 
     * A new basic block will start with an `if`, `for`, `while`, `goto` */
    switch (kind) {
        case AST_LABEL:
        case AST_ADDR:
        case AST_FUNC:
            break;
 
        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:
        case AST_ARRAY_INIT:
        case AST_ASM_FUNCALL:
        case AST_FUNCALL:
        case AST_FUNPTR_CALL:
        case AST_DECL:
        case AST_LVAR: {
            AstArrayPush(bb->ast_array,ast);
            break;
        }

        case AST_IF: {
            bb = builder->bb;
            cgfHandleIfBlock(builder,ast);
            break;
        }

        case AST_FOR: {
            cfgHandleForLoop(builder,ast);
            break;
        }

        case AST_RETURN: {
            cfgHandleReturn(builder,ast);
            break;
        }

        case AST_GOTO:
        case AST_DO_WHILE:
        case AST_WHILE:

        case AST_CLASS_REF:
        case AST_DEREF:

        case AST_JUMP:
        case AST_CASE:

        case AST_BREAK:
        case AST_CONTINUE:
            break;

        /* We are in a jump */
        case AST_COMPOUND_STMT: {
            stmts = ast->stms;
            if (ListEmpty(stmts)) return;
            ListForEach(stmts) {
                Ast *ast = cast(Ast *,it->value);
                bb = builder->bb;
                cfgHandleAstNode(builder,ast);
            }
            break;
        }

        case AST_STRING:
        case AST_LITERAL:
        case AST_OP_ADD:
        case AST_ASM_STMT:
        case AST_ASM_FUNC_BIND:
        case AST_FUNPTR:
        case AST_DEFAULT_PARAM:
        case AST_VAR_ARGS:
        case AST_ASM_FUNCDEF:
        case AST_CAST:
        case AST_FUN_PROTO:
        case AST_EXTERN_FUNC:
        case AST_PLACEHOLDER:

        default:
            /* @Refactor */
            if (AstIsAssignment(kind)) {
                AstArrayPush(bb->ast_array,ast);
            } else {
                AstArrayPush(bb->ast_array,ast);
            }
            break;
    }
}

/* Split out to make it simpler to reason with */
static void cfgConstructFunction(CFGBuilder *builder, List *stmts) {
    if (ListEmpty(stmts)) return;
    builder->ast_list = stmts;
    ListForEach(stmts) {
        Ast *ast = cast(Ast *,it->value);
        builder->ast_iter = it;
        cfgHandleAstNode(builder,ast);
    }
}

static void cfgCreateGraphVizShapes(aoStr *str, BasicBlock *bb) {
    BasicBlock *_if, *_else, *next;
    for (; bb; bb = bb->next) {
        next = bb->next;
        _if = bb->_if;
        _else = bb->_else;

        assert(!(next && _if && _else));

        if (bb->ast_array->count) {
            aoStr *internal = aoStrAlloc(256);
            char *lvalue_str;
            for (int i = 0; i < bb->ast_array->count; ++i) {
                Ast *ast = bb->ast_array->entries[i];
                lvalue_str = AstLValueToString(ast);
                aoStrCatPrintf(internal, "%s\\n",lvalue_str);
                free(lvalue_str);
            }

            switch (bb->type) {
                case CFG_BRANCH_BLOCK:
                    aoStrCatPrintf(str,
                            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb%d\\> BRANCH:|%s}\"];\n",
                            bb->block_no,
                            bb->block_no,
                            internal->data);
                    break;
                default:
                    aoStrCatPrintf(str,
                            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb%d\\>:|%s}\"];\n",
                            bb->block_no,
                            bb->block_no,
                            internal->data);
                    break;
            }
            aoStrRelease(internal);
        }

        if (!bb->_if && !bb->_else && !next) {
            return;
        }
        if (_if)   cfgCreateGraphVizShapes(str,_if);
        if (_else) cfgCreateGraphVizShapes(str,_else);
    }
}

static void cfgCreateGraphVizMappings(Dict *mappings, aoStr *str, BasicBlock *bb) {
    char buffer[BUFSIZ];
    char *key;
    ssize_t len;
    BasicBlock *_if, *_else, *next;

    for (; bb; bb = bb->next) {
        next = bb->next;
        _if = bb->_if;
        _else = bb->_else;

        if (_if) {
            len = snprintf(buffer,sizeof(buffer),"    bb%d -> bb%d",
                    bb->block_no,
                    _if->block_no);
            buffer[len]='\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
        }

        if (_else) {
            len = snprintf(buffer,sizeof(buffer),"    bb%d -> bb%d",
                    bb->block_no,
                    _else->block_no);
            buffer[len]='\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
        }

        if (next) {
            len = snprintf(buffer,sizeof(buffer),"    bb%d -> bb%d",
                    bb->block_no,
                    next->block_no);
            buffer[len]='\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
        }

        if (_if)   cfgCreateGraphVizMappings(mappings,str,_if);
        if (_else) cfgCreateGraphVizMappings(mappings,str,_else);
        
    }
}

static void cfgCreateGraphVizBody(aoStr *str, CFG *cfg) {
    BasicBlock *bb = cfg->head;
    Dict *mappings = DictNew(&default_table_type);
    cfgCreateGraphVizShapes(str,bb);
    cfgCreateGraphVizMappings(mappings,str,bb);

    for (ssize_t i = 0; i < (ssize_t)mappings->capacity; ++i) {
        DictNode *dn = mappings->body[i];
        while (dn) {
            aoStrCatPrintf(str,"%s\n",dn->key);
            dn = dn->next;
        }
    }
    DictRelease(mappings);
}

aoStr *cfgCreateGraphViz(CFG *cfg) {
    aoStr *str = aoStrAlloc(1024);
    aoStrCatPrintf(str,"digraph \"%s\" {\n",cfg->ref_fname->data);
    cfgCreateGraphVizBody(str,cfg);
    aoStrCatPrintf(str,"}");
    return str;
}

void cfgToFile(CFG *cfg, const char *filename) {
    aoStr *cfg_string = cfgCreateGraphViz(cfg);
    int fd = open(filename,O_CREAT|O_TRUNC|O_RDWR,0644);
    if (fd == -1) {
        loggerPanic("Failed to open file '%s': %s\n",
                filename,strerror(errno));
    }
    write(fd,cfg_string->data,cfg_string->len);
    close(fd);
    aoStrRelease(cfg_string);
}

CFG *cfgConstruct(Cctrl *cc) {
    Ast *ast;
    CFG *cfg;
    CFGBuilder builder;
    BasicBlock *bb;

    cfgBuilderInit(&builder,cc);

    ListForEach(cc->ast_list) {
        ast = (Ast *)it->value;
        if (ast->kind == AST_FUNC) {
            bb = cfgBuilderAllocBasicBlock(&builder,CFG_HEAD_BLOCK);
            cfg = cfgNew(ast->fname,bb);
            bb->next = cfgBuilderAllocBasicBlock(&builder,CFG_CONTROL_BLOCK);
            cfgBuilderSetCFG(&builder,cfg);
            cfgBuilderSetBasicBlock(&builder,bb->next);
            cfgConstructFunction(&builder,ast->body->stms);
        }
    }
    cfgToFile(cfg,"./ex.dot");
    return cfg;
}
