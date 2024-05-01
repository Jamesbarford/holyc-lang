#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

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
                realloc(builder->bb_pool,new_cap));
        if (bb_pool == NULL) {
            loggerPanic("Failed to reallocate memory for basic block pool\n");
        }
        builder->bb_pool = bb_pool;
        builder->bb_cap = new_cap;
    }
    BasicBlock *bb = &builder->bb_pool[builder->bb_pos++];
    bb->type = 0;
    bb->prev_cnt = 0;
    bb->block_no = 0;
    bb->ast_array = AstArrayNew(16);
    bb->block_no = builder->bb_count++;
    bb->_else = bb->_if = NULL;
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

/* @Buggy */
static void cgfHandleIfBlock(CFGBuilder *builder, Ast *ast) {
    assert(ast != NULL);
    BasicBlock *bb = builder->bb;
    BasicBlock *new_block, *then_body, *else_body;
    builder->flags |= CFG_FLAG_IN_CONDITIONAL;
    AstArrayPush(bb->ast_array,ast->cond);


    /* @Confirm
     * should the else block be part of this basic block and then the `bb->next` 
     * pointer set to the next block as opposed to explicitly setting a 
     * `bb->_else`? */

    new_block = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);

    then_body = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);

    then_body->prev[0] = bb;
    bb->_if = then_body;
    bb->_if->next = new_block;

    if (ast->els) {
        else_body = cfgBuilderAllocBasicBlock(builder,CFG_CONTROL_BLOCK);
        bb->_else = else_body;
        else_body->prev[0] = bb;
    }

    /* Start of a new basic block */
    cfgBuilderSetBasicBlock(builder,then_body);
    cfgHandleAstNode(builder,ast->then);

    if (ast->els) {
        cfgBuilderSetBasicBlock(builder,else_body);
        cfgHandleAstNode(builder,ast->els);
        else_body->next = new_block;
    }

    cfgBuilderSetBasicBlock(builder,new_block);
    builder->flags &= ~(CFG_FLAG_IN_CONDITIONAL);
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
 
        case AST_LVAR: {
            AstArrayPush(bb->ast_array,ast);
            break;
        }

        case AST_DECL: {
            AstArrayPush(bb->ast_array,ast);
            break;
        }

        case AST_IF: {
            cgfHandleIfBlock(builder,ast);
            break;
        }

        case TK_PRE_PLUS_PLUS:
        case TK_PLUS_PLUS:   
        case TK_PRE_MINUS_MINUS:
        case TK_MINUS_MINUS:

        case AST_ASM_FUNCALL:
        case AST_FUNCALL:
        case AST_FUNPTR_CALL: {
            AstArrayPush(bb->ast_array,ast);
            break;
        }

        case AST_CLASS_REF:
        case AST_DEREF:
        case AST_GOTO:
        case AST_ARRAY_INIT:
        case AST_DO_WHILE:
        case AST_WHILE:
        case AST_FOR:
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
                cfgHandleAstNode(builder,ast);
            }
            break;
        }

        case AST_STRING:
        case AST_LITERAL:
        case AST_RETURN:
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
            if (AstIsAssignment(kind)) {
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

/**
 * @Hack
 * It should be possible to be able to construct this tree without having
 * to subsequently fix it.
 */
static void cfgFix(BasicBlock *bb) {
    BasicBlock *_if, *_else, *next;
    for (; bb; bb = bb->next) {
        next = bb->next;
        _if = bb->_if;
        _else = bb->_else;

        if (_if && _else && next) {
            if (_if && _if->next->next != next)     _if->next = next;
            if (_else && _else->next->next != next) _else->next = next;
            bb->next = NULL;
        }

        if (_if && _if->next->next == NULL && next)     _if->next = next;
        if (_else && _else->next->next == NULL && next) _else->next = next;

        if (_if)   cfgFix(bb->_if);
        if (_else) cfgFix(bb->_else);
    }
}

static void cfgCreateGraphVizShapes(aoStr *str, BasicBlock *bb) {
    BasicBlock *_if, *_else, *next;
    for (; bb; bb = bb->next) {
        next = bb->next;
        _if = bb->_if;
        _else = bb->_else;

        if (!bb->_if && !bb->_else && !next) return;

        if (bb->ast_array->count) {
            aoStr *internal = aoStrAlloc(256);
            char *lvalue_str;
            for (int i = 0; i < bb->ast_array->count; ++i) {
                lvalue_str = AstLValueToString(bb->ast_array->entries[i]);
                aoStrCatPrintf(internal, "%s\\n",lvalue_str);
                free(lvalue_str);
            }
            aoStrCatPrintf(str,
                    "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb%d\\>:|%s}\"];\n",
                    bb->block_no,
                    bb->block_no,
                    internal->data);
            aoStrRelease(internal);
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
    cfgFix(cfg->head);
    aoStr *data = cfgCreateGraphViz(cfg);
    /* @Continue */
    printf("%s\n",data->data);
    aoStrRelease(data);
    return cfg;
}
