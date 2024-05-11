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

typedef struct CfgGraphVizBuilder {
    BasicBlock *bb;
    aoStr *viz;
    /* @Optimise, this could be a hash set for O(1), however for a small 
     * enough seen count this is fairly in-expensive */
    int *seen_blocks;
    int seen_cnt;
    int block_cnt;
} CfgGraphVizBuilder; 


static void cfgHandleAstNode(CFGBuilder *builder, Ast *ast);

static BasicBlock *cfgBuilderAllocBasicBlock(CFGBuilder *builder, int type) {
    /* @Bug, @HashTable
     * This can and will create a new pointer sometimes and thus all references
     * in the allocated instances will be invalid. Hence we need a bigger pool,
     * or no `->next` or `->else` and work off an adjacency list. Essentially
     * either one massive slab needs to be allocated up front or we work off
     * numerical references in a hash table.
     * */
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
    bb->flags = 0;
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
     * | }                        | 
     * | return 10;               |                                           
     * +--------------------------+                                           
     */

    BasicBlock *bb = builder->bb;
    bb->type = BB_BRANCH_BLOCK;
    AstArrayPush(bb->ast_array,ast->cond);
    builder->flags |= CFG_BUILDER_FLAG_IN_CONDITIONAL;

    BasicBlock *else_body = cfgBuilderAllocBasicBlock(builder,
                                                      BB_CONTROL_BLOCK);
    BasicBlock *if_body = cfgBuilderAllocBasicBlock(builder,
                                                    BB_CONTROL_BLOCK);
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
            new_block = cfgBuilderAllocBasicBlock(builder,BB_CONTROL_BLOCK);
            bb->_else->next = new_block;
            bb->_if->next = new_block;
            cfgBuilderSetBasicBlock(builder,new_block);
        }
    } else {
        bb->_if->next = else_body;
        cfgBuilderSetBasicBlock(builder,else_body);
    }
    builder->flags |= ~(CFG_BUILDER_FLAG_IN_CONDITIONAL);
}

static void cfgHandleForLoop(CFGBuilder *builder, Ast *ast) {
    Ast *init = ast->forinit;
    Ast *cond = ast->forcond;
    Ast *body = ast->forbody;
    Ast *step = ast->forstep;

    builder->flags |= CFG_BUILDER_FLAG_IN_LOOP;
    BasicBlock *bb_prev_loop = builder->bb_cur_loop;

    BasicBlock *bb = builder->bb;
    BasicBlock *bb_cond_else = cfgBuilderAllocBasicBlock(builder,
                                                         BB_CONTROL_BLOCK);
    BasicBlock *bb_cond = cfgBuilderAllocBasicBlock(builder,BB_BRANCH_BLOCK);
    BasicBlock *bb_body = cfgBuilderAllocBasicBlock(builder,BB_LOOP_BLOCK);

    if (init) AstArrayPush(bb->ast_array,init);

    bb_cond->flags |= BB_FLAG_LOOP_HEAD;
    bb->next = bb_cond;
    bb_cond->prev = bb;
    /* Jump into loop body if condition is met */
    bb_cond->_if = bb_body;
    /* Else move past loop body */
    bb_cond->_else = bb_cond_else;
    /* And the previously met block was the condition */
    bb_cond_else->prev = bb_cond;

    builder->bb_cur_loop = bb_body;

    cfgBuilderSetBasicBlock(builder,bb_cond);
    cfgHandleAstNode(builder,cond);

    /* Forms a loop, but the type is also set as a BB_LOOP_BLOCK so it
     * should be identifiable without (bb->next == bb->prev) */
    bb_body->prev = bb_cond;

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

    /* @Wrong 
     * We realistically do not want to be merging these two together in the 
     * same unit.
     *
     * Think step should be separate and that bb_body->next always should point
     * to it?
     * */
    cfgBuilderSetBasicBlock(builder,bb_body);
    cfgHandleAstNode(builder,body);
    cfgHandleAstNode(builder,step);

    
    /* This means we had branches or possibly inner loops in our loop */
    if (builder->bb != bb_body && !builder->bb->next) {
        builder->bb->type = BB_LOOP_BLOCK;
        builder->bb->prev = bb_cond;
    }
    builder->bb->flags |= BB_FLAG_LOOP_END;

    cfgBuilderSetBasicBlock(builder,bb_cond_else);
    builder->flags |= ~(CFG_BUILDER_FLAG_IN_LOOP);
    builder->bb_cur_loop = bb_prev_loop;
}

static void cfgHandleReturn(CFGBuilder *builder, Ast *ast) {
    BasicBlock *bb = builder->bb;
    BasicBlock *bb_return;

    if (bb->ast_array->count == 0) {
        bb_return = bb;
        bb_return->type = BB_RETURN_BLOCK;
    } else {
        bb_return = cfgBuilderAllocBasicBlock(builder,BB_RETURN_BLOCK);
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
            break;

        case AST_BREAK: {
            /* This is essentially a goto */
            assert(builder->flags & CFG_BUILDER_FLAG_IN_LOOP);
            if (builder->flags & CFG_BUILDER_FLAG_IN_CONDITIONAL) {
                loggerDebug("We in an if\n");
                BasicBlock *break_block = cfgBuilderAllocBasicBlock(builder,
                        BB_BREAK_BLOCK);
                break_block->prev = builder->bb;
                builder->bb = break_block;
            }
            break;
        }

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

static void cfgGraphVizBuilderClear(CfgGraphVizBuilder *builder) {
    if (builder->seen_blocks) {
        free(builder->seen_blocks);
        builder->seen_blocks = NULL;
        builder->block_cnt = 0;
    }
}

static aoStr *cfgGraphVizBuilderDestroyAndReturnVizString(CfgGraphVizBuilder *builder) {
    cfgGraphVizBuilderClear(builder);
    return builder->viz;
}

static void cfgGraphVizBuilderInit(CfgGraphVizBuilder *builder, CFG *cfg) {
    builder->seen_blocks = (int *)calloc(cfg->bb_count,sizeof(int));
    builder->seen_cnt = 0;
    builder->block_cnt = cfg->bb_count;
    builder->viz = aoStrAlloc(1<<10);
}

static void cfgGraphVizBuilderSetSeen(CfgGraphVizBuilder *builder, int block_no)
{
    builder->seen_blocks[builder->seen_cnt++] = block_no;
}

static int cfgGraphVizBuilderHasSeen(CfgGraphVizBuilder *builder, int block_no)
{
    int block_cnt = builder->block_cnt;
    int *seen = builder->seen_blocks;
    for (int i = 0; i < block_cnt; ++i) {
        if (seen[i] == block_no) {
            return 1;
        }
    }
    return 0;
}

static void cfgCreateGraphVizShapes(CfgGraphVizBuilder *builder,
        BasicBlock *bb)
{
    BasicBlock *_if, *_else, *next;
    aoStr *str = builder->viz;

    for (; bb; bb = bb->next) {
        int ast_count = bb->ast_array->count;
        next = bb->next;
        _if = bb->_if;
        _else = bb->_else;

        if (cfgGraphVizBuilderHasSeen(builder,bb->block_no)) return;
        else cfgGraphVizBuilderSetSeen(builder,bb->block_no);

        if (bb->flags & BB_FLAG_LOOP_HEAD) {
            aoStrCat(str,"subgraph cluster1_1 {\nstyle=\"filled\"\n;"
                    "color=\"darkgreen\";\n"
                    "fillcolor=\"grey88\";\n"
                    "label=\"loop 1\";\n"
                    "labeljust=l;\n"
                    "penwidt=2;\n");
        }

        if (ast_count) {
            aoStr *internal = aoStrAlloc(256);
            char *lvalue_str;
            for (int i = 0; i < ast_count; ++i) {
                Ast *ast = bb->ast_array->entries[i];
                lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
                if (i + 1 == ast_count) {
                    if (bb->type == BB_BRANCH_BLOCK) {
                        if (ast_count > 1) {
                            aoStrPutChar(internal,'|');
                        }
                        aoStrCatPrintf(internal,
                                "if (%s)\\l\\"
                                "  goto \\<%d bb\\>\\l\\"
                                "else\\l\\\n"
                                "  goto \\<%d bb\\>\\l\\",
                                lvalue_str,
                                _if->block_no,
                                _else->block_no);

                    } else if (bb->type == BB_LOOP_BLOCK) {
                        aoStrCatPrintf(internal,
                                "%s\\l\\\n"
                                "|goto \\<%d bb\\>\\l\\",
                                lvalue_str,
                                bb->prev->block_no);
                    } else if (next) {
                        aoStrCatPrintf(internal,
                                "%s\\l\\\n"
                                "|goto \\<%d bb\\>\\l\\",
                                lvalue_str,
                                next->block_no);
                    } else {
                        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
                    }
                } else {
                    aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
                }
                free(lvalue_str);
            }

            switch (bb->type) {
                case BB_BRANCH_BLOCK:
                    aoStrCatPrintf(str,
                            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n%s\n}\"];\n",
                            bb->block_no,
                            bb->block_no,
                            internal->data,
                            _if->block_no);
                    break;
                case BB_RETURN_BLOCK:
                    aoStrCatPrintf(str,
                            "    bb%d [shape=doublecircle,style=filled,fillcolor=white,label=\"\\<bb %d\\>\\n%s\"];\n",
                            bb->block_no,
                            bb->block_no,
                            internal->data);
                    break;
                default:
                    aoStrCatPrintf(str,
                            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n%s\n}\"];\n",
                            bb->block_no,
                            bb->block_no,
                            internal->data);
                    break;
            }
            aoStrRelease(internal);
        } else if (bb->type == BB_HEAD_BLOCK) {
            aoStrCatPrintf(str,
                    "    bb%d [shape=circle,style=filled,fillcolor=white,label=\"Entry\"];\n",
                    bb->block_no,
                    bb->block_no);
        }

        if (_if)   cfgCreateGraphVizShapes(builder,_if);
        if (_else) cfgCreateGraphVizShapes(builder,_else);
        if (bb->flags & BB_FLAG_LOOP_END) {
            aoStrCat(str,"}\n");
        }
    }
}

static void cfgCreateGraphVizMappings(CfgGraphVizBuilder *builder,
        Dict *mappings, BasicBlock *bb)
{
    char buffer[BUFSIZ];
    char *key;
    ssize_t len;
    BasicBlock *_if, *_else, *next;

    for (; bb; bb = bb->next) {
        next = bb->next;
        _if = bb->_if;
        _else = bb->_else;

        if (bb->type == BB_LOOP_BLOCK) {
            len = snprintf(buffer,sizeof(buffer),
                    "    bb%d:s -> bb%d:n [style=\"dotted,bold\",color=blue,weight=10,constraint=false];",
                    bb->block_no,
                    bb->prev->block_no);
            buffer[len] = '\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
            continue;
        }

        if (_if) {
            len = snprintf(buffer,sizeof(buffer),
                    "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=forestgreen,weight=10,constraint=true];",
                    bb->block_no,
                    _if->block_no);
            buffer[len] = '\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
        }

        if (_else) {
            len = snprintf(buffer,sizeof(buffer),
                    "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=darkorange,weight=10,constraint=true];",
                    bb->block_no,
                    _else->block_no);
            buffer[len] = '\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
        }

        if (next) {
            len = snprintf(buffer,sizeof(buffer),
                    "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=black,weight=100,constraint=true];",
                    bb->block_no,
                    next->block_no);
            buffer[len] = '\0';
            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }
        }

        if (_if)   cfgCreateGraphVizMappings(builder,mappings,_if);
        if (_else) cfgCreateGraphVizMappings(builder,mappings,_else);
    }
}

static void cfgCreateGraphVizBody(CfgGraphVizBuilder *builder, CFG *cfg) {
    BasicBlock *bb = cfg->head;
    Dict *mappings = DictNew(&default_table_type);
    cfgCreateGraphVizShapes(builder,bb);
    cfgCreateGraphVizMappings(builder,mappings,bb);
    for (ssize_t i = 0; i < (ssize_t)mappings->capacity; ++i) {
        DictNode *dn = mappings->body[i];
        while (dn) {
            aoStrCatPrintf(builder->viz,"%s\n",dn->key);
            dn = dn->next;
        }
    }
    DictRelease(mappings);
}

aoStr *cfgCreateGraphViz(CFG *cfg) {
    CfgGraphVizBuilder builder;
    cfgGraphVizBuilderInit(&builder,cfg);

    aoStrCatPrintf(builder.viz,"digraph \"%s\" {\n overlap=false;\n",cfg->ref_fname->data);
    cfgCreateGraphVizBody(&builder,cfg);
    aoStrCatPrintf(builder.viz,"}");
    return cfgGraphVizBuilderDestroyAndReturnVizString(&builder);
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
            bb = cfgBuilderAllocBasicBlock(&builder,BB_HEAD_BLOCK);
            cfg = cfgNew(ast->fname,bb);
            bb->next = cfgBuilderAllocBasicBlock(&builder,BB_CONTROL_BLOCK);
            cfgBuilderSetCFG(&builder,cfg);
            cfgBuilderSetBasicBlock(&builder,bb->next);
            cfgConstructFunction(&builder,ast->body->stms);
            cfg->bb_count = builder.bb_count;
        }
    }
    cfgToFile(cfg,"./loop.dot");
    return cfg;
}
