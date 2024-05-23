#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#include "aostr.h"
#include "ast.h"
#include "cfg.h"
#include "cfg-print.h"
#include "cfg.h"
#include "dict.h"
#include "lexer.h"
#include "util.h"

typedef struct CfgGraphVizBuilder {
    aoStr *viz;
    /* @Optimise, this could be a hash set for O(1), however for a small 
     * enough seen count this is fairly in-expensive */
    int *seen_blocks;
    int seen_cnt;
    int block_cnt;
    int loop_cnt;
    /* This is if a loop does not have a break clause immediately in it */
    BasicBlock *break_blocks[256];
    int break_idx;
} CfgGraphVizBuilder; 

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
    builder->loop_cnt = 0;
    builder->break_idx = 0;
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

static void cfgBranchPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *lvalue_str;
    int ast_count = ast_array->count;
    aoStr *internal = aoStrAlloc(256);
    Ast *cond = ast_array->entries[ast_count - 1];

    for (int i = 0; i < ast_count-1; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(internal,'|');
        }
        free(lvalue_str);
    }

    assert(cond != NULL);
    lvalue_str = AstLValueToString(cond,LEXEME_ENCODE_PUNCT);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\n"
            "if (%s)\\l\\"
            "  goto \\<%d bb\\>\\l\\"
            "else\\l\\\n"
            "  goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data,
            lvalue_str,
            bb->_if->block_no,
            bb->_else->block_no);
    aoStrRelease(internal);
}

static void cfgDoWhileCondPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *lvalue_str;
    int ast_count = ast_array->count;
    aoStr *internal = aoStrAlloc(256);
    Ast *cond = ast_array->entries[ast_count - 1];

    for (int i = 0; i < ast_count-1; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(internal,'|');
        }
        free(lvalue_str);
    }

    assert(cond != NULL);
    lvalue_str = AstLValueToString(cond,LEXEME_ENCODE_PUNCT);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\n"
            "if (%s)\\l\\"
            "  goto \\<%d bb\\>\\l\\"
            "else\\l\\\n"
            "  goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data,
            lvalue_str,
            bb->prev->block_no,
            bb->next->block_no);
    aoStrRelease(internal);
}

static void cfgLoopPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *lvalue_str;
    int ast_count = ast_array->count;
    aoStr *internal = aoStrAlloc(256);

    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
        if (ast_count > 1 && i + 1 != ast_count) {
            aoStrPutChar(internal,'|');
        }
        free(lvalue_str);
    }

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\n"
            "|goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data,
            bb->prev->block_no);

    aoStrRelease(internal);
}

static void cfgBreakPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *lvalue_str;
    int ast_count = ast_array->count;
    aoStr *internal = aoStrAlloc(256);

    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
        if (ast_count > 1 && i + 1 != ast_count) {
            aoStrPutChar(internal,'|');
        }
        free(lvalue_str);
    }

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\\l\\\n"
            "|goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data,
            bb->next->block_no);
    aoStrRelease(internal);
}

static void cfgDefaultPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *lvalue_str;
    int ast_count = ast_array->count;
    aoStr *internal = aoStrAlloc(256);

    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
        if (ast_count > 1 && i + 1 != ast_count) {
            aoStrPutChar(internal,'|');
        }
        free(lvalue_str);
    }

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n%s",
            bb->block_no,
            bb->block_no,
            internal->data);

    if (bb->next) {
        aoStrCatPrintf(builder->viz,"|goto \\<%d bb\\>\\l\\ \n}\"];\n\n",
                bb->next->block_no);
    } else {
        loggerWarning("Block unexpectedly terminates: %dbb, type %d\n",
                bb->block_no, bb->type);
        aoStrCat(builder->viz,"\n}\"];\n\n");
    }
    aoStrRelease(internal);
}

static void cfgHeadPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=circle,style=filled,fillcolor=white,label=\"Entry\"];\n",
            bb->block_no,
            bb->block_no);
}

static void cfgReturnPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *lvalue_str;
    int ast_count = ast_array->count;
    aoStr *internal = aoStrAlloc(256);

    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(internal,"%s\\l\\\n",lvalue_str);
        if (ast_count > 1 && i + 1 != ast_count) {
            aoStrPutChar(internal,'|');
        }
        free(lvalue_str);
    }

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=doublecircle,style=filled,fillcolor=white,label=\" \\<bb %d\\>\n %s \"];\n\n",
            bb->block_no,
            bb->block_no,
            internal->data);
    aoStrRelease(internal);
}

static void cfgCreateGraphVizShapes(CfgGraphVizBuilder *builder,
        BasicBlock *bb)
{
    BasicBlock *_if, *_else;

    for (; bb; bb = bb->next) {
        _if = bb->_if;
        _else = bb->_else;

        if (cfgGraphVizBuilderHasSeen(builder,bb->block_no)) return;
        else cfgGraphVizBuilderSetSeen(builder,bb->block_no);

        /* @Cleanup
         * We could do this thing in the cfg.c file and then remove the loop 
         * head if there is an immeidate 'break'. This is not urgent */
        if (bb->flags & BB_FLAG_LOOP_HEAD) {
            if (!_if || (_if && _if->type != BB_BREAK_BLOCK)) {
                int cnt = ++builder->loop_cnt;
                aoStrCatPrintf(builder->viz,"subgraph cluster1_%d {\nstyle=\"filled\";\n"
                        "color=\"darkgreen\";\n"
                        "fillcolor=\"grey88\";\n"
                        "label=\"loop %d\";\n"
                        "labeljust=l;\n"
                        "penwidth=2;\n",cnt,cnt);
            }
        }

        switch (bb->type) {
            case BB_HEAD_BLOCK:    cfgHeadPrintf(builder,bb);    break;
            case BB_LOOP_BLOCK:    cfgLoopPrintf(builder,bb);    break;
            case BB_DO_WHILE_COND: cfgDoWhileCondPrintf(builder,bb);  break;
            case BB_BRANCH_BLOCK:  cfgBranchPrintf(builder,bb);  break;
            case BB_BREAK_BLOCK:   cfgBreakPrintf(builder,bb);   break;
            case BB_END_BLOCK:
            case BB_RETURN_BLOCK:  cfgReturnPrintf(builder,bb);  break;

            default:               cfgDefaultPrintf(builder,bb); break;
        }

        if (bb->type == BB_END_BLOCK) break;

        if (_if && _else) {
            /* Break blocks visually live in the outer scope of the loop,
             * so we save them for later. */
            if (_if->type == BB_BREAK_BLOCK && _else->type == BB_BREAK_BLOCK) {
                builder->break_blocks[builder->break_idx++] = _if;
                builder->break_blocks[builder->break_idx++] = _else;
            } else if (_if->type == BB_BREAK_BLOCK) {
                cfgCreateGraphVizShapes(builder,_else);
                builder->break_blocks[builder->break_idx++] = _if;
            } else if (_else->type == BB_BREAK_BLOCK) {
                cfgCreateGraphVizShapes(builder,_if);
                builder->break_blocks[builder->break_idx++] = _else;
            } else {
                cfgCreateGraphVizShapes(builder,_if);
                cfgCreateGraphVizShapes(builder,_else);
            }

            /* This is for when the body of a loop breaks immediately, then 
             * we will never hit the bb->flags & BB_FLAG_LOOP_END. 
             * There feels like there should be a better way of doing this 
             * though. */
            if ((_if->flags & BB_FLAG_REDUNDANT_LOOP) ||
                (_else->flags & BB_FLAG_REDUNDANT_LOOP)) {
                while (builder->break_idx) {
                    cfgCreateGraphVizShapes(builder,
                            builder->break_blocks[--builder->break_idx]);
                }
            }
        }


        if (bb->flags & BB_FLAG_LOOP_END) {
            while (builder->break_idx) {
                cfgCreateGraphVizShapes(builder,
                        builder->break_blocks[--builder->break_idx]);
            }
            if (!(bb->flags & BB_FLAG_REDUNDANT_LOOP)) {
                aoStrCat(builder->viz,"}\n");
            }
        }

        /* If we have a goto we do not want to print it's bb->next as 
         * the code is non-linear and we will eventually get there. The 
         * link will already be printed */
        if (bb->type == BB_GOTO && !(bb->flags & BB_FLAG_UNCONDITIONAL_JUMP)) break;
        if (bb->prev) cfgCreateGraphVizShapes(builder,bb->prev);
        if (bb->type == BB_BRANCH_BLOCK) break;
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

        if (bb->type == BB_DO_WHILE_COND) {

            len = snprintf(buffer,sizeof(buffer),
                    "    bb%d:s -> bb%d:n [style=\"dotted,bold\",color=blue,weight=10,constraint=false];",
                    bb->block_no,
                    bb->prev->block_no);
            buffer[len] = '\0';

            if (!DictGet(mappings,buffer)) {
                key = strndup(buffer,len);
                DictSet(mappings,key,bb);
            }


            len = snprintf(buffer,sizeof(buffer),
                    "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=darkorange,weight=10,constraint=true];",
                    bb->block_no,
                    bb->next->block_no);
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

/* This is to create the arrows between the blocks in the graphviz */
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
