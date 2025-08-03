#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <fcntl.h>

#include "aostr.h"
#include "ast.h"
#include "cfg.h"
#include "cfg-print.h"
#include "containers.h"
#include "lexer.h"
#include "util.h"

#define CFG_GRAPHVIZ_ERROR_INVALID_TYPE (0)

#define BB_FMT_BLUE_DOTTED ("    bb%d:s -> bb%d:n [style=\"dotted,bold\",color=blue,weight=10,constraint=false];\n")
#define BB_FMT_RED_SOLID   ("    bb%d:s -> bb%d:n [style=\"solid,bold\",color=firebrick1,weight=10,constraint=true];\n")
#define BB_FMT_GREEN_SOLID ("    bb%d:s -> bb%d:n [style=\"solid,bold\",color=forestgreen,weight=10,constraint=true];\n")
#define BB_FMT_BLACK_SOLID ("    bb%d:s -> bb%d:n [style=\"solid,bold\",color=black,weight=100,constraint=true];\n")
#define BB_FMT_BLACK_END_NODE ("    bb%d:s -> endbb%d:n [style=\"solid,bold\",color=black,weight=100,constraint=true];\n")


static char *depth_to_loop_color[] = {
    [0] = "grey88",
    [1] = "grey78",
    [2] = "grey68",
    [3] = "grey58",
    [4] = "grey48",
    [5] = "grey38",
    [6] = "grey28",
    [7] = "grey28",
    [8] = "grey18",
};

static const char *cfgPrintGetLoopColor(int depth) {
    static int len = sizeof(depth_to_loop_color)/sizeof(depth_to_loop_color[0]);
    if (depth >= len) return "grey88";
    return depth_to_loop_color[depth];
}

typedef struct CfgGraphVizBuilder {
    AoStr *viz;
    CFG *cfg;
    /* Need a counter to graphviz does not merge loops */
    int loop_cnt;
    /* To change the shade of grey based on how deep into a nested loop we 
     * are */
    int loop_nesting;

    /* This is if a loop does not have a break clause immediately in it */
    int break_idx;
    BasicBlock *break_blocks[256];

    int loop_idx;
    BasicBlock *loop_stack[32];

    int return_idx;
    BasicBlock *return_statements[32];
} CfgGraphVizBuilder;

static void cfgCreatePictureUtil(CfgGraphVizBuilder *builder,
                                 BasicBlock *bb,
                                 Set *seen);

static void cfgGraphVizBuilderInit(CfgGraphVizBuilder *builder) {
    builder->viz = aoStrAlloc(1<<10);
    builder->loop_nesting = 0;
    builder->loop_cnt = 0;
    builder->break_idx = 0;
    builder->loop_idx = 0;
    builder->return_idx = 0;
}

static AoStr *bbAstArrayToString(Vec *ast_array, int ast_count) {
    if (ast_count == 0) return NULL;
    AoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = vecGet(Ast *,ast_array,i);
        lvalue_str = astLValueToString(ast,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
        aoStrCatPrintf(ast_str,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(ast_str,'|');
        }
    }
    return ast_str;
}

static void cfgBranchPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    char *fillcolor;
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size-1);
    Ast *cond = (Ast *)bb->ast_array->entries[bb->ast_array->size-1];

    assert(cond != NULL);
    char *lvalue_str = astLValueToString(cond,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));

    if (bb->flags & BB_FLAG_LOOP_HEAD) {
        fillcolor = "lightpink";
    } else {
        fillcolor = "coral";
    }

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=%s,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            fillcolor,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"%s\n",internal->data);
    }
    aoStrCatPrintf(builder->viz,"if (%s)\\l\\\n}\"];\n\n",lvalue_str);
}

static void cfgDoWhileCondPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    char *fillcolor = "lightpink";
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size-1);
    Ast *cond = (Ast *)bb->ast_array->entries[bb->ast_array->size-1];

    if (bb->prev->flags & BB_FLAG_LOOP_HEAD) {
        fillcolor = "lightskyblue";
    }

    char *lvalue_str = astLValueToString(cond,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=%s,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            fillcolor,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"%s\n",internal->data);
    }

    aoStrCatPrintf(builder->viz,"if (%s)\\l\\\n}\"];\n\n",lvalue_str);
}

static void cfgLoopPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgreen,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"%s\n}\"];\n\n",internal->data);
    } else {
        aoStrCatPrintf(builder->viz,"}\"];\n\n");
    }

}

static void cfgBreakPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=violet,label=\"{\\<bb %d\\>\n",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"%s\\l\\\n",internal->data);
    }
    aoStrCatPrintf(builder->viz,"|break\\l\\\n\n}\"];\n\n");
}

static void cfgDefaultPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);
    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"|\n%s",internal->data);
    }

    if (bb->next) {
        aoStrCatPrintf(builder->viz,
                 "|goto \\<%d bb\\>"
                 " \n}\"];\n\n",
                 bb->next->block_no
                );
    } else {
        loggerWarning("Block unexpectedly terminates: %dbb, type %d\n",
                bb->block_no, bb->type);
        aoStrCat(builder->viz,"\n}\"];\n\n");
    }
}

static void cfgHeadPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=circle,style=filled,fillcolor=white,label=\"Entry\"];\n",
            bb->block_no,
            bb->block_no);
}

static void cfgReturnPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>",
            bb->block_no,
            bb->block_no);
    if (internal) {
        aoStrCatPrintf(builder->viz,"|%s \n}\"];\n\n",internal->data);
    } else {
        aoStrCat(builder->viz, "|return (void) \n}\"];\n\n");
    }
    aoStrCatPrintf(builder->viz,
    "    endbb%d [shape=doublecircle,style=filled,fontcolor=white,fillcolor=black,label=\"\\<end\\>\"];\n\n",
    bb->block_no
    );
}

static void cfgCasePrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    int ast_count = bb->ast_array->size;
    Vec *ast_array = bb->ast_array;
    AoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    Ast *_case = vecGet(Ast *,ast_array,0);

    for (int i = 1; i < ast_count; ++i) {
        Ast *ast = vecGet(Ast *,ast_array,i);
        lvalue_str = astLValueToString(ast,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
        aoStrCatPrintf(ast_str,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(ast_str,'|');
        }
    }

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>\n",
            bb->block_no,
            bb->block_no);

    lvalue_str = astLValueToString(_case,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
    aoStrCatPrintf(builder->viz,"|%s",lvalue_str);
    if (ast_str->len) {
        aoStrCatPrintf(builder->viz,"|%s",ast_str->data);
    }

    if (bb->next) {
        aoStrCatPrintf(builder->viz,
                "|goto \\<%d bb\\>"
                " \n}\"];\n\n",
                bb->next->block_no);
    }

}

static void cfgSwitchPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size-1);
    
    Ast *ast = vecGet(Ast *,bb->ast_array,bb->ast_array->size - 1);
    lvalue_str = astLValueToString(ast,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
    aoStrCatPrintf(ast_str,"test (%s)\\l\\\n",lvalue_str);

    if (internal) {
        aoStrCatPrintf(builder->viz,
                "    bb%d [shape=record,style=filled,fillcolor=darkorchid2,label=\"{\\<bb %d\\>|\n%s|%s",
                bb->block_no,
                bb->block_no,
                internal->data,
                ast_str->data);
    } else {
        aoStrCatPrintf(builder->viz,
                "    bb%d [shape=record,style=filled,fillcolor=darkorchid2,label=\"{\\<bb %d\\>|\n%s",
                bb->block_no,
                bb->block_no,
                ast_str->data);
    }

    aoStrCat(builder->viz,"\n}\"];\n\n");
}

static void cfgContinuePrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);
    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=violet,label=\"{\\<bb %d\\>\n",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"|%s",internal->data);
    }

    aoStrCatPrintf(builder->viz,"|continue\\l\\\n\n}\"];\n\n");
}

/* Side-effect of mutating the loop counter on the builder */
static void cfgLoopHeadPrintf(CfgGraphVizBuilder *builder) {
    int color = builder->loop_nesting;
    int count = ++builder->loop_cnt;
    const char *loop_color = cfgPrintGetLoopColor(color);
    aoStrCatPrintf(builder->viz,
            "subgraph cluster1_%d {\nstyle=\"filled\";\n"
            "color=\"darkgreen\";\n"
            "fillcolor=\"%s\";\n"
            "label=\"loop %d\";\n"
            "labeljust=l;\n"
            "penwidth=2;\n",
            count,
            loop_color,
            count);
}

static void bbPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    switch (bb->type) {
        case BB_HEAD_BLOCK:    cfgHeadPrintf(builder,bb);    break;
        case BB_LOOP_BLOCK:    cfgLoopPrintf(builder,bb);    break;
        case BB_DO_WHILE_COND: cfgDoWhileCondPrintf(builder,bb); break;
        case BB_BRANCH_BLOCK:  cfgBranchPrintf(builder,bb);      break;
        case BB_BREAK_BLOCK:   cfgBreakPrintf(builder,bb);       break;
        case BB_END_BLOCK:
        case BB_RETURN_BLOCK:  cfgReturnPrintf(builder,bb);   break;
        case BB_CASE:          cfgCasePrintf(builder,bb);     break;
        case BB_SWITCH:        cfgSwitchPrintf(builder,bb);   break;
        case BB_CONTINUE:      cfgContinuePrintf(builder,bb); break;
        default:               cfgDefaultPrintf(builder,bb);  break;
    }
}

static void cfgPrintBreaks(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    while (builder->break_idx) {
        int idx = builder->break_idx-1;
        BasicBlock *break_block = builder->break_blocks[idx];
        builder->break_idx--;
        if (break_block == bb) {
            break;
        }
        bbPrintf(builder,break_block);
    }
}

/**
 * @Hack 
 * We do this to ensure everything inside of the do while loop body gets 
 * printed. Otherwise the next pointer of the condition will start exploring 
 * nodes outside of the loop prematurely. 
 *
 * Adding it to the seen set means it will never get explored. */
static BasicBlock *cfgGetHandleDoWhileHead(Set *seen, BasicBlock *bb) {
    BasicBlock *while_cond = NULL;
    if (bb->flags & BB_FLAG_LOOP_HEAD) {
        MapIter it;
        mapIterInit(bb->prev_blocks, &it);
        while (mapIterNext(&it)) {
            BasicBlock *prev = (BasicBlock *)it.node->value;
            if (prev->type == BB_DO_WHILE_COND && prev->prev == bb) {
                while_cond = prev;
                break;
            }
        }

        if (while_cond) {
            setAdd(seen,(void *)(long)while_cond->block_no);
        }
    }
    return while_cond;
}

char *cfgGraphVizError(CfgGraphVizBuilder *builder, BasicBlock *bb, int error_code) {
    switch (error_code) {
        case CFG_GRAPHVIZ_ERROR_INVALID_TYPE: {
            char *type_string = bbTypeToString(bb->type);
            char *error = mprintf("%s() has an invalid type '%s' for bb%d\n",
                    builder->cfg->ref_fname->data, type_string, bb->block_no);
            return error;
        }
        default:
            loggerPanic("Unknown error code: %d\n", error_code);
    }
}

static void cfgCreatePictureUtil(CfgGraphVizBuilder *builder,
                                 BasicBlock *bb,
                                 Set *seen)
{
    BasicBlock *while_cond = cfgGetHandleDoWhileHead(seen,bb);

    void *block_no_key = (void *)(long)bb->block_no;
    if (setHas(seen,block_no_key)) return;
    else setAdd(seen,block_no_key);

    if (bb->flags & BB_FLAG_LOOP_HEAD) {
        builder->loop_nesting++;
        cfgLoopHeadPrintf(builder);
    }

    switch (bb->type) {
        case BB_CONTINUE:
        case BB_LOOP_BLOCK:
            bbPrintf(builder,bb);
            if (bb->flags & BB_FLAG_UNCONDITIONAL_JUMP && bb->next) {
                cfgCreatePictureUtil(builder,bb->next,seen);
            }
            break;

        case BB_DO_WHILE_COND:
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
               builder->loop_stack[builder->loop_idx++] = bb;
               /* Push the head onto the stack so that we can bookend the 
                * break clauses and scope them to the current loop */
               builder->break_blocks[builder->break_idx++] = bb;
            }

            cfgDoWhileCondPrintf(builder,bb);
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
                cfgCreatePictureUtil(builder,bb->next,seen);
            }

            if ((bb->flags & BB_FLAG_LOOP_END) && !bb->visited) {
                aoStrCat(builder->viz,"}\n");
                bb->visited++;
            }

            if (bb->flags & BB_FLAG_LOOP_END) {
                while (builder->break_idx) {
                    int idx = builder->break_idx-1;
                    BasicBlock *break_block = builder->break_blocks[idx];
                    builder->break_idx--;
                    if (break_block == bb) {
                        break;
                    }
                    bbPrintf(builder,break_block);
                }
            }
            break;

        case BB_BRANCH_BLOCK: {
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
                builder->loop_stack[builder->loop_idx++] = bb;
                /* Push the head onto the stack so that we can bookend the 
                 * break clauses and scope them to the current loop */
                builder->break_blocks[builder->break_idx++] = bb;
            }

            cfgBranchPrintf(builder,bb);
            cfgCreatePictureUtil(builder,bb->_if,seen);

            /* @Bug - should know how many loops a block ends */
            MapIter it;
            int loop_ends = 0;
            mapIterInit(bb->_else->prev_blocks, &it);
            while (mapIterNext(&it)) {
                BasicBlock *prev = (BasicBlock *)it.node->value;
                if (prev->flags & BB_FLAG_LOOP_HEAD) {
                    loop_ends++;
                }
            }

            if ((bb->_else->flags & BB_FLAG_LOOP_END) && bb->_else->visited != loop_ends) {
                builder->loop_nesting--;
                bb->_else->visited++;
                aoStrCat(builder->viz,"}\n");
            }

            cfgCreatePictureUtil(builder,bb->_else,seen);

            if (bb->_else->flags & BB_FLAG_LOOP_END) {
                cfgPrintBreaks(builder,bb);
            }
            break;
        }

        case BB_SWITCH: {
            bbPrintf(builder,bb);
            for (u64 i = 0; i < bb->next_blocks->size; ++i) {
                BasicBlock *it = vecGet(BasicBlock *,bb->next_blocks,i);
                cfgCreatePictureUtil(builder,it,seen);
            }
            cfgCreatePictureUtil(builder,bb->next,seen);
            break;
        }

        case BB_GOTO:
            bbPrintf(builder,bb);
            if (bbPrevCnt(bb->next) >= 1 || bb->flags & BB_FLAG_UNCONDITIONAL_JUMP) {
                cfgCreatePictureUtil(builder,bb->next,seen);
            }
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
                if (bb->prev && bb->prev->type == BB_DO_WHILE_COND) {
                    cfgCreatePictureUtil(builder,bb->prev->next,seen);
                }
            }
            break;

        case BB_CASE:
        case BB_CONTROL_BLOCK:
        case BB_HEAD_BLOCK: {
            bbPrintf(builder,bb);
            cfgCreatePictureUtil(builder,bb->next,seen);
            break;
        }

        case BB_BREAK_BLOCK:
            builder->break_blocks[builder->break_idx++] = bb;
            break;

        case BB_END_BLOCK:
        case BB_RETURN_BLOCK:
            if (!(builder->return_idx < 32)) {
                assert(builder->return_idx < 32);
            }
            builder->return_statements[builder->return_idx++] = bb;
            break;

        default: {
            char *error = cfgGraphVizError(builder,bb,
                    CFG_GRAPHVIZ_ERROR_INVALID_TYPE);
            loggerWarning("%s\n",error);
            break;
        }
    }

    /* XXX: hack for do while loop */
    if (while_cond) {
        cfgDoWhileCondPrintf(builder,while_cond);
        if (while_cond->flags & BB_FLAG_LOOP_END) {
            builder->loop_nesting--;
            aoStrCat(builder->viz,"}\n");
        }

        if (while_cond->next) {
            cfgCreatePictureUtil(builder,while_cond->next,seen);
        }

        if (while_cond->flags & BB_FLAG_LOOP_END) {
            cfgPrintBreaks(builder,while_cond);
        }
    }
}

static void cfgCreatePicture(CfgGraphVizBuilder *builder, CFG *cfg) {
    Set *seen = setNew(32, &set_int_type);
    builder->return_idx = 0;
    builder->break_idx = 0;
    builder->loop_idx = 0;
    builder->cfg = cfg;
    cfgCreatePictureUtil(builder,cfg->head,seen);
    /* Returns always exit the scope of where ever they are placed, so saving 
     * them till the end makes sense */
    for (int i = 0; i < builder->return_idx; ++i) {
        cfgReturnPrintf(builder,builder->return_statements[i]);
    }
    setRelease(seen);
}

static void cfgGraphVizAddMappings(CfgGraphVizBuilder *builder, CFG *cfg) {
    loggerDebug("Creating mappings for: %s, size: %lu\n",
            cfg->ref_fname->data,cfg->no_to_block->size);
    MapIter it;
    mapIterInit(cfg->no_to_block, &it);
    while (mapIterNext(&it)) {
        BasicBlock *cur = (BasicBlock *)it.node->value;

        switch (cur->type) {
            case BB_CONTINUE: {
                aoStrCatPrintf(builder->viz,
                        BB_FMT_BLUE_DOTTED,
                        cur->block_no,
                        cur->prev->block_no);
                break;
            }

            case BB_LOOP_BLOCK: {
                aoStrCatPrintf(builder->viz,
                        BB_FMT_BLUE_DOTTED,
                        cur->block_no,
                        cur->prev->block_no);
                if (cur->flags & BB_FLAG_UNCONDITIONAL_JUMP && cur->next) {
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_BLACK_SOLID,
                            cur->block_no,
                            cur->next->block_no);
                }
                break;
            }

            case BB_DO_WHILE_COND:
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_BLUE_DOTTED,
                            cur->block_no,
                            cur->prev->block_no);
                    if (cur->next) {
                        aoStrCatPrintf(builder->viz,
                                BB_FMT_RED_SOLID,
                                cur->block_no,
                                cur->next->block_no);
                    }
                break;

            case BB_BRANCH_BLOCK:
                aoStrCatPrintf(builder->viz,
                        BB_FMT_RED_SOLID,
                        cur->block_no,
                        cur->_else->block_no);
                aoStrCatPrintf(builder->viz,
                        BB_FMT_GREEN_SOLID,
                        cur->block_no,
                        cur->_if->block_no);
                break;

            case BB_GOTO:
                if (cur->flags & (BB_FLAG_GOTO_LOOP)) {
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_BLUE_DOTTED,
                            cur->block_no,
                            cur->prev->block_no);
                } else {
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_BLACK_SOLID,
                            cur->block_no,
                            cur->next->block_no);
                }
                break;

            case BB_CONTROL_BLOCK:
            case BB_HEAD_BLOCK:
            case BB_CASE:
            case BB_BREAK_BLOCK: {
                aoStrCatPrintf(builder->viz, BB_FMT_BLACK_SOLID,
                        cur->block_no,
                        cur->next->block_no);
                break;
            }

            case BB_SWITCH: {
                for (u64 i = 0; i < cur->next_blocks->size; ++i) {
                    BasicBlock *bb = vecGet(BasicBlock *,cur->next_blocks,i);
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_BLACK_SOLID,
                            cur->block_no,
                            bb->block_no);
                }
                if (cur->next) {
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_BLACK_SOLID,
                            cur->block_no,
                            cur->next->block_no);
                }
                break;
            }

            /* Add a fake node so it is obvious that it is the end */
            case BB_END_BLOCK:
            case BB_RETURN_BLOCK: 
                aoStrCatPrintf(builder->viz,
                        BB_FMT_BLACK_END_NODE,
                        cur->block_no,
                        cur->block_no);
                break;

            default:
                loggerWarning("Unhandled! loopidx=%llu: bb%d %s\n",
                    it.idx,cur->block_no,bbToString(cur));
        }
    }
    loggerDebug("Created mapping for: %s\n", cfg->ref_fname->data);
}


void cfgBuilderWriteToFile(CfgGraphVizBuilder *builder, char *filename);
static void cfgCreateGraphViz(CfgGraphVizBuilder *builder, CFG *cfg) {
    aoStrCatPrintf(builder->viz,
            "overlap=false;\n"
            "subgraph \"cluster_%s\" {\n"
            "    style=\"dashed\";\n"
            "    color=\"black\";\n"
            "    label=\"%s()\";\n",
            cfg->ref_fname->data,
            cfg->ref_fname->data);

    cfgCreatePicture(builder,cfg);
    /* Connects the nodes together in graphviz */
    cfgGraphVizAddMappings(builder,cfg);

    aoStrCatPrintf(builder->viz,"}//ending function\n\n");
}

void cfgBuilderWriteToFile(CfgGraphVizBuilder *builder, char *filename) {
    AoStr *cfg_string = builder->viz;
    int fd = open(filename,O_CREAT|O_TRUNC|O_RDWR,0644);
    if (fd == -1) {
        loggerPanic("Failed to open file '%s': %s\n",
                filename,strerror(errno));
    }

    s64 written = write(fd, cfg_string->data, cfg_string->len);
    if (written != (ssize_t)cfg_string->len) {
        loggerPanic("Failed to write CFG '%s'\n", filename);
    }
    fsync(fd);
    close(fd);
}

void cfgToFile(CFG *cfg, char *filename) {
    CfgGraphVizBuilder builder;

    cfgGraphVizBuilderInit(&builder);
    cfgCreateGraphViz(&builder,cfg);
    cfgBuilderWriteToFile(&builder,filename);
}

void cfgsToFile(Vec *cfgs, char *filename) {
    CfgGraphVizBuilder builder;

    cfgGraphVizBuilderInit(&builder);

    aoStrCat(builder.viz,"digraph user_program {\n ");
    for (u64 i = 0; i < cfgs->size; ++i) {
        cfgCreateGraphViz(&builder,(CFG *)cfgs->entries[i]);
    }
    aoStrCatPrintf(builder.viz, "} // ending graph\n");
    cfgBuilderWriteToFile(&builder,filename);
}
