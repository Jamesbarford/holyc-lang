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
#include "cfg.h"
#include "dict.h"
#include "lexer.h"
#include "util.h"

#define CFG_GRAPHVIZ_ERROR_INVALID_TYPE (0)

#define BB_FMT_BLUE_DOTTED ("    bb%d:s -> bb%d:n [style=\"dotted,bold\",color=blue,weight=10,constraint=false];\n")
#define BB_FMT_RED_SOLID   ("    bb%d:s -> bb%d:n [style=\"solid,bold\",color=firebrick1,weight=10,constraint=true];\n")
#define BB_FMT_GREEN_SOLID ("    bb%d:s -> bb%d:n [style=\"solid,bold\",color=forestgreen,weight=10,constraint=true];\n")
#define BB_FMT_BLACK_SOLID ("    bb%d:s -> bb%d:n [style=\"solid,bold\",color=black,weight=100,constraint=true];\n")
#define BB_FMT_BLACK_END_NODE ("    bb%d:s -> endbb%d:n [style=\"solid,bold\",color=black,weight=100,constraint=true];\n")


static char *depth_to_loop_color[] = {
    [1] = "grey88",
    [2] = "grey78",
    [3] = "grey68",
    [4] = "grey58",
    [5] = "grey48",
    [6] = "grey38",
    [7] = "grey28",
    [8] = "grey28",
    [9] = "grey18",
};

static const char *cfgPrintGetLoopColor(int depth) {
    static int len = sizeof(depth_to_loop_color)/sizeof(depth_to_loop_color[0]);
    if (depth >= len) return "grey88";
    return depth_to_loop_color[depth];
}

typedef struct CfgGraphVizBuilder {
    aoStr *viz;
    CFG *cfg;
    int loop_cnt;

    /* This is if a loop does not have a break clause immediately in it */
    int break_idx;
    BasicBlock *break_blocks[256];

    int loop_idx;
    BasicBlock *loop_stack[32];

    int return_idx;
    BasicBlock *return_statements[32];
} CfgGraphVizBuilder;

static void cfgCreatePictureUtil(CfgGraphVizBuilder *builder,
        BasicBlock *bb, IntMap *seen);

static void cfgGraphVizBuilderInit(CfgGraphVizBuilder *builder) {
    builder->viz = aoStrAlloc(1<<10);
    builder->loop_cnt = 0;
    builder->break_idx = 0;
    builder->loop_idx = 0;
    builder->return_idx = 0;
}

static aoStr *bbAstArrayToString(PtrVec *ast_array, int ast_count) {
    if (ast_count == 0) return NULL;
    aoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = vecGet(Ast *,ast_array,i);
        lvalue_str = astLValueToString(ast,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
        aoStrCatPrintf(ast_str,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(ast_str,'|');
        }
        // free(lvalue_str);
    }
    return ast_str;
}

static void cfgBranchPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    char *fillcolor;
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size-1);
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
    aoStrCatPrintf(builder->viz,
            "if (%s)\\l\\"
            //"  goto \\<%d bb\\>\\l\\"
            //"else\\l\\\n"
            //"  goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            lvalue_str
            //   bb->_if->block_no,
            //  bb->_else->block_no
            );
    // free(lvalue_str);
    aoStrRelease(internal);
}

static void cfgDoWhileCondPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    char *fillcolor = "lightpink";
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size-1);
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

    aoStrCatPrintf(builder->viz,
            "if (%s)\\l\\"
            // "  goto \\<%d bb\\>\\l\\"
          //  "else\\l\\\n"
        //     "  goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            lvalue_str
            // bb->prev->block_no,
            // bb->next->block_no
            );
    aoStrRelease(internal);
}

static void cfgLoopPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgreen,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"%s\n}\"];\n\n",internal->data);
    } else {
        aoStrCatPrintf(builder->viz,
                //           "|goto \\<%d bb\\>\\l\\"
                "}\"];\n\n"
                //     bb->prev->block_no
                );
    }


    aoStrRelease(internal);
}

static void cfgBreakPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStr *internal = 
        bbAstArrayToString(bb->ast_array,bb->ast_array->size);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=violet,label=\"{\\<bb %d\\>\n",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"%s\\l\\\n",internal->data);
    }
    aoStrCatPrintf(builder->viz,
            "|break\\l\\\n"
            //"|goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n"
            // bb->next->block_no
            );
    aoStrRelease(internal);
}

static void cfgDefaultPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);
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
    aoStrRelease(internal);
}

static void cfgHeadPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=circle,style=filled,fillcolor=white,label=\"Entry\"];\n",
            bb->block_no,
            bb->block_no);
}

static void cfgReturnPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);

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
    aoStrRelease(internal);
}

static void cfgCasePrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    int ast_count = bb->ast_array->size;
    PtrVec *ast_array = bb->ast_array;
    aoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    Ast *_case = vecGet(Ast *,ast_array,0);

    for (int i = 1; i < ast_count; ++i) {
        Ast *ast = vecGet(Ast *,ast_array,i);
        lvalue_str = astLValueToString(ast,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
        aoStrCatPrintf(ast_str,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(ast_str,'|');
        }
        free(lvalue_str);
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

    // free(lvalue_str);
    aoStrRelease(ast_str);
}

static void cfgSwitchPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size-1);
    
    Ast *ast = vecGet(Ast *,bb->ast_array,bb->ast_array->size - 1);
    lvalue_str = astLValueToString(ast,(LEXEME_ENCODE_PUNCT|LEXEME_GRAPH_VIZ_ENCODE_PUNCT));
    aoStrCatPrintf(ast_str,"test (%s)\\l\\\n",lvalue_str);
   // free(lvalue_str);

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
    aoStrRelease(ast_str);
    aoStrRelease(internal);
}

static void cfgContinuePrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    aoStr *internal = bbAstArrayToString(bb->ast_array,bb->ast_array->size);
    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=violet,label=\"{\\<bb %d\\>\n",
            bb->block_no,
            bb->block_no);

    if (internal) {
        aoStrCatPrintf(builder->viz,"|%s",internal->data);
    }

    aoStrCatPrintf(builder->viz,
            "|continue\\l\\\n"
            //"|goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n"
            // bb->next->block_no
            );
}

/* Side-effect of mutating the loop counter on the builder */
static void cfgLoopHeadPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    int cnt = ++builder->loop_cnt;
    const char *loop_color = cfgPrintGetLoopColor(cnt);
    aoStrCatPrintf(builder->viz,
            "subgraph cluster1_%d {\nstyle=\"filled\";\n"
            "color=\"darkgreen\";\n"
            "fillcolor=\"%s\";\n"
            "label=\"loop %d\";\n"
            "labeljust=l;\n"
            "penwidth=2;\n",
            cnt,loop_color,cnt);
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


/* how many of the previous blocks were gotos? */
int cfgGraphVizGetLoopGotoCount(BasicBlock *bb) {
    int goto_cnt = 0;
    for (int i = 0; i < bb->prev_cnt; ++i) {
        if (bb->prev_blocks[i]->type == BB_GOTO) {
            goto_cnt++;
        }
    }
    return goto_cnt;
}

BasicBlock *cfgFindLoop(BasicBlock *bb) {
    if (!(bb->flags & BB_FLAG_LOOP_HEAD)) return NULL;
    for (int i = 0; i < bb->prev_cnt; ++i) {
        BasicBlock *prev = bb->prev_blocks[i];
        if (prev->flags & BB_FLAG_LOOP_END) {
            return prev;
        }
    }
    return NULL;
}

int cfgGraphVizGetLoopBreakCount(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    if (builder->loop_idx - 1 < 0) return 0;
    BasicBlock *head = builder->loop_stack[builder->loop_idx-1];
    if (builder->break_idx == 0) return 0;
    if (builder->break_blocks[builder->break_idx-1] == head) return 0;

    int i = 0;
    for (int j = builder->break_idx-1; j >= 0; --j) {
        BasicBlock *block = builder->break_blocks[j];
        if (block->type != BB_BREAK_BLOCK) {
            if (block == head) {
                return i;
            }
            return 0;
        }
        i++;
    }
    return 0;
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

static BasicBlock *cfgGetHandleDoWhileHead(CfgGraphVizBuilder *builder, 
        IntMap *seen, BasicBlock *bb)
{
    BasicBlock *while_cond = NULL;
    if (bb->flags & BB_FLAG_LOOP_HEAD) {
        for (int i = 0; i < bb->prev_cnt; ++i) {
            BasicBlock *prev = bb->prev_blocks[i];
            if (prev->flags & BB_DO_WHILE_COND && prev->prev == bb) {
                while_cond = prev;
                break;
            }
        }

        if (while_cond) {
            intMapSet(seen,while_cond->block_no,NULL);
        }
    }
    return while_cond;
}

char *cfgGraphVizError(CfgGraphVizBuilder *builder, BasicBlock *bb, int error_code) {
    switch (error_code) {
        case CFG_GRAPHVIZ_ERROR_INVALID_TYPE: {
            char *error = mprintf("%s() has an invalid type '%s' for bb%d\n",
                    builder->cfg->ref_fname->data, bbTypeToString(bb->type), bb->block_no);
            return error;
        }
        default:
            loggerPanic("Unknown error code: %d\n", error_code);
    }

}

static void cfgCreatePictureUtil(CfgGraphVizBuilder *builder,
        BasicBlock *bb, IntMap *seen)
{
    BasicBlock *while_cond = cfgGetHandleDoWhileHead(builder,seen,bb);

    if (intMapHas(seen,bb->block_no)) return;
    else intMapSet(seen,bb->block_no,NULL);

    if (bb->flags & BB_FLAG_LOOP_HEAD) {
        cfgLoopHeadPrintf(builder,bb);
    }

    switch (bb->type) {
        case BB_CONTINUE:
        case BB_LOOP_BLOCK:
            bbPrintf(builder,bb);
            if (bb->flags & BB_FLAG_UNCONDITIONAL_JUMP && bb->next) {
                cfgCreatePictureUtil(builder,bb->next,seen);
            }
            cfgCreatePictureUtil(builder,bb->prev,seen);
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

            if (bb->flags & BB_FLAG_LOOP_END) {
                aoStrCat(builder->viz,"}\n");
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

        case BB_BRANCH_BLOCK:
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
                builder->loop_stack[builder->loop_idx++] = bb;
                /* Push the head onto the stack so that we can bookend the 
                 * break clauses and scope them to the current loop */
                builder->break_blocks[builder->break_idx++] = bb;
            }

            cfgBranchPrintf(builder,bb);
            cfgCreatePictureUtil(builder,bb->_if,seen);

            if (bb->_else->flags & BB_FLAG_LOOP_END) {
                aoStrCat(builder->viz,"}\n");
            }

            cfgCreatePictureUtil(builder,bb->_else,seen);

            if (bb->_else->flags & BB_FLAG_LOOP_END) {
                cfgPrintBreaks(builder,bb);
            }
            break;

        case BB_SWITCH: {
            bbPrintf(builder,bb);
            for (int i = 0; i < bb->next_blocks->size; ++i) {
                BasicBlock *it = vecGet(BasicBlock *,bb->next_blocks,i);
                cfgCreatePictureUtil(builder,it,seen);
                //if (it->next != bb->next) {
                //    cfgCreatePictureUtil(builder,it->next,seen);
                //}
            }
            cfgCreatePictureUtil(builder,bb->next,seen);
            break;
        }

        case BB_GOTO:
            bbPrintf(builder,bb);
            if (bb->next->prev_cnt == 1 || bb->flags & BB_FLAG_UNCONDITIONAL_JUMP) {
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
            free(error);
        }
    }

    /* XXX: hack for do while loop */
    if (while_cond) {
        cfgDoWhileCondPrintf(builder,while_cond);
        if (while_cond->flags & BB_FLAG_LOOP_END) {
            aoStrCat(builder->viz,"}\n");
        }

        cfgCreatePictureUtil(builder,while_cond->next,seen);

        if (while_cond->flags & BB_FLAG_LOOP_END) {
            cfgPrintBreaks(builder,while_cond);
        }
    }
}

static void cfgCreatePicture(CfgGraphVizBuilder *builder, CFG *cfg) {
    IntMap *seen = intMapNew(32);
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
    intMapRelease(seen);
}

static void cfgGraphVizAddMappings(CfgGraphVizBuilder *builder, CFG *cfg) {
    IntMap *map = cfg->no_to_block;
    loggerDebug("Creating mappings for: %s, size: %lu\n",
            cfg->ref_fname->data,map->size);

    for (int i = 0; i < map->size; ++i) {
        long idx = map->indexes[i];
        IntMapNode *node = map->entries[idx];
        if (!node) {
            for (int j = 0; j < map->size; ++j) {
                printf("%ld ",map->indexes[j]);
            }
            printf("\n");
            loggerPanic("Failed while creating mappings for: %s idx=%d, block_no=%ld\n",
                    cfg->ref_fname->data,i,map->indexes[i]);
        }

        BasicBlock *cur = (BasicBlock *)node->value;

        switch (cur->type) {
            case BB_CONTINUE:
                aoStrCatPrintf(builder->viz,
                        BB_FMT_BLUE_DOTTED,
                        cur->block_no,
                        cur->prev->block_no);
                break;

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
                    aoStrCatPrintf(builder->viz,
                            BB_FMT_RED_SOLID,
                            cur->block_no,
                            cur->next->block_no);
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
                for (int i = 0; i < cur->next_blocks->size; ++i) {
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
                loggerWarning("Unhandled! loopidx=%d: bb%d %s\n",
                    i,cur->block_no,bbToString(cur));
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

    aoStrCatPrintf(builder->viz,"}//ending function\n");
}

void cfgBuilderWriteToFile(CfgGraphVizBuilder *builder, char *filename) {
    aoStr *cfg_string = builder->viz;
    int fd = open(filename,O_CREAT|O_TRUNC|O_RDWR,0644);
    if (fd == -1) {
        loggerPanic("Failed to open file '%s': %s\n",
                filename,strerror(errno));
    }
    assert(write(fd,cfg_string->data,cfg_string->len) == cfg_string->len);
    fsync(fd);
    close(fd);
    aoStrRelease(cfg_string);
}

void cfgToFile(CFG *cfg, char *filename) {
    CfgGraphVizBuilder builder;

    cfgGraphVizBuilderInit(&builder);
    cfgCreateGraphViz(&builder,cfg);
    cfgBuilderWriteToFile(&builder,filename);
}

void cfgsToFile(PtrVec *cfgs, char *filename) {
    CfgGraphVizBuilder builder;

    cfgGraphVizBuilderInit(&builder);

    aoStrCat(builder.viz,"digraph user_program {\n ");
    for (int i = 0; i < cfgs->size; ++i) {
        cfgCreateGraphViz(&builder,(CFG *)cfgs->entries[i]);
    }
    aoStrCatPrintf(builder.viz, "} // ending graph\n");
    cfgBuilderWriteToFile(&builder,filename);
}
