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

char *bbPreviousBlockNumbersToString(BasicBlock *bb) {
    aoStr *str = aoStrNew();
    aoStrCatPrintf(str,"prev_cnt = %d: ",bb->prev_cnt);
    for (int i = 0; i < bb->prev_cnt; ++i) {
        aoStrCatPrintf(str,"%dbb", bb->prev_blocks[i]->block_no);
        if (i + 1 != bb->prev_cnt) aoStrCatPrintf(str,", ");
    }
    return aoStrMove(str);
}


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
    List *loop_queue;
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
    builder->loop_queue = ListNew();
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

static aoStr *bbAstArrayToString(AstArray *ast_array, int ast_count) {
    aoStr *ast_str = aoStrAlloc(256);
    char *lvalue_str;
    for (int i = 0; i < ast_count; ++i) {
        Ast *ast = ast_array->entries[i];
        lvalue_str = AstLValueToString(ast,LEXEME_ENCODE_PUNCT);
        aoStrCatPrintf(ast_str,"%s\\l\\\n",lvalue_str);
        if (i + 1 != ast_count) {
            aoStrPutChar(ast_str,'|');
        }
        free(lvalue_str);
    }
    return ast_str;
}

static void bbPrintInfo(BasicBlock *bb) {
    char *prev = bbPreviousBlockNumbersToString(bb);
    printf("bb%2d type = %-*s flags = %-*s, prev=%s\n\n",bb->block_no,
            18,
            bbTypeToString(bb->type),
            40,
            bbFlagsToString(bb->flags),
            prev);
    free(prev);

}

static void cfgBranchPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *fillcolor;
    int ast_count = ast_array->count;
    aoStr *internal = bbAstArrayToString(bb->ast_array,ast_count-1);
    Ast *cond = ast_array->entries[ast_count - 1];

    assert(cond != NULL);
    char *lvalue_str = AstLValueToString(cond,LEXEME_ENCODE_PUNCT);

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

    aoStrCatPrintf(builder->viz,
            "%s\n"
            "if (%s)\\l\\"
            //"  goto \\<%d bb\\>\\l\\"
            //"else\\l\\\n"
            //"  goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data,
            lvalue_str
         //   bb->_if->block_no,
          //  bb->_else->block_no
            );
    aoStrRelease(internal);
}

static void cfgDoWhileCondPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    char *fillcolor = "lightpink";
    int ast_count = ast_array->count;
    aoStr *internal = bbAstArrayToString(ast_array,ast_count-1);
    Ast *cond = ast_array->entries[ast_count - 1];

    if (bb->prev->flags & BB_FLAG_LOOP_HEAD) {
        fillcolor = "lightskyblue";
    }

    char *lvalue_str = AstLValueToString(cond,LEXEME_ENCODE_PUNCT);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=%s,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            fillcolor,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\n"
            "if (%s)\\l\\"
            // "  goto \\<%d bb\\>\\l\\"
          //  "else\\l\\\n"
        //     "  goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data,
            lvalue_str
            // bb->prev->block_no,
            // bb->next->block_no
            );
    aoStrRelease(internal);
}

static void cfgLoopPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    int ast_count = ast_array->count;
    aoStr *internal = bbAstArrayToString(ast_array,ast_count);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgreen,label=\"{\\<bb %d\\>|\n",
            bb->block_no,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\n"
 //           "|goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data
        //     bb->prev->block_no
            );

    aoStrRelease(internal);
}

static void cfgBreakPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    int ast_count = ast_array->count;
    aoStr *internal = bbAstArrayToString(ast_array,ast_count);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=violet,label=\"{\\<bb %d\\>\n",
            bb->block_no,
            bb->block_no);

    aoStrCatPrintf(builder->viz,
            "%s\\l\\\n"
            "|break\\l\\\n"
            //"|goto \\<%d bb\\>\\l\\"
            "\n}\"];\n\n",
            internal->data
            // bb->next->block_no
            );
    aoStrRelease(internal);
}

static void cfgDefaultPrintf(CfgGraphVizBuilder *builder, BasicBlock *bb) {
    AstArray *ast_array = bb->ast_array;
    int ast_count = ast_array->count;
    aoStr *internal = bbAstArrayToString(ast_array,ast_count); 

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=record,style=filled,fillcolor=lightgrey,label=\"{\\<bb %d\\>|\n%s",
            bb->block_no,
            bb->block_no,
            internal->data);

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
    AstArray *ast_array = bb->ast_array;
    int ast_count = ast_array->count;
    aoStr *internal = bbAstArrayToString(ast_array,ast_count);

    aoStrCatPrintf(builder->viz,
            "    bb%d [shape=doublecircle,style=filled,fontcolor=white,fillcolor=black,label=\" \\<bb %d\\>\n %s \"];\n\n",
            bb->block_no,
            bb->block_no,
            internal->data);
    aoStrRelease(internal);
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

static int bbIsLoopEnd(BasicBlock *bb) {
    if (bb->type == BB_LOOP_BLOCK || bb->type == BB_DO_WHILE_COND) { //&& bb->flags & BB_FLAG_ELSE_BRANCH) { 
        return 1;
       // for (int i = 0; i < bb->prev_cnt; ++i) {
       //     BasicBlock *it = bb->prev_blocks[i];
       //     if (it->flags & BB_FLAG_LOOP_HEAD) {
       //         return 1;
       //     }
       // }
    }
    return 0;
}

static void cfgCreateGraphVizShapes(CfgGraphVizBuilder *builder,
        BasicBlock *bb)
{
    BasicBlock *_if, *_else;

    for (; bb; bb = bb->next) {
        _if = bb->_if;
        _else = bb->_else;

        bb->visited++;
        if (bb->type == BB_LOOP_BLOCK) {
            if (bb->visited > 1 && bb->visited == bb->prev_cnt) {
                aoStrCatPrintf(builder->viz, "}//1: terminating bb%d\n",bb->block_no);
            }
        }

        if (cfgGraphVizBuilderHasSeen(builder,bb->block_no)) return;
        else cfgGraphVizBuilderSetSeen(builder,bb->block_no);

        loggerDebug("bb%d type: %s flags: %s\n",bb->block_no,
                bbTypeToString(bb->type),bbFlagsToString(bb->flags));
        printf("bb%2d prev_cnt = %d: ", bb->block_no,bb->prev_cnt);
        for (int i = 0; i < bb->prev_cnt; ++i) {
            printf("%dbb", bb->prev_blocks[i]->block_no);
            if (i + 1 != bb->prev_cnt) printf(", ");
        }
        printf("\n");


        /* @Cleanup
         * We could do this thing in the cfg.c file and then remove the loop 
         * head if there is an immeidate 'break'. This is not urgent */
        if (bb->flags & BB_FLAG_LOOP_HEAD) {
            if (!_if || (_if && _if->type != BB_BREAK_BLOCK)) {
                int cnt = ++builder->loop_cnt;
                const char *loop_color = cfgPrintGetLoopColor(cnt);
                aoStrCatPrintf(builder->viz,"subgraph cluster1_%d {\nstyle=\"filled\";\n"
                        "color=\"darkgreen\";\n"
                        "fillcolor=\"%s\";\n"
                        "label=\"loop %d\";\n"
                        "labeljust=l;\n"
                        "penwidth=2;\n",cnt,loop_color,cnt);
            }
        }

        int is_loop_block = bb->type == BB_LOOP_BLOCK && 
                            bb->type == BB_DO_WHILE_COND;

        int is_do_while_end = bb->flags & BB_FLAG_LOOP_END && 
                              bb->type == BB_DO_WHILE_COND;

        if (is_do_while_end) {
            loggerDebug("YUP: bb%d\n", bb->block_no);
        }

        int is_loop_end = bbIsLoopEnd(bb);

        if (is_loop_end) {
            aoStrCatPrintf(builder->viz,"}// end: bb%d\n",bb->block_no);
        }


   //     if (bb->flags & BB_FLAG_LOOP_END && !is_loop_block) {
   //         if (bb->visited >= 1 && bb->visited == bb->prev_cnt) {
   //             aoStrCatPrintf(builder->viz, "}//2: terminating loop: bb%d\n",
   //                     bb->block_no);
   //         }
   //     }

        switch (bb->type) {
            case BB_HEAD_BLOCK:    cfgHeadPrintf(builder,bb);    break;
            case BB_LOOP_BLOCK:    cfgLoopPrintf(builder,bb);    break;
            case BB_DO_WHILE_COND: cfgDoWhileCondPrintf(builder,bb);  break;
            case BB_BRANCH_BLOCK:  cfgBranchPrintf(builder,bb);       break;
            case BB_BREAK_BLOCK:   cfgBreakPrintf(builder,bb);        break;
            case BB_END_BLOCK:
            case BB_RETURN_BLOCK:  cfgReturnPrintf(builder,bb);  break;
            default:               cfgDefaultPrintf(builder,bb); break;
        }


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
        }

//        if (bb->flags & BB_FLAG_LOOP_END) {// && !is_loop_block) {
//            if (bb->visited >= 1 && bb->prev_cnt == 1) {
//                aoStrCatPrintf(builder->viz, "}//3: terminating bb%d\n",bb->block_no);
//            }
//        }

        /* If we have a goto we do not want to print it's bb->next as 
         * the code is non-linear and we will eventually get there. The 
         * link will already be printed */
        //if (bb->type == BB_GOTO && !(bb->flags & BB_FLAG_UNCONDITIONAL_JUMP)) break;
        if (bb->type == BB_GOTO && is_loop_block) {
            bb->next->visited++;
            break;
        }
        if (bb->type == BB_END_BLOCK) break;
    }
}

static void bbFindAllLoopNodes(BasicBlock *loop_head, BasicBlock *bb, IntMap *nodes) {
    if (bb->flags & BB_FLAG_LOOP_END) {
        if (bb->prev == loop_head) {
            loggerWarning("bb%d ended by bb%d\n", loop_head->block_no, bb->block_no);
            return;
        }
    }

    switch (bb->type) {
        case BB_LOOP_BLOCK:
            if (bb == loop_head) {
                intMapSet(nodes,bb->block_no,NULL);
            }
            break;

        case BB_DO_WHILE_COND:
            intMapSet(nodes,bb->block_no,NULL);
            bbFindAllLoopNodes(loop_head,bb->next,nodes);
            break;

        case BB_BRANCH_BLOCK:
            intMapSet(nodes,bb->block_no,NULL);
            bbFindAllLoopNodes(loop_head,bb->_if,nodes);
            bbFindAllLoopNodes(loop_head,bb->_else,nodes);
            break;

        case BB_HEAD_BLOCK:
            break;
 
        case BB_GOTO:
            intMapSet(nodes,bb->block_no,NULL);
            bbFindAllLoopNodes(loop_head,bb->next,nodes);
            break;

        case BB_CONTROL_BLOCK:
            intMapSet(nodes,bb->block_no,NULL);
            bbFindAllLoopNodes(loop_head,bb->next,nodes);
            break;
        
        case BB_BREAK_BLOCK: 
            intMapSet(nodes,bb->block_no,NULL);
            bbFindAllLoopNodes(loop_head,bb->next,nodes);
            // cfgBreakPrintf(builder,bb);
            // cfgCreatePictureUtil(builder,map,bb->next,seen);
            break;

        case BB_END_BLOCK:
        case BB_RETURN_BLOCK:
            break;
    }
}

static void cfgCreatePictureUtil(CfgGraphVizBuilder *builder,
        IntMap *map, BasicBlock *bb, IntMap *seen)
{
    if (intMapHas(seen,bb->block_no)) return;
    else intMapSet(seen,bb->block_no,NULL);

    switch (bb->type) {
        /* I want to visit the if block first then the else */
        case BB_LOOP_BLOCK:
            cfgLoopPrintf(builder,bb);
            break;

        case BB_DO_WHILE_COND:
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
                IntMap *nodes = intMapNew(16);
                intMapSet(nodes,bb->block_no,NULL);
                bbFindAllLoopNodes(bb,bb,nodes);
                long *indexes = nodes->indexes->entries;

                ListAppend(builder->loop_queue,bb);
                loggerWarning("All nodes for loop  bb%d\n", bb->block_no);
                for (int i = 0; i < nodes->size; ++i) {
                    int idx = (int)indexes[i];
                    int block_no = (int)map->entries[idx]->key;
                    loggerDebug("  %d\n", block_no);
                }
            }
            cfgDoWhileCondPrintf(builder,bb);
            cfgCreatePictureUtil(builder,map,bb->next,seen);
            break;

        case BB_BRANCH_BLOCK:
            if (bb->flags & BB_FLAG_LOOP_HEAD) {
                IntMap *nodes = intMapNew(16);
                intMapSet(nodes,bb->block_no,NULL);
                bbFindAllLoopNodes(bb,bb,nodes);
                long *indexes = nodes->indexes->entries;

                ListAppend(builder->loop_queue,bb);
                loggerWarning("All nodes for loop  bb%d\n", bb->block_no);
                for (int i = 0; i < nodes->size; ++i) {
                    int idx = (int)indexes[i];
                    int block_no = (int)map->entries[idx]->key;
                    loggerDebug("  idx=%d block_no=%d\n",idx, block_no);
                }
            }
            cfgBranchPrintf(builder,bb);
            cfgCreatePictureUtil(builder,map,bb->_if,seen);
            cfgCreatePictureUtil(builder,map,bb->_else,seen);
            break;

        case BB_HEAD_BLOCK:
            cfgHeadPrintf(builder,bb);
            cfgCreatePictureUtil(builder,map,bb->next,seen);
            break;
 
        case BB_GOTO:
            cfgDefaultPrintf(builder,bb);
            cfgCreatePictureUtil(builder,map,bb->next,seen);
            break;

        case BB_CONTROL_BLOCK:
            cfgDefaultPrintf(builder,bb);
            cfgCreatePictureUtil(builder,map,bb->next,seen);
            break;
        
        case BB_BREAK_BLOCK: 
            cfgBreakPrintf(builder,bb);
            cfgCreatePictureUtil(builder,map,bb->next,seen);
            break;

        case BB_END_BLOCK:
        case BB_RETURN_BLOCK:
            cfgReturnPrintf(builder,bb);
            break;

        default:
            loggerWarning("how?\n");
    }
}

static void cfgCreatePicture(CfgGraphVizBuilder *builder, CFG *cfg) {
    IntMap *map = cfg->graph;
    IntMap *seen = intMapNew(32);
    long *index_entries = map->indexes->entries;
    cfgCreatePictureUtil(builder,map,cfg->head,seen);
    //for (int i = 0; i < map->size; ++i) {
    //    long idx = index_entries[i];
    //    BasicBlock *bb = &(cfg->head[idx-1]);

    //    switch (bb->type) {
    //        case BB_LOOP_BLOCK:
    //            break;

    //        case BB_DO_WHILE_COND:
    //            break;

    //        case BB_BRANCH_BLOCK:
    //            break;

    //        case BB_HEAD_BLOCK:
    //            loggerDebug("hit head block\n");
    //        case BB_GOTO:
    //        case BB_CONTROL_BLOCK:
    //        case BB_BREAK_BLOCK: 
    //            break;

    //            /* These don't goto anything */
    //        case BB_END_BLOCK:
    //        case BB_RETURN_BLOCK: 
    //            break;
    //        default:
    //            loggerWarning("how?\n");
    //    }
    //}
}

static void cfgGraphVizAddMappings(CfgGraphVizBuilder *builder, CFG *cfg) {
    IntMap *map = cfg->graph;
    long *index_entries = map->indexes->entries;

    for (int i = 0; i < map->size; ++i) {
        long idx = index_entries[i];
        BasicBlock *cur = &cfg->head[idx-1];

        switch (cur->type) {
            case BB_LOOP_BLOCK:
                aoStrCatPrintf(builder->viz,
                        "    bb%d:s -> bb%d:n [style=\"dotted,bold\",color=blue,weight=10,constraint=false];\n",
                        cur->block_no,
                        cur->prev->block_no);
                break;

            case BB_DO_WHILE_COND:
                    aoStrCatPrintf(builder->viz,
                            "    bb%d:s -> bb%d:n [style=\"dotted,bold\",color=blue,weight=10,constraint=false];\n",
                            cur->block_no,
                            cur->prev->block_no);
                    aoStrCatPrintf(builder->viz,
                            "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=firebrick1,weight=10,constraint=true];\n",
                            cur->block_no,
                            cur->next->block_no);
                break;

            case BB_BRANCH_BLOCK:
                aoStrCatPrintf(builder->viz,
                        "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=firebrick1,weight=10,constraint=true];\n",
                        cur->block_no,
                        cur->_else->block_no);
                aoStrCatPrintf(builder->viz,
                        "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=forestgreen,weight=10,constraint=true];\n",
                        cur->block_no,
                        cur->_if->block_no);
                break;

            case BB_HEAD_BLOCK:
                loggerDebug("hit head block\n");
            case BB_GOTO:
            case BB_CONTROL_BLOCK:
            case BB_BREAK_BLOCK: 
                aoStrCatPrintf(builder->viz,
                        "    bb%d:s -> bb%d:n [style=\"solid,bold\",color=black,weight=100,constraint=true];\n",
                        cur->block_no,
                        cur->next->block_no);
                break;

                /* These don't goto anything */
            case BB_END_BLOCK:
            case BB_RETURN_BLOCK: 
                break;
            default:
                loggerWarning("how?\n");
        }
    }
}

aoStr *cfgCreateGraphViz(CFG *cfg) {
    CfgGraphVizBuilder builder;
    cfgGraphVizBuilderInit(&builder,cfg);

    aoStrCatPrintf(builder.viz,
            "digraph \"%s\" {\n "
            "overlap=false;\n"
            "subgraph \"cluster_%s\" {\n"
            "    style=\"dashed\";\n"
            "    color=\"black\";\n"
            "    label=\"%s()\";\n",
            cfg->ref_fname->data,
            cfg->ref_fname->data,
            cfg->ref_fname->data);

    cfgCreatePicture(&builder,cfg);
    /* Connects the nodes together in graphviz */
    cfgGraphVizAddMappings(&builder,cfg);

    aoStrCatPrintf(builder.viz,"}//ending function\n} // ending graph\n");
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
