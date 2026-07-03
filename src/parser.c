#include <setjmp.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "aostr.h"
#include "asm.h"
#include "ast.h"
#include "cctrl.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "prslib.h"
#include "prsasm.h"
#include "prsutil.h"
#include "util.h"

#define MAX_ALIGN         16

/* PARSER Prototypes */
Ast *parseStatement(Cctrl *cc);
Ast *parseIfStatement(Cctrl *cc);
Ast *parseForStatement(Cctrl *cc);
Ast *parseVariableInitialiser(Cctrl *cc, Ast *var, s64 terminator_flags);
Ast *parseDecl(Cctrl *cc);
Ast *parseDeclOrStatement(Cctrl *cc);
Ast *parseCompoundStatement(Cctrl *cc);
Ast *parseTryStatement(Cctrl *cc);
Ast *parseThrowStatement(Cctrl *cc);
AstType *parseClassDef(Cctrl *cc, int is_intrinsic);
AstType *parseUnionDef(Cctrl *cc);

/* TempleOS-style `reg <REG>` / `noreg` modifier after a decl type.
 * If the next token is `reg`, consume it plus the following register-
 * name identifier; if `noreg`, consume it. Otherwise leave the token
 * stream alone and report LVAR_AUTO. Shared with parseParams in
 * prslib.c for the parameter-list case. */
void parseRegModifier(Cctrl *cc, int *kind_out, AoStr **reg_out) {
    *kind_out = LVAR_AUTO;
    *reg_out = NULL;
    Lexeme *peek = cctrlTokenPeek(cc);
    if (!peek || peek->tk_type != TK_KEYWORD) return;
    if (peek->i64 == KW_REG) {
        cctrlTokenGet(cc);  /* consume `reg` */
        Lexeme *reg_tok = cctrlTokenGet(cc);
        if (!reg_tok || reg_tok->tk_type != TK_IDENT) {
            cctrlRaiseException(cc,
                "`reg` must be followed by a register name, got `%.*s`",
                reg_tok ? reg_tok->len : 0,
                reg_tok ? reg_tok->start : "");
        }
        *kind_out = LVAR_REG;
        *reg_out = aoStrDupRaw(reg_tok->start, reg_tok->len);
    } else if (peek->i64 == KW_NOREG) {
        cctrlTokenGet(cc);  /* consume `noreg` */
        *kind_out = LVAR_NOREG;
    }
}

static AoStr *getRangeLoopIdx(void) {
    static int id = 0;
    return aoStrPrintf("___tmp%d___", ++id);
}

/* Kinda cheating converting it to a string and calling printf */
Ast *parseFloatingCharConst(Cctrl *cc, Lexeme *tok) {
    u64 ch = (unsigned long)tok->i64;
    char str[16];
    Vec *argv = astVecNew();
    int len = 0;
    int real_len = 0;

    while (ch) {
        switch (ch & 0xFF) {
            case '\'': str[len++] = '\\'; str[len++] = '\''; break;
            case '\\': str[len++] = '\\'; str[len++] = '\\'; break;
            case '\"': str[len++] = '\''; str[len++] = '\"'; break;
            case '\b': str[len++] = '\\';  str[len++] = 'b'; break;
            case '\n': str[len++] = '\\';  str[len++] = 'n'; break;
            case '\t': str[len++] = '\\';  str[len++] = 't'; break;
            case '\v': str[len++] = '\\';  str[len++] = 'v'; break;
            case '\f': str[len++] = '\\';  str[len++] = 'f'; break;
            case '\r': str[len++] = '\\';  str[len++] = 'r'; break;

            default: str[len++] = ch & 0xFF; break;
        }
        real_len++;
        ch = ch >> 8;
    }

    Ast *ast = cctrlGetOrSetString(cc,str,len,real_len);
    vecPush(argv, ast);
    cctrlTokenExpect(cc,';');
    return astFunctionCall(ast_void_type,"printf",6,argv);
}

void parseTypeCheckClassFieldInitaliser(Cctrl *cc, AstType *cls_field_type, Ast *init) {
    if (!astTypeCheck(cls_field_type, init, AST_BIN_OP_ASSIGN)) {
        char *cls_field_str = astTypeToColorString(cls_field_type);
        char *init_field = astTypeToColorString(init->type);
        char *var_string = astLValueToString(init,0);
        cctrlWarning(cc,"Incompatible value being assigned to class field expected '%s' got '%s %s'",
                cls_field_str,init_field,var_string);
    }
}

/* Fold a constant aggregate-initialiser element to a literal of the
 * destination element/field type. Global aggregate initialisers are written
 * out as static data, and the data emitters (aarch64DataInternal /
 * jitWriteInit) read each entry's literal value and size it by the entry's
 * own type. Two problems this fixes:
 *   - a bare `7` parses as the default 8-byte integer, so `I32 a[] = {7,8,9}`
 *     would emit `.quad` per element and be read back at the wrong stride;
 *   - an expression element like `-1` or `2+3` is not a literal at all, so
 *     the emitter would read garbage (writing 0 bytes / wrong value).
 * Evaluate the constant expression and rebuild a literal carrying the dest
 * type (masked to width for integers). Non-constant elements are left as-is
 * for their own branches to handle. */
static Ast *parseFoldInitElement(Ast *init, AstType *dst) {
    if (!init || !dst) {
        return init;
    }
    if (init->kind == AST_STRING || init->kind == AST_ARRAY_INIT) {
        return init;
    }
    if (astIsIntType(dst)) {
        int ok = 1;
        s64 v = evalIntConstExprOrErr(init, &ok);
        if (!ok) {
            return init;
        }
        switch (dst->size) {
            case 1: v = dst->issigned ? (s64)(s8)v  : (s64)(u8)v;  break;
            case 2: v = dst->issigned ? (s64)(s16)v : (s64)(u16)v; break;
            case 4: v = dst->issigned ? (s64)(s32)v : (s64)(u32)v; break;
            default: break;
        }
        Ast *lit = astI64Type(v);
        lit->type = astTypeCopy(dst);
        return lit;
    }
    if (dst->kind == AST_TYPE_FLOAT) {
        int ok = 1;
        double d = evalFloatExprOrErr(init, &ok);
        if (!ok) {
            return init;
        }
        Ast *lit = astF64Type(d);
        lit->type = astTypeCopy(dst);
        return lit;
    }
    return init;
}

Ast *parseDeclArrayInitInt(Cctrl *cc, AstType *type) {
    Lexeme *tok = cctrlTokenGet(cc);
    List *initlist;
    Ast *init;

    if (type->ptr && type->ptr->kind == AST_TYPE_CHAR && tok->tk_type == TK_STR) {
        return cctrlGetOrSetString(cc,tok->start,tok->len,tok->i64);
    }

    if (!tokenPunctIs(tok, '{')) {
        cctrlRaiseException(cc,"Expected intializer list starting with '{' got: '%c'",
                (char)tok->i64);
    }

    initlist = listNew();
    u64 i = 0;
    Map *cls_fields = NULL;
    if (type->kind == AST_TYPE_CLASS) {
        cls_fields = type->fields;
    }

    while (1) {
        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, '}')) {
            break;
        }
        /* C99 designated initialisers (`.field = value`) are not part of
         * HolyC; reject them with a clear message instead of feeding the
         * leading `.` into parseExpr (which crashes on the dangling
         * member access). */
        if (tokenPunctIs(tok, '.')) {
            cctrlRaiseException(cc,
                "Designated initialisers ('.field = value') are not "
                "supported; use a positional initialiser list");
        }
        cctrlTokenRewind(cc);
        if (tokenPunctIs(tok,'{')) {
            init = parseDeclArrayInitInt(cc,type->ptr);
            tok = cctrlTokenGet(cc);
            listAppend(initlist,init);
            if (tokenPunctIs(tok,'}')) {
                init = astArrayInit(initlist);
                return init;
            }
            continue;
        } else {
            init = parseExpr(cc,16);
            if (init == NULL) {
                cctrlRaiseExceptionFromTo(cc,NULL,'{','}',"Array initaliser encountered an unexpected token");
            } else if (type->ptr) {
              if ((astGetResultType(AST_BIN_OP_ASSIGN, init->type, type->ptr)) == NULL) {
                  cctrlRaiseException(cc,"Incompatiable types: %s %s",
                          astTypeToString(init->type),
                          astTypeToString(type->ptr));
              }
              init = parseFoldInitElement(init, type->ptr);
            } else if (type->kind == AST_TYPE_CLASS) {
                if (i >= cls_fields->indexes->size) {
                    cctrlRaiseException(cc, 
                            "More initialisers than class fields for class: %s",
                            astTypeToString(type));
                }

                u64 cls_field_idx = (unsigned long)cls_fields->indexes->entries[i];
                MapNode *entry = &cls_fields->entries[cls_field_idx];
                AstType *cls_field_type = entry->value;
                parseTypeCheckClassFieldInitaliser(cc,cls_field_type,init);
                init = parseFoldInitElement(init, cls_field_type);
                i++;
            }
        }
        listAppend(initlist,init);

        tok = cctrlTokenGet(cc);
        if (!tokenPunctIs(tok, ',')) {
            cctrlTokenRewind(cc);
        }
    }
    return astArrayInit(initlist);
}

void parseFlattenAnnonymous(AstType *anon,
                            Map *fields_dict,
                            int offset,
                            int make_copy)
{
    MapIter it;
    mapIterInit(anon->fields, &it);
    while (mapIterNext(&it)) {
        MapNode *n = it.node;
        AstType *base = (AstType *)n->value;
        AstType *type = NULL;
        if (make_copy) {
            type = astTypeCopy(base);
        } else {
            type = base;
        }
        type->offset += offset;

        mapAdd(fields_dict,n->key,type);
    }
}

typedef struct ClsField {
    AstType *type;
    AoStr *field_name;
} ClsField;

ClsField *clsFieldNew(AstType *type, AoStr *field_name) {
    ClsField *f = (ClsField *)malloc(sizeof(ClsField));
    f->type = type;
    f->field_name = field_name;
    return f;
}

List *parseClassOrUnionFields(Cctrl *cc, AoStr *name,
        u32 (*computeSize)(List *), u32 *_size)
{
    u32 size;
    char *fnptr_name;
    int fnptr_name_len;
    Lexeme *tok, *tok_name;
    AstType *next_type = NULL, *base_type = NULL, *field_type = NULL;
    AoStr  *field_name = NULL;
    List *fields_list;

    tok = cctrlTokenGet(cc);

    if (!tokenPunctIs(tok,'{')) {
        cctrlTokenRewind(cc);
        return NULL;
    }

    fields_list = listNew();

    while (1) {
        /* Peek not a consume */
        tok_name = cctrlTokenPeek(cc);
        if (tokenPunctIs(tok_name,'}')) {
            break;
        } else if (cctrlIsKeyword(cc,tok_name->start,tok_name->len)) {
            base_type = parseBaseDeclSpec(cc);
        } else if (name && !memcmp(name->data,tok_name->start,tok_name->len)) {
            base_type = parseBaseDeclSpec(cc);
            if (base_type == NULL) {
                base_type = astClassType(NULL, aoStrDup(name),8,0);
            }
        } else if (tok_name->tk_type == TK_KEYWORD) { 
            switch (tok_name->i64) {
                case KW_UNION: {
                    AstType *_union = parseUnionDef(cc);
                    _union->kind = AST_TYPE_UNION;
                    listAppend(fields_list, clsFieldNew(_union,_union->clsname));
                    cctrlTokenExpect(cc,';');
                    continue;
                }
                case KW_CLASS: {
                    AstType *cls = parseClassDef(cc,0);
                    listAppend(fields_list, clsFieldNew(cls,cls->clsname));
                    cctrlTokenExpect(cc,';');
                    continue;
                }
                default:
                    cctrlRaiseException(cc,"Unexpected keyword: `%.*s` while parsing class %s",
                            tok_name->len,
                            tok_name->start,
                            name->data);
            }
        } else {
            if (name) {
                cctrlRaiseException(cc,"Unexpected type `%.*s` while parsing class %s",
                        tok_name->len, tok_name->start, name->data);
            } else {
                cctrlRewindUntilStrMatch(cc,tok_name->start,tok_name->len,NULL);
                cctrlRaiseException(cc,"Unexpected type `%.*s` while parsing annoymous class",
                        tok_name->len, tok_name->start);
            }
        }

        while (1) {
            next_type = parsePointerType(cc,base_type);
            tok_name = cctrlTokenGet(cc);
            if (!tok_name) {
                cctrlRaiseException(cc, "<type> <variable_name | (> expected got NULL while parsing class %s",
                        name->data);
            }
            if (tok_name->tk_type != TK_IDENT && !tokenPunctIs(tok_name, '(')) {
                cctrlRewindUntilPunctMatch(cc,tok_name->i64,NULL);
                cctrlRaiseException(cc, "Unexpected character `%.*s` while parsing class %s",
                        tok_name->len,tok_name->start,name->data);
            } else if (tokenPunctIs(tok_name, '(')) {
                next_type = parseFunctionPointerType(cc,&fnptr_name,&fnptr_name_len,next_type);
                field_name = aoStrDupRaw(fnptr_name,fnptr_name_len);
            } else  {
                /* XXX: this does not work properly for classes which are not 
                 * pointers as we miss one of the strings */
                next_type = parseArrayDimensions(cc,next_type);
                field_name = aoStrDupRaw(tok_name->start,tok_name->len);
            }

            field_type = astMakeClassField(next_type, 0);
            if (next_type && next_type->clsname) {
                field_type->clsname = next_type->clsname;
            }
            /* The list is here to ease calculating the offset of the class 
             * fields as they have been defined by the programmer... Hashing 
             * loses the ordering.*/
            listAppend(fields_list,clsFieldNew(field_type,field_name));

            tok = cctrlTokenGet(cc);
            if (tokenPunctIs(tok, ',')) {
                continue;
            } else if (tokenPunctIs(tok,';')) {
                break;
            } else {
                cctrlRewindUntilPunctMatch(cc,tok->i64,NULL);
                cctrlRaiseException(cc, "Unexpected token '%.*s' while parsing class %s",
                        tok->len, tok->start, name->data);
           }
        }
    }
    cctrlTokenExpect(cc,'}');
    size = 0;
    size = computeSize(fields_list);

    if (_size) {
        *_size = size;
    }
    return fields_list;
}

u32 CalcUnionSize(List *fields) {
    int max = 0;
    AstType *type;
    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        type = cls_field->type;
        if (max < type->size) {
            max = type->size;
        }
    }
    return max;
}

u32 CalcClassSize(List *fields) {
    u32 offset = 0;
    u32 size = 0;
    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        AstType *type = cls_field->type;
        if (type->size < MAX_ALIGN) size = type->size;
        else                        size = MAX_ALIGN;

        if (size == 0) continue;
        if (offset % size != 0) {
            offset += size - offset % size;
        }

        type->offset = offset;
        offset += type->size;
    }
    return offset;
}

int CalcPadding(int offset, int size) {
    /* Zero-size fields (e.g. trailing `U0 body;` placeholders used as
     * "rest of bytes" markers) need no padding. Guard before `% size`
     * so we don't depend on platform UB for `% 0` (x86 traps, ARM
     * returns the dividend - which here would roll `offset` back to 0
     * and zero out the whole class size). */
    if (size == 0) return 0;
    return offset % size == 0 ? 0 : size - offset % size;
}

Map *parseClassOffsets(Cctrl *cc,
                       int *aligned_size,
                       int *out_align,
                       List *fields,
                       AstType *base_class,
                       AoStr *clsname,
                       int is_intrinsic)
{
    int offset;
    AstType *field;
    Map *fields_dict = astTypeMapNew();
    int max_align = 1;

    /* XXX: Assumes the class definition will be made later */
    if (listEmpty(fields)) {
        *out_align = max_align;
        return fields_dict;
    }

    if (base_class) {
        offset = base_class->size;
        int ba = astTypeAlign(base_class);
        if (ba > max_align) max_align = ba;
        if (cc->flags & CCTRL_SAVE_ANONYMOUS) {
            mapAdd(fields_dict,astAnnonymousLabel(),base_class);
        }
        parseFlattenAnnonymous(base_class,fields_dict,0,1);
    } else {
        offset = 0;
    }

    if (is_intrinsic) {
        *aligned_size = 16;
        *out_align = 8;
        listForEach(fields) {
            ClsField *cls_field = (ClsField *)it->value;
            field = cls_field->type;
            field->offset = offset;
            offset+=field->size;
            if (cls_field->field_name) {
                mapAdd(fields_dict,cls_field->field_name->data,field);
            }
        }

        return fields_dict;
    }

    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        field = cls_field->type;
        AoStr *field_name = cls_field->field_name;
        int fa = astTypeAlign(field);

        if (field_name == NULL && parseIsClassOrUnion(field->kind)) {
            if (cc->flags & CCTRL_SAVE_ANONYMOUS) {
                mapAdd(fields_dict,astAnnonymousLabel(),field);
            }
            offset += CalcPadding(offset, fa);
            parseFlattenAnnonymous(field,fields_dict,offset,0);
            offset += field->size;
        } else {
            if (field->kind == AST_TYPE_POINTER &&
                    (field->ptr->kind == AST_TYPE_CLASS || field->ptr->kind == AST_TYPE_UNION)) {
                if (clsname && field->ptr->clsname) {
                    if (aoStrCmp(field->ptr->clsname, clsname)) {
                        field->fields = fields_dict;
                    }
                }
            }

            offset += CalcPadding(offset, fa);
            field->offset = offset;
            offset += field->size;
        }

        if (fa > max_align) max_align = fa;
        if (field_name) {
            mapAdd(fields_dict,field_name->data,field);
        }
        free(cls_field);
    }

    *out_align = max_align;
    *aligned_size = offset + CalcPadding(offset, max_align);
    return fields_dict;
}

Map *parseUnionOffsets(Cctrl *cc, int *real_size, int *out_align, List *fields) {
    int max_size = 0, max_align = 1;
    AstType *field;
    Map *fields_dict = astTypeMapNew();

    listForEach(fields) {
        ClsField *cls_field = (ClsField *)it->value;
        field = cls_field->type;
        AoStr *field_name = cls_field->field_name;
        int fa = astTypeAlign(field);
        if (fa > max_align) max_align = fa;
        if (max_size < field->size) {
            max_size = field->size;
        }
        if (field->clsname == NULL && parseIsClassOrUnion(field->kind)) {
            if (cc->flags & CCTRL_SAVE_ANONYMOUS) {
                mapAdd(fields_dict,astAnnonymousLabel(),field);
            }
            parseFlattenAnnonymous(field,fields_dict,0,0);
            continue;
        }

        field->offset = 0;
        if (field_name) {
            mapAdd(fields_dict,field_name->data,field);
        }
    }
    *out_align = max_align;
    *real_size = max_size + CalcPadding(max_size, max_align);
    return fields_dict;
}

AstType *parseClassOrUnion(Cctrl *cc, Map *env,
        int is_class,
        u32 (*computeSize)(List *),
        int is_intrinsic)
{
    AoStr *tag = NULL;
    int aligned_size = 0;
    int aligned = 1;
    u32 class_size;
    Lexeme *tok = cctrlTokenGet(cc);
    AstType *prev = NULL, *ref = NULL, *base_class = NULL;
    List *fields = NULL;
    Map *fields_dict;
    cc->localenv = cctrlCreateAstMap(cc->localenv);

    if (tok->tk_type == TK_IDENT) {
        tag = aoStrDupRaw(tok->start,tok->len);

        tok = cctrlTokenGet(cc);
        if (tokenPunctIs(tok, ':')) { // Class inheritance
            if (!is_class) {
                cctrlRaiseException(cc,"Cannot use inheritance with a union");
            }
            tok = cctrlTokenGet(cc);
            if (tok->tk_type != TK_IDENT) {
                cctrlRaiseException(cc, "Expected Identifier for class inheritance");
            }
            base_class = mapGetLen(cc->clsdefs,tok->start,tok->len);
            if (base_class == NULL) {
                cctrlRaiseException(cc,"class %.*s has not been defined\n",
                        tok->len,tok->start);
            }

            if (tokenPunctIs(cctrlTokenPeek(cc),',')) {
                cctrlRaiseException(cc,"Only one base class allowed at this time");
            }
        } else {
            cctrlTokenRewind(cc);
        }
    }

    if (tag) {
        prev = mapGetLen(env, tag->data, tag->len);
    }

    /* Pre-register the tag with an incomplete placeholder so a self-reference
     * like `class T { T *next; };` resolves to this declaration during body
     * parsing, instead of raising "Type T has not been declared". The
     * fields_dict and size are filled in later via the `prev && fields_dict`. */
    if (tag && !prev) {
        prev = astClassType(NULL, tag, 0, is_intrinsic);
        if (!is_class) prev->kind = AST_TYPE_UNION;
        mapAdd(env, tag->data, prev);
    }

    fields = parseClassOrUnionFields(cc,tag,computeSize,&class_size);

    if (prev && !fields) {
        return prev;
    }

    if (is_class) {
        fields_dict = parseClassOffsets(cc,&aligned_size,&aligned,fields,base_class,tag,is_intrinsic);
    } else {
        fields_dict = parseUnionOffsets(cc,&aligned_size,&aligned,fields);
    }
    listRelease(fields,NULL);

    if (prev && fields_dict) {
        prev->fields = fields_dict;
        prev->size = aligned_size;
        prev->alignment = (u32)aligned;
        return prev;
    }

    if (fields_dict) {
        ref = astClassType(fields_dict,tag,aligned_size,is_intrinsic);
        if (base_class) {
            ref->size += base_class->size;
        }
    } else {
        ref = astClassType(NULL,tag,aligned_size,is_intrinsic);
    }
    ref->alignment = (u32)aligned;
    if (!is_class) ref->kind = AST_TYPE_UNION;
    if (tag) {
        mapAdd(env,tag->data,ref);
    }
    return ref;
}

AstType *parseClassDef(Cctrl *cc, int is_intrinsic) {
    return parseClassOrUnion(cc,cc->clsdefs,1,CalcClassSize,is_intrinsic);
}

AstType *parseUnionDef(Cctrl *cc) {
    AstType *_union = parseClassOrUnion(cc,cc->uniondefs,0,CalcUnionSize,0);
    _union->kind = AST_TYPE_UNION;
    return _union;
}

Ast *parseVariableAssignment(Cctrl *cc, Ast *var, s64 terminator_flags) {
    Ast *init;
    int len;
    Lexeme *peek = cctrlTokenPeek(cc);
    assert(var);

    if (var->type->kind == AST_TYPE_ARRAY) {
        init = parseDeclArrayInitInt(cc,var->type);
        if (init->kind == AST_STRING) {
            len = init->sval->len+1;
        } else {
            len = listCount(init->arrayinit);
        }
        if (var->type->len == -1) {
            var->type->len = len;
            var->type->size = len * var->type->ptr->size;
        } else if (var->type->len != len) {
            cctrlRaiseExceptionFromTo(cc, NULL, '{', '}',
                                     "Invalid array initializer: expected %d items but got %d",
                                      var->type->len, len);
        }
        Lexeme *tok = cctrlTokenGet(cc);
        assertTokenIsTerminator(cc,tok,terminator_flags);
        return astDecl(var,init);
    } else if (var->type->kind == AST_TYPE_CLASS && 
               !var->type->is_intrinsic && tokenPunctIs(peek,'{')) {
        init = parseDeclArrayInitInt(cc,var->type);
        Lexeme *tok = cctrlTokenGet(cc);
        assertTokenIsTerminator(cc,tok,terminator_flags);
        return astDecl(var,init);
    }

    init = parseExpr(cc,16);
    Lexeme *tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(cc,tok,terminator_flags);
    if (var->kind == AST_GVAR && var->type->kind == AST_TYPE_INT) {
        init = astI64Type(evalIntConstExpr(init));
    }

    if (var->type->kind == AST_TYPE_AUTO) {
        var->type = init->type;
    }

    /* Check what we are trying to assign is valid */
    AstType *ok = astTypeCheck(var->type,init,AST_BIN_OP_ASSIGN);
    if (!ok) {
        typeCheckWarn(cc,'=',var,init);
    }

    /* This is for when we have parsed a call to an inline function that is 
     * being assigned to a variable */
    if (init->kind == AST_COMPOUND_STMT) {
        /* Attach the Ast to the current function that is being called */
        if (cc->tmp_func) {
            listAppend(cc->tmp_func->body->stms,init);
        }
        return astDecl(var,init->inline_ret);
    }

    return astDecl(var,init);
}

Ast *parseVariableInitialiser(Cctrl *cc, Ast *var, s64 terminator_flags) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,'=')) {
        return parseVariableAssignment(cc,var,terminator_flags);
    }
    if (var->type->len == -1) {
        cctrlRaiseException(cc, "Missing array initializer: %s",astToString(var));
    }
    cctrlTokenRewind(cc);
    tok = cctrlTokenGet(cc);

    assertTokenIsTerminator(cc,tok,terminator_flags);
    return astDecl(var,NULL);
}

Ast *parseDecl(Cctrl *cc) {
    AstType *type;
    Lexeme *varname;
    Ast *var, *ast;

    parseDeclInternal(cc,&varname,&type);
    if (varname == NULL) {
        cctrlTokenExpect(cc,';');
        return NULL;
    }
    var = astLVar(type,varname->start,varname->len);
    if (!mapAddOrErr(cc->localenv,var->lname->data,var)) {
        cctrlRaiseException(cc,"variable %s already declared",astLValueToString(var,0));
    }
    if (cc->tmp_locals) {
        listAppend(cc->tmp_locals, var);
    }
    ast = parseVariableInitialiser(cc,var,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    if (type->kind == AST_TYPE_AUTO) {
        parseAssignAuto(cc,ast);
    }
    return ast;
}

int parseValidPostControlFlowToken(Lexeme *tok) {
    if (tok->tk_type == TK_IDENT) return 1;
    if (tok->tk_type == TK_STR) return 1;
    if (tok->tk_type == TK_PUNCT) {
        switch (tok->i64) {
            case TK_MINUS_MINUS:
            case TK_PLUS_PLUS:
            case '{':
            case ';':
            case '*':
                return 1;
        }
    }
    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_DO:
            case KW_FOR:
            case KW_GOTO:
            case KW_IF:
            case KW_RETURN:
            case KW_SWITCH:
            case KW_WHILE:
            case KW_BREAK:
            case KW_CONTINUE:
            case KW_TRY:
            case KW_THROW:
            case KW_ASM:
                return 1;
        }
    }
    return 0;
}

/* Parse one clause inside an `if (...)` header - either a declaration
 * (`Type name [= expr]`, including pointers) or a plain expression - and
 * report which terminator (';' or ')') closed it. Used to support C++17
 * style `if (init-statement; condition)` and a declaration used directly
 * as the condition. The declared variable is registered in the current
 * (if-scoped) localenv. */
static Ast *parseIfClause(Cctrl *cc, char *term_out) {
    Ast *clause;
    Lexeme *tok = cctrlTokenPeek(cc);
    if (tok && cctrlIsKeyword(cc, tok->start, tok->len)) {
        AstType *type = parseDeclSpec(cc);
        Lexeme *name = cctrlTokenGet(cc);
        if (!name || name->tk_type != TK_IDENT) {
            cctrlRaiseException(cc,
                "Expected identifier in `if` declaration, got `%.*s`",
                name ? name->len : 0, name ? name->start : "");
        }
        Ast *var = astLVar(type, name->start, name->len);
        if (!mapAddOrErr(cc->localenv, var->lname->data, var)) {
            cctrlRaiseException(cc, "variable %s already declared",
                                astLValueToString(var, 0));
        }
        if (cc->tmp_locals) {
            listAppend(cc->tmp_locals, var);
        }
        Lexeme *eq = cctrlTokenGet(cc);
        if (tokenPunctIs(eq, '=')) {
            Ast *init = parseExpr(cc, 16);
            clause = astDecl(var, init);
            if (type->kind == AST_TYPE_AUTO) {
                parseAssignAuto(cc, clause);
            }
        } else {
            cctrlTokenRewind(cc);
            clause = astDecl(var, NULL);
        }
    } else {
        clause = parseExpr(cc, 16);
    }

    Lexeme *t = cctrlTokenGet(cc);
    if (tokenPunctIs(t, ';'))      *term_out = ';';
    else if (tokenPunctIs(t, ')')) *term_out = ')';
    else {
        cctrlRaiseException(cc,
            "Expected ';' or ')' in `if (...)`, got `%.*s`",
            t ? t->len : 0, t ? t->start : "");
    }
    return clause;
}

Ast *parseIfStatement(Cctrl *cc) {
    cctrlTokenExpect(cc,'(');

    /* C++17 `if (init; cond)`: zero or more `;`-separated init statements
     * followed by the condition. The init declarations and a
     * declaration-condition are scoped to the if (and its bodies), so we
     * open a child environment and, when present, desugar to a block. */
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    List *pre = listNew();
    Ast *cond_clause = NULL;
    while (1) {
        char term = 0;
        Ast *clause = parseIfClause(cc, &term);
        if (term == ')') { cond_clause = clause; break; }
        listAppend(pre, clause);   /* an init statement */
    }

    /* A declaration used as the condition evaluates to the declared
     * variable; emit the declaration first, then test the variable. */
    Ast *cond;
    if (cond_clause->kind == AST_DECL) {
        listAppend(pre, cond_clause);
        cond = cond_clause->declvar;
    } else {
        cond = cond_clause;
    }

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing if body",
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }

    Ast *then = parseStatement(cc);
    Lexeme *tok = cctrlTokenGet(cc);
    Ast *els = NULL;

    if (tok && tok->tk_type == TK_KEYWORD && tok->i64 == KW_ELSE) {
        Lexeme *epeek = cctrlTokenPeek(cc);
        if (!parseValidPostControlFlowToken(epeek)) {
            cctrlTokenRewind(cc);
            cctrlTokenRewind(cc);
            cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing else body",
                    lexemeTypeToString(epeek->tk_type), epeek->len, epeek->start);
        }
        els = parseStatement(cc);
    } else {
        cctrlTokenRewind(cc);
    }

    cc->localenv = cc->localenv->parent;

    Ast *if_ast = astIf(cond, then, els);
    if (listEmpty(pre)) {
        return if_ast;
    }
    listAppend(pre, if_ast);
    return astCompountStatement(pre);
}

Ast *parseOptDeclOrStmt(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    return parseDeclOrStatement(cc);
}

Ast *parseOptExpr(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    Ast *ast = parseExpr(cc,16);
    tok = cctrlTokenGet(cc);
    assertTokenIsTerminatorWithMsg(cc,tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA, "for loop requires either a conditional statement or a semi colon to terminate the initalisation");
    return ast;
}

Ast *parseDesugarArrayLoop(Cctrl *cc, Ast *iteratee, Ast *static_array) {
    /* Create a temporay variable as the counter */
    AoStr *range_tmp_var = getRangeLoopIdx();
    Ast *counter_var = astLVar(astTypeCopy(ast_int_type),range_tmp_var->data,
                               range_tmp_var->len);

    Ast *counter = astDecl(counter_var,astI64Type(0));

    /* How much memory it takes up / size of one element */
    Ast *array_len = astI64Type(static_array->type->size/static_array->type->ptr->size);

    if (iteratee->type->kind == AST_TYPE_AUTO) {
        AstType *deref_type = static_array->type->ptr;
        iteratee->type = deref_type;
    }

    mapAddLen(cc->localenv,range_tmp_var->data,
                 range_tmp_var->len,counter_var);
    mapAdd(cc->localenv,iteratee->lname->data,iteratee);

    if (cc->tmp_locals) {
        listAppend(cc->tmp_locals,counter_var);
        listAppend(cc->tmp_locals,iteratee);
    }

    Ast *cond = parseCreateBinaryOp(cc,AST_BIN_OP_LT, counter_var, array_len);


    Ast *iterator = astDecl(iteratee,
            astUnaryOperator(static_array->type->ptr,
                AST_UN_OP_DEREF,
                parseCreateBinaryOp(cc,AST_BIN_OP_ADD, static_array, counter_var))
            );
    Ast *step = astUnaryOperator(astTypeCopy(ast_int_type),AST_UN_OP_PRE_INC,counter_var);

    cctrlTokenExpect(cc,')');
    Ast *forbody = parseStatement(cc);
    listPrepend(forbody->stms,iterator);
    return astFor(counter,cond,step,forbody,NULL,NULL,NULL);
}

void parseAssertContainerHasFields(Cctrl *cc, AstType *size_field, 
                                   AstType *entries_field)
{
    if (!size_field) {
        cctrlRaiseException(cc,"Range for loop must be on a struct with both a 'size' and 'entries' property");
    }

    if (!astIsIntType(size_field)) {
        cctrlRaiseException(cc,"Range for loop struct's size field must be an int got: %s",
                astTypeToColorString(size_field));
    }

    if (!entries_field) {
        cctrlRaiseException(cc,"Range for loop must be on a struct with both a 'size' and 'entries' property");
    }

    if (entries_field->kind != AST_TYPE_POINTER && entries_field->kind != AST_TYPE_ARRAY) {
        cctrlRaiseException(cc,"'entries' field must be a pointer or array got '%s'",
                astTypeKindToString(entries_field->kind));
    }

    if (entries_field->ptr->kind == AST_TYPE_VOID) {
        cctrlRaiseException(cc,"cannot dereference void pointer");
    }
}

Ast *parseCreateForRange(Cctrl *cc, Ast *iteratee,
                         Ast *size_ref, Ast *entries_ref)
{
    AoStr *range_tmp_var = getRangeLoopIdx();
    Ast *counter_var = astLVar(astTypeCopy(ast_int_type),range_tmp_var->data,
                               range_tmp_var->len);
    Ast *counter = astDecl(counter_var,astI64Type(0));

    mapAddLen(cc->localenv,range_tmp_var->data,
                 range_tmp_var->len,counter_var);
    mapAdd(cc->localenv,iteratee->lname->data,iteratee);
    if (cc->tmp_locals) {
        listAppend(cc->tmp_locals,counter_var);
        listAppend(cc->tmp_locals,iteratee);
    }

    Ast *cond = parseCreateBinaryOp(cc,AST_BIN_OP_LT, counter_var, size_ref);
    Ast *iterator = astDecl(iteratee,
            astUnaryOperator(entries_ref->type->ptr,
                AST_UN_OP_DEREF,
                parseCreateBinaryOp(cc,AST_BIN_OP_ADD, entries_ref, counter_var))
            );
    Ast *step = astUnaryOperator(astTypeCopy(ast_int_type),AST_UN_OP_PRE_INC,counter_var);
    cctrlTokenExpect(cc,')');
    Ast *forbody = parseStatement(cc);
    listPrepend(forbody->stms,iterator);
    return astFor(counter,cond,step,forbody,NULL,NULL,NULL);
}

Ast *parseRangeLoop(Cctrl *cc, Ast *iteratee) {
    cctrlTokenGet(cc);
    Ast *container = parseExpr(cc,16);

    if (container->kind == AST_LVAR) {
        if (container->type->kind == AST_TYPE_POINTER) {
            if (container->type->ptr->kind != AST_TYPE_CLASS) {
                cctrlRaiseException(cc,"pointer '%s' has no fields; range requires 'I64 size' and '<type> *entries'",
                        astLValueToString(container, 0));
            }

            AstType *size_field = mapGetLen(container->type->ptr->fields, str_lit("size"));
            AstType *entries_field = mapGetLen(container->type->ptr->fields, str_lit("entries"));

            parseAssertContainerHasFields(cc,size_field,entries_field);

            Ast *deref = astUnaryOperator(container->type->ptr,
                                          AST_UN_OP_DEREF,
                                          container);
            Ast *size_ref = astClassRef(size_field,deref,"size");
            Ast *entries_ref = astClassRef(entries_field,deref,"entries");

            if (iteratee->type->kind == AST_TYPE_AUTO) {
                AstType *deref_type = entries_field->ptr;
                iteratee->type = deref_type;
            }

            return parseCreateForRange(cc, iteratee, size_ref, entries_ref);
        } else if (container->type->kind == AST_TYPE_ARRAY) {
            return parseDesugarArrayLoop(cc,iteratee,container);
        } else {
            cctrlRaiseException(cc,"can only range over arrays and pointers");
        }
    } else if (container->kind == AST_CLASS_REF) {
        Map *fields = NULL;
        if (container->type->kind == AST_TYPE_POINTER) {
            fields = container->type->ptr->fields;
        } else if (container->type->kind == AST_TYPE_ARRAY) {
            return parseDesugarArrayLoop(cc,iteratee,container);
        } else {
            fields = container->type->fields;
        }

        AstType *size_field = mapGetLen(fields, str_lit("size"));
        AstType *entries_field = mapGetLen(fields, str_lit("entries"));

        parseAssertContainerHasFields(cc,size_field,entries_field);

        Ast *deref = astUnaryOperator(container->type->ptr,AST_UN_OP_DEREF,container);
        Ast *size_ref = astClassRef(size_field,deref,"size");
        Ast *entries_ref = astClassRef(entries_field,deref,"entries");

        if (iteratee->type->kind == AST_TYPE_AUTO) {
            AstType *deref_type = entries_ref->type;
            iteratee->type = deref_type;
        }

        return parseCreateForRange(cc, iteratee, size_ref, entries_ref);
    }
    cctrlRaiseException(cc,"Can only handle lvars, arrays and class references at this time got: %s %s",
                astKindToString(container->kind), astToString(container));
}

/* Either parse the initialiser or a range loop. Range loop is experimental */
Ast *parseForLoopInitialiser(Cctrl *cc) {
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok,';')) {
        return NULL;
    }
    cctrlTokenRewind(cc);
    
    tok = cctrlTokenPeek(cc);
    if (!tok) {
        return NULL;
    }

    if (cctrlIsKeyword(cc,tok->start,tok->len)) {
        AstType *type = parseDeclSpec(cc);
        tok = cctrlTokenGet(cc);
        if (tok->tk_type != TK_IDENT) {
            cctrlRaiseException(cc,"expected Identifier got: %s",
                    lexemeToString(tok)); 
        }

        /* We have a for loop variable */
        Ast *init_var = astLVar(type,tok->start,tok->len);

        /* can be : for an auto loop or ';' for a normal loop */
        tok = cctrlTokenPeek(cc); 
        if (!mapAddOrErr(cc->localenv,init_var->lname->data,init_var)) {
            cctrlRaiseException(cc,"variable `%s` already declared!",
                    astLValueToString(init_var,0));
        }
        
        /* For this to work the type must have:
         * 1) a pointer called entries 
         * 2) a size value that must be an integer */
        if (tokenPunctIs(tok,':')) {
            return parseRangeLoop(cc,init_var);
        } else if (tokenPunctIs(tok, '=')) {
            if (cc->tmp_locals) {
                listAppend(cc->tmp_locals, init_var);
            }
            Ast *ast = parseVariableInitialiser(cc,init_var,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
            if (type->kind == AST_TYPE_AUTO) {
                parseAssignAuto(cc,ast);
            }
            return ast;
        }
    }
    return parseStatement(cc);
}

Ast *parseForStatement(Cctrl *cc) {
    Ast *forinit, *forcond, *forstep, *forbody;
    AoStr *for_begin, *for_end, *for_middle,
          *prev_begin, *prev_end;
    cctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    for_begin = astMakeLabel();
    for_middle = astMakeLabel();
    for_end = astMakeLabel();

    cc->tmp_loop_begin = for_middle;
    cc->tmp_loop_end = for_end;

    cc->localenv = cctrlCreateAstMap(cc->localenv);
    forinit = parseForLoopInitialiser(cc);
    //parseOptDeclOrStmt(cc);

    if (forinit && forinit->kind == AST_FOR) {
        forinit->for_begin = for_begin;
        forinit->for_middle = for_middle;
        forinit->for_end = for_end;
        cc->localenv = cc->localenv->parent;
        cc->tmp_loop_begin = prev_begin;
        cc->tmp_loop_end = prev_end;
        return forinit;
    }

    forcond = parseOptExpr(cc);
    if (tokenPunctIs(cctrlTokenPeek(cc), ')')) {
        forstep = NULL;
    } else {
        forstep = parseExpr(cc,16);
    }
    cctrlTokenExpect(cc,')');

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing for loop body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }
    forbody = parseStatement(cc);
    /* Go back up */
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astFor(forinit,forcond,forstep,forbody,for_begin,for_middle,for_end);
}

Ast *parseWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    AoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    cctrlTokenExpect(cc,'(');

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = astMakeLabel();
    while_end = astMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = cctrlCreateAstMap(cc->localenv);
    whilecond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing while loop body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }

    whilebody = parseStatement(cc);
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *parseDoWhileStatement(Cctrl *cc) {
    Ast *whilecond, *whilebody;
    Lexeme *tok;
    AoStr *while_begin, *while_end,
          *prev_begin, *prev_end;
    

    prev_begin = cc->tmp_loop_begin;
    prev_end = cc->tmp_loop_end;

    while_begin = astMakeLabel();
    while_end = astMakeLabel();
    cc->tmp_loop_begin = while_begin;
    cc->tmp_loop_end = while_end;

    cc->localenv = cctrlCreateAstMap(cc->localenv);

    Lexeme *peek = cctrlTokenPeek(cc);
    if (!parseValidPostControlFlowToken(peek)) {
        cctrlRewindUntilStrMatch(cc,peek->start,peek->len,NULL);
        cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing do while loop body", 
                lexemeTypeToString(peek->tk_type), peek->len, peek->start);
    }


    whilebody = parseStatement(cc);
    
    tok = cctrlTokenGet(cc);

    if (tok->tk_type != TK_KEYWORD && tok->i64 != KW_WHILE) {
        cctrlRaiseException(cc,"Expected keyword 'while'");
    }

    cctrlTokenExpect(cc, '(');
    whilecond = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');
    cctrlTokenExpect(cc,';');
    cc->localenv = cc->localenv->parent;
    cc->tmp_loop_begin = prev_begin;
    cc->tmp_loop_end = prev_end;
    return astDoWhile(whilecond,whilebody,while_begin, while_end);
}

Ast *parseBreakStatement(Cctrl *cc) {
    if (cc->tmp_loop_end == NULL) {
        cctrlRaiseException(cc,"Floating break, not inside a breakable statement");
    }
    Ast *ast = astBreak(cc->tmp_loop_end);
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseContinueStatement(Cctrl *cc) {
    if (cc->tmp_loop_begin == NULL) {
        cctrlRaiseException(cc,"Floating continue, not inside a loop");
    }
    Ast *ast = astContinue(cc->tmp_loop_begin);
    cctrlTokenExpect(cc,';');
    return ast;
}

Ast *parseReturnStatement(Cctrl *cc) {
    Ast *retval = parseExpr(cc,16);
    AstType *check;
    cctrlTokenExpect(cc,';');
    /* A best attempt at trying to get the return type of a function */
    if (cc->tmp_rettype->kind == AST_TYPE_AUTO) {
        cc->tmp_rettype = parseReturnAuto(cc,retval);
    }

    Ast *maybe_fn = findFunctionDecl(cc,cc->tmp_fname->data,cc->tmp_fname->len);

    if (retval) check = retval->type;
    else        check = ast_void_type;

    if (check->kind == AST_TYPE_VOID && maybe_fn->type->rettype->kind == AST_TYPE_VOID) {
        if (maybe_fn->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
            return astDecl(maybe_fn->inline_ret,NULL);
        }
        return astReturn(retval,cc->tmp_rettype);
    }

    AstType *ok = astTypeCheck(cc->tmp_rettype,retval,'\0');
    if (!ok) {
        Ast *func = mapGet(cc->global_env, cc->tmp_fname->data);
        typeCheckReturnTypeWarn(cc,func,check,retval);
    }
    if (maybe_fn->flags & AST_FLAG_INLINE && !(cc->flags & CCTRL_TRANSPILING)) {
        return astDecl(maybe_fn->inline_ret,retval);
    }
    return astReturn(retval,cc->tmp_rettype);
}

/* TempleOS-strict `try { ... } catch <stmt>`. The catch handler is a
 * single statement (often `{...}` so braces still work via the
 * normal block-statement path) and does NOT bind the thrown value -
 * read it via the global `Fs->except_ch`. */
Ast *parseTryStatement(Cctrl *cc) {
    cctrlTokenExpect(cc,'{');
    Ast *try_body = parseCompoundStatement(cc);
    Lexeme *tok = cctrlTokenPeek(cc);
    if (!tok) {
        cctrlRaiseException(cc,
            "Expected `catch` after `try { ... }`, got end of input");
    }
    if (tok->tk_type != TK_KEYWORD || tok->i64 != KW_CATCH) {
        cctrlRaiseException(cc,
            "Expected `catch` after `try { ... }`, got `%.*s`",
            tok->len, tok->start);
    }
    cctrlTokenGet(cc);  /* consume `catch` */
    Ast *catch_body = parseStatement(cc);
    return astTry(try_body, catch_body);
}

/* `throw(expr);` - the expression yields a u64 (commonly a multi-
 * char constant like `'BlkDev'` which the lexer packs into 8 bytes
 * via TK_CHAR_CONST). The runtime stashes it into `Fs->except_ch`
 * and longjmps to the nearest enclosing catch. */
Ast *parseThrowStatement(Cctrl *cc) {
    cctrlTokenExpect(cc,'(');
    Ast *value = parseExpr(cc,16);
    cctrlTokenExpect(cc,')');
    cctrlTokenExpect(cc,';');
    return astThrow(value);
}

void parseRaiseCaseException(Cctrl *cc, Ast *case_expr) {
    cctrlRewindUntilStrMatch(cc,str_lit("case"),NULL);
    char *exp = astLValueToString(case_expr,0);
    char *type = astTypeToString(case_expr->type);
    char *suggestion = mprintf("Invalid use of type %s", type);
    cctrlRaiseExceptionFromTo(cc, suggestion, 'c', ':', "`case` must be followed by an integer constant got - %s", exp);
}

Ast *parseCaseLabel(Cctrl *cc, Lexeme *tok) {
    if (cc->tmp_case_list == NULL) {
        cctrlRaiseException(cc,"unexpected 'case' found");
    }
    Ast *case_, *prev, *case_expr = NULL;
    Lexeme *peek;
    AoStr *label;
    int begining,end;
    int ok = 1;

    peek = cctrlTokenPeek(cc);
    if (tokenPunctIs(peek,':')) {
        if (vecEmpty(cc->tmp_case_list)) {
            begining = 0;
        } else {
            prev = cc->tmp_case_list->entries[cc->tmp_case_list->size - 1];
            begining = prev->case_end+1;
        }
        label = astMakeLabel();
    } else {
        case_expr = parseExpr(cc,16);
        begining = evalIntConstExprOrErr(case_expr, &ok);
        if (!ok) {
            if (cc->flags & CCTRL_PASTE_DEFINES && case_expr->kind == AST_LVAR) {
                label = case_expr->lname;
            } else {
                parseRaiseCaseException(cc,case_expr);
            }
        } else {
            label = astMakeLabel();
        }
    }

    tok = cctrlTokenPeek(cc);

    /* We're not doing label to label for transpilation */
    if (tokenPunctIs(tok,TK_ELLIPSIS)) {
        cctrlTokenGet(cc);
        case_expr = parseExpr(cc,16);
        ok = 1;
        end = evalIntConstExprOrErr(case_expr, &ok);
        cctrlTokenExpect(cc,':');
        if (begining > end) {
            cctrlRewindUntilStrMatch(cc,str_lit("case"),NULL);
            char *suggestion = mprintf("Swap the conditions around `case %d ... %d: `",end,begining);
            cctrlRaiseExceptionFromTo(cc, suggestion, 'c', ':', " Condition is in the wrong order '%d' must be lower than '%d'",
                    begining, end);
        }
    } else {
        cctrlTokenExpect(cc,':');
        end = begining;
    }

    List *stmts = listNew();
    peek = cctrlTokenPeek(cc);

    if (!ok && cc->flags & CCTRL_PASTE_DEFINES) {
        case_ = astCase(label,begining,end,stmts);
        case_->type = ast_void_type;
    } else {
        case_ = astCase(label,begining,end,stmts);
        assertUniqueSwitchCaseLabels(cc,cc->tmp_case_list,case_);
    }

    vecPush(cc->tmp_case_list,case_);

    do {
        Ast *stmt = parseStatement(cc);
        if (stmt && stmt->kind != AST_CASE && stmt->kind != AST_DEFAULT) {
            listAppend(stmts,stmt);
        }
        if (stmt->kind == AST_COMPOUND_STMT || stmt->kind == AST_CASE ||
                stmt->kind == AST_DEFAULT || stmt->kind == AST_BREAK || stmt->kind == AST_RETURN
                || stmt->kind == AST_GOTO) break;
        peek = cctrlTokenPeek(cc);
   
        /* @Bug - Something is afoot, this ensures we don't go on forever
         * parsing if there isn't a break and the case is a fall through...
         * feels as though we could go on and just ommit the case. Which would
         * avoid the call to `astCasesCompress()`? */
        if (tokenPunctIs(peek,'}')) break;
    } while (1);

    return case_;
}

Ast *parseDefaultStatement(Cctrl *cc) {
    cctrlTokenExpect(cc,':');
    if (cc->tmp_default_case) {
        cctrlRaiseException(cc,"Duplicate default case");
    }
    Lexeme *peek;
    List *stmts = listNew();
    AoStr *default_label = astMakeLabel();
    /* set here so this is non-null for the next call to parseStatement */
    cc->tmp_default_case = astDefault(default_label,stmts);

    do {
        Ast *stmt = parseStatement(cc);
        if (stmt && stmt->kind != AST_CASE) {
            listAppend(stmts,stmt);
        }
        if (stmt->kind == AST_COMPOUND_STMT || stmt->kind == AST_CASE || stmt->kind == AST_BREAK || stmt->kind == AST_RETURN
                || stmt->kind == AST_GOTO) break;
        peek = cctrlTokenPeek(cc);
        if (tokenPunctIs(peek,'}')) break;
    } while (1);

    return cc->tmp_default_case;
}

Ast *parseSwitchStatement(Cctrl *cc) {
    Ast *cond, *tmp, *original_default_label;
    Lexeme *peek;
    Vec *original_cases;
    AoStr *end_label,*tmp_name,*original_break;
    int switch_bounds_checked = 1;
    char terminating_char = ')';

    peek = cctrlTokenPeek(cc);

    if (!tokenPunctIs(peek,'[') && !tokenPunctIs(peek, '(')) {
        cctrlRaiseException(cc,"Switch '(' or '[' expected got: %s",
                lexemeToString(peek));
    }

    /* Walk past and set the expected terminting character */
    if (peek->i64 == '[') {
        switch_bounds_checked = 0;
        terminating_char = ']';
    }

    cctrlTokenGet(cc);
    cond = parseExpr(cc,16);
    if (!astIsIntType(cond->type)) {
        cctrlRaiseException(cc,"Switch can only have int's at this time");
    }
    cctrlTokenExpect(cc,terminating_char);

    original_break = cc->tmp_loop_end;
    original_default_label = cc->tmp_default_case;
    original_cases = cc->tmp_case_list;

    cc->tmp_case_list = astVecNew();
    cc->tmp_default_case = NULL;

    end_label = astMakeLabel();

    /* this is the current label */
    cc->tmp_loop_end = end_label;

    tmp_name = astMakeTmpName();

    tmp = astLVar(cond->type, tmp_name->data, tmp_name->len);
    listAppend(cc->tmp_locals,tmp);

    parseStatement(cc);
    aoStrRelease(tmp_name);

    Ast *switch_ast = astSwitch(
        cond,
        cc->tmp_case_list,
        cc->tmp_default_case,
        end_label,
        switch_bounds_checked
    );

    cc->tmp_loop_end = original_break;
    cc->tmp_case_list = original_cases;
    cc->tmp_default_case = original_default_label;
    return switch_ast;
}

/* Concatinate the label of the goto with the name of the function 
 * currently being parsed to be able to have uniqe goto labels  */
AoStr *createFunctionLevelGotoLabel(Cctrl *cc, Lexeme *tok) {
    AoStr *label = aoStrNew();
    aoStrCatFmt(label,".%S_%.*s",cc->tmp_fname,tok->len,tok->start);
    return label;
}

Ast *parseStatement(Cctrl *cc) {
    Lexeme *tok, *peek;
    AoStr *label;
    Ast *ret, *ast;
    Map *env;
    tok = cctrlTokenGet(cc);

    if (tok->tk_type == TK_KEYWORD) {
        switch (tok->i64) {
            case KW_IF:       return parseIfStatement(cc);
            case KW_FOR:      return parseForStatement(cc);
            case KW_WHILE:    return parseWhileStatement(cc);
            case KW_DO:       return parseDoWhileStatement(cc);
            case KW_RETURN:   return parseReturnStatement(cc);
            case KW_TRY:      return parseTryStatement(cc);
            case KW_THROW:    return parseThrowStatement(cc);
            case KW_SWITCH:   return parseSwitchStatement(cc);
            case KW_CASE:     return parseCaseLabel(cc,tok);
            case KW_DEFAULT:  return parseDefaultStatement(cc);
            case KW_BREAK:    return parseBreakStatement(cc);
            case KW_CONTINUE: return parseContinueStatement(cc);
            case KW_ASM: {
                /* Inline `asm { ... }` block as a statement. TempleOS
                 * lets functions splice raw asm directly into the body
                 * (no `Name::` wrapper inside). prsAsm's `parse_one`
                 * mode is the one-block-no-label path we need. */
                Lexeme *p = cctrlTokenPeek(cc);
                if (!tokenPunctIs(p,'{')) {
                    cctrlRaiseException(cc,
                        "Expected `{` after `asm`, got `%.*s`",
                        p ? p->len : 0, p ? p->start : "");
                }
                Ast *asm_block = prsAsm(cc, 1);
                listAppend(cc->asm_blocks, asm_block);
                return asm_block;
            }
            case KW_STATIC: {
                env = cc->localenv;
                cc->localenv = NULL;

                AstType *type = parseFullType(cc);
                tok = cctrlTokenGet(cc);

                if (tok->tk_type != TK_IDENT) {
                    cctrlTokenRewind(cc);
                    cctrlRaiseException(cc,"Expected variable name following type declaration '%s' - '%s' <var_name>",
                            astTypeToString(type), astTypeToString(type));
                }
                type = parseArrayDimensions(cc,type);

                Ast *gvar_ast = astGVar(type,tok->start,tok->len,1);


                if (type->kind == AST_TYPE_ARRAY) {
                    ast = parseVariableInitialiser(cc,gvar_ast,
                            PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);

                    cc->localenv = env;
                    mapAdd(env,gvar_ast->gname->data,gvar_ast);
                    listAppend(cc->ast_list,ast);
                    return ast;
                }

                peek = cctrlTokenPeek(cc);
                if (tokenPunctIs(peek,'=')) {
                    ast = parseVariableInitialiser(cc,gvar_ast,
                            PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);

                    if (type->kind == AST_TYPE_AUTO) {
                        gvar_ast->type = ast->declinit->type;
                        if (ast->declinit->kind == AST_STRING) {
                            /* @Leak: we've just lost the original string array
                             * that was parsed... or have we? Possibly not as it
                             * would exist on the declinit->type and we do not 
                             * change it */
                            gvar_ast->type = astMakePointerType(ast_u8_type);
                            gvar_ast->type->len = ast->declinit->type->len;
                        }
                    }

                    if (ast->declinit->kind == AST_ASM_FUNCALL ||
                        parseIsFunctionCall(ast->declinit))
                    {
                        cctrlRaiseException(cc,"'%s %s' must be a compile time "
                                "constant",
                                astTypeToColorString(gvar_ast->type),
                                astLValueToString(ast,0));
                    }
                } else {
                    cctrlTokenExpect(cc,';');
                    ast = astDecl(gvar_ast,NULL);
                }
                cc->localenv = env;
                mapAdd(env,gvar_ast->gname->data,gvar_ast);
                listAppend(cc->ast_list,ast);
                return ast;
            }

            case KW_GOTO: {
                tok = cctrlTokenGet(cc);
                label = createFunctionLevelGotoLabel(cc,tok);
                ret = astGoto(label);
                cctrlTokenExpect(cc,';');
                return ret;
            }

            default: {
                cctrlTokenRewind(cc);
                cctrlRaiseException(cc,"Keyword '%.*s' cannot be used in this context",
                        tok->len,tok->start);
            }
        }
    }

    if (tok->tk_type == TK_CHAR_CONST) {
        return parseFloatingCharConst(cc,tok);
    }

    if (tok->tk_type == TK_STR) {
        cctrlTokenRewind(cc);
        /* HACK in holyc printf */
        return parseFunctionArguments(cc,"printf",6,';');
    }

    /* Hacked in goto label ;) */
    peek = cctrlTokenPeek(cc);
    if (tok->tk_type == TK_IDENT && tokenPunctIs(peek,':')) {
        label = createFunctionLevelGotoLabel(cc,tok);
        ret = astLabel(label);
        /* consume ':' */
        cctrlTokenExpect(cc,':');
        return ret;
    }

    if (tokenPunctIs(tok,'{')) {
        return parseCompoundStatement(cc);
    }

    if (tok->tk_type == TK_I64) {
        cctrlRaiseException(cc,"Floating integer constant '%ld' cannot be used in this context", tok->i64);
    } else if (tok->tk_type == TK_F64) {
        cctrlRaiseException(cc,"Floating integer constant '%f' cannot be used in this context", tok->f64);
    } else if (tok->tk_type == TK_PUNCT && (tok->i64 == ',')) {
        lexemePrint(tok);
        cctrlTokenRewind(cc);
        cctrlRaiseException(cc, "Floating '%c' cannot be used in this context", tok->i64);
    }

    cctrlTokenRewind(cc);
    ast = parseExpr(cc,16);
    tok = cctrlTokenGet(cc);
    assertTokenIsTerminator(cc,tok,PUNCT_TERM_SEMI|PUNCT_TERM_COMMA);
    return ast;
}

Ast *parseDeclOrStatement(Cctrl *cc) {
    Lexeme *tok = cctrlTokenPeek(cc);
    if (!tok) {
        return NULL;
    }
    if (cctrlIsKeyword(cc,tok->start,tok->len)) {
        return parseDecl(cc);
    }
    return parseStatement(cc);
}

void parseCompoundStatementInternal(Cctrl *cc, Ast *body) {
    Ast *stmt = NULL;
    Ast *var = NULL;
    AstType *base_type, *type, *next_type;
    Lexeme *tok, *varname, *peek;
    Map *block_scope = cctrlCreateAstMap(cc->localenv);
    cc->localenv = block_scope;
    tok = NULL;

    /* Per-statement recovery point hoisted to the function frame
     * so the jmp_buf has a stable address across iterations. A
     * longjmp here lands at the top of the iteration's body via
     * the setjmp check below; we restore the block's scope (in
     * case a half-parsed nested block left us in a child) and
     * resync the token stream to the next `;` or `}`. The outer
     * recovery (toplevel decl) is reinstated when this function
     * returns so the caller's failure mode is unchanged. */
    jmp_buf stmt_recovery;
    jmp_buf *outer_recovery = cc->current_recovery;
    cc->current_recovery = &stmt_recovery;

    tok = cctrlTokenPeek(cc);

    while (tok && !tokenPunctIs(tok, '}')) {
        /* Snapshot the tail of body->stms and cc->tmp_locals
         * *before* attempting the next statement. If the parse
         * longjmps mid-way, half-built nodes may already have
         * been appended; truncate back to these saved tails so
         * the recovered block has only the statements that
         * actually parsed cleanly. Allocations themselves leak
         * into the arena (freed at end of compilation), but the
         * AST no longer points at them. */
        List *body_tail = (body && body->stms) ? body->stms->prev : NULL;
        List *locals_tail = cc->tmp_locals ? cc->tmp_locals->prev : NULL;

        if (setjmp(stmt_recovery) != 0) {
            cc->localenv = block_scope;
            if (body && body->stms && body_tail) {
                body_tail->next = body->stms;
                body->stms->prev = body_tail;
            }
            if (cc->tmp_locals && locals_tail) {
                locals_tail->next = cc->tmp_locals;
                cc->tmp_locals->prev = locals_tail;
            }
            cctrlSyncStatement(cc);
            tok = cctrlTokenPeek(cc);
            continue;
        }

        if (cctrlIsKeyword(cc,tok->start,tok->len)) {
            base_type = parseBaseDeclSpec(cc);
            while (1) {
                next_type = parsePointerType(cc,base_type);
                int pinned_kind;
                AoStr *pinned_reg;
                parseRegModifier(cc, &pinned_kind, &pinned_reg);
                peek = cctrlTokenPeek(cc);

                if (!tokenPunctIs(peek,'(')) {
                    /* A normal variable */
                    varname = cctrlTokenGet(cc);
                    if (varname->tk_type != TK_IDENT) {
                        cctrlTokenRewind(cc);
                        cctrlRaiseException(cc,"Expected type declaration with identifer got '%.*s' - should be"ESC_BLUE" '%s "ESC_RESET ESC_BOLD"<var_name>'",
                                peek->len, peek->start, astTypeToString(next_type));
                        break;
                    }
                    type = parseArrayDimensions(cc,next_type);
                    var = astLVar(type,varname->start,varname->len);
                    var->pinned_kind = pinned_kind;
                    var->pinned_reg = pinned_reg;
                    if (!mapAddOrErr(cc->localenv,var->lname->data,var)) {
                        cctrlRewindUntilStrMatch(cc,var->lname->data,var->lname->len,NULL);
                        cctrlRaiseException(cc,"variable `%s` already declared",
                                astLValueToString(var,0));
                    }
                } else {
                    cctrlTokenGet(cc);
                    var = parseFunctionPointer(cc,next_type);
                    mapAdd(cc->localenv,var->fname->data,var);
                }

                if (cc->tmp_locals) {
                    listAppend(cc->tmp_locals, var);
                }

                stmt = parseVariableInitialiser(cc,var,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
                if (next_type->kind == AST_TYPE_AUTO) {
                    parseAssignAuto(cc,stmt);
                }

                if (stmt) {
                    listAppend(body->stms,stmt);
                }
                cctrlTokenRewind(cc);
                tok = cctrlTokenGet(cc);

                if (tokenPunctIs(tok, ',')) {
                    continue;
                } else if (tokenPunctIs(tok,';')) {
                    break;
                } else {
                    cctrlRaiseException(cc,"Unexpected %s `%.*s` while parsing statement, perhaps you meant to terminate the declaration with `;`?",
                            lexemeTypeToString(tok->tk_type), tok->len,tok->start);
                }
            }
        } else { 
            if ((stmt = parseStatement(cc)) != NULL) {
                if (stmt->kind == AST_DECL && stmt->declvar->kind == AST_GVAR) {
                    tok = cctrlTokenPeek(cc);
                    continue;
                }
                listAppend(body->stms,stmt);
            } else {
                break;
            }
        }
        tok = cctrlTokenPeek(cc);
    }
    cc->current_recovery = outer_recovery;
    cc->localenv = cc->localenv->parent;
    cctrlTokenExpect(cc,'}');
    cctrlTokenPeek(cc);
}

Ast *parseCompoundStatement(Cctrl *cc) {
    List *stmts = listNew();
    Ast *ast_compound = astCompountStatement(stmts);
    parseCompoundStatementInternal(cc, ast_compound);
    return ast_compound;
}

/* ---- "non-void function falls off the end" analysis ----
 *
 * `astStmtFallsThrough` answers: can control flow run off the end of
 * `s` (i.e. reach the next statement) rather than diverting via
 * return/throw/goto/infinite-loop? A non-void function whose body falls
 * through can return an undefined value, which is almost always a
 * missing `return`. The analysis is deliberately conservative -
 * anything it can't model returns "falls through" only when that's the
 * safe assumption, so we under-warn rather than cry wolf. */

static int astIsConstTrueCond(Ast *c) {
    /* `while(1)` / `for(;1;)` / `for(;;)` style infinite loops. */
    return c && c->kind == AST_LITERAL && c->type &&
           c->type->kind == AST_TYPE_INT && c->i64 != 0;
}

/* Does `s` contain a `break` that targets the *enclosing* loop/switch
 * (i.e. not one captured by a nested loop/switch of its own)? */
static int astStmtHasBreak(Ast *s) {
    if (!s) return 0;
    switch (s->kind) {
        case AST_BREAK:
            return 1;
        /* Nested loops / switch capture their own breaks. */
        case AST_FOR:
        case AST_WHILE:
        case AST_DO_WHILE:
        case AST_SWITCH:
            return 0;
        case AST_COMPOUND_STMT:
            if (s->stms) {
                listForEach(s->stms) {
                    if (astStmtHasBreak((Ast *)it->value)) return 1;
                }
            }
            return 0;
        case AST_IF:
            return astStmtHasBreak(s->then) || astStmtHasBreak(s->els);
        default:
            return 0;
    }
}

static int astStmtFallsThrough(Ast *s) {
    if (!s) return 1;
    switch (s->kind) {
        case AST_RETURN:
        case AST_THROW:
        case AST_JUMP:    /* goto - control leaves, doesn't reach the end */
            return 0;
        case AST_COMPOUND_STMT:
            if (s->stms) {
                listForEach(s->stms) {
                    if (!astStmtFallsThrough((Ast *)it->value)) return 0;
                }
            }
            return 1;
        case AST_IF:
            /* No else: the condition-false path reaches the end. */
            if (!s->els) return 1;
            return astStmtFallsThrough(s->then) ||
                   astStmtFallsThrough(s->els);
        case AST_FOR:
            if ((s->forcond == NULL || astIsConstTrueCond(s->forcond)) &&
                !astStmtHasBreak(s->forbody))
                return 0;
            return 1;
        case AST_WHILE:
        case AST_DO_WHILE:
            if (astIsConstTrueCond(s->whilecond) &&
                !astStmtHasBreak(s->whilebody))
                return 0;
            return 1;
        case AST_SWITCH:
            /* A switch with no default leaves the unmatched value to
             * reach the end. With a default we assume the author
             * handled every arm (the common exhaustive-switch-with-
             * returns pattern) rather than risk a false positive. */
            return s->case_default ? 0 : 1;
        default:
            /* Plain statements (calls, decls, assignments, ...). */
            return 1;
    }
}

/* Naked `asm { }` functions get no compiler-generated epilogue - the
 * body is pasted verbatim, so a missing `RET` lets the CPU fall off the
 * end into whatever follows (typically a SIGILL). Heuristic: scan for a
 * whole-word control transfer that ends the function - RET/ERET/IRET or
 * a tail branch (JMP/B/BR) - case-insensitive. A tail branch counts so
 * legitimate tail-call exits don't trip the warning. */
static int asmTextHasReturn(AoStr *text) {
    static const char *exits[] = { "ret", "eret", "iret", "jmp", "br", "b" };
    if (!text || !text->data) return 0;
    const char *s = text->data;
    int n = (int)text->len;
    for (int i = 0; i < n; i++) {
        if (i > 0) {
            char p = s[i - 1];   /* only test at a word start */
            if ((p >= 'A' && p <= 'Z') || (p >= 'a' && p <= 'z') ||
                (p >= '0' && p <= '9') || p == '_')
                continue;
        }
        for (size_t k = 0; k < sizeof(exits) / sizeof(exits[0]); k++) {
            const char *w = exits[k];
            int wl = (int)strlen(w);
            if (i + wl > n) continue;
            int match = 1;
            for (int j = 0; j < wl; j++) {
                char a = s[i + j];
                if (a >= 'A' && a <= 'Z') a = (char)(a - 'A' + 'a');
                if (a != w[j]) { match = 0; break; }
            }
            if (!match) continue;
            char after = (i + wl < n) ? s[i + wl] : '\0';   /* whole word */
            if ((after >= 'A' && after <= 'Z') || (after >= 'a' && after <= 'z') ||
                (after >= '0' && after <= '9') || after == '_')
                continue;
            return 1;
        }
    }
    return 0;
}

Ast *parseFunctionDef(Cctrl *cc, AstType *rettype,
        char *fname, int len, Vec *params, int has_var_args, int is_inline)
{
    Lexeme *next = cctrlTokenPeek(cc);
    /* Anchor for the end-of-function missing-return warning: by the time
     * we know whether the body falls through, the cursor has moved past
     * the whole function to the next declaration. Snapshot the body's
     * opening token now so the warning points at this function. */
    s64 fn_line = next ? next->line : cc->lineno;
    s64 fn_col  = next ? next->col  : 0;
    s64 fn_len  = next ? next->len  : 1;
    if (next->tk_type == TK_KEYWORD && next->i64 == KW_ASM) {
        cctrlTokenGet(cc);
        Lexeme *peek = cctrlTokenPeek(cc);
        if (tokenPunctIs(peek,'{')) {
            AoStr *fname_duped = aoStrDupRaw(fname,len);
            /* Target-aware: adds the leading `_` on Mach-O so it
             * matches what `bl _Add` callers expect. The compile-time
             * `astNormaliseFunctionName` only handles IS_BSD and would
             * leave the symbol as `Add` (no underscore) on this build. */
            AoStr *asm_fname = aoStrPrintf("%s",
                asmNormaliseFunctionName(cc, fname_duped));
            AoStr *prev_asm_name = cc->tmp_asm_fname;
            cc->tmp_asm_fname = asm_fname;
            Ast *asm_block = prsAsm(cc,1);

            /* If any param is `reg <REG>` pinned, emit shuffle moves
             * up-front so the user's asm can refer to the pinned
             * register names. The asm-only-function path bypasses
             * the normal ParamSpills path that runs for IR-codegen'd
             * functions; we splice the shuffle directly into the asm
             * text. Note: this path doesn't save/restore callee-
             * saved regs - the user's `RET` exits before any epilogue
             * can run, so if the body clobbers callee-saved registers
             * the caller is on the hook for that ABI rule.
             * TempleOS-flavoured asm functions are explicit by design.
             *
             * Target-aware: AArch64 uses x0..x7 (8 int arg regs, mov
             * "dst, src" order); x86_64 SysV uses rdi/rsi/rdx/rcx/r8/r9
             * (6 int arg regs, AT&T "movq %src, %dst" order). */
            const char **kIntArgRegs;
            int max_int_args;
            int is_x86; /* kept for symmetry with the switch below */
            switch (cc->target) {
                case TARGET_AARCH64_APPLE_DARWIN:
                case TARGET_AARCH64_UNKNOWN_LINUX_GNU: {
                    static const char *aarch64_regs[] = {
                        "x0", "x1", "x2", "x3",
                        "x4", "x5", "x6", "x7"
                    };
                    kIntArgRegs = aarch64_regs;
                    max_int_args = 8;
                    is_x86 = 0;
                    break;
                }
                case TARGET_X86_64_APPLE_DARWIN:
                case TARGET_X86_64_UNKNOWN_LINUX_GNU: {
                    static const char *x86_regs[] = {
                        "rdi", "rsi", "rdx", "rcx", "r8", "r9"
                    };
                    kIntArgRegs = x86_regs;
                    max_int_args = 6;
                    is_x86 = 1;
                    break;
                }
                default:
                    kIntArgRegs = NULL;
                    max_int_args = 0;
                    is_x86 = 0;
            }
            (void)is_x86;

            if (params && kIntArgRegs) {
                AoStr *shuffle = aoStrNew();
                int int_idx = 0;
                for (u64 pi = 0; pi < params->size; ++pi) {
                    Ast *p = vecGet(Ast *, params, pi);
                    if (!p || p->kind != AST_LVAR) continue;
                    if (p->pinned_kind != LVAR_REG || !p->pinned_reg) {
                        if (p->type && p->type->kind != AST_TYPE_FLOAT) {
                            int_idx++;
                        }
                        continue;
                    }
                    if (int_idx < max_int_args) {
                        /* libtasm dialect, both arches: "mov dst, src"
                         * => pinned = arg. */
                        aoStrCatFmt(shuffle,
                                "\tMOV %S, %s\n",
                                p->pinned_reg, kIntArgRegs[int_idx]);
                    }
                    int_idx++;
                }
                if (shuffle->len > 0) {
                    aoStrCatAoStr(shuffle, asm_block->asm_stmt);
                    aoStrRelease(asm_block->asm_stmt);
                    asm_block->asm_stmt = shuffle;
                } else {
                    aoStrRelease(shuffle);
                }
            }

            Ast *asm_function = astAsmFunctionDef(asm_fname, asm_block->asm_stmt);

            listAppend(asm_block->funcs, asm_function);
            listAppend(cc->asm_blocks, asm_block);

            Ast *asm_func = astAsmFunctionBind(
                    astMakeFunctionType(rettype, params),
                    asm_fname,asm_fname,params);

            if (!mapAddOrErr(cc->asm_functions, asm_fname->data, asm_func)) {
                cctrlIce(cc, "Already defined assembly function: %s", asm_fname->data);
            }

            if (!mapAddOrErr(cc->global_env, fname_duped->data, asm_func)) {
                cctrlIce(cc, "Already defined assembly function: %s as a non Assembly function", asm_fname->data);
            }

            if (is_inline) {
                asm_func->flags = AST_FLAG_INLINE;
            }

            if (!asmTextHasReturn(asm_block->asm_stmt)) {
                cctrlWarningAt(cc, fn_line, fn_col, fn_len,
                    "asm function '%.*s' has no RET; execution runs off the "
                    "end of the function - naked asm functions get no "
                    "epilogue, so add a `RET` yourself",
                    len, fname);
            }

            cctrlTokenExpect(cc,'}');
            cc->tmp_asm_fname = prev_asm_name;
            return asm_func;
        } else {
            cctrlRaiseException(cc,"Floating \"asm\" keyword, expected \"asm {\" got - %.*s", 
                    peek->len, peek->start);
        }
    }

    List *locals = listNew();
    Ast *func = NULL;
    List *body = listNew();
    Ast *func_body = astCompountStatement(body);
    AstType *fn_type = NULL;

    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cc->tmp_locals = locals;

    /* Upgrade a prototype to an actual function */
    func = mapGetLen(cc->global_env, fname, len);
    if (!func) {
        cc->tmp_params = params;
        cc->tmp_rettype = rettype;
        fn_type = astMakeFunctionType(cc->tmp_rettype, params);
        func = astFunction(fn_type,fname,len,params,NULL,locals,
                has_var_args);
        mapAdd(cc->global_env, func->fname->data, func);
    } else {
        switch (func->kind) {
            case AST_EXTERN_FUNC:
                cctrlRaiseException(cc,"Cannot redefine extern function: %.*s",len,fname);

            case AST_FUNC:
                if (cc->flags & CCTRL_REPL) {
                    /* REPL: shadow the old definition with a fresh
                     * function AST. Code already compiled against the
                     * old body keeps its old address; anything parsed
                     * from here on binds to this one. */
                    cc->tmp_params = params;
                    cc->tmp_rettype = rettype;
                    fn_type = astMakeFunctionType(cc->tmp_rettype, params);
                    func = astFunction(fn_type,fname,len,params,NULL,locals,
                            has_var_args);
                    mapAdd(cc->global_env, func->fname->data, func);
                    break;
                }
                cctrlRaiseException(cc,"Cannot redefine function: %.*s",len,fname);

            case AST_ASM_FUNC_BIND:
                cctrlRaiseException(cc,"Cannot redefine asm function: %.*s",
                        len,fname);

            case AST_FUN_PROTO:
                /* upgrade prototype to a function */
                func->locals = cc->tmp_locals;
                func->params = params;
                cc->tmp_params = func->params;
                cc->tmp_rettype = func->type->rettype;
                fn_type = func->type;
                func->kind = AST_FUNC;
                break;

            default:
                cctrlRaiseException(cc,"Unexpected function: %.*s -> %s",
                        cc->lineno,
                        len,fname, astToString(func));
                break;
        }
    }

    /* XXX: This allows us to do recursion by parsing the body after */
    cc->tmp_fname = func->fname;
    Ast *prev_func = cc->tmp_func;
    cc->tmp_func = func;
    func->body = func_body;

    if (is_inline) {
        func->flags |= AST_FLAG_INLINE;
        func->inline_ret = astLVar(func->type->rettype, str_lit("retval"));
    }
    parseCompoundStatementInternal(cc, func_body);
    fn_type->rettype = cc->tmp_rettype;
    /* `astFunction` shallow-copies the type into the AST node, so
     * `func->type` is a different AstType than the local `fn_type`.
     * Updating only `fn_type->rettype` would leave the version stored
     * in `cc->global_env` (and hence visible to every later caller)
     * still pointing at the original `auto` type. Mirror the
     * resolution onto `func->type` too. */
    if (func->type) {
        func->type->rettype = cc->tmp_rettype;
    }
    if (is_inline) {
        listAppend(func->locals,func->inline_ret);
    }

    /* TempleOS rule: a function containing an `asm { }` block may
     * declare register-pinned locals via `<Type> reg <REG> name`;
     * the asm block can then reference the register directly. Plain
     * locals (default stack) still work as ordinary stack slots that
     * the asm can reach through `&var[RBP]`. We reject `noreg` here
     * because the explicit-stack form duplicates the default and just
     * adds complexity. */
    int has_asm = 0;
    if (func_body->stms) {
        listForEach(func_body->stms) {
            Ast *stmt = (Ast *)it->value;
            if (stmt && stmt->kind == AST_ASM_STMT) {
                has_asm = 1;
                break;
            }
        }
    }
    if (has_asm && func->locals) {
        listForEach(func->locals) {
            Ast *l = (Ast *)it->value;
            if (!l || l->kind != AST_LVAR) continue;
            if (l->pinned_kind == LVAR_NOREG) {
                cctrlRaiseException(cc,
                    "`noreg` is not supported in `asm { }` functions; "
                    "drop the modifier (locals default to a stack slot) "
                    "or use `reg <REG>` to pin to a register",
                    l->lname ? l->lname->data : "?",
                    len, fname);
            }
        }
    }

    /* Warn when a value-returning function can run off its end without
     * a `return` - control would then return whatever happens to be in
     * the return register. `inline` functions return through `retval`
     * and `asm { }` bodies are opaque, so neither is checked. */
    AstType *rt = func->type ? func->type->rettype : NULL;
    if (rt && rt->kind != AST_TYPE_VOID && !(func->flags & AST_FLAG_INLINE) &&
        !has_asm && astStmtFallsThrough(func_body))
    {
        cctrlWarningAt(cc, fn_line, fn_col, fn_len,
            "control may reach the end of non-void function '%.*s' "
            "without returning a value",
            len, fname);
    }

    cc->localenv = NULL;
    cc->tmp_locals = NULL;
    cc->tmp_rettype = NULL;
    cc->tmp_params = NULL;
    cc->tmp_fname = NULL;
    cc->tmp_func = prev_func;
    return func;
}

Ast *parseExternFunctionProto(Cctrl *cc, AstType *rettype, char *fname, int len) {
    Ast *func;
    int has_var_args = 0;
    Lexeme *tok;
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cc->tmp_locals = NULL;

    Vec *params = parseParams(cc,')', &has_var_args,1);
    tok = cctrlTokenGet(cc);
    if (!tokenPunctIs(tok, ';')) {
        cctrlRaiseException(cc,"extern %.*s() cannot have a function body "
                "this will be defined elsewhere",
                len,fname);
    }
    AstType *type = astMakeFunctionType(rettype, params);
    func = astFunction(type,fname,len,params,NULL,NULL,0);
    func->kind = AST_EXTERN_FUNC;
    mapAdd(cc->global_env,func->fname->data,func);
    return func;
}

Ast *parseFunctionOrDef(Cctrl *cc, AstType *rettype, char *fname, int len, int is_inline) {
    int has_var_args = 0;
    cctrlTokenExpect(cc,'(');
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cc->tmp_locals = listNew();

    /* Reset the unique id counter otherwise we get ridiculous numbers */
    astResetLVarId();

    Vec *params = parseParams(cc,')',&has_var_args,1);
    Lexeme *tok = cctrlTokenGet(cc);
    if (tokenPunctIs(tok, '{')) {
        return parseFunctionDef(cc,rettype,fname,len,params,has_var_args,is_inline);
    } else if (tokenPunctIs(tok, ';')) {
        if (rettype->kind == AST_TYPE_AUTO) {
            cctrlRaiseException(cc,"auto cannot be used with a function prototype %.*s() at this time",
                    len,fname);
        }
        AstType *type = astMakeFunctionType(rettype, params);
        Ast *fn = astFunction(type,fname,len,params,NULL,NULL,has_var_args);
        fn->kind = AST_FUN_PROTO;
        mapAdd(cc->global_env,fn->fname->data,fn);
        return fn;
    } else {
        /* Neither `{ ... }` (definition) nor `;` (prototype). Almost
         * always a missing brace - call it out specifically so the
         * user doesn't get a downstream "TK_KEYWORD X can only prefix
         * a class" from the rest of the function body being parsed as
         * top-level declarations. */
        if (!tok) {
            cctrlRaiseException(cc,
                "Expected `{` to open the body of `%.*s()` or `;` for a "
                "prototype, got end of input",
                len, fname);
        }
        cctrlRaiseException(cc,
            "Expected `{` to open the body of `%.*s()` or `;` for a "
            "prototype, got `%.*s`",
            len, fname, tok->len, tok->start);
    }
}

Ast *parseAsmFunctionBinding(Cctrl *cc) {
    Lexeme *tok;
    AoStr *asm_fname, *c_fname;
    AstType *rettype;
    Ast *asm_func;
    int has_var_args = 0, is_inline = 0;

    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT && tok->start[0] != '_') {
        cctrlRaiseException(cc,"ASM function binds must begin with '_' got: %.*s",
                tok->len,tok->start);
    }

    asm_fname = aoStrDupRaw(tok->start,tok->len);
    Ast *asm_blk = mapGetLen(cc->asm_functions, asm_fname->data, asm_fname->len);

    rettype = parseDeclSpec(cc);

    if (rettype->kind == AST_TYPE_AUTO) {
        cctrlRaiseException(cc,"auto cannot be used with an assembly binding for function %s(), type cannot be automatically deduced",
                asm_fname->data);
    }

    tok = cctrlTokenGet(cc);
    if (tok->tk_type != TK_IDENT) {
        cctrlRaiseException(cc,"line %d: ASM function requires c function name");
    }
    c_fname = aoStrDupRaw(tok->start,tok->len);
    cc->localenv = cctrlCreateAstMap(cc->localenv);
    cctrlTokenExpect(cc,'(');

    Vec *params = parseParams(cc,')',&has_var_args,0);

    asm_func = astAsmFunctionBind(
            astMakeFunctionType(rettype, params),
            asm_fname,c_fname,params);

    /* update the assembly block so it knows the HC function name */
    if (asm_blk && asm_blk->fname == NULL) {
        asm_blk->fname = c_fname;
    }

    cc->localenv = NULL;
    /* Map a c function to an ASM function */
    mapAdd(cc->asm_funcs,c_fname->data,asm_func);
    cctrlTokenExpect(cc,';');
    if (is_inline) {
        asm_func->flags |= AST_FLAG_INLINE;
    }
    return asm_func;
}

Ast *parseToplevelDef(Cctrl *cc, int *is_global) {
    Ast *variable, *asm_block, *asm_func, *extern_func, *ast;
    AstType *type = NULL;
    Lexeme *tok, *name, *peek;
    int is_static = 0;

    while (1) {
        if ((tok = cctrlTokenGet(cc)) == NULL) {
            return NULL;
        }

        if (tok->tk_type == TK_KEYWORD) {
            switch (tok->i64) {
                case KW_ASM_EXTERN: {
                    if ((asm_func = parseAsmFunctionBinding(cc)) != NULL) {
                        return asm_func;
                    }
                    cctrlRaiseException(cc,"Floating \"_extern\" keyword, expected \"_extern '_ASM_FUNC'\""); 
                }

                case KW_ASM: {
                    peek = cctrlTokenPeek(cc);
                    if (tokenPunctIs(peek,'{')) {
                        asm_block = prsAsm(cc, 0);
                        listAppend(cc->asm_blocks,asm_block);
                        return asm_block;
                    }
                    cctrlRaiseException(cc,"Floating \"asm\" keyword, expected \"asm {\" got - %.*s",
                            peek->len, peek->start);
                }

                case KW_EXTERN: {
                    tok = cctrlTokenGet(cc);
                    if (tok->tk_type == TK_STR && !strncmp(tok->start,"c",1)) {
                        /* extern "c" func(...) - C-ABI function decl. */
                        type = parseDeclSpec(cc);
                        name = cctrlTokenGet(cc);
                        cctrlTokenExpect(cc,'(');
                        extern_func = parseExternFunctionProto(cc,type,
                                name->start,name->len);
                        return extern_func;
                    }
                    /* TempleOS-style `extern Type name;` data forward
                     * declaration. The next token was the start of the
                     * type; put it back so parseFullType sees it. */
                    cctrlTokenRewind(cc);
                    type = parseFullType(cc);
                    name = cctrlTokenGet(cc);
                    if (name->tk_type != TK_IDENT) {
                        cctrlRaiseException(cc,
                            "`extern <Type>` must be followed by an "
                            "identifier, got `%.*s`",
                            name->len, name->start);
                    }
                    type = parseArrayDimensions(cc, type);
                    cctrlTokenExpect(cc,';');
                    Ast *gvar = astGVar(type, name->start, name->len, 0);
                    Ast *decl = astDecl(gvar, NULL);
                    decl->flags |= AST_FLAG_EXTERN;
                    gvar->flags |= AST_FLAG_EXTERN;
                    mapAdd(cc->global_env, gvar->gname->data, gvar);
                    return decl;
                }
                case KW_INLINE: {
                    peek = cctrlTokenPeek(cc);
                    if (!cctrlGetKeyWord(cc, peek->start, peek->len)) {
                        cctrlRaiseException(cc,"Expected return type declaration got: '%.*s'",
                                peek->len,peek->start);
                    }
                    type = parseFullType(cc);
                    name = cctrlTokenGet(cc);
                    
                    ast = parseFunctionOrDef(cc,type,name->start,name->len,1); 
                    ast->flags |= AST_FLAG_INLINE;
                    return ast;
                }
                case KW_PUBLIC:
                case KW_PRIVATE:
                case KW_ATOMIC:
                    continue;

                case KW_STATIC:
                    type = parseDeclSpec(cc);
                    if (type == NULL) {
                        cctrlRaiseException(cc,"Expected type declaration");
                    }
                    /* static at the global scope does not yet do anything */
                    is_static = 1;
                    (void)is_static;
                    break;
                
                case KW_CLASS:
                    parseClassDef(cc,0);
                    cctrlTokenExpect(cc,';');
                    continue;

                case KW_UNION:
                    parseUnionDef(cc);
                    cctrlTokenExpect(cc,';');
                    continue;

                case KW_U0: 
                case KW_BOOL:
                case KW_I8:
                case KW_U8:
                case KW_I16:
                case KW_U16:
                case KW_I32:
                case KW_U32:
                case KW_I64:
                case KW_U64:
                case KW_F32:
                case KW_F64:
                case KW_AUTO:
                    cctrlTokenRewind(cc);
                    type = parseDeclSpec(cc);
                    break;

                case KW_IF:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseIfStatement(cc);

                case KW_WHILE:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseWhileStatement(cc);

                case KW_DO:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseDoWhileStatement(cc);

                case KW_FOR:
                    cc->tmp_locals = listNew();
                    *is_global = 1;
                    return parseForStatement(cc);

                case KW_SIZEOF:
                case KW_ALIGNOF:
                case KW_TYPEOF:
                    /* Floating `sizeof(x);` / `typeof(x);` - in the
                     * REPL these are expressions to evaluate + echo,
                     * same as `2+2;`. */
                    if (cc->flags & CCTRL_REPL) {
                        cctrlTokenRewind(cc);
                        ast = parseExpr(cc,16);
                        cctrlTokenExpect(cc,';');
                        *is_global = 1;
                        return ast;
                    }
                    cctrlRaiseException(cc,"Unexpected floating keyword: %.*s",
                            tok->len,tok->start);

                default:
                    cctrlRaiseException(cc,"Unexpected floating keyword: %.*s",
                            tok->len,tok->start);
            }
        } else if (tok->tk_type == TK_IDENT) {
            /* top level function call */
            peek = cctrlTokenPeek(cc);
            if (tokenPunctIs(peek,'(')) {
                cctrlTokenGet(cc);
                ast = parseFunctionArguments(cc,tok->start,tok->len,')');
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else if ((variable = mapGetLen(cc->global_env,tok->start, tok->len))) {
                cctrlTokenRewind(cc);
                ast = parseExpr(cc,16);
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            } else {
                cctrlTokenRewind(cc);
                type = parseDeclSpec(cc);
                if (type == NULL) {
                    cctrlRaiseException(cc,"Undefined type: %.*s",tok->len,tok->start);
                }
            }
        } else if (tok->tk_type == TK_CHAR_CONST) {
            ast = parseFloatingCharConst(cc,tok);
            *is_global = 1;
            return ast;
        } else if (tok->tk_type == TK_STR) {
            cctrlTokenRewind(cc);
            ast = parseFunctionArguments(cc,"printf",6,';');
            *is_global = 1;
            return ast;
        } else if (tok->tk_type == TK_I64) {
            if (cc->flags & CCTRL_REPL) {
                /* REPL: `2+2;` is an expression to evaluate + echo. */
                cctrlTokenRewind(cc);
                ast = parseExpr(cc,16);
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            }
            cctrlRaiseException(cc,"Floating integer constant '%ld' cannot be used in this context", tok->i64);
        } else if (tok->tk_type == TK_F64) {
            if (cc->flags & CCTRL_REPL) {
                cctrlTokenRewind(cc);
                ast = parseExpr(cc,16);
                cctrlTokenExpect(cc,';');
                *is_global = 1;
                return ast;
            }
            cctrlRaiseException(cc,"Floating float constant '%f' cannot be used in this context", tok->f64);
        } else if (tokenPunctIs(tok,TK_PLUS_PLUS) ||
                   tokenPunctIs(tok,TK_MINUS_MINUS))
        {
            /* `++x;` / `--x;` - a side-effecting statement, legal at
             * the top level in scripting mode. */
            cctrlTokenRewind(cc);
            ast = parseExpr(cc,16);
            cctrlTokenExpect(cc,';');
            *is_global = 1;
            return ast;
        } else if ((cc->flags & CCTRL_REPL) &&
                   (tokenPunctIs(tok,'(') || tokenPunctIs(tok,'-') ||
                    tokenPunctIs(tok,'+') || tokenPunctIs(tok,'~') ||
                    tokenPunctIs(tok,'!') || tokenPunctIs(tok,'*') ||
                    tokenPunctIs(tok,'&')))
        {
            /* REPL: unary/parenthesised expression at the top level. */
            cctrlTokenRewind(cc);
            ast = parseExpr(cc,16);
            cctrlTokenExpect(cc,';');
            *is_global = 1;
            return ast;
        }

        name = cctrlTokenGet(cc);
        /* End of input mid-definition: stop parsing so any accumulated
         * diagnostics surface, rather than dereferencing a NULL token. */
        if (name == NULL) {
            return NULL;
        }

        if (name->tk_type == TK_KEYWORD) {
            switch (name->i64) {
                case KW_CLASS:
                    if (!astIsIntType(type)) {
                        cctrlRaiseException(cc,"Can only make intrinsic types from integer types, got %s",
                                astTypeToString(type));
                    }
                    parseClassDef(cc,1);
                    cctrlTokenExpect(cc,';');
                    continue;
                default: {
                    cctrlRaiseException(cc,"%s can only prefix a class",lexemeToString(name));
                }
            }
        }

        if (name->tk_type != TK_IDENT) {
            cctrlRaiseException(cc,"Identifier expected: got %s",lexemeToString(name));
        }

        /* Every legitimate route to the declaration code below parsed a
         * type first. Reaching here without one means the input opened
         * with a punct we don't treat as a statement (`*p = ...;` at
         * the top level, say) - raise instead of dereferencing NULL. */
        if (type == NULL) {
            cctrlRaiseException(cc,
                    "Expected a type declaration before `%.*s`",
                    name->len, name->start);
        }

        type = parseArrayDimensions(cc,type);
        tok = cctrlTokenPeek(cc);

        /* Global class/struct/union with an aggregate initialiser:
         *   Color Black = {0,0,0};
         * Route through parseVariableInitialiser like arrays do — its
         * parseVariableAssignment already lowers `{...}` into an ARRAY_INIT
         * declinit that the data emitters write out. The scalar `=` branch
         * below parses the RHS with parseExpr, which can't handle `{`.
         * (peek(1) is the token after `=`, i.e. the `{`.) */
        if (tokenPunctIs(tok,'=') &&
            type->kind == AST_TYPE_CLASS && !type->is_intrinsic &&
            tokenPunctIs(cctrlTokenPeekBy(cc,1),'{'))
        {
            variable = astGVar(type,name->start,name->len,0);
            mapAdd(cc->global_env,variable->gname->data,variable);
            cc->tmp_gvar_decl = variable;
            return parseVariableInitialiser(cc,variable,
                                            PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
        }

        if (tokenPunctIs(tok,'=') && type->kind != AST_TYPE_ARRAY) {
            variable = astGVar(type,name->start,name->len,0);
            Ast *ast_decl = astDecl(variable,NULL);

            listAppend(cc->ast_list,ast_decl);
            mapAdd(cc->global_env,variable->gname->data,variable);
            cc->tmp_gvar_decl = variable;

            /* `auto foo = <expr>;` at file scope: the variable's type
             * isn't known until the initialiser is parsed, and building
             * the assignment with an unresolved `auto` LHS makes the
             * type-checker reject it. Consume the `=`, parse just the
             * RHS, infer the type, then build the (now well-typed)
             * assignment by hand. */
            if (type->kind == AST_TYPE_AUTO) {
                cctrlTokenGet(cc);                  /* consume '=' */
                Ast *rhs = parseExpr(cc,16);
                if (!rhs || !rhs->type) {
                    cctrlRaiseException(cc,
                            "auto cannot be used without an initialiser");
                }
                variable->type = rhs->type;
                if (rhs->kind == AST_STRING) {
                    ast_decl->declinit = rhs;
                    cctrlTokenExpect(cc,';');
                    return ast_decl;
                }
                int is_err = 0;
                Ast *assign = astBinaryOp(AST_BIN_OP_ASSIGN, variable,
                                          rhs, &is_err);
                *is_global = 1;
                cctrlTokenExpect(cc,';');
                return assign;
            }

            cctrlTokenRewind(cc);

            Ast *ast_expr = parseExpr(cc,16); // parseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);

            if (ast_expr->right->kind != AST_STRING) {
                *is_global = 1;
            } else {
                ast_decl->declinit = ast_expr->right;
                cctrlTokenExpect(cc,';');
                return ast_decl;
            }
            cctrlTokenExpect(cc,';');
            return ast_expr;
        } else if (type->kind == AST_TYPE_ARRAY) {
            variable = astGVar(type,name->start,name->len,0);
            mapAdd(cc->global_env,variable->gname->data,variable);
            cc->tmp_gvar_decl = variable;
            ast = parseVariableInitialiser(cc,variable,PUNCT_TERM_COMMA|PUNCT_TERM_SEMI);
            if (type->kind == AST_TYPE_AUTO) {
                parseAssignAuto(cc,ast);
            }
            return ast;
        }

        if (tokenPunctIs(tok, '(')) {
            return parseFunctionOrDef(cc,type,name->start,name->len,0); 
        }

        if (tokenPunctIs(tok,';') || tokenPunctIs(tok, ',')) {
            cctrlTokenGet(cc);
            variable = astGVar(type,name->start,name->len,0);
            mapAdd(cc->global_env,variable->gname->data,variable);
            return astDecl(variable,NULL);
        }
        cctrlRaiseException(cc,"Cannot handle '%s'",lexemeToString(tok));
    }
}

void parseToAst(Cctrl *cc) {
    Ast *ast;
    Lexeme *tok;
    int is_global = 0;

    /* The previous parse can leave these dangling: the loop below
     * only resets them when it keeps iterating, not when it breaks at
     * EOF. A stale tmp_locals is fatal for the next parse (the REPL) -
     * the first global statement would listMergeAppend (and free!) a
     * list that may already have been merged and recycled. */
    cc->tmp_locals = NULL;
    cc->localenv = NULL;
    cc->tmp_gvar_decl = NULL;

    /* Top-level recovery point. cctrlRaiseException longjmps here
     * once a CctrlDiagnostic is queued; we wipe function-scoped state
     * and skip tokens until the next plausible decl boundary, then
     * loop. parseCompoundStatementInternal installs its own
     * (finer-grained) recovery point for statements inside a
     * function body and restores ours on return. */
    jmp_buf recovery;
    jmp_buf *prev_recovery = cc->current_recovery;
    cc->current_recovery = &recovery;

    while (1) {
        /* Snapshot the tails of the lists a top-level decl can
         * append to. Mirrors the per-statement snapshot in
         * parseCompoundStatementInternal: if the decl longjmps
         * partway, we truncate any half-built additions so the
         * accumulated program list stays consistent. */
        List *ast_tail = cc->ast_list ? cc->ast_list->prev : NULL;
        List *init_tail = cc->initalisers ? cc->initalisers->prev : NULL;
        List *init_locals_tail = cc->initaliser_locals
                                 ? cc->initaliser_locals->prev : NULL;

        if (setjmp(recovery) != 0) {
            cc->tmp_locals = NULL;
            cc->localenv = NULL;
            cc->tmp_func = NULL;
            cc->tmp_rettype = NULL;
            cc->tmp_loop_begin = NULL;
            cc->tmp_loop_end = NULL;
            /* The lists below roll back the half-built AST_DECL; the
             * variable's global_env entry must go with it, or later
             * statements can reference storage that no longer exists. */
            if (cc->tmp_gvar_decl) {
                mapRemove(cc->global_env, cc->tmp_gvar_decl->gname->data);
                cc->tmp_gvar_decl = NULL;
            }
            if (cc->ast_list && ast_tail) {
                ast_tail->next = cc->ast_list;
                cc->ast_list->prev = ast_tail;
            }
            if (cc->initalisers && init_tail) {
                init_tail->next = cc->initalisers;
                cc->initalisers->prev = init_tail;
            }
            if (cc->initaliser_locals && init_locals_tail) {
                init_locals_tail->next = cc->initaliser_locals;
                cc->initaliser_locals->prev = init_locals_tail;
            }
            cctrlSyncToplevel(cc);
            tok = cctrlTokenPeek(cc);
            if (!tok) break;
            continue;
        }

        ast = parseToplevelDef(cc, &is_global);
        cc->tmp_gvar_decl = NULL; /* decl completed - nothing to roll back */
        if (ast == NULL) break;
        if (is_global) {
            listAppend(cc->initalisers,ast);
            if (!listEmpty(cc->tmp_locals)) {
                listMergeAppend(cc->initaliser_locals,cc->tmp_locals);
            }
        } else if (ast->kind != AST_FUN_PROTO) {
            listAppend(cc->ast_list,ast);
        }
        is_global = 0;
        tok = cctrlTokenPeek(cc);
        if (!tok) break;
        cc->tmp_locals = NULL;
        cc->localenv = NULL;
    }

    cc->current_recovery = prev_recovery;
}
