set cindent
set filetype=HC
set syntax=c

syn keyword cTypeUnhighlighted bool char short int float double unsigned signed const void

syn keyword cType     _extern extern inline static U0 Bool I8 U8 I16 U16 I32 U32 I64 U64 F64 auto atomic
syn keyword cRepeat   while for do
syn keyword cConstant NULL FALSE TRUE EXIT_FAIL EXIT_OK I64_MIN I64_MAX U64_MAX U8_MAX I8_MAX I8_MIN STDOUT STDERR __BUFSIZ__ STDIN
syn keyword cOperator public private sizeof
syn clear cStatement
syn keyword cCast cast
syn keyword cKeyword return continue break goto
syn keyword cClass class

syn match cAsmKeyword "\<\asm\>"
hi def link cAsmKeyword Keyword
hi def link cKeyword Keyword
hi def link cCast Statement
hi def link cClass cTypedef

" this is a bit iffy
syntax region cAsm start="\<asm\>\s*{" end="}" contains=cAsmLabel,cAsmFunction,cAsmOp,cAsmCall,cAsmMath,cAsmKeyword,cComment,cCommentL,cNumbers
syntax match cAsmLabel "@@\d\+" contained
syntax match cAsmFunction /^\s*\(\w\+::\)/ contained
syntax match cAsmOp "\<\(MOV\|PUSH\|POP\|LEA\|TEST\
            \|CLD\|REP\|LOD\|STOSB\|DEC\|RET\|CMP\|INC\|SCASB\|XCHG\
            \|CLI\|BT\|PAUSE\|JMP\|JZ\|JNZ\|JE\|JNE\|JB\|JBE\|JA\|JAE\)\S*\>" contained
syntax match cAsmCall "\<\(CALL\)\S*\>" contained
syntax match cAsmMath "\<\(ADD\|SUB\|XOR\|OR\|AND\|MUL\
            \|NOT\|MOD\|DIV\|SHL\|SHR\)\S*\>" contained

hi def link cAsmFunction Function
hi def link cAsmLabel cOperator 
hi def link cAsmOp cType
hi def link cAsmMath cOperator
hi def link cAsmCall cOperator 

syntax region _HCString start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=HCStringEscape,HCStringFormat

syntax match HCStringEscape /\\./ contained
syntax match HCStringFormat /%\(\d\+\$\)\=[-+' #0*]*\(\d*\|\*\|\*\d\+\$\)\(\.\(\d*\|\*\|\*\d\+\$\)\)\=\([hlLjzt]\|ll\|hh\)\=\([aAbdiuoxXDOUfFeEgGcCsSpn]\|\[\^\=.[^]]*\]\)/ contained
syntax match HCStringFormat /%%/ contained
syntax match HCStringFormat /\\[nrvtb'\\]/ contained
hi def link _HCString String
hi def link HCStringFormat cFormat

syn keyword cDefine elifdef
hi def link cAnsiFunction cFunction
hi def link cAnsiName cIdentifier
syn keyword cDefined defined contained containedin=cDefine
hi def link cDefined cDefine
hi def link cUserFunction cFunction
hi def link cFunction Function
hi def link cIdentifier Identifier
hi def link cDelimiter Delimiter
hi def link cBraces Delimiter
hi def link cBoolean Boolean
