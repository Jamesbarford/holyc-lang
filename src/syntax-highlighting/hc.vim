" Vim syntax file
" Language:   HolyC (TempleOS / TempleOS-derived compilers)
" Filename:   *.HC, *.HH
" Note:       This is a *standalone* syntax definition. It deliberately does
"             NOT inherit from the bundled C syntax. Every construct is
"             defined here so HolyC keeps its own identity.
"
" Ordering matters: when a plain :syn-match and a :syn-region can start at the
" same column, the item defined LAST wins. So the greedy single-character
" operator/delimiter matches are defined first (lowest priority) and the
" things that must beat them (comments, strings, numbers) come afterwards.
" :syn-keyword items always outrank matches/regions regardless of order.

" ---------------------------------------------------------------------------
" Buffer-local options for HolyC files
" ---------------------------------------------------------------------------
setlocal cindent
setlocal tabstop=4 softtabstop=0 expandtab shiftwidth=2 smarttab

" Keep the filetype as HC so the LSP / format keymaps still recognise it.
if &filetype !=# "HC"
  setlocal filetype=HC
endif

" Don't rebuild the syntax if it is already in place for this buffer.
if exists("b:current_syntax") && b:current_syntax ==# "hc"
  finish
endif

syntax clear
syntax case match

" ---------------------------------------------------------------------------
" Operators & delimiters (lowest priority is defined first)
" ---------------------------------------------------------------------------
" syn match hcOperator  "[-+*/%=<>!~&|^?:]" " I'm not a fan so commented out
" syn match hcDelimiter "[(){}\[\];,.]"     " I also don't particularly like
                                            " coloured punctuation

" ---------------------------------------------------------------------------
" Numbers
" ---------------------------------------------------------------------------
syn match hcNumber /\<0[xX]\x\+\>/
syn match hcNumber /\<0[bB][01]\+\>/
syn match hcNumber /\<\d\+\%([uU]\=[lL]\{0,2}\)\>/
syn match hcFloat  /\<\d\+\.\d*\%([eE][-+]\=\d\+\)\=\>/
syn match hcFloat  /\<\d\+[eE][-+]\=\d\+\>/
syn match hcFloat  /\.\d\+\%([eE][-+]\=\d\+\)\=\>/

" ---------------------------------------------------------------------------
" Strings & character constants
" ---------------------------------------------------------------------------
syn match  hcEscape /\\./ contained
" printf-style conversion specifiers (with HolyC extras: b z q t)
syn match  hcFormat /%\%(\d\+\$\)\=[-+' #0*]*\%(\d*\|\*\|\*\d\+\$\)\%(\.\%(\d*\|\*\|\*\d\+\$\)\)\=\%([hlLjzt]\|ll\|hh\)\=\%([aAbdiuoxXDOUfFeEgGcCsSpnqtz]\|\[\^\=.[^]]*\]\)/ contained
syn match  hcFormat /%%/ contained
" HolyC DolDoc command embedded in a string ($FG,2$ ... $FG$); literal $ is $$.
syn match  hcDolDoc /\$[^$]*\$/ contained

syn region hcString    start=+"+ skip=+\\\\\|\\"+ end=+"+ contains=hcEscape,hcFormat,hcDolDoc
" HolyC character constants may pack up to 8 bytes: 'A', '\n', 'TEXT'
syn match  hcCharacter "'\%(\\.\|[^'\\]\)\{1,8}'" contains=hcEscape

" ---------------------------------------------------------------------------
" Comments
" ---------------------------------------------------------------------------
syn keyword hcTodo            contained TODO FIXME XXX NOTE HACK BUG
syn match   hcCommentAnnotation /@\w\+/ contained
syn region  hcCommentLine     start="//" skip="\\$" end="$" keepend contains=hcTodo,hcCommentAnnotation,@Spell
syn region  hcComment         start="/\*" end="\*/" contains=hcTodo,hcCommentAnnotation,@Spell

" ---------------------------------------------------------------------------
" Preprocessor / compiler directives
" ---------------------------------------------------------------------------
syn match   hcPreProc /^\s*#\s*\%(define\|undef\|ifdef\|ifndef\|ifaot\|ifjit\|if\|elifdef\|elif\|else\|endif\|exe\|help_index\|help_file\|assert\|error\|warning\|pragma\|import\|public\|extern\)\>/
syn keyword hcPreProcDefined defined

syn region	hcIncluded	display contained start=+"+ skip=+\\\\\|\\"+ end=+"+
syn match	hcIncluded	display contained "<[^>]*>"
syn match	hcInclude	display "^\s*\zs\%(%:\|#\)\s*include\>\s*["<]" contains=hcIncluded
syn match	hcInclude	display "^\s*\zs\%(%:\|#\)\s*link\>\s*["<]" contains=hcIncluded

" ---------------------------------------------------------------------------
" Types & structures
" ---------------------------------------------------------------------------
syn keyword hcType      U0 Bool I8 U8 I16 U16 I32 U32 I64 U64 F32 F64 auto
syn keyword hcStructure class union

syntax match PascalType /\%(\( \|\t\|\n\|$\|<\|}\|(\)\{1}\)\@<=\<\u[[:alnum:]]*\>\( \|\t\|\n\|$\|>\)\%( \|=\|,\|!\|:\)\@!/ containedin=cStatement,cType,cStructure
highlight def link PascalType Type

" ---------------------------------------------------------------------------
" Storage classes / qualifiers
" ---------------------------------------------------------------------------
syn keyword hcStorageClass extern _extern import _import public private static
syn keyword hcStorageClass reg noreg interrupt haserrcode argpop noargpop
syn keyword hcStorageClass nofink inline atomic lastclass

" ---------------------------------------------------------------------------
" Statements / keywords
" ---------------------------------------------------------------------------
syn keyword hcConditional if else switch case default start end
syn keyword hcRepeat      while for do
syn keyword hcStatement   return goto break continue
syn keyword hcOperatorKw  sizeof alignof offset try catch throw
" Note: `asm` is intentionally NOT a keyword here a keyword match at `asm`
" would block the asm{} region below from ever starting. The region's
" matchgroup highlights `asm {` instead.

" ---------------------------------------------------------------------------
" Constants
" ---------------------------------------------------------------------------
syn keyword hcBoolean  TRUE FALSE ON OFF
syn keyword hcConstant NULL EXIT_FAILURE EXIT_SUCCESS EXIT_FAIL EXIT_OK
syn keyword hcConstant I64_MIN I64_MAX I32_MIN I32_MAX I16_MIN I16_MAX
syn keyword hcConstant I8_MIN I8_MAX U64_MAX U32_MAX U16_MAX U8_MAX
syn keyword hcConstant STDIN STDOUT STDERR __BUFSIZ__

" ---------------------------------------------------------------------------
" Functions / scopes (matches outranked by the keywords above)
" ---------------------------------------------------------------------------
" An identifier immediately followed by '(' is a function (def or call).
"syn match hcFunction /\<\h\w*\ze\s*(/
" Member / scope resolution: Name::
syn match hcScope    /\<\h\w*\ze::/

" ---------------------------------------------------------------------------
" Inline assembly: asm { ... }
" ---------------------------------------------------------------------------
syn region hcAsm matchgroup=hcLabelKw start="\<asm\>\s*{" end="}" contains=hcAsmLabel,hcAsmFunction,hcComment,hcCommentLine,hcNumber,hcString,hcCharacter
syn match  hcAsmLabel    "@@\d\+" contained
syn match  hcAsmFunction /^\s*\w\+::/ contained

" ---------------------------------------------------------------------------
" Highlight links
" ---------------------------------------------------------------------------
hi def link hcComment           Comment
hi def link hcCommentLine       Comment
hi def link hcTodo              Todo
hi def link hcCommentAnnotation Todo

hi def link hcPreProc           PreProc
hi def link hcPreProcDefined    PreProc

hi def link hcType              Type
hi def link hcStructure         Structure
hi def link hcStorageClass      StorageClass

hi def link hcConditional       Conditional
hi def link hcRepeat            Repeat
hi def link hcStatement         Statement
hi def link hcOperatorKw        Statement
hi def link hcLabelKw           Keyword

hi def link hcBoolean           Boolean
hi def link hcConstant          Constant

hi def link hcNumber            Number
hi def link hcFloat             Float

hi def link hcString            String
hi def link hcCharacter         Character
hi def link hcEscape            SpecialChar
hi def link hcFormat            Special
hi def link hcDolDoc            PreProc

hi def link hcFunction          Function
hi def link hcScope             Identifier

hi def link hcOperator          Operator
hi def link hcDelimiter         Delimiter

hi def link hcAsm               Special
hi def link hcAsmLabel          Label
hi def link hcAsmFunction       Function

hi def link hcInclude           Include
hi def link hcIncluded          hcString



let b:current_syntax = "hc"
