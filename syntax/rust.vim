if version < 600
    syntax clear
elseif exists("b:current_syntax")
    finish
endif
let b:current_syntax = "rust"

syntax region rsTypeParams matchgroup=rsDelimiter start='<' end='>' contains=@rsTOP

"
" Conditionals
"

syntax keyword rsElse else
syntax keyword rsIf if
syntax keyword rsMatch match

highlight default link rsElse rsConditional
highlight default link rsIf rsConditional
highlight default link rsMatch rsConditional

"
" Repeat
"

syntax keyword rsFor for
syntax keyword rsLoop loop
syntax keyword rsWhile while

highlight default link rsFor rsRepeat
highlight default link rsLoop rsRepeat
highlight default link rsWhile rsRepeat

"
" Includes
"

syntax keyword rsUse use
highlight default link rsUse rsInclude

"
" Other keywords
"

syntax keyword rsAs as
syntax keyword rsAsync async
syntax keyword rsAwait await
syntax keyword rsBreak break
syntax keyword rsConst const nextgroup=rsFn,rsIdentDef,rsUnusedIdentDef skipwhite skipempty
syntax keyword rsContinue continue
syntax keyword rsCrate crate
syntax keyword rsDyn dyn
syntax keyword rsEnum enum nextgroup=rsTypeDef,rsUnusedTypeDef skipwhite skipempty
syntax keyword rsExtern extern
syntax keyword rsFn fn nextgroup=rsFuncDef,rsUnusedFuncDef skipwhite skipempty
" TODO: wrong
syntax keyword rsImpl impl nextgroup=rsTypeDefParams
syntax keyword rsIn in
syntax keyword rsLet let nextgroup=rsIdentDef,rsUnusedIdentDef,rsMut,rsRef,rsPattern skipwhite skipempty
syntax keyword rsMod mod
syntax keyword rsMove move
syntax keyword rsMut mut nextgroup=rsIdentDef,rsUnusedIdentDef,rsLibraryType,rsSelfType,rsSelfValue,rsUserType skipwhite skipempty
syntax keyword rsPub pub
syntax keyword rsRef ref nextgroup=rsIdentDef,rsUnusedIdentDef,rsMut skipwhite skipempty
syntax keyword rsReturn return
syntax keyword rsSelfType Self
syntax keyword rsSelfValue self
syntax keyword rsStatic static nextgroup=rsIdentDef,rsUnusedIdentDef,rsRef skipwhite skipempty
syntax keyword rsStruct struct nextgroup=rsTypeDef,rsUnusedTypeDef skipwhite skipempty
syntax keyword rsSuper super
syntax keyword rsTrait trait nextgroup=rsTypeDef,rsUnusedTypeDef skipwhite skipempty
syntax keyword rsTypeAlias type nextgroup=rsTypeDef,rsUnusedTypeDef skipwhite skipempty
syntax keyword rsUnderscore _
syntax keyword rsUnion union nextgroup=rsTypeDef,rsUnusedTypeDef skipwhite skipempty
syntax keyword rsUnsafe unsafe
syntax keyword rsWhere where

highlight default link rsAs rsOperator
highlight default link rsAsync rsKeyword
highlight default link rsAwait rsKeyword
highlight default link rsBreak rsKeyword
highlight default link rsConst rsKeyword
highlight default link rsContinue rsKeyword
highlight default link rsCrate rsKeyword
highlight default link rsDyn rsKeyword
highlight default link rsEnum rsKeyword
highlight default link rsExtern rsKeyword
highlight default link rsFn rsKeyword
highlight default link rsImpl rsKeyword
highlight default link rsIn rsKeyword
highlight default link rsLet rsKeyword
highlight default link rsMod rsKeyword
highlight default link rsMove rsKeyword
highlight default link rsMut rsKeyword
highlight default link rsPub rsKeyword
highlight default link rsRef rsKeyword
highlight default link rsReturn rsKeyword
highlight default link rsSelfType Type
highlight default link rsSelfValue Constant
highlight default link rsStatic rsKeyword
highlight default link rsStruct rsKeyword
highlight default link rsSuper rsKeyword
highlight default link rsTrait rsKeyword
highlight default link rsTypeAlias rsKeyword
highlight default link rsUnderscore Operator
highlight default link rsUnion rsKeyword
highlight default link rsUnsafe rsKeyword
highlight default link rsWhere rsKeyword

"
" Booleans
"

syntax keyword rsTrue true
syntax keyword rsFalse false

highlight default link rsTrue rsBoolean
highlight default link rsFalse rsBoolean

"
" Strings
"

syntax region rsString
            \ matchgroup=rsQuote
            \ start='[br]\?"'
            \ skip='\\"'
            \ end='"'
            \ contains=@Spell

"
" Field access
"

syntax match rsFieldAccess '\v(\.)@<=[a-z][a-z0-9_]*>(\()@!'

"
" Helpers for matching foreign and crate-local items
"

" Foreign items are always preceded by zero or more type names separated by ‘::’
" (think nested enum variants) and at least one module path. This module path is
" preceded by a word separator to prevent matching partially on type names (i.e.
" skipping the instal capital letter).
function! MatchForeign(regex, groupName, extraParams)
    execute 'syntax match ' . a:groupName . ' "\v(<[a-z][a-z0-9_]*::([A-Z][A-Za-z0-9]*::)*)@<=' . a:regex . '"' . a:extraParams
endfunction

"
" Generic types
"

syntax match rsUserType '\v<[A-Z][A-Za-z0-9]*\ze\s*\<' nextgroup=rsTypeParams
call MatchForeign('[A-Z][A-Za-z0-9]*\ze\s*\<', 'rsForeignType', ' nextgroup=rsTypeParams')



" Constructor, enum variants
syn match rsVariant '\<[A-Z][A-Za-z0-9]*\ze\s*('
" must exit type region(?), TODO: for expr
syn match rsVariant '\(\(impl\|for\|->[^{]*\)\s*\)\@<!\<[A-Z][A-Za-z0-9]*\ze\s*{'
" TODO: associated types
syn match rsVariant '\v(<[A-Z][A-Za-z0-9]*::)@<=[A-Z][A-Za-z0-9]*'
syn keyword rsVariant None

"
" Standard library lowercase types
"

let s:standardLibraryTypes = ["blkcnt_t", "blksize_t", "bool", "c_char", "c_double", "c_float", "c_int", "c_long", "c_longlong", "c_schar", "c_short", "c_uchar", "c_uint", "c_ulong", "c_ulonglong", "c_ushort", "char", "dev_t", "f32", "f64", "gid_t", "i128", "i16", "i32", "i64", "i8", "ino_t", "isize", "mode_t", "nlink_t", "off_t", "pid_t", "pthread_t", "str", "time_t", "u128", "u16", "u32", "u64", "u8", "uid_t", "usize"]

for s:standardLibraryType in s:standardLibraryTypes
    execute 'syntax keyword rsLibraryType ' . s:standardLibraryType . ' nextgroup=rsTypeParams'
endfor

"
" Constants
"

syntax match rsUserConst '\v<[A-Z][A-Z0-9_]+>'
call MatchForeign('[A-Z][A-Z0-9_]+>', 'rsForeignConst', '')


"
" Macros
"

syntax match rsUserMacro '\v<[a-z][a-z0-9_]*!'
call MatchForeign('[a-z][a-z0-9_]*!', 'rsForeignMacro', '')

"
" Functions
"

syntax match rsUserFunc '\v<[a-z]\w*(\()@='

call MatchForeign('[a-z][a-z0-9_]*(\()@=', 'rsForeignFunc', '')

syntax match rsUserMethod '\v(\.)@<=[a-z][a-z0-9_]*(\(|::)@='
highlight default link rsUserMethod rsUserFunc

"
" Lifetimes
"

syntax match rsUserLifetime "'[a-z][a-z0-9_]*"

syntax match rsInferredLifetime "'_"
syntax match rsStaticLifetime "'static"

highlight default link rsInferredLifetime rsSpecialLifetime
highlight default link rsStaticLifetime rsSpecialLifetime

"
" Type definitions
"

syntax match rsTypeDef '\v[A-Z][A-Za-z0-9]*'
            \ contained
            \ nextgroup=rsTypeDefParams

syntax match rsUnusedTypeDef '\v_[A-Za-z0-9]+'
            \ contained
            \ nextgroup=rsTypeDefParams

highlight default link rsUnusedTypeDef rsTypeDef

" Type parameters
syntax region rsTypeDefParams
            \ matchgroup=rsDelimiter
            \ start='<'
            \ end='>'
            \ contains=@rsTOP

syntax match rsTypeParamDef '[A-Z][A-Za-z0-9]*\ze:'
            \ contained
            \ containedin=rsTypeDefParams

highlight default link rsTypeParamDef rsTypeDef

"
" Function definitions
"

syntax match rsFuncDef '\v<[a-z]\w*'
            \ contained
            \ nextgroup=rsTypeDefParams

syntax match rsUnusedFuncDef '\v<_[a-z0-9_]+'
            \ contained
            \ nextgroup=rsTypeDefParams

highlight default link rsUnusedFuncDef rsFuncDef

"
" Identifier definitions
"

syntax match rsIdentDef '\v<[a-z][a-z0-9_]*>' contained display
syntax match rsIdentDef '\v<[A-Z][A-Z0-9_]*>' contained display

syntax match rsUnusedIdentDef '\v<_[a-z0-9_]+>' contained display
syntax match rsUnusedIdentDef '\v<_[A-Z0-9_]+>' contained display

highlight default link rsUnusedIdentDef rsIdentDef

syntax region rsPattern
            \ matchgroup=rsDelimiter
            \ start='('
            \ end=')'
            \ contained
            \ contains=rsMut,rsRef,rsDelimiter,rsOperator,rsLibraryType,rsUserType,rsIdentDef,rsUnusedIdentDef,rsUnderscore

"
" Lifetime definitions
"

syntax match rsLifetimeDef "'[a-z][a-z0-9_]*"
            \ contained
            \ containedin=rsTypeDefParams

"
" Numbers
"

syntax match rsNumber '\v<\d[0-9_]*((u|i)(size|8|16|32|64|128))?'
syntax match rsFloat '\v<\d[0-9_]*\.\d[0-9_]*(f(32|64))?'

"
" Attributes
"

syntax region rsAttribute
            \ matchgroup=rsDelimiter
            \ start='\v#!?\['
            \ end=']'

syntax region rsAttributeParen
            \ matchgroup=rsDelimiter
            \ start='('
            \ end=')'
            \ containedin=rsAttribute
            \ contains=@rsTOP

"
" Macro identifiers
"

" Macros frequently interpolate identifiers with names like #foobar.
syntax match rsUserIdent '\v#[a-z][a-z0-9_]*'

" macro_rules! uses $foobar for parameters
syntax match rsUserIdent '\v\$[a-z][a-z0-9_]*'

"
" Characters
"

syntax match rsCharacter "'.'"

"
" Delimiters
"

syntax match rsDelimiter '[\.,:;]'
syntax region rsParen   matchgroup=rsDelimiter  start='(' end=')' contains=@rsTOP
syntax region rsBrace   matchgroup=rsDelimiter  start='{' end='}' contains=@rsTOP
syntax region rsBracket matchgroup=rsDelimiter start='\[' end=']' contains=@rsTOP

"
" Operators
"

syntax match rsOperator '[!%&|/\*+<=>?\^-]\+'

" We highlight mutable references separately as an operator because otherwise
" they would be recognised as the ‘mut’ keyword, thus whatever comes after the
" ‘mut’ is highlighted as an identifier definition.
syntax match rsOperator '&mut'

"
" Comments
"

syntax region rsComment start='//' end='$' contains=@Spell

syntax region rsBlockComment start='/\*' end='\*/' contains=@Spell

syntax region rsDocComment start='///' end='$' contains=@Spell
syntax region rsDocComment start='//!' end='$' contains=@Spell

syntax match rsCommentNote '\v(TODO|NOTE|BUG|FIXME)(:)@='
            \ contained
            \ containedin=rsComment,rsDocComment

"
" Rust syntax groups that don't have "contained" argument. Use @rsTOP instead
" of TOP to make highlighting robust when this definition is included by other
" syntax definition, e.g. markdown.
"

syntax cluster rsTOP contains=rsTypeParams,rsElse,rsIf,rsMatch,rsFor,rsLoop,rsWhile,rsUse,rsAs,rsAsync,rsAwait,rsBreak,rsConst,rsContinue,rsCrate,rsDyn,rsEnum,rsExtern,rsFn,rsImpl,rsIn,rsLet,rsMod,rsMove,rsMut,rsPub,rsRef,rsReturn,rsSelfType,rsSelfValue,rsStatic,rsStruct,rsSuper,rsTrait,rsTypeAlias,rsUnderscore,rsUnion,rsUnsafe,rsWhere,rsTrue,rsFalse,rsString,rsFieldAccess,rsUserType,rsForeignType,rsVariant,rsLibraryType,rsUserConst,rsForeignConst,rsUserMacro,rsForeignMacro,rsUserFunc,rsForeignFunc,rsUserMethod,rsUserLifetime,rsInferredLifetime,rsStaticLifetime,rsNumber,rsFloat,rsAttribute,rsAttributeParen,rsUserIdent,rsCharacter,rsDelimiter,rsParen,rsBrace,rsBracket,rsOperator,rsComment,rsBlockComment,rsDocComment

"
" Default linkages
"

highlight default link rsAttribute rsKeyword
highlight default link rsBoolean Boolean
highlight default link rsCharacter Character
highlight default link rsComment Comment
highlight default link rsCommentNote Todo
highlight default link rsConditional Conditional
highlight default link rsDelimiter Delimiter
highlight default link rsDocComment Comment
" highlight default link rsFieldAccess Identifier
highlight default link rsFloat Float
highlight default link rsForeignConst Constant
highlight default link rsForeignFunc Function
highlight default link rsForeignMacro Macro
highlight default link rsForeignType Type
highlight default link rsVariant Constant
highlight default link rsFuncDef Function
highlight default link rsIdentDef Identifier
highlight default link rsInclude Include
highlight default link rsKeyword Keyword
highlight default link rsLibraryType Type
highlight default link rsLifetimeDef Special
highlight default link rsNumber Number
highlight default link rsOperator Operator
highlight default link rsQuote StringDelimiter
highlight default link rsRepeat Repeat
highlight default link rsSpecialLifetime Special
highlight default link rsString String
highlight default link rsTypeDef Identifier
highlight default link rsUserConst Constant
highlight default link rsUserFunc Function
highlight default link rsUserIdent Identifier
highlight default link rsUserLifetime Special
highlight default link rsUserMacro Macro
highlight default link rsUserType Type

" Account for the vast majority of colourschemes not highlighting string
" delimiters explicitly.
highlight default link StringDelimiter String
