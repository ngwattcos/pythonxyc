%{
open Printf
open List
open Ast
%}


/* Ocamlyacc Declarations */
%token NEWLINE EXTEND
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token IMPORT FROM AS
%token JLET JCONST EQUALS
$token NONE
%token <string> VAR
%token COMMA DQUOTE SQUOTE
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%token PLUS MINUS EXP DIVIDE MODULO GT GE LT LE DOUBLE_EQUALS NOT_EQUALS
%token COLON END
%token IF ELIF ELSE AND OR NOT
%token PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS MODULO_EQUALS
%token <bool> BOOL
%token WHILE FOR IN BREAK CONTINUE
%token DEF CLASS LAMBDA RETURN
%token DOT IS DELETE
%token TRY EXCEPT RAISE
%token <char> REACT_CHAR
%token EOF

/* Q: How to define tokens representing text inside react blocks? */

/* We need to define this in side the lexer too? */
%left NEG		/* negation -- unary minus */

%start input
%type <unit> input

/* Grammar rules */
%%
input:	/* empty */	{ }
	| input line	{ }
;

import: IMPORT VAR FROM STRING              { Import($2, $4, $2) }
    | IMPORT VAR FROM STRING AS VAR         { Import($2, $4, $6) }
;

/* use exp instead of bexp since we can use variable namaes, access properties, etc.*/
bexp: BOOL                                  { $1 }
    | exp AND exp                         { Binop($1, And, $3) }
    | exp OR exp                          { Binop($1, Or, $3) }
    | NOT exp                              { Unop($2, Not) }
    | exp DOUBLE_EQUALS exp                 { Binop($1, EQ, $3) }
    | exp NOT_EQUALS exp                    { Binop($1, NE, $3) }
    | exp GT exp                            { Binop($1, GT, $3) }
    | exp GE exp                            { Binop($1, GE, $3) }
    | exp LT exp                            { Binop($1, LT, $3) }
    | exp LE exp                            { Binop($1, LE, $3) }
;

/*
var.prop.prop
var.prop["key"][var].prop
*/
var_exp: VAR                                { VarAccess($1) }
    | var_exp DOT VAR                       { VarAccess(Dot($3)) }
    | var_exp LBRACKET exp RBRACKET         { VarAccess(Key($3)) }
;

/* #1 is a command, #2 is a lambda expression */
function_decl: DEF VAR LPAREN function_parameters RPAREN COLON command_seq
{ FuncDef($2, Func(Lambda($4, $7))) }
    | LPAREN function_parameters COLON expr { Func($2, $4) }
;

/* The arguments and parameters WILL parse backwards. */
function_parameters: VAR                    { [$1] }
    | function_parameters comma VAR         { $3::$1 }
;

function_arguments: expr                    { [$1] }
    | function_arguments COMMA exp          { $3::$1 }
;

function_call_exp: var_exp LPAREN RPAREN            { FuncCallVal($1, []) }
    | var_exp LPAREN function_arguments RPAREN      { FuncCallVal($1, $3) }
;

while_com: WHILE bexp COLON command_seq             { While($2, $3) }
;
/* #2: for i in args (var) or for in len(args) (ex[r]) */
for_com: FOR VAR IN function_call_exp COLON command_seq     { For(ForRange($2, $4, $6)) }
    | FOR VAR in expr COLON command_seq                      { For(ForIterVar($2, $4, $6))}
;

if_com: IF bexp COLON command_seq                           { IfBase($2, $4, []) }
    | if bexp COLON command_seq if_elifs                    { IfBase($2, $4, $5) }
    | if bexp COLON command_seq if_elifs ELSE command_seq   { IfElse($2, $4, $5, $6) }
;

if_elifs: ELIF bexp COLON command_seq                       { [Elif($2, $4)] }
    | if_elifs ELIF bexp COLON command_seq                  { Elif($3, $5)::$1 }
;

dict_entries: expr COLON expr                               { [($1, $2)] }
    | dict_entries COMMA expr COLON expr                    { ($3, $5)::$1 }
;

dict: LBRACE RBRACE                                         { Dict([]) }
    | LBRACE dict_entries RBRACE                            { Dict($2) }
;

num: INT                                                    { Int($1) }
    | FLOAT                                                 { Float($1) }
;

list_items: exp                                             { [$1] }
    | list_items COMMA exp                                  { $3::$1 }
;

list: LBRACKET RBRACKET                                     { List([]) }
    | LBRACKET list_items RBRACKET                          { List($2) }
;


exp: NONE                                                   { None }
    | num                                                   { $1 }
    | BOOL                                                  { $1 }
    | STRING                                                { String($1) }
    | dict                                                  { $1 }
    | list                                                  { $1 }
    | var_exp                                               { $1 }
	| exp PLUS exp                                          { Binop($1, Plus, $3) }
	| exp MINUS exp                                         { Binop($1, Minus, $3) }
	| exp TIMES exp                                         { Binop($1, Times, $3) }
	| exp DIVIDE exp                                        { Binop($1, Divide, $3) }
	| exp MODULO exp                                        { Binop($1, Modulo, $3) }
	| MINUS exp %prec NEG                                   { Unop($2, Neg) }
	| LPAREN exp RPAREN                                     { Paren($2) }
;

command:
    | JLET VAR EQUALS exp
    | JCONST VAR EQUALS exp
	| var_exp EQUALS exp
	| var_exp PLUS_EQUALS exp
	| var_exp MINUS_EQUALS exp
	| var_exp TIMES_EQUALS exp
	| var_exp DIVIDE_EQUALS exp
	| var_exp MODULO_EQUALS exp
    | var_exp LPAREN RPAREN
    | function_call_exp
    | function_decl END
    | while_com END
    | for_com END
    | if_com END
    | RETURN exp
    | RETURN
    | BREAK
    | CONTINUE
    | RAISE
;

command_seq: NEWLINE                        { Empty }
    | command                               { [$1] }
    | command_seq NEWLINE command           { $3::$1 }