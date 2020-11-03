%{
open Printf
open List
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
%token PLUS MINUS EXP DIVIDE MODULO GT GE LT LE DOUBLE_EQUALS
%token COLON END
%token IF ELIF ELSE AND OR NOT
%token PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS MODULO_EQUALS
%token <bool> BOOL
%token WHILE FOR IN BREAK CONTINUE
%token DEF CLASS LAMBDA RETURN
%token DOT IS DELETE
%token TRY EXCEPT RAISE
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

bexp: BOOL                                  { $1 }
    | bexp AND bexp                         { Binop(And, $1, $3) }
    | bexp OR bexp                          { Binop(Or, $1, $3) }
    | NOT bexp                              { Not($2) }
    | exp DOUBLE_EQUALS exp                 { Binop(Equals, $1, $3) }
    | exp GT exp                            { Binop(GT, $1, $3) }
    | exp GE exp                            { Binop(GE, $1, $3) }
    | exp LT exp                            { Binop(LT, $1, $3) }
    | exp LE exp                            { Binop(LE, $1, $3) }
;

/*
var.prop.prop
var.prop["key"][var].prop
*/
var_exp: VAR                                { $1 }
    | var_exp DOT VAR                       { Dot($1, $3) }
    | var_exp LBRACKET exp RBRACKET         { Access($1, $3) }
;

function_decl: DEF VAR LPAREN function_parameters RPAREN COLON command_seq
{ Defun($2, $4, $7) }
    | LPAREN function_parameters COLON expr { Delam($2, $4) }
;

/* The arguments and parameters WILL parse backwards. */
function_parameters: VAR                    { [$1] }
    | function_parameters comma VAR         { $1::$3 }
;

function_arguments: expr                    { [$1] }
    | function_arguments COMMA exp          { $1::$3 }
;

function_call_exp: var_exp LPAREN RPAREN            { Func($1, []) }
    | var_exp LPAREN function_arguments RPAREN      { Func($1, $3) }
;

while_com: WHILE bool_exp COLON command_seq
;

for_com: FOR VAR IN VAR function_call_exp COLON command_seq
    | FOR VAR in VAR COLON command_seq
;

if_com: IF bexp COLON command_seq
    | IF bexp COLON command_seq ELIF command_seq
    | if ELIF command_seq
    | if ELSE command_seq
;


dict_entries: expr COLON expr
    | dict_entries COMMA dict_entries
;

dict: LBRACE RBRACE
    | LBRACE dict_entries RBRACE
;

num: INT
    | FLOAT
;

list_items: exp
    | list_items COMMA exp
;

list: LBRACKET RBRACKET
    | LBRACKET list_items RBRACKET
;


exp: NONE
    | num
    | BOOL
    | STRING
    | dict
    | list
    | var_exp
	| exp PLUS exp
	| exp MINUS exp
	| exp TIMES exp
	| exp DIVIDE exp
	| exp MODULO exp
	| MINUS exp %prec NEG
	| LPAREN exp RPAREN
;

command:
    | JLET VAR EQUALS exp
    | JCONST VAR EQUALS exp
	| var_exp EQUALS exp
	| var_exp PLUS_EQUALS exp
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

command_seq: NEWLINE                        { ComSeq(Empty) }
    | command                               { ComSeq(Com, $1) }
    | command_seq NEWLINE command           { ComSeq(ComSeq, $1, $3)}