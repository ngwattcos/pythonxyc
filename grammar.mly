%{
open Printf
open List
open Ast
%}


/* Ocamlyacc Declarations */
%token IMPORT FROM AS
%token JLET JCONST EQUALS
%token NONE
%token <string> VAR
%token COMMA
%token DQUOTE SQUOTE
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%right PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS MODULO_EQUALS
%left AND OR
%nonassoc GT GE LT LE DOUBLE_EQUALS NOT_EQUALS
%left PLUS MINUS TIMES DIVIDE MODULO
%right EXP
%token COLON END
%token IF ELIF ELSE
%left NOT
%token <bool> BOOL
%token WHILE FOR IN BREAK CONTINUE
%token DEF CLASS LAMBDA RETURN
%left DOT IS DELETE
%token TRY EXCEPT RAISE
%token <char> REACT_CHAR
%token NEWLINE
%token LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET
%token EOF

%left NEG		/* negation -- unary minus */


%type <Ast.program> program
%start program

/* Grammar rules */
%%

program: command_seq EOF                    { $1 }
;

bexp:
    | bool_primitive AND bool_primitive                     { BoolOp($1, And, $3) }
    | bool_primitive OR bool_primitive                      { BoolOp($1, Or, $3) }
    | NOT bool_primitive                                    { Not($2) }
    | comparable DOUBLE_EQUALS comparable                   { BoolOp($1, EQ, $3) }
    | comparable NOT_EQUALS comparable                      { BoolOp($1, NE, $3) }
;

bool_primitive:
    | BOOL                                                  { Boolean $1 }
    | var_exp                                               { $1 }
    | function_call                                         { $1 }
;

comparable:
    | bexp                                                  { $1 }
    | string                                                { $1 }
    | num                                                   { $1 }
    | var_exp                                               { $1 }
    | function_call                                         { $1 }
    | list                                                  { $1 }
;

boolop:
    | numeric GT numeric                                    { BoolOp($1, GT, $3) }
    | numeric GE numeric                                    { BoolOp($1, GE, $3) }
    | numeric LT numeric                                    { BoolOp($1, LT, $3) }
    | numeric LE numeric                                    { BoolOp($1, LE, $3) }
;

num: INT                                                    { Int($1) }
    | FLOAT                                                 { Float($1) }
;

string:
    | STRING                                                { String($1) }
;

var_exp: VAR                                { Var($1) }
    | var_exp DOT VAR                       { Dot($1, $3) }
    | var_exp LBRACKET exp RBRACKET         { Key($1, $3) }
;


dict_entries: exp COLON exp                               { [($1, $3)] }
    | dict_entries COMMA exp COLON exp                    { ($3, $5)::$1 }
;

dict: LBRACE RBRACE                                         { Dict([]) }
    | LBRACE dict_entries RBRACE                            { Dict($2) }
;

list_items: exp                                             { [$1] }
    | list_items COMMA exp                                  { $3::$1 }
;

list: LBRACKET RBRACKET                                     { List([]) }
    | LBRACKET list_items RBRACKET                          { List($2) }
;

binop:
	| numeric PLUS numeric                                  { Binop($1, Plus, $3) }
	| numeric MINUS numeric                                 { Binop($1, Minus, $3) }
	| numeric TIMES numeric                                 { Binop($1, Times, $3) }
	| numeric DIVIDE numeric                                { Binop($1, Divide, $3) }
	| numeric MODULO numeric                                { Binop($1, Modulo, $3) }
	| numeric EXP numeric                                   { Binop($1, Expon, $3) }
;

numeric: num                                                { $1 }
    | var_exp                                               { $1 }
    | function_call                                         { $1 }
	| MINUS numeric %prec NEG                               { Neg($2) }
    | binop                                                 { $1 }
;

exp: NONE                                                   { None }
    | num                                                   { $1 }
	| MINUS numeric %prec NEG                               { Neg($2) }
    | binop                                                 { $1 }
    | bexp                                                  { $1 }
    | boolop                                                { $1 }
    | dict                                                  { $1 }
    | list                                                  { $1 }
    | var_exp                                               { VarAccess($1) }
    | LAMBDA function_parameters COLON exp                  { Lambda(Params($2), $4) }
    | function_call                                         { $1 }
	| LPAREN exp RPAREN                                     { Paren($2) }
;

val_update:
    | JLET VAR EQUALS exp                                   { JLet($2, $4) }
    | JCONST VAR EQUALS exp                                 { JConst($2, $4) }
	| var_exp PLUS_EQUALS exp                               { BinopCom($1, PlusEquals, $3) }
	| var_exp MINUS_EQUALS exp                              { BinopCom($1, MinusEquals, $3) }
	| var_exp TIMES_EQUALS exp                              { BinopCom($1, TimesEquals, $3) }
	| var_exp DIVIDE_EQUALS exp                             { BinopCom($1, DivideEquals, $3) }
	| var_exp MODULO_EQUALS exp                             { BinopCom($1, ModuloEquals, $3) }
;

import: IMPORT VAR FROM STRING                              { ImportBase($2, $4) }
    | IMPORT VAR FROM STRING AS VAR                         { ImportAs($2, $4, $6) }
;

while_com: WHILE exp COLON command_seq END                  { While($2, $4) }
;

function_parameters: VAR                                    { [$1] }
    | function_parameters COMMA VAR                         { $3::$1 }
;

function_arguments: exp                                     { [$1] }
    | function_arguments COMMA exp                          { $3::$1 }
;

function_call: var_exp LPAREN RPAREN                        { FuncCallVal($1, Args([])) }
    | var_exp LPAREN function_arguments RPAREN              { FuncCallVal($1, Args($3)) }
;

for_com: FOR VAR IN
    VAR LPAREN function_arguments RPAREN
    COLON command_seq END                           { ForFunc($2, ($4, Args($6)), $9) }
    | FOR VAR IN VAR COLON command_seq END          { ForIterVar($2, $4, $6) }
;

if_com: IF exp COLON command_seq END                       { IfBase($2, $4, []) }
    | IF exp COLON command_seq if_elifs END                { IfBase($2, $4, $5) }
    | IF exp COLON command_seq if_elifs ELSE command_seq END  { IfElse($2, $4, $5, $7) }
;

if_elifs: ELIF exp COLON command_seq                       { [Elif($2, $4)] }
    | if_elifs ELIF exp COLON command_seq                  { Elif($3, $5)::$1 }
;

command:
    | val_update                                            { ValUpdate($1) }
    | while_com                                             { $1 }
    | for_com                                               { For($1) }
    | if_com                                                { If($1) }
    | function_call                                         { $1 }
    | DEF VAR LPAREN function_parameters RPAREN COLON
        command_seq END                                     { FuncDef($2, Params($4), $7) }
    | RETURN exp                                            { ReturnExp($2) }
    | RETURN                                                { Return }
    | BREAK                                                 { Break }
    | import                                                { Import($1)}
    | CONTINUE                                              { Continue }
;

command_seq:
    | command                                               { [$1] }
    | command_seq NEWLINE command                           { $3::$1 }