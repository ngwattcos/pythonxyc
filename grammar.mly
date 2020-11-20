%{
open Printf
open List
open Ast
%}


/* Ocamlyacc Declarations */
%token IMPORT FROM AS
%token JLET JCONST
%token NONE
%token <string> VAR
%token COMMA
%token DQUOTE SQUOTE
%token <string> STRING
%token <int> INT
%token <float> FLOAT
%right PLUS_EQUALS MINUS_EQUALS TIMES_EQUALS DIVIDE_EQUALS MODULO_EQUALS EQUALS
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


%type <Ast.aexp> aexp
%type <Ast.bexp> bexp

/*%type <Ast.com> c
%type <Ast.com> p*/

%type <Ast.program> program
%start program

/* Grammar rules */
%%

program: command_seq EOF                    { $1 }
;

var_access: VAR                                                { Var($1) }
    | var_access DOT VAR                                       { Dot($1, $3) }
    | var_access LBRACKET exp RBRACKET                         { Key($1, $3) }
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

aexp:
    | modulo_exp                                            { $1 }
;

modulo_exp:
    | modulo_exp MODULO add_exp                             { Mod($1, $3) }
;


add_exp:
    | add_exp PLUS times_exp                                { Plus($1, $3) }
    | add_exp MINUS times_exp                               { Minus($1, $3) }
    | times_exp                                             { $1 }
;

times_exp:
    | times_exp TIMES neg_exp                               { Times($1, $3) }
    | times_exp DIVIDE neg_exp                              { Div($1, $3) }
    | exponen_exp                                           { $1 }
;

neg_exp:
	| MINUS exponen_exp %prec NEG                           { Neg($2) }
;

exponen_exp:
    | exponen_exp EXP aexp_primitive                        { Expon($1, $3) }
;

aexp_primitive:
    | INT                                                   { Int($1) }
    | FLOAT                                                 { Float($1) }
    | var_access                                            { IntVarAccess($1) }
    | function_call_val                                     { IntFuncCallVal($1) }
    | LPAREN aexp RPAREN                                    { IntParen($2) }
;

/* Boolean expressions */
bexp:
    | aexp GE aexp                                          { GE($1, $3) }
    | aexp GT aexp                                          { GT($1, $3) }
    | aexp LE aexp                                          { LE($1, $3) }
    | aexp LT aexp                                          { LT($1, $3) }
    | or_exp                                                { $1 }
;

or_exp:
    | or_exp OR and_exp                                     { Or($1, $3) }
    | and_exp                                               { $1 }
;

and_exp:
    | and_exp AND not_exp                                   { And($1, $3) }
    | not_exp                                               { $1 }
;

not_exp:
    | NOT bexp_primitive                                    { Not($2) }
    | bexp_primitive                                        { $1 }
;


bexp_primitive:
    | BOOL                                                  { Bool($1) }
    | var_access                                            { BoolVarAccess($1) }
    | function_call_val                                     { BoolFuncCallVal($1) }
    | LPAREN bexp RPAREN                                    { BoolParen($2) }
;

paren_exp:
	| LPAREN exp RPAREN                                     { Paren($2) }
;

exp:
    | NONE                                                  { None }
    | STRING                                                { String($1) }
    | var_access                                            { VarAccess($1) }
    | dict                                                  { $1 }
    | list                                                  { $1 }
    | aexp                                                  { Aexp($1) }
    | bexp                                                  { Bexp($1) }
    | LAMBDA function_parameters COLON exp                  { Lambda(Params($2), $4) }
    | function_call_val                                     { FuncCallVal($1) }
    | paren_exp                                             { $1 }
;

val_update:
    | JLET VAR EQUALS exp                                   { JLet($2, $4) }
    | JCONST VAR EQUALS exp                                 { JConst($2, $4) }
	| var_access EQUALS exp                                 { Update($1, Equals, $3) }
	| var_access PLUS_EQUALS exp                            { Update($1, PlusEquals, $3) }
	| var_access MINUS_EQUALS exp                           { Update($1, MinusEquals, $3) }
	| var_access TIMES_EQUALS exp                           { Update($1, TimesEquals, $3) }
	| var_access DIVIDE_EQUALS exp                          { Update($1, DivideEquals, $3) }
	| var_access MODULO_EQUALS exp                          { Update($1, ModuloEquals, $3) }
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

function_call_val: var_access LPAREN RPAREN                 { Call($1, Args([])) }
    | var_access LPAREN function_arguments RPAREN           { Call($1, Args($3)) }
;

function_call_com: var_access LPAREN RPAREN                 { Call($1, Args([])) }
    | var_access LPAREN function_arguments RPAREN           { Call($1, Args($3)) }
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
    | function_call_com                                     { FuncCallCom($1) }
    | DEF VAR LPAREN function_parameters RPAREN COLON
        command_seq END                                     { FuncDef($2, Params($4), $7) }
    | RETURN exp                                            { ReturnExp($2) }
    | RETURN                                                { Return }
    | BREAK                                                 { Break }
    | import                                                { Import($1)}
    | CONTINUE                                              { Continue }
;

command_seq:
    | command_seq NEWLINE command                           { $3::$1 }
    | command                                               { [$1] }