%{
#include "ast.h"
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
extern char yytext[];
extern int yylex();
static void yyerror(char *);
%}

%token IDENTIFIER 
%token PI
%token UNSIGNED_INTEGER
%token UNSIGNED_DECIMAL
%token ATAN
%token COS 
%token COSH 
%token EXPN /* EXP is already used */
%token LN
%token SIN
%token SINH
%token SQRT
%token TAN
%token REAL
%token INTERVAL
%token IN
%token TO
%token FOR
%token DIFF 
%token INITIALVALUE 

%token PERIODS         
%token UNCERTAINTIES
%token SAMPLINGTIME
%token INITIALSET
%token TARGETSET
%token OBSTACLESET
%token OPERATINGRANGE
%token STATESPACEDISCRETIZATION
%token INPUTSPACEDISCRETIZATION
%token MEASUREMENTERRORS
%token INTEGR_ORDER
%token INTEGR_ORDER_GROWTH

%union { Expression expr ; Declaration decl ; Assignment def ; Statement stmt ; FunctionDefinition fct ; Iteration ite ; Ode ode ; Option opt ; struct ASTNODE * ast; }

%type <expr> identifier
%type <expr> identifier_list
%type <expr> identifier_tuple 
%type <expr> component_expression
%type <expr> component_expression_list
%type <expr> component_expression_tuple 
%type <expr> expression 
%type <expr> multiplicative_expression
%type <expr> signed_expression
%type <expr> exponential_expression
%type <expr> primary_expression
%type <expr> literal_unsigned_integer
%type <expr> literal_unsigned_decimal
%type <expr> literal_unsigned_number
%type <expr> elementary_function_expression
%type <expr> literal_interval_expression
%type <expr> interval_expression_list   
%type <expr> interval_expression    
%type <expr> interval_hyperinterval_expression_tuple     
%type <expr> interval_hyperinterval_expression_list     
%type <expr> hyperinterval_expression    
%type <expr> literal_hyperinterval_expression   
%type <expr> literal_unsigned_integer_list  
%type <expr> literal_unsigned_integer_tuple
%type <expr> expression_list expression_tuple 
%type <decl> declaration
%type <decl>  declaration_and_definition
%type <def>  definition
%type <fct>  function_definition
%type <expr>  function_image
%type <ite>  iteration_head 
%type <stmt> statement 
%type <stmt> statement_list 
%type <stmt> compound_statement
%type <stmt> iteration_statement
%type <stmt> initialvalue 
%type <stmt> initialvalue_list
%type <ode> ode_definition  
%type <ode> ode_statement
%type <ode> ode_equation
%type <opt> parameter 
%type <opt> parameter_list
%type <ast> program


%start program
%%
/****************************************************************
 *                                                              *
 *  Yacc grammar specification                                  *
 *                                                              *
 ****************************************************************/
declaration 
	: REAL     component_expression ';' { $$ = install_declaration(TY_REAL,$2); }
	| INTERVAL component_expression ';' { $$ = install_declaration(TY_INTV,$2); }
	;
definition  
	: component_expression '=' expression		          ';' { $$ = install_assignment(1,$1,$3); }
	| component_expression '=' literal_interval_expression ';' { $$ = install_assignment(1,$1,$3); }
	| component_expression  IN interval_expression         ';' { $$ = install_assignment(3,$1,$3); }
	;
declaration_and_definition 
	: REAL      identifier '='  expression                  ';' { $$ = install_declaration_and_definition(TY_REAL,$2,$4); }
	| INTERVAL  identifier '='  literal_interval_expression ';' { $$ = install_declaration_and_definition(TY_INTV,$2,$4); }
	| REAL      identifier  IN literal_interval_expression ';'  { $$ = install_declaration_and_definition(TY_REAL,$2,$4); }
	;
statement 
	: declaration                { $$ = install_statement(DECL,(void *)$1)     ; }
	| definition                 { $$ = install_statement(DEFI,(void *)$1)     ; }
	| declaration_and_definition { $$ = install_statement(DECLDEFI,(void *)$1) ; }
	| iteration_statement        { $$ = $1                                     ; }
	;
statement_list 
	: statement                 { $$ = $1 ;                                    } 
	| statement_list statement  { $$ = (Statement)append(AST_STMT,(void *)$1,(void *)$2); }
	;
compound_statement 
	: '{' statement_list '}' { $$ = $2 ; }
	;
/*********************************************
 *     Iterations                            *
 *********************************************/
iteration_head 
	: FOR '(' identifier '=' expression ',' expression ',' '+' ')'         { $$ = install_iteration(IT_FOR_INC,$3,$5,$7) ; }
	| FOR '(' identifier '=' expression ',' expression ',' '-' ')'         { $$ = install_iteration(IT_FOR_DEC,$3,$5,$7) ; }
	;
iteration_statement 
	: iteration_head compound_statement                    { $$ = install_statement(ITER,(void *)build_iteration($1,$2));   }
	;
/*********************************************
 *     Identifiers                           *
 *********************************************/
identifier 
	: IDENTIFIER { $$ = current_id ; }
	;
component_expression 
	: identifier  					                     	    { $$ = $1 ;                                 }
	| identifier '[' expression ']'		      	 /* vector */ { $$ = build_PostFixExpression($1,$3,NULL); }
	| identifier '[' expression ']' '[' expression ']' /* matrix */ { $$ = build_PostFixExpression($1,$3,$6);   }
	;
/*********************************************
 *     Expressions                           *
 *********************************************/
expression
	: multiplicative_expression			   { $$ = $1                              ; }
	| expression '+' multiplicative_expression { $$ = install_expression(PLUS,$1,$3)  ; }
	| expression '-' multiplicative_expression { $$ = install_expression(MINUS,$1,$3) ; }
	;
multiplicative_expression
	: signed_expression                               { $$ = $1                              ; }
	| multiplicative_expression '*' signed_expression { $$ = install_expression(TIMES,$1,$3) ; }
	| multiplicative_expression '/' signed_expression { $$ = install_expression(DIV,$1,$3)   ; }
	;
signed_expression
	: exponential_expression                         { $$ = $1 ;                              }
	| '-' exponential_expression                     { $$ = install_expression(TIMES,install_Number(NULL,NUM_INT,"-1"),$2) ; }
	| '+' exponential_expression                     { $$ = $2 ;                              }
	;
exponential_expression
	: primary_expression
	| primary_expression '^' signed_expression  { $$ = install_expression(EXP,$1,$3)   ; } /* exponentiation */
	;
primary_expression
	: '(' expression ')'                         { $$ = $2 ;                              }
	| elementary_function_expression  			{ $$ = $1 ;                              }
	| literal_unsigned_number               	{ $$ = $1 ;                              }
	| component_expression                       { $$ = $1 ;                              }
	;
elementary_function_expression
	: ATAN  '(' expression ')' { $$ = install_expression(FCT_ATAN,NULL,$3) ; }
	| COS   '(' expression ')' { $$ = install_expression(FCT_COS,NULL,$3)  ; }
	| COSH  '(' expression ')' { $$ = install_expression(FCT_COSH,NULL,$3) ; }
	| EXPN  '(' expression ')' { $$ = install_expression(FCT_EXP,NULL,$3)  ; }
	| LN    '(' expression ')' { $$ = install_expression(FCT_LOG,NULL,$3)  ; }
	| SIN   '(' expression ')' { $$ = install_expression(FCT_SIN,NULL,$3)  ; }
	| SINH  '(' expression ')' { $$ = install_expression(FCT_SINH,NULL,$3) ; }
	| SQRT  '(' expression ')' { $$ = install_expression(FCT_SQRT,NULL,$3) ; }
	| TAN   '(' expression ')' { $$ = install_expression(FCT_TAN,NULL,$3)  ; }
	; 
literal_unsigned_integer
	: UNSIGNED_INTEGER             { $$ = install_Number(NULL,NUM_INT,yytext);      }
	;
literal_unsigned_decimal
	: UNSIGNED_DECIMAL             { $$ = install_Number(NULL,NUM_DEC,yytext);      }
	;
literal_unsigned_number
	: literal_unsigned_integer             { $$ = $1 ;  }
	| literal_unsigned_decimal             { $$ = $1 ;  }
	| PI                                   { $$ = install_Number(NULL,NUM_CONST,yytext);    } /* mathematical constants, e.g. Pi = 3.14159... */ 
	;  
/*********************************************
 * Basic lists                               *
 *********************************************/
identifier_list         
	: identifier                        { $$ = $1; }
	| identifier_list ',' identifier    { $$ = (Identifier)append(AST_EXPR,(void *)$1,(void *)$3); }
	;
identifier_tuple
	: '(' identifier_list ')'           {  $$ = $2 ;              }
	;
component_expression_list  
	: component_expression                                   { $$ = $1 ;  }
	| component_expression_list ',' component_expression       { $$ = (Identifier)append(AST_EXPR,(void *)$1,(void *)$3);             }
	;
component_expression_tuple  
	: '(' component_expression_list ')' { $$ = $2 ;  }
	;
/*********************************************
 *    Intervals                              *
 *********************************************/
literal_interval_expression
	: '[' expression ',' expression ']' { $$ = install_expression(CONT_INTV,$2,$4) ; }
	;
interval_expression
	: component_expression             { $$ = $1 ; }
	| literal_interval_expression      { $$ = $1 ; }
	| REAL						{ $$ = install_expression(REAL_SPACE,NULL,NULL); }
	;
interval_expression_list
	: interval_expression                                    { $$ = $1 ; }
	| interval_expression_list ',' interval_expression       { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3) ;    }
	;
/*********************************************
 *    Hyper-intervals                        *
 *********************************************/
literal_hyperinterval_expression
	: '(' interval_expression_list ')'   { $$ = install_expression(LIST,$2,NULL) ; }
	;

hyperinterval_expression
	: literal_hyperinterval_expression   { $$ = $1 ; }
	| REAL '[' expression ']'            { $$ = install_expression(REAL_SPACE,NULL,$3) ; }  
	;
interval_hyperinterval_expression_list
	: interval_expression                                                    { $$ = $1; }
	| hyperinterval_expression                                               { $$ = $1; }
	| interval_hyperinterval_expression_list ',' interval_expression           { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3) ;     }
	| interval_hyperinterval_expression_list ',' hyperinterval_expression      { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3) ;     }
	;
interval_hyperinterval_expression_tuple
	: '(' interval_hyperinterval_expression_list ')'  { $$ = $2 ; }
	;
/*********************************************
 *    Explicitly defined function            *
 *********************************************/
function_definition     
	: function_declaration function_head compound_statement { $$ = finish_function(current_fct,$3)    ; }
	;
function_declaration    
	: identifier ':'                   { current_fct = install_function($1); }
	;
function_image
	: component_expression_tuple { $$ = $1 ; }
	| component_expression       { $$ = $1 ; }
	;
function_head
	: identifier        IN interval_expression                     TO function_image    { build_function(IDENT,current_fct,$1,$3,$5);   } 
	| identifier        IN hyperinterval_expression                TO function_image    { build_function(MIXED,current_fct,$1,$3,$5);   } 
	| identifier_tuple  IN interval_hyperinterval_expression_tuple TO function_image    { build_function(NESTED,current_fct,$1,$3,$5);  }
	;
/*********************************************
 *    Ordinary differential equation         *
 *********************************************/
ode_definition
	: function_declaration function_head ode_statement           { $$ = $3 ; }
	;
ode_statement
	: '{' ode_equation ';' initialvalue_list '}'  { $$ = build_ode_statement($2,$4); }
	| '{' initialvalue_list ode_equation ';' '}'  { $$ = build_ode_statement($3,$2); }
	;
ode_equation
	: DIFF '(' identifier       ',' identifier ')' '=' identifier identifier_tuple  { $$ = install_ode_statement($3,$5,$8,$9); }
	| DIFF '(' identifier_tuple ',' identifier ')' '=' identifier identifier_tuple  { $$ = install_ode_statement($3,$5,$8,$9); }
	;
initialvalue
	: INITIALVALUE '(' component_expression ',' expression ')' ';' { $$ = install_statement(DEFI,(void *)install_assignment(1,$3,$5)); }
	;
initialvalue_list 
	: initialvalue                             { $$ = $1 ; }
	| initialvalue_list initialvalue           { $$ = (Statement)append(AST_STMT,(void *)$1,(void *)$2); }
	;
/*********************************************
 * Abstraction-based controller synthesis    *
 *********************************************/
literal_unsigned_integer_list
	: literal_unsigned_integer                                       { $$ = $1 ; }
	| literal_unsigned_integer_list ',' literal_unsigned_integer     { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3); }
	;
literal_unsigned_integer_tuple
	: '(' literal_unsigned_integer_list ')'                     { $$ = install_expression(LIST,$2,NULL) ;         }
	;
expression_list
	: expression                                    { $$ = $1; }
	| expression_list ',' expression               { $$ = (Expression)append(AST_EXPR,(void *)$1,(void *)$3); }
	;
expression_tuple
	: '(' expression_list ')'               { $$ = install_expression(LIST,$2,NULL)  ;   }
	; 
parameter_list
	: parameter                           { $$ = $1 ; }
	| parameter_list parameter      		{ $$ = $1 ; }
	;
parameter
	: PERIODS                  ':' literal_unsigned_integer_tuple   ';'   { $$ = build_option(OPTION_PERIODS,current_opt,$3)     ; }
     | STATESPACEDISCRETIZATION ':' literal_unsigned_integer_tuple   ';'   { $$ = build_option(OPTION_STATE_SPACE,current_opt,$3)       ; }
     | INPUTSPACEDISCRETIZATION ':' literal_unsigned_integer_tuple   ';'   { $$ = build_option(OPTION_INPUT_SPACE,current_opt,$3)       ; }
	| SAMPLINGTIME             ':' literal_unsigned_number          ';'   { $$ = build_option(OPTION_SAMPLINGTIME,current_opt,$3); }
     | INITIALSET               ':' literal_hyperinterval_expression ';'   { $$ = build_option(OPTION_INITIALSET,current_opt,$3) ; }
     | TARGETSET                ':' literal_hyperinterval_expression ';'   { $$ = build_option(OPTION_TARGETSET,current_opt,$3) ; }
     | OBSTACLESET              ':' literal_hyperinterval_expression ';'   { $$ = build_option(OPTION_OBSTACLESET,current_opt,$3) ; }
	| OPERATINGRANGE           ':' literal_hyperinterval_expression ';'   { $$ = build_option(OPTION_RANGE,current_opt,$3) ; }
     | UNCERTAINTIES            ':' expression_tuple                 ';'   { $$ = build_option(OPTION_UNCERTAINTIES,current_opt,$3); }
     | MEASUREMENTERRORS        ':' expression_tuple                 ';'   { $$ = build_option(OPTION_MEAS,current_opt,$3); }
	| INTEGR_ORDER             ':' literal_unsigned_integer         ';'   { $$ = build_option(OPTION_INTEGR_ORDER,current_opt,$3); }
	| INTEGR_ORDER_GROWTH      ':' literal_unsigned_integer         ';'   { $$ = build_option(OPTION_INTEGR_ORDER_GROWTH,current_opt,$3); }
	;
/*********************************************
 * Program                                   *
 *********************************************/
program		        
	: function_definition                                           { AST = install_program(NULL,$1,NULL,NULL) ; }
	| function_definition parameter_list                            { AST = install_program(NULL,$1,NULL,$2) ;   }
	| function_definition ode_definition                            { AST = install_program(NULL,$1,$2,NULL) ;   }
	| statement_list function_definition                            { AST = install_program($1,$2,NULL,NULL) ;   }
	| statement_list function_definition parameter_list             { AST = install_program($1,$2,NULL,$3) ;     }
	| statement_list function_definition ode_definition             { AST = install_program($1,$2,$3,NULL) ;     }
	;
/****************************************************************
 *                                                              *
 *    End of grammar specification                              *
 *                                                              *
 ****************************************************************/
%%
void yyerror(char s[])
{
   extern int yylineno;
   extern char yytext[];
   fprintf(stderr,"Line %d near '%s':  %s\n", yylineno, yytext, s);
   exit(EXIT_FAILURE);
}
