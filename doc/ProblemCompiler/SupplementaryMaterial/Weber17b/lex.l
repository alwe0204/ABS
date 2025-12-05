%{
#include "ast.h"
#include "y.tab.h"
%}
%array
%option yylineno


ID  [A-Za-z][A-Za-z0-9_]* 
D   [0-9]
E   [Ee][+-]?{D}+

INTEGER  {D}+
%%

"/*"        {   /* C style comment */
                  register int c = input();
                  while(1) 
                   { 
		     if( c == '*')  
                      {
                         c = input();
                         if( c  == '/')  break;
                      }
  		    else if( c == '\0')
		      {
                        (void)fprintf(stderr,
                        "Error at line %d, unterminated comment\n", yylineno);
                        exit(1);  
		      }
		    else  c = input();
                  }
           }


"Real"          { return REAL         ;  }
"Interval"      { return INTERVAL     ;  }
"in"            { return IN           ;  }
"to"            { return TO           ;  }
"for"           { return FOR          ;  }   
"diff"          { return DIFF         ;  }
"initialvalue"  { return INITIALVALUE ;  }
"Option"        { return OPTION       ;  }
"order"         { return ORDER        ;  }

"Pi"            { return _PI    ; }

"atan"          { return _ATAN  ; }
"cos"           { return _COS   ; }
"cosh"          { return _COSH  ; }
"exp"           { return _EXP   ; }
"ln"            { return _LN    ; }
"sin"           { return _SIN   ; }
"sinh"          { return _SINH  ; }
"sqrt"          { return _SQRT  ; }
"tan"           { return _TAN   ; }

{ID}          { lex_id(yytext); return IDENTIFIER ;       } 
{INTEGER}     {                 return NNINTEGER  ;       }

{D}*"."{D}+({E})? { return DECIMAL;  }	
{D}+"."{D}*({E})? { return DECIMAL;  }

";"           { return(';');  }
","           { return(',');  }
"+"           { return('+');  }
"-"           { return('-');  }
"*"           { return('*');  }
"/"           { return('/');  } 
"^"           { return('^');  }
"="           { return('=');  }
"("           { return('(');  }
")"           { return(')');  }
"{"           { return('{');  }
"}"           { return('}');  }
"["           { return('[');  }
"]"           { return(']');  }
":"           { return(':');  }
"."           { return('.');  }

[ \t\n]+

.         {  (void)fprintf(stderr,"Error: line %d, near '%s': illegal character\n",yylineno, yytext);
                return(yytext[0]); 
          }

%%
