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

"Pi"            { return PI    ; }

"atan"          { return ATAN  ; }
"cos"           { return COS   ; }
"cosh"          { return COSH  ; }
"exp"           { return EXPN  ; }
"ln"            { return LN    ; }
"sin"           { return SIN   ; }
"sinh"          { return SINH  ; }
"sqrt"          { return SQRT  ; }
"tan"           { return TAN   ; }


"ListOfPeriodicComponents"                        { return PERIODS              ; }
"BoundOnDynamicUncertainties"                  { return UNCERTAINTIES        ; }
"SamplingTime"                   { return SAMPLINGTIME         ; }

"BoundOnMeasurementErrors"              { return MEASUREMENTERRORS    ; }

"InitialSet"                     { return INITIALSET           ; }
"TargetSet"                      { return TARGETSET            ; }
"ObstacleSet"                    { return OBSTACLESET          ; }

"OperatingRange"                 { return OPERATINGRANGE       ; }
"InitialStateSpaceDiscretization"       { return STATESPACEDISCRETIZATION   ; }
"InitialInputSpaceDiscretization"       { return INPUTSPACEDISCRETIZATION   ; }

"IntegrationOrder"               { return INTEGR_ORDER         ; }
"IntegrationOrderGrowthBound"    { return INTEGR_ORDER_GROWTH  ; }




{ID}          { lex_id(yytext); return IDENTIFIER ;       } 
{INTEGER}     {                 return UNSIGNED_INTEGER  ;       }

{D}*"."{D}+({E})? { return UNSIGNED_DECIMAL;  }	
{D}+"."{D}*({E})? { return UNSIGNED_DECIMAL;  }

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
