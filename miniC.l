%{
#include<stdio.h>
%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include "y.tab.h"
extern int line;
%}

%%

"//".*  {;}

"/*"[^*]*([*][^/]*)*"*/"        {;}

"main"  { return MAIN;  }

"void"  { return VOID;  }

"int"   {
        yylval.ival=0;
        return INT;
        }

"float" {
        yylval.ival=1;
        return FLOAT;
        }

"char"  {
        yylval.ival=2;
        return CHAR;
        }

"if"    { return IF;    }

"else"  { return ELSE;  }

"while" { return WHILE; }

"for"   { return FOR;   }

[-+*/;,{}()=]   { return *yytext;       }

"<="|">="|"=="|"<"|">"  { return CE;    }

"'"."'" {
        yylval.cval= yytext[1];
        return CHRC;
        }

[a-zA-Z_][a-zA-Z_0-9]*  {
                        yylval.ptr=checksym(yytext);
                        return VAR;
                        }

[0-9][0-9]*     {
                yylval.ival=atoi(yytext);
                return INTC;
                }

[0-9]*\.[0-9]+  {
                yylval.fval=atof(yytext);
                return FLTC;
                }

" "|\t  {;}

\n      { line++;       }

"."     { printf("\n%s is INVALID",yytext);     }

%%
