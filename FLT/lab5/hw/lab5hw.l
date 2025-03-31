%{
    #include "y.tab.h"
%}

%%

[0-9]+ {
    yylval.ival = atoi(yytext);
    return NUMBER;
}

[-+*/\n] return *yytext;

[ \t] ;

%%