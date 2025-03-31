%{
    #include "y.tab.h"
%}

%%

[A-Z] {
    yylval.ival = *yytext - 'A';
    return VAR;
}

[0-9]+ {
    yylval.ival = atoi(yytext);
    return NUMBER;
}

[-+*=;\n] return *yytext;

[ \t] ;

. yyerror("Unknown charachter");

%%