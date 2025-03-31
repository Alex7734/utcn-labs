%{
    #include "y.tab.h"
%}

%%

[0-9]+ {
    yylval.str = strdup(yytext);
    return NUMBER;
}

[a-zA-Z] {
    yylval.str = strdup(yytext);
    return VAR;
}

[-+*/^\n] return *yytext;


[ \t] ;

