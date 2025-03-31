%{
    #include "y.tab.h"
%}

%%

[0-9]+ { yylval.ival = atoi(yytext);
         return NUMBER; }
CAR return CAR;
CDR return CDR;
APPEND return APPEND;
CONS return CONS;
REVERSE return REVERSE;
[+'()\n] return *yytext;
[ \t] ;
