%{
    #include <stdio.h>
%}

%union {
    char *str;
}

%left '+' '-'
%left '*' '/'
%left MINUSUNARY
%right '^'

%token <str> NUMBER VAR

%type <str> expr

%%

file: file expr '\n' { printf("%s\n", $2); }
    | file '\n' 
    |
    ;

expr:  expr  expr '+'  { sprintf($$, "+ %s %s", $1, $2); }
   |   expr  expr '-'  { sprintf($$, "- %s %s", $1, $2); }
   |   expr  expr '*'  { sprintf($$, "* %s %s", $1, $2); }
   |   expr  expr '/'  { sprintf($$, "/ %s %s", $1, $2); }
   |   expr  expr '^'  { sprintf($$, "^ %s %s", $1, $2); }
   |   NUMBER  
   |   VAR 
   ;



