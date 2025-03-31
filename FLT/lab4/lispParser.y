%{
    #include <stdio.h>
    #include <stdlib.h>

    typedef struct _cons {
        int car;
        struct _cons *cdr;
    } list_c;

    // Function prototypes
    list_c *cons(int nr, list_c *l);
    list_c *append(list_c *f, list_c *s);
    int car(list_c *l);
    list_c* cdr(list_c *l);
    list_c *reverse(list_c *l);
    void print_list(list_c *l);
%}

%union {
    int ival;
    struct _cons *list;
}

// Define tokens
%token <ival> NUMBER
%token CAR CDR APPEND CONS REVERSE

// Define non-terminal types
%type <ival> i_form i_command
%type <list> l_form l_command enum

%%

file : file form '\n' 
    | file '\n' 
    | /* empty */ 
    ; 

form : i_form { printf("%d\n", $1); }
     | l_form { print_list($1); }
     ;

i_form : '(' i_command ')' { $$ = $2; } 
       | NUMBER { $$ = $1; }
       ;

i_command : CAR l_form { $$ = car($2); }
          | '+' i_form i_form { $$ = $2 + $3; } 
          ;

l_form : '(' l_command ')' { $$ = $2; }
       | '\'' '(' enum ')' { $$ = $3; }
       ;

l_command : CDR l_form { $$ = cdr($2); }
          | CONS i_form l_form { $$ = append(cons($2, NULL), $3); }
          | APPEND l_form l_form { $$ = append($2, $3); }   
          | REVERSE l_form { $$ = reverse($2); }
          ;

enum : NUMBER enum { $$ = cons($1, $2); }
     | NUMBER { $$ = cons($1, NULL); }
     ;

%%

list_c *cons(int nr, list_c *l) {
    list_c *l_aux = (list_c*)malloc(sizeof(list_c));
    if (!l_aux) {
        fprintf(stderr, "Memory allocation error\n");
        exit(1);
    }
    l_aux->car = nr;
    l_aux->cdr = l;
    return l_aux;
}

list_c *reverse(list_c *l) {
    list_c *prev = NULL;
    list_c *current = l;
    list_c *next = NULL;
    while (current != NULL) {
        next = current->cdr;
        current->cdr = prev;
        prev = current;
        current = next;
    }
    return prev;
}

list_c *append(list_c *f, list_c *s) {
    if (!f) return s;
    list_c *curr = f;
    while (curr->cdr != NULL)
        curr = curr->cdr;
    curr->cdr = s;
    return f;
}

int car(list_c *l) {
    if (l == NULL) {
        fprintf(stderr, "Error: empty list\n");
        exit(1);
    }
    return l->car;
}

list_c* cdr(list_c *l) {
    if (l == NULL) {
        fprintf(stderr, "Error: empty list\n");
        exit(1);
    }
    return l->cdr;
}

void print_list(list_c *l) {
    list_c *curr = l;
    while (curr != NULL) {
        printf("%d ", curr->car);
        curr = curr->cdr;
    }
    printf("\n");
}
