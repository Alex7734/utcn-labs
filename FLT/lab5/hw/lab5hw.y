%{
    #include <stdio.h>
    #include <stdlib.h>

    #define STACK_SIZE 100

    int stack[STACK_SIZE];
    int stack_top = -1;

    void push(int value);
    int pop();
%}

%union {
    int ival;
}

%token <ival> NUMBER
%type <ival> expr

%%

file: file expr '\n' { printf("Result: %d\n", pop()); }
    | file '\n'
    |
    ;

expr:  expr expr '+' { int b = pop(); int a = pop(); push(a + b); }
   |   expr expr '-' { int b = pop(); int a = pop(); push(a - b); }
   |   expr expr '*' { int b = pop(); int a = pop(); push(a * b); }
   |   expr expr '/' { int b = pop(); if (b == 0) { fprintf(stderr, "Division by zero\n"); exit(1); } int a = pop(); push(a / b); }
   |   NUMBER { push($1); }
   ;

%%

void push(int value) {
    if (stack_top >= STACK_SIZE - 1) {
        fprintf(stderr, "Stack overflow\n");
        exit(1);
    }

    stack[++stack_top] = value;
}

int pop() {
    if (stack_top < 0) {
        fprintf(stderr, "Stack underflow\n");
        exit(1);
    }
    
    return stack[stack_top--];
}