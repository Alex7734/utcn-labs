%{
    #include <stdio.h>
    #define MAX 10

    typedef struct _line{
        int elems[MAX];
        int no_columms_used;
    } line;

    typedef struct _matr {
        line *rows[MAX];
        int no_rows_used;
    } matr;

    matr *mem[26];

    // Function prototypes
    line *create_row(int nr);
    line* add_number_to_row(line *l, int nr);
    matr *create_matrix(line *l);
    matr* add_row(matr *m, line *l);
    void print_matrix(matr *m);
    matr *sum_matrices(matr *m1, matr *m2);
    matr *sub_matrices(matr *m1, matr *m2);
    matr *mul_matrices(matr *m1, matr *m2);

    // Helper function prototypes
    int check_dimensions(matr *m1, matr *m2);
    matr *initialize_matrix(int rows, int cols);
%}

%union {
    int ival;
    struct _line *row;
    struct _matr *matrix;
}

// define terminals
%token <ival> VAR NUMBER

// define non terminals
%type <matrix> stmt expr matrix
%type <row> row

%left '+' '-'
%left '*'

%%

file : file stmt '\n' {print_matrix($2);}
     | file '\n' 
     |
     ;

stmt : VAR '=' matrix ';' { mem[$1] = $3; $$ = $3; }
     | expr ';' { $$ = $1; }
     ;

expr : expr '+' expr { $$ = sum_matrices($1, $3); }
     | expr '-' expr { $$ = sub_matrices($1, $3); }
     | expr '*' expr { $$ = mul_matrices($1, $3); }
     | VAR  { $$ = mem[$1]; }
     ;

matrix : matrix '\n' row { add_row($1, $3); }
       | row { $$ = create_matrix($1); }
       ;

row : row NUMBER { add_number_to_row($1, $2); }
    | NUMBER { $$ = create_row($1); }
    ;

%%

line *create_row(int nr) {
    line *l = (line *)malloc(sizeof(line));
    l->no_columms_used = 1;
    l->elems[0] = nr;
    return l;
}

line* add_number_to_row(line *l, int nr) {
    l->elems[l->no_columms_used++] = nr;
    return l;
}

matr *create_matrix(line *l) {
    matr *m = (matr *)malloc(sizeof(matr));
    m->no_rows_used = 1;
    m->rows[0] = l;
    return m;
}

matr* add_row(matr *m, line *l) {
    m->rows[m->no_rows_used++] = l;
    return m;
}

void print_matrix(matr *m) {
    for (int i = 0; i < m->no_rows_used; i++) {
        for (int j = 0; j < m->rows[i]->no_columms_used; j++) {
            printf("%d\t", m->rows[i]->elems[j]);
        }
        printf("\n");
    }
    printf("\n");
}

int check_dimensions(matr *m1, matr *m2) {
    return m1->no_rows_used == m2->no_rows_used && 
    m1->rows[0]->no_columms_used == m2->rows[0]->no_columms_used;
}

matr *initialize_matrix(int rows, int cols) {
    matr *m = (matr *)malloc(sizeof(matr));
    m->no_rows_used = rows;

    for (int i = 0; i < rows; i++) {
        m->rows[i] = (line *)malloc(sizeof(line));
        m->rows[i]->no_columms_used = cols;
    }

    return m;
}

matr *sum_matrices(matr *m1, matr *m2) {
    if (!check_dimensions(m1, m2)) {
        fprintf(stderr, "Error: Matrices dimensions do not match for addition\n");
        exit(1);
    }

    matr *m = initialize_matrix(m1->no_rows_used, m1->rows[0]->no_columms_used);

    for (int i = 0; i < m1->no_rows_used; i++) {
        for (int j = 0; j < m1->rows[i]->no_columms_used; j++) {
            m->rows[i]->elems[j] = m1->rows[i]->elems[j] + m2->rows[i]->elems[j];
        }
    }

    return m;
}

matr *sub_matrices(matr *m1, matr *m2) {
    if (!check_dimensions(m1, m2)) {
        fprintf(stderr, "Error: Matrices dimensions do not match for subtraction\n");
        exit(1);
    }

    matr *m = initialize_matrix(m1->no_rows_used, m1->rows[0]->no_columms_used);

    for (int i = 0; i < m1->no_rows_used; i++) {
        for (int j = 0; j < m1->rows[i]->no_columms_used; j++) {
            m->rows[i]->elems[j] = m1->rows[i]->elems[j] - m2->rows[i]->elems[j];
        }
    }

    return m;
}

matr *mul_matrices(matr *m1, matr *m2) {
    if (m1->rows[0]->no_columms_used != m2->no_rows_used) {
        fprintf(stderr, "Error: Matrices dimensions have to be compatible\n");
        exit(1);
    }

    matr *m = initialize_matrix(m1->no_rows_used, m2->rows[0]->no_columms_used);

    for (int i = 0; i < m1->no_rows_used; i++) {
        for (int j = 0; j < m2->rows[0]->no_columms_used; j++) {
            m->rows[i]->elems[j] = 0;
            
            for (int k = 0; k < m1->rows[i]->no_columms_used; k++) {
                m->rows[i]->elems[j] += m1->rows[i]->elems[k] * m2->rows[k]->elems[j];
            }
        }
    }

    return m;
}





