%token <int> INT
%token PLUS TIMES
%token LPAREN RPAREN
%token EOL
%left TIMES /* Only these    */
%left PLUS  /* lines change. */
%start main
%type <int> main
%%

main:
    expr EOL {$1}
;

expr:
    INT {$1}
    | LPAREN expr RPAREN {$2}
    | expr PLUS expr {$1 + $3}
    | expr TIMES expr {$1 * $3}
    | EOL {0}
;

