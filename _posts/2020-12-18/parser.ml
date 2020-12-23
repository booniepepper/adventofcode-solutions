type token =
  | INT of (int)
  | PLUS
  | TIMES
  | LPAREN
  | RPAREN
  | EOL

open Parsing;;
let _ = parse_error;;
let yytransl_const = [|
  258 (* PLUS *);
  259 (* TIMES *);
  260 (* LPAREN *);
  261 (* RPAREN *);
  262 (* EOL *);
    0|]

let yytransl_block = [|
  257 (* INT *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\002\000\002\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\003\000\003\000\001\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\002\000\000\000\006\000\007\000\000\000\000\000\
\000\000\000\000\001\000\003\000\004\000\000\000"

let yydgoto = "\002\000\
\006\000\007\000"

let yysindex = "\001\000\
\003\255\000\000\000\000\003\255\000\000\000\000\008\255\010\255\
\003\255\003\255\000\000\000\000\000\000\255\254"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\255"

let yygindex = "\000\000\
\000\000\252\255"

let yytablesize = 19
let yytable = "\008\000\
\009\000\001\000\000\000\003\000\013\000\014\000\004\000\000\000\
\005\000\009\000\010\000\009\000\010\000\011\000\012\000\005\000\
\000\000\005\000\005\000"

let yycheck = "\004\000\
\002\001\001\000\255\255\001\001\009\000\010\000\004\001\255\255\
\006\001\002\001\003\001\002\001\003\001\006\001\005\001\003\001\
\255\255\005\001\006\001"

let yynames_const = "\
  PLUS\000\
  TIMES\000\
  LPAREN\000\
  RPAREN\000\
  EOL\000\
  "

let yynames_block = "\
  INT\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 12 "parser.mly"
             (_1)
# 78 "parser.ml"
               : int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 16 "parser.mly"
        (_1)
# 85 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 17 "parser.mly"
                         (_2)
# 92 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 18 "parser.mly"
                     (_1 + _3)
# 100 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 19 "parser.mly"
                      (_1 * _3)
# 108 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    Obj.repr(
# 20 "parser.mly"
          (0)
# 114 "parser.ml"
               : 'expr))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : int)
