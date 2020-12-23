{
    open Parser
    exception Eof
}

rule token = parse
    [' '] { token lexbuf }
    | ['\n'] { EOL }
    | ['0'-'9']+ as digits { INT(int_of_string digits) }
    | '+' { PLUS }
    | '*' { TIMES }
    | '(' { LPAREN }
    | ')' { RPAREN }
    | eof { raise Eof }

