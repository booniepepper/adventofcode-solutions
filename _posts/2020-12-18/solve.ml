open Printf;;

let lexed_file f =
    Lexing.from_channel (open_in f)
;;

let rec eval lexbuf acc =
    try
        eval lexbuf (acc + (Parser.main Lexer.token lexbuf))
    with Lexer.Eof ->
        acc
;;

let lexbuf = lexed_file "./input" in
let final_sum = eval lexbuf 0 in

printf "Final result: %d\n" final_sum

