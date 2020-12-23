(*
Lifted almost entirely from this example:
https://www.ocaml.org/releases/4.11/htmlman/lexyacc.html#s%3Alexyacc-example

ocamllex lexer.mll \
&& ocamlyacc parser.mly \
&& ocamlc -c parser.mli \
&& ocamlc -c lexer.ml \
&& ocamlc -c parser.ml \
&& ocamlc -c solve.ml \
&& ocamlc lexer.cmo parser.cmo solve.cmo -o solve \
&& ./solve

*)

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

