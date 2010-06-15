let main () =
  try
    let lexbuf = Lexing.from_channel stdin in
    print_string ">> ";
    flush stdout;
    Parser.input Lexer.tokenizer lexbuf
  with End_of_file -> (print_endline "[eof]"; exit 0)

let _ = Printexc.print main ()
