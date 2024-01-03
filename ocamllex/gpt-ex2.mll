{
  open Lexing
}

let digit = ['0'-'9']
let plus = '+'

rule token = parse
  | digit+ as lxm { print_endline ("Number: " ^ Lexing.lexeme lexbuf); token lexbuf }
  | plus   { print_endline ("Plus: " ^ Lexing.lexeme lexbuf); token lexbuf }
  | eof    { print_endline "End of input"; exit 0 }
  | _      { print_endline "Unknown character"; token lexbuf }
