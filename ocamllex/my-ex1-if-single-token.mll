{
  type token = 
    | INT of int
    | IF
}

rule token = parse
| ['0'-'9']+ as lxm {INT (int_of_string lxm)}
| "if" {IF}
| _ {token lexbuf}
| eof {raise End_of_file}