{
  type token = 
    | INT of int
    | PLUS
}

rule token = parse
| ['0'-'9']+ as lxm { INT (int_of_string lxm) }
| '+' { PLUS }
| _   { token lexbuf } (* 他の文字は無視 *)
| eof { raise End_of_file }
