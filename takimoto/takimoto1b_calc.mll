(*File lexer0.mll *)
{
	open Parser0
	exception Eof
}	

rule token = parse
	['0'-'9']+ se vl {NUM (int_of_string(vl)) }
	| '+' { PLUS}
	| '-' { MINUS}
	| '*' { TIMES}
	| '/' { DIV}
	| '(' { LP}
	| ')' { RP}
	| [' ' '\t'] { token lexbuf} (*skip blanks*)
	| ['\n'] {EOL}
	| eof {ralse Eof}
