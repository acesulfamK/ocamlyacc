/* hello */
/*頭部省略。ocamlを書く*/
/*宣言部*/
/* token宣言。<int>は属性の付加*/

%token <int> NUM
%token PLUS MINUS TIMES DIV LP RP EOL

/*関数開始記号*/
%start prog
/*
 非終端記号の属性を宣言する
 注意:
 - 多くの非終端記号は型が推論できるので、わざわざここに書かなくてよい
 - startで使用した記号は必ず%typeで宣言しなくてはいけない
 - <>の内部は省略してもよい。ただし、<>自体を省略してはいけない。
 */
%type <int> prog

/*結合強さを指定するところ
 - 下に行くほど優先度が高い
 - shift/reduceとの関係も理解すること
 */
%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

/*文法規則部*/
%%

prog: expr EOL
	;

expr: expr PLUS term {$1 + $3}
	| expr MINUS term {$1 - $3}
	| term {$1}
	;

term: term TIMES factor {$1 * $3}
	| term DIV factor {$1 / $3}
	| factor {$1}
	;

factor: NUM {$1}
	| LP expr RP {$2}
	;

%%

/*末尾部。ocamlを書く*/

