CC = gcc
NAME = ex1

clean:
	rm a.out


$(NAME): $(NAME)-lexer.cmo $(NAME)-parser.cmo $(NAME)-calc.cmo
	ocamlc -o $@ $^


$(NAME)-lexer.ml: $(NAME)-lexer.mll
	ocamllex $<


$(NAME)-parser.ml: $(NAME)-parser.mly
	ocamlyacc $<

$(NAME)-parser.mli: $(NAME)-parser.mly
	ocamlyacc $<


$(NAME)-lexer.cmo: $(NAME)-lexer.ml
	ocamlc -c $<

$(NAME)-parser.cmo: $(NAME)-parser.ml
	ocamlc -c $<

$(NAME)-parser.cmo: $(NAME)-parser.mli
	ocamlc -c $<

$(NAME)-calc.cmo: $(NAME)-calc.ml
	ocamlc -c $<