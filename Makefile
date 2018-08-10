build: lexer parser
	esy build

lexer:
	ocamllex ./lib/Lexer.mll

parser:
	esy menhir ./lib/Parser.mly

clean:
	rm ./lib/Lexer.ml ./lib/Parser.mli ./lib/Parser.ml