build: lexer parser
	@esy build

test: build
	./_build/default/bin/test.exe

lexer:
	@ocamllex -q ./lib/Lexer.mll

parser:
	@esy menhir ./lib/Parser.mly

clean:
	rm ./lib/Lexer.ml ./lib/Parser.mli ./lib/Parser.ml