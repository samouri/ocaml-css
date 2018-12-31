build: lexer #parser
	@esy build

test: build
	./_build/install/default/bin/test

lexer:
	@ocamllex -q ./lib/Lexer.mll

parser:
	#@esy menhir --table --trace --explain --dump ./lib/Parser.mly
	@esy menhir --table --explain ./lib/Parser.mly

release:
	@esy release

format: 
	@esy dune build @fmt --auto-promote

clean:
	rm -rf ./lib/Lexer.ml ./lib/Parser.mli ./lib/Parser.ml _build _release