# css-parse
[![Build Status](https://travis-ci.org/samouri/ocaml-css.svg?branch=master)](https://travis-ci.org/samouri/ocaml-css)

parse, detect errors, and pretty-print css directly in terminal, natively in OCaml, or even from JavaScript.

**this is under active development, please do not use yet**

### Overview

`css-parse` is written in OCaml and utilizes ocamllex and [menhir](http://gallium.inria.fr/~fpottier/menhir/manual.html) for lexer and parser generation.  A pretty good introduction and explanation of the tools can be found in [Real World Ocaml](https://v1.realworldocaml.org/v1/en/html/parsing-with-ocamllex-and-menhir.html).
We generate a binary as well that can be run from the command line.

### Development

You'll need to install esy as well as menhir.
To bootstrap the project run:
```
esy install
make
```

### Testing Done

In order to test this module, I've stolen test cases from the very well tested [reworkcss](https://github.com/reworkcss/css).
All of the test cases are run through OUnit and can be executed with:

```
make test
```
