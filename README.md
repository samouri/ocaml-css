# css ocaml
[![Build Status](https://travis-ci.org/samouri/ocaml-css.svg?branch=master)](https://travis-ci.org/samouri/ocaml-css)

parse, detect errors, and pretty-print css natively in OCaml.

**this is under active development, please do not use yet**

## Overview

`css-parse` is written in OCaml and utilized ocamllex and [menhir](http://gallium.inria.fr/~fpottier/menhir/manual.html) for lexer and parser generation.
We generate a binary as well 

## Development

You'll need to install esy as well as mehir.
To bootstrap the project run:
```
esy install
make
```

### Testing Done

In order to test this module, I've stolen all of the test cases from the very well tested [reworkcss](https://github.com/reworkcss/css).
All of the test cases are run through OUnit and can be executed with:

```
make test
```
