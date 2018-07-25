# hello-ocaml

[![Build Status](https://travis-ci.org/esy-ocaml/hello-ocaml.svg?branch=master)](https://travis-ci.org/esy-ocaml/hello-ocaml)

A project which demonstrates an OCaml workflow with [Esy][].

[Esy]: https://github.com/esy-ocaml/esy
[npm]: https://www.npmjs.com

## Usage

You need Esy, you can install the beta using [npm][]:

    % npm install -g esy

Then you can install the project dependencies using:

    % esy install

Then build the project dependencies along with the project itself:

    % esy build

Now you can run your editor within the environment (which also includes merlin):

    % esy $EDITOR
    % esy vim

After you make some changes to source code, you can re-run project's build
using:

    % esy build

And test compiled executable:

    % esy ./_build/default/bin/hello.exe

Shell into environment:

    % esy shell
