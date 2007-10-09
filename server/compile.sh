#!/bin/bash
echo "**Running ocamllex**"
ocamllex lexer.mll
echo "**Running ocamlyacc**"
ocamlyacc parser.mly
echo "**Compiling parser.mli**"
ocamlc -c parser.mli
echo "**Compiling parser.ml**"
ocamlc -c parser.ml
echo "**Compiling lexer.ml**"
ocamlc lexer.ml
echo "**Done**"