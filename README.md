[![Build SystemP](https://github.com/hoffjan/ppl-systemp/actions/workflows/build.yml/badge.svg)](https://github.com/hoffjan/ppl-systemp/actions/workflows/build.yml)

# System P

System P is a probabilistic programming language based on System P. It
has been developed at Carnegie Mellon University as part of the course
15-819: Probabilistic Programming Languages.

The following files contain additional information

- ISSUES.md
  a list of know bugs and issues

- LICENSE.md
  information about the software license

## System Requirements

- OCaml, Opam, and dependencies such as Core

  System P has been checked to build with the following OCaml versions: 4.14.2

- [POSIX](https://en.wikipedia.org/wiki/POSIX)-compliant Operating System

## Building System P

1) Install opam.

   Follow the instructions at https://opam.ocaml.org/doc/Install.html

   This should include updating your shell environment by running the
   following.

        eval `opam config env`

2) Clone the System P repository

3) In the toplevel directory of the repository, run the following
   command

        opam install .

   The binary file systemp should now be in your path.

## Syntax

The deterministic fragment of System P contains functions, lists, primitive
recursion on lists, labeled products and sums, and constants and primitive
operations for integers, floats, and strings.

A System P program begins with a (possibly empty) sequence of type
declarations followed by an expression, as defined by the following
grammar.

```
PROG ::= ('type' ID '=' SUMTYPE)* EXP

EXP ::= ID                                       (variable)
        CONST                 					 (constant)
        ID (ID ':' TYPE) EXP    				 (function abstraction)
		EXP EXP                                  (function application)
		OP EXP                					 (unary operator)
		EXP OP EXP            				  	 (binary operator)
		CID EXP                                  (injection; sum introduction)
		'case' ['[' TYPE ']'] EXP CASES          (case analysis; sum elimination)
		'{' [ID '=' EXP (',' ID '=' EXP)*] '}'   (named tuple; product introduction)
		EXP '.' ID                               (projection; product elimination)
		'let' ID = EXP 'in' EXP                  (let binding)
		'Nil' '[' TYPE ']'                       (empty list)
        'Cons' '(' EXP ',' EXP ')'               (nonempty list)
		'rec' EXP "{" "Nil" "->" EXP " |" "Cons" (list recursor)
	        "(" ID "," "ID" ")" "->" EXP "}"

ID ::= ( a-z ) ( a-z | A-Z )*

CID ::= ( A-Z ) ( a-z | A-Z )*

CASES = '{' [ CID ID '->' EXP ('|' CID ID '->' EXP)* ] '}'

CONST ::= FLOAT | INT | STRING

SUMTYPE ::= '[' [CID ':' TYPE (',' CID ':' TYPE)*] ']'
PRDTYPE ::= '{' [ID ':' TYPE (',' ID ':' TYPE)*] '}'

TYPE ::= 'int'            (integer base type)
         'string'         (string base type)
         'float'          (float base type)
         TYPE '->' TYPE   (function type)
         TYPE 'list'      (list type)
         SUMTYPE          (sum type)
         PRDTYPE          (product type)

```

## Usage

```
  systemp SUBCOMMAND

=== subcommands ===

  eval FILENAME              . Evaluate a System P program
  typecheck FILENAME         . Evaluate a System P program
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)
```
