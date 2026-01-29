[![Build SystemP](https://github.com/hoffjan/systemp/actions/workflows/build.yml/badge.svg)](https://github.com/hoffjan/systemp/actions/workflows/build.yml)

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

- POSIX compliant OS

## Building System P

1) Install opam.

   Follow the instructions at https://opam.ocaml.org/doc/Install.html

   This should include updating your shell environment by running the
   following.

   eval `opam config env`

2) Clone the System P repository

3) In the toplevel directory of the repository, run the following
   command

   opam install --deps-only .

   The binary file systemp should now be in your path.

   [FS: I needed to write `opam install .` to build systemp, not just deps.]

## Syntax

The deterministic fragment of System P contains functions, lists, primitive recursion on lists, labeled products and sums, and constants and primitive operations for integers, floats, and strings. Expressions are defined as follows.

EXP ::= ID                                       (variable)
        CONST                 					 (constant)
        ID (ID ':' TYPE) EXP    				 (function abstraction)
		EXP EXP                                  (function application)
		OP EXP                					 (unary operator)
		EXP OP EXP            				  	 (binary operator)
		CID EXP                                  (injection)
		'case' ['[' TYPE ']'] EXP CASES          (case analysis)
		'{' [ID '=' EXP (',' ID '=' EXP)* ] '}'  (product)
		EXP '.' ID                               (projection)
		'let' ID = EXP 'in' EXP                  (let binding)
		'Nil' '[' TYPE ']'                       (empty list)
        'Cons' '(' EXP ',' EXP ')'               (non-empty list)
		'rec' EXP "{" "Nil" "->" EXP " |" "Cons" (recursor) 
	        "(" ID "," "ID" ")" "->" EXP "}"

ID ::= ( a-z ) ( a-z | A-Z )*

CID ::= ( A-Z ) ( a-z | A-Z )*

CASES = '{' [ CID ID '->' EXP ('|' CID ID '->' EXP)* ] '}'

CONST ::= FLOAT | INT | STRING


## Usage

  systemp SUBCOMMAND

=== subcommands ===

  eval FILENAME              . Evaluate a System P program
  typecheck FILENAME         . Evaluate a System P program
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)
