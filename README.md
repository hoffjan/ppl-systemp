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
	    'true' | 'false' |                       (boolean constants)
		'if' EXP 'then' EXP 'else' EXP |         (conditional)
		'sample' EXP 'at' EXP |                  (sample)
		DIST                                     (built-in distribution)

DIST = bernoulli | binomial | nominal

ID ::= ( a-z ) ( a-z | A-Z )*

CID ::= ( A-Z ) ( a-z | A-Z )*

CASES = '{' [ CID ID '->' EXP ('|' CID ID '->' EXP)* ] '}'

CONST ::= FLOAT | INT | STRING

OP ::= '^' | '::' | '+' | '*' | '/' | '%' | '=' | '/=' | | '<' | '<=' | '>' | '=>'

SUMTYPE ::= '[' [CID ':' TYPE (',' CID ':' TYPE)*] ']'
PRDTYPE ::= '{' [ID ':' TYPE (',' ID ':' TYPE)*] '}'

TYPE ::= 'int'            (integer base type)
         'string'         (string base type)
         'float'          (float base type)
         TYPE '->' TYPE   (function type)
         TYPE 'list'      (list type)
         SUMTYPE          (sum type)
         PRDTYPE          (product type)
		 TYPE 'dist'      (distribution type)
```

## Usage

### Executable

```
  systemp SUBCOMMAND

=== subcommands ===

  assess                     . Compute the weight of a complete trace of a
                               System P program
  eval                       . Evaluate a System P program
  simulate                   . Sample a trace from a System P program
  typecheck                  . Evaluate a System P program
  version                    . print version information
  help                       . explain a given subcommand (perhaps recursively)
```

### Top Level

You can run `dune utop` to get access to the model Systemp, which 
contains the interface for use in a top level CLI.

```
utop # #show Systemp;;
module Systemp :
  sig
    module Statics = Systemp__.Statics
    module Parser = Systemp__.Parser
    module Syntax = Systemp__.Syntax
    module Dynamics = Systemp__.Dynamics
    val model_of_file : string -> Syntax.Exp.t
    val exp_of_str : string -> Syntax.Exp.t
    val model_of_string : string -> Systemp.Dynamics.Value.t Dynamics.Trace.t
    val simulate :
      ?seed:int ->
      Syntax.Exp.t -> Dynamics.Value.t option Dynamics.Value.result
    val assess :
      Dynamics.Value.t Dynamics.Trace.t ->
      Syntax.Exp.t -> Dynamics.Value.t option Dynamics.Value.result
    val eval : Syntax.Exp.t -> Dynamics.Value.t
    val print_value : Dynamics.Value.t -> unit
    val print_trace : Dynamics.Value.t Dynamics.Trace.t -> unit
    val print_result :
      ?weight:bool ->
      ?trace:bool -> Dynamics.Value.t option Dynamics.Value.result -> unit
  end
```
