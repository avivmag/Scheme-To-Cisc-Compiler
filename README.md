# Scheme-To-Cisc-Compiler

A compiler which compiles Scheme programs to cisc-like architecture assembly through most of the compiling stages.

The development of this compiler was done in pairs as part of multiple assignments in "Compiler Principles" course, by the guidance of the lecturer Dr. Goldberg Mayer, at Ben-Gurion University in the fall of 2017.

Note: this repository demonstrates a lot of great ideas. For now, a brief explaination of the implementation is covered here. A fully detailed description of the implementation can be found in the assignments descriptions attached.

Every compiling stage uses the last stage output as its input.</br>
The stages of this process is as follows:

## Lexical analysis

This part gets as an input the high-level language user code and constructs a list of meaningful tokens to deliver to the next part. It ignores white spaces, comments and other insignificant characters. It will discover some very simple syntax errors (i.e. missing characters such as closing `)` or `"`).

## Syntax analysis

This part gets as an input the tokens and parse them to an abstract syntax tree (AST).

## Semantic Analysis

Checks whether the parse tree constructed follows the rules of the language. The semantic analyzer keeps track of identifiers, their types and expressions and produces an annotated syntax tree as an output.
Also, optimizations are made on this stage.

## Code Generation

The copiler generates the final assembly code.

## Etc.

Some more minor exercises that are part of the assignment attached: 
* remww.scm 
* sce.scm

## Getting Started
### Prerequisites

1. Kubuntu - this program was tested only on kubuntu, but it probably can be ran on any other known qemu and gcc compatible operating systems.
	https://kubuntu.org/getkubuntu/</br>
2. Chez scheme
	can be downloaded and installed from https://www.scheme.com/download/.
3. gcc compiler
	via ```sudo apt-get install gcc-4.8``` on ubuntu based os (kubuntu included).

### Simulating the process

1. open terminal and navigate to the program directory
2. edit input.scm file as you like - we will compile that file.
3. run scheme via `scheme` command.
4. when shell is available, type `(compile-scheme-file "input.scm" "out.c")`, now the compiler build out.c file from compiling the input.scm file.
5. you can either run the next command on two ways:</br>
	a. from scheme type: `(begin (system "gcc -o out out.c") (system "out") (display " "))`</br>
	b. exit scheme via `(exit)`, type `gcc -o out out.c` and then run `out`.</br>
	both ways should compile the compiled out.c file to a truly runnable linux file.</br>
6. watch you program being excuted and enjoy :).

## Built With

* [Chez scheme](https://www.scheme.com/download/).
* [gcc](https://gcc.gnu.org/).

## Useful links

* The original sources of the assignment: 
	[part 1](https://www.cs.bgu.ac.il/~comp171/wiki.files/hw1.pdf)
	[part 2](https://www.cs.bgu.ac.il/~comp171/wiki.files/hw2.pdf)
	[part 3](https://www.cs.bgu.ac.il/~comp171/wiki.files/hw3.pdf)
	[part 4](https://www.cs.bgu.ac.il/~comp171/wiki.files/proj.pdf)
* https://en.wikipedia.org/wiki/Complex_instruction_set_computer
* https://en.wikipedia.org/wiki/Assembly_language
* https://en.wikipedia.org/wiki/Compiler
* https://www.tutorialspoint.com/compiler_design/compiler_design_phases_of_compiler.htm
