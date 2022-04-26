# MicroMorpho compiler

This is a compiler for the programming language MicroMorpho
written in Haskell. It was produced during the course
TÖL202M Þýðendur in the University of Iceland on compiler
design, taught by Snorri Agnarsson in 2022.

## Installation

To install this compiler, the build tool Cabal is required,
along with a Haskell compiler. It should suffice to install
[GHCup](https://www.haskell.org/ghcup/install/).

Now, simply run the following command in the project's root directory:

> cabal install

This creates an executable `mm-compiler` and places it
in `$HOME/.cabal/bin/` on UNIX machines and
in `%APPDATA%\cabal\bin` on Windows (this behavior can
be changed with Cabal). 

This directory can be added to the `PATH` environment
variable to enable running the compiler from the command line.

## Uninstallation

To uninstall this compiler, simply remove the `.cabal` directory
specified above. If this is not desirable (e.g.~if other executables
or libraries in use are found there), simply remove `mm-compiler`
from `.cabal/bin` and the appropriate directories from `.cabal/store`.

## Simple use

To illustrate the use of this compiler, let us write a simple program in MicroMorpho.

Begin by placing the following in the file `hello.mm`:

> writeln("Hello, world!");

Now, compile the program using the command below:

> $ mm-compiler hello.mm

Finally, run the program:

> $ mm-compiler run hello.mexe
> Hello, world!

## Further information

A manual (written in Icelandic) describing the syntax and semantics of MicroMorpho
can be found in `doc/manual/main.pdf`.

To see a comprehensive summary of the compilers uses,
call the compiler with the `--help` option.

> $ mm-compiler --help
