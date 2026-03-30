## Bootstrap compiler for the early version of Admiran, written in Miranda

This is the original Miranda bootstrap compiler used to build the original Admiran compiler (then called "mirac").
To build this, you need the original Miranda language distribution compiler "mira".

### Setup

The miraBoot compiler uses Miranda's "-exec" option to be able to run as a command-line program; it
assumes that the mira compiler is installed in /usr/local/bin/mira. If not, you will need to edit the
first line of miraBoot/mirac.m to provide the correct path.

### Compiling miraBoot/mirac.m

In the miraBoot directory, type "mira mirac", which will compile the mirac.m boostrap compiler.
To test it, go into the "examples" directory and type "../miraBoot/mirac.m primes", which should create
a "primes.s" asm file.  To run this, type "cc -o primes primes.s ../lib/runtime.c" to create the native
"primes" executable.

### Building the original Admiran compiler

The early version of the Admiran compiler can be built by going into the compiler directory, and type
"../miraBoot/mirac.m mirac".  Note: this takes a lot of Miranda heap space to build; you will need to
bump the heap to 250000000 cells with the /heap command, and it will take ~20 minutes to build.
