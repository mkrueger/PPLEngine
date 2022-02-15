# PPLEngine

An engine for PPL/PPE PCBoard handling - just for fun.
Based on the great PPLD decompiler https://github.com/astuder/ppld
But the goal is to have an engine that can run .PPE files.
Extra goal would be a PPL compiler.

## What does PPLD do?

It decompiles [PCBoard](https://en.wikipedia.org/wiki/PCBoard) PPL files. PPL was a basic written for
this BBS system.

## Why are you doing this?

Basically to learn the rust programming language. I still have much to do and I needed something
and the chance that anyone else is wasting time on that is basically 0. It may be useful for getting the old
BBS software running under modern telnet bases BBSes.
I still have to learn a lot of the rust programming language but it was worth every second. Now I realize that I basically wasted years not writing programs in rust :) 

## What works

Decompiler is pretty complete (report bugs!). I would say it's better than everything we had back in the 90'.
 
### Decompiler
Basically it's PPLD

 * PPE 3.30 Support
 * Reconstruction of control structures if then/elseif/else, while…endwhile, for…next, break & continue support
 * It's possible to see the raw output with the -r option
### Compiler
  * Nothing

### Runner
  * pplrun is able to run 99bottles.ppe
## TODO

* Implement naming scheme - many names can be guessed. Like for loop variables i, j, k… or file handles, file names etc..
  There are many possibilities to implement variable name guessing strategies from the variable usage.
* Execution engine just started.
* Finish parsing layer. Basically all control constructs are missing and semantic error checking is missing.


## Building & Running

* Get rust
* cargo build -r
* cd target/release
* ./ppld [PPEFILE] 
* ./pplrun [PPEFILE] - it's likely to crash not many instructions are implemented
* ./pplc - doesn't do anything yet

Result is printed to stdout