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

* Decompiler is pretty complete (report bugs!). I would say it's better than everything we had back in the 90'.
* Compiler should be able to parse a PPS and print an executable AST - I didn't really shoot for a real compiler it's enough to be able to interpret .pps Files
* Runner should basically work but most pre defined functions are not implemented

For my purposes this project is "completed" - up to the point I'll decide to write my own PCBoard like BBS :).

### Decompiler
Basically it's the PPLD from DOS in an enchanced version.
It adds:

 * PPE 3.30 Support
 * Reconstruction of control structures if then/elseif/else, while…endwhile, for…next, break & continue support
 * It tries to do some name guessing based on variable usage.
 * It's possible to see the raw output with the -r option
 * A bit more control over the output keyword style with the -s option

### Compiler
  * Just prints what it could parse
### Runner
  * pplrun is able to run 99bottles.ppe and surely some more complex PPEs

## TODO

* Execution engine needs to be completed.
* Compiler output to PPE?

## Building & Running

* Get rust
* cargo build -r
* cd target/release
* ./ppld [PPEFILE]
* ./pplrun [PPEFILE] - it's likely to crash not many instructions are implemented
* ./pplc - doesn't do anything yet

Result is printed to stdout
