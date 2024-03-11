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
* Compiler should be able to parse a PPS and generate running PPE files
* Runner should basically work but most pre defined functions are not implemented
* Started to implement a LSP to provide syntax highlighting and tooltips 

For my purposes this project is "completed" - up to the point I'll decide to write my own PCBoard like BBS :).

### Decompiler
Basically it's the PPLD from DOS in an enchanced version.
It adds:

 * PPE 3.30 Support
 * Reconstruction of control structures if then/elseif/else, while…endwhile, for…next, break & continue, select case support
 * It tries to do some name guessing based on variable usage.
 * It's possible to see the raw output with the -r option
 * A bit more control over the output keyword style with the -s option

### Compiler
  * Compiles to .ppe but is probably full of bugs due to current AST refactorings

### Runner
  * pplx is able to run 99bottles.ppe and surely some more complex PPEs (agsenter and agslogin work)
  * But probably ~95% won't run but feel free to request some - it's easy to get one specific to run.

## TODO

* Execution engine needs to be completed.
* Real Compiler/Decompiler support for 15.4 (debugging the old dos pplc/pcobard is the way to go here)
* Possible rewrite the expression decompilation part - it's hard to follow atm.
* LSP needs to be extended - find references, rename, code completion etc.

## Building & Running

* Get rust
* cargo build -r
* cd target/release
* ./ppld [PPEFILE]
* ./pplrun [PPEFILE] - it's likely to crash not many instructions are implemented
* ./pplc - doesn't do anything yet

Result is printed to stdout
