# PPLEngine

An engine for PPL/PPE PCBoard handling - just for fun.

Features:

* A decompiler (ppld) based on PPLD <https://github.com/astuder/ppld>
* A compiler (pplc) that compiles UTF-8 files to output CP437 PPEs
* A runtime (pplx) that runs .PPE files on console
* A language server that provides developer functionality in any editor that supports lsp

## Why are you doing this?

Just for fun. Started this project initially to learn rust.

## What works

* Decompiler is pretty complete (report bugs!). I would say it's better than everything we had back in the 90'.
* Compiler should be able to parse a PPS and generate running PPE files
* Runner should basically work.
* Started to implement a LSP to provide syntax highlighting and tooltips.

### Decompiler

Basically it's the PPLD from DOS in an enchanced version.
It adds:

* PPE 3.30 Support
* Reconstruction of control structures if then/elseif/else, while…endwhile, for…next, break & continue, select case support
* It tries to do some name guessing based on variable usage.
* It's possible to see the raw output with the -r option
* A bit more control over the output keyword style with the -s option

### Compiler

* Supports --nouvar Disable automatic user variable generation
* Supports 15.4 PPL extensions but not the container format

### Runner

* pplx is able to run several PPEs on command line still has many limits but it's usable
* Feel free to request support for missing PPEs there are still bugs

## TODO

* Execution engine needs to be completed.
* Real Compiler/Decompiler support for 15.4 (debugging the old dos pplc/pcobard is the way to go here)
* Possible rewrite the expression decompilation part - it's hard to follow atm.
* LSP needs to be extended - find references, rename, code completion etc.

## Building & Running

* Get rust on your system <https://www.rust-lang.org/tools/install>

```bash
cd PPLEngine
cargo build -r
```

running in release mode:

```bash
cd target/release
./ppld [PPEFILE]
./pplx [PPEFILE]
./pplc [PPLFILE]
```

Result is printed to stdout

Testing the lsp using vs code:

```bash
cd ppl-lsp
pnpm i
cargo build 
```

then start vs code with

```bash
code .
```

And run from inside vs code the project with F5. A new vs code opens with PPL support.
(Still need to figure out how to plug in the lsp for other editors)

## PPL differences

The aim is to be 100% compatible.

* Added € as valid identifier character.
  