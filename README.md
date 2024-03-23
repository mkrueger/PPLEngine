# PPLEngine

An engine for PPL/PPE PCBoard handling - just for fun.

Features:

* A new decompiler/disassembler engine (ppld)
* A compiler (pplc) that compiles UTF-8/CP437 files to output CP437 PPEs
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

Decompiler is completely rewritten and can disassemble now on top of recompilation.

* PPE 3.30 Support
* Reconstruction of control structures beside if…then is currently broken.
* It tries to do some name guessing based on variable usage.

```text
Usage: ppld [OPTIONS] INPUT

Arguments:
  INPUT  file[.ppe] to decompile

Options:
  -r, --raw            raw ppe without reconstruction control structures
  -d, --disassemble    raw ppe without reconstruction control structures
  -o, --output         output to console instead of writing to file
  -s, --style STYLE  keyword casing style, valid values are u=upper (default), l=lower, c=camel
  -h, --help           Print help
  -V, --version        Print version
```

The dissamble output can be used to see what the compilers are generating and for debugging purposes.

### Compiler

Supports 15.4 PPL extensions but not the container format yet.

Should be compatible to the old PCB compiler with some slight differences - I added some keywords which are not available as identifier (but as label):
```text
LET,
IF, ELSE, ELSEIF,ENDIF,
WHILE, ENDWHILE,
FOR, NEXT, BREAK, CONTINUE, RETURN,
GOSUB, GOTO,
SELECT, CASE, DEFAULT, ENDSELECT
```

I think it improves the language and it's open for discussion. Note that some aliases like "quit" for the break keyword is not a keyword but is recognized as 'break' statement. I can change the status of a keyword so it's not a hard limit - as said "open for discussion".

The compiler automatically generates user variables, if needed but behavior can be changed with command line options. It does some optimizations so it should produce smaller & faster exectuables than the original one.

pplc has following options:

```text
Usage: pplc [OPTIONS] INPUT
Arguments:
  INPUT  file[.pps] to compile (extension defaults to .pps if not specified)

Options:
  -d, --disassemble                Output the disassembly instead of compiling
      --nouvar                     Force no user variables
      --forceuvar                  Force user variables
      --nowarnings                 Don't report any warnings
      --ppl-version PPL_VERSION    Version number for the compiler, valid: 100, 200, 300, 310, 330 (default), 340
      --dos                        Input file is CP437
  -h, --help                       Print help
  -V, --version                    Print version

As default the compiler takes UTF8 input - DOS special chars are translated to CP437 in the output.
```

Note:  All old DOS files are usually CP437 - so it's recommended to use --dos for compiling these.

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

The aim is to be as compatible as possible.

* Added € as valid identifier character. (for UTF8 files)
