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

* PPE 3.40 Support
* Reconstruction of control structures beside if…then is currently broken.
* It tries to do some name guessing based on variable usage.

```text
Usage: ppld <input> [--raw] [-d] [--output] [--style <style>]

PCBoard Programming Language Decompiler

Positional Arguments:
  input             file[.ppe] to decompile

Options:
  --raw             raw ppe without reconstruction control structures
  -d, --disassemble output the disassembly instead of ppl
  --output          output to console instead of writing to file
  --style           keyword casing style, valid values are u=upper (default),
                    l=lower, c=camel
  --help            display usage information
```

The dissamble output can be used to see what the compilers are generating and for debugging purposes.

### Compiler

Supports up to 15.4 PPL (1.0 -> 3.40 PPE format)

Should be compatible to the old PCB compiler with some slight differences (see PPL differences)

The compiler automatically generates user variables, if needed but behavior can be changed with command line options. It does some optimizations so it should produce smaller & faster exectuables than the original one.

pplc has following options:

```text
Usage: pplc <input> [-d] [--nouvar] [--forceuvar] [--nowarnings] [--ppl-version <ppl-version>] [--dos]

PCBoard Programming Language Compiler

Positional Arguments:
  input             file[.pps] to compile (extension defaults to .pps if not
                    specified)

Options:
  -d, --disassemble output the disassembly instead of compiling
  --nouvar          force no user variables
  --forceuvar       force user variables
  --nowarnings      don't report any warnings
  --ppl-version     version number for the compiler, valid: 100, 200, 300, 310,
                    330 (default), 340
  --dos             input file is CP437
  --help            display usage information

As default the compiler takes UTF8 input - DOS special chars are translated to CP437 in the output.
```

Note:  All old DOS files are usually CP437 - so it's recommended to use --dos for compiling these.

#### PPL differences

The aim is to be as compatible as possible.

* Added keywords that are invalid as identifiers (but are ok for labels):
  ```LET```, ```IF```, ```ELSE```, ```ELSEIF```, ```ENDIF```, ```WHILE```, ```ENDWHILE```, ```FOR```, ```NEXT```, ```BREAK```, ```CONTINUE```, ```RETURN```, ```GOSUB```, ```GOTO```, ```SELECT```, ```CASE```, ```DEFAULT```, ```ENDSELECT```

I think it improves the language and it's open for discussion. Note that some aliases like "quit" for the break keyword is not a keyword but is recognized as 'break' statement. I can change the status of a keyword so it's not a hard limit - as said "open for discussion".

* Added ```€``` as valid identifier character. (for UTF8 files)
* Return type differences in function declaration/implementation is an error, original compiler didn't care.

### Runner

* pplx is able to run several PPEs on command line still has many limits but it's usable
* Feel free to request support for missing PPEs there are still bugs

#### Runner differences

Fixed an recursion bug:
  
```PPL
DECLARE PROCEDURE FOO(VAR BYTE X)
BYTE C
FOO(C)
PRINTLN "End value:", C

PROCEDURE FOO(VAR BYTE X)
  BYTE LOC
  LOC = X + 1
  if (X > 2) RETURN
  X = X * 2
  PRINTLN LOC ,":", X
  FOO(LOC)
  PRINTLN LOC ,":", X
  X = LOC
ENDPROC	
```

The value of LOC changes between prints but does not in PCBoard. 
pcboard prints:
```TEXT
1:0
2:2
3:4
3:4
2:2
1:0
End value:1
```

Correct is:
```TEXT
1:0
2:2
3:4
3:4
3:2
3:0
End value:3
```

Would be easy to simulate the bug (just swap local write back & variable write back in endproc handling).
But I don't assume that this bug affects any existing PPEs.

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
