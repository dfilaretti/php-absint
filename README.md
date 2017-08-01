# PhpAbsint

This aims to be a complete rewrite of KPHP and KPHP#, using F# and the .NET framework.

Before using F#, we used Racket. It has been rewritten a few times! 

## Current Status

At the moment, we model pretty much everything that used to be in `memory.k` in the old `KPHP` (in what now is `KphpLib.fs`)
This means we have the "correct" memory model, including arrays, complex array reference etc. `KphpLib.fs` takes the form of library code, to be used by the actual interpreter. 

On top of it, we have an interpreter, in a [CESK-like style](http://matt.might.net/articles/cesk-machines/), which looks pretty similar in spirits to `K` and our `KPHP/#`.
We don't have many features yet. Well, we can currently only do assignments... including complex ones! Like `$x[][][] = 123` and so on. 
This comes basically "for free" after having defined `KphpLib.fs`. 

The focus now is mainly on getting the architecture right.  
From the experiments so far, it seems everything translates pretty well from `KPHP` to F#! :)

## Notes/Info

### Why F#?

F# is a language in the ML family (similar to OCAML) which is driven by both open source
community and Microsoft. It is a .NET language, meaning it targets Microsoft CLI (Common
Language Runtime) in the same way as e.g. C# and VB does. This also means the whole .NET
libraries and ecosystem are available to F# programs.

### Parsing

We use a PHP parser from https://www.devsense.com/ The parser is written in C# but can be
accessed via F# (as they both target the underlying intermediate language).
It seems to be fully featured (error management, full  line number info etc.) although
I haven't explored all of this in detail.

## Setting up a development environment (DEPRECATED!)

## NOTE: things have changed since this section have been written. The project now opens out of the box in Visual Studio! Should figure out how to make a bit more cross-platform (e.g. .NET Core + Ionide??).  ##

You can either follow the traditional route of installing everything you need on your computer or use the Vagrant virtual machine provided. See next subsections for details. 

### Configure your machine for F# development

This is the "traditional" way. All you need to do is basically to [set up
F# on your host machine](http://fsharp.org/). Please follow the instruction on the official homepage. 

### Using the provided Vagrant box

If you do not want to configure your system for F# development, a Vagrant box is provided (if you are wondering what Vagrant is, please read [here](https://www.vagrantup.com/intro/index.html)).

In order to use this method, you first need to [install Vagrant on your machine](https://www.vagrantup.com/intro/getting-started/install.html).

Once Vagrant is installed, simply position yourself into this folder (the repository root) and type

```bash
vagrant up
```
This command will magically create a lightweight Ubuntu virtual machine and
install all required dependencies on it for you (F#. Mono, etc.).
It may take a few minutes for all this to happen.
Once this is finished, type

```bash
vagrant ssh
```
and you will be now logged into your new virtual machine (you'll see
the command prompt changing).

Now, try

```bash
cd /vagrant
ls
```

and notice how the contents of the `vagrant` folder is the content of the
repository itself. Indeed, this folder is shared between your host machine
and the virtual machine.

This means that, if you fancy, you could edit the code from your host machine
using whatever tool/editor you prefer, and the changes will be immediately
reflected inside the virtual machine. From there, you could then compile and
run the code.

If you want to learn more about Vagrant, please read the docs!

## Building

Just run

```bash
build.sh
```

from the root folder of the repo. 
Notice that the first time you run it, it will bootstrap the [Packet package manager](https://fsprojects.github.io/Paket/) and pull in all required dependencies for the project, which may take a bit more then usual. 

At the moment, the compilation process will generate a `dll`, i.e. there is no executable to run directly (TODO). The generated `dll` library can either be used in other projects or from F# `fsx` [script files](https://stackoverflow.com/questions/23292701/what-are-the-differences-between-in-fsx-fsi-and-fs-file-in-f) 

## Running the interpreter and doing other things

Fortunately is still very easy to run and test
the interpreter, as well as interactively play with the existing functionality. How? Simply use the F# REPL or, better, a script file (`fsx`).
The `scripts` folder contains a few commented examples to get started

Try 

```bash
fsharpi scripts/script1.fsx
```
or 

```bash
fsharpi scripts/script2.fsx
```

etc. Btw, `fsharpi` invokes the F# interpreter, while `fsharpc` is for the F# compiler. 


## Using an IDE

One of the good things about F# is that it has very good tool/IDE support.
Common choices are

* [Visual Studio](https://www.visualstudio.com/): cool for very big/complex projects. Probably Windows only?
* [Ionide plugin for Visual Studio Code or Atom](http://ionide.io/): cool plugin for [Visual Studio Code](https://code.visualstudio.com/) or [Atom](https://atom.io/) editors. Ideal from someone like me who'd like to use a general purpose editor but never got their head around emacs properly. I am currently using Visual Studio Code with Ionide.

There should be good F# support as well for emacs and vim, but I didn't investigate that.

### Caveats

If using the Vagrant solution, you could easily edit the code in any way you want from your host OS, and compile/run it from the guest VM.
There could be issues, however, in case you wish to use a rich development environment such as Visual Studio or Ionide: I haven't tried it yet, but I believe that with such a setup you would not be able to run the F# REPL and run code "on the fly" from inside the editor, unless you install F# on the host machine (in that case, why using Vagrant?). This is because such IDEs, need access to a interpreter/compiler in order to make interactive code evaluation (as well as typechecking etc.) possible.
I need to investigate more but I don't think this will wotrk righ now. A quick search about "IDE remote interpreter" reveals that some Python IDE is able to do this. There is some discussion online about the topic.


  

