# Reveal

Read Eval Visualize Loop for Clojure

[![Clojars Project](https://img.shields.io/clojars/v/vlaaad/reveal.svg?logo=clojure&logoColor=white)](https://clojars.org/vlaaad/reveal)

## Rationale

I want to be able hold a value in my hand. I want to understand it better and with less 
effort.

Repl is a great window into a running program, but text representation is too limited for 
inspection. Reveal recognizes the value of a text as universal interface, that's why its 
output looks as a text by default: you can select it, copy it, save it into a file. It's 
not just an array of characters though: printed string representations of objects hold 
references to objects themselves, making value inspection as easy as bringing up a context 
menu.

If I want to be able to understand the values I'm looking at, the tool to do it needs to
treat these values as sacred. Where datafy-and-nav based tools like REBL pretend that 
atoms are single-element vectors, Reveal never obscures evaluation results, even the 
metadata.

Not being limited to just text, Reveal uses judicious syntax highlighting and sometimes 
simplified string representations for data to make it more approachable yet distinctive. 
For example, `java.lang.Integer` has different formatting depending on whether it 
was produced from symbol or class, and function `+` is displayed as `clojure.core/+`, 
not an `#object[clojure.core$_PLUS_ 0x1e295f7f "clojure.core$_PLUS_@1e295f7f"]`, while 
still looking different from the symbol `'clojure.core/+`


## Project status

Early Access: everything is a subject to change, lots of stuff to be implemented yet, but 
it's already a superior experience compared to repl.

## How it works

It does not depend on any particular IDE or text editor, it works in-process instead: when 
started, it will open a window where evaluation results will appear. The window supports 
both mouse and keyboard navigation. 

Context menu on selected value is opened either by pressing <kbd>space</kbd> or by right 
mouse button click. Selecting action to run is done either with arrow keys and 
<kbd>enter</kbd>, or with left mouse button click.

## Installation and requirements

Add a dev dependency on the latest version:

[![Reveal on Clojars](https://clojars.org/vlaaad/reveal/latest-version.svg)](https://clojars.org/vlaaad/reveal)

Minimum required clojure version is `1.10`.

If you are using nrepl, just add `vlaaad.reveal.nrepl/middleware` to the middleware list. 
Minimum nrepl version is `0.6.0`.

If you are using socket repl, just run main in `vlaaad.reveal.prepl` namespace.

If you don't want to use it as a repl, just call `(vlaaad.reveal.ui/make)` and it will 
open a window and return a function: call it with 1 argument to submit a value, call it 
with 0 arguments to close the window and dispose it.

## Try it out

### As a repl in a shell

```sh
clj -Sdeps '{:deps {vlaaad/reveal {:mvn/version "0.1.0-ea2"}}}' -m vlaaad.reveal.prepl
```

### As an nrepl middleware in a shell

```sh
clj \
-Sdeps '{:deps {vlaaad/reveal {:mvn/version "0.1.0-ea2"} nrepl {:mvn/version "0.6.0"}}}' \
-m nrepl.cmdline --middleware '[vlaaad.reveal.nrepl/middleware]'
```

## To do

- accordion navigation: auto-scroll to new tab, ctrl+arrows to go to next/prev 
- multiple accordions
- text search (triggered either just by typing, or with a shortcut)
- context menu should allow text input that is then evaluated on selection
- more representations of objects:
  - as arbitrary JavaFx/cljfx nodes
  - atoms: watch (always show latest), log (as a seq of successors)
  - url: as content, maybe even as a web page
  - directory: as file tree
  - file: as content
  - coll of maps, coll of same-length vectors: as table
  - documentable things (fns, vars, keywords): as formatted documentation
  - sourceable things: as source code
- improve datafy/nav support (on sets, for example)
- structural navigation
- remember window position and size
- popup might appear in weird locations
- logo and window icons
