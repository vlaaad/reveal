# ![logo](src/vlaaad/reveal/logo-32.png) Reveal: Read Eval Visualize Loop for Clojure

![demo](doc/demo.gif)

[![Clojars Project](https://img.shields.io/clojars/v/vlaaad/reveal.svg?logo=clojure&logoColor=white)](https://clojars.org/vlaaad/reveal)
[![Slack Channel](https://img.shields.io/badge/slack-reveal@clojurians-blue.svg?logo=slack)](https://clojurians.slack.com/messages/reveal/)

## Rationale

I want to be able to hold a value in my hand. I want to understand it better and with fewer 
keystrokes.

Repl is a great window into a running program, but text representation is too limited for 
inspection. Reveal recognizes the value of a text as a universal interface, that's why its 
output looks like text by default: you can select it, copy it, save it into a file. It's 
not just an array of characters though: printed string representations of objects hold 
references to objects themselves, making value inspection as easy as bringing up a context 
menu.

If I want to be able to understand the values I'm looking at, the tool to do it needs to
treat these values as sacred. Where datafy-and-nav based tools like REBL pretend atoms are
single-element vectors, Reveal never obscures evaluation results, even the metadata.

Not being limited to just text, Reveal uses judicious syntax highlighting and sometimes 
simplified string representations for data to make it more approachable yet keep it 
distinctive. For example, `java.lang.Integer` has different formatting depending on 
whether it was produced from symbol or class, and function `+` is displayed as 
`clojure.core/+`, not an `#object[clojure.core$_PLUS_ 0x1e295f7f "clojure.core$_PLUS_@1e295f7f"]`, 
while still looking different from the symbol `'clojure.core/+`.


## Project status

Early Access: everything is a subject to change, lots of stuff to be implemented yet, but 
it's already a superior experience compared to repl.

## How it works

It does not depend on any particular IDE or text editor, it works in-process instead: when 
started, it will open a window where evaluation results will appear. The window supports 
both mouse and keyboard navigation. 

The main way to interact with values is using the context menu. You can press 
<kbd>Space</kbd>, <kbd>Enter</kbd> or right mouse button to open it. In the 
context menu, you can:
- write an expression to eval on selection by typing it and pressing <kbd>Enter</kbd>;
- select a built-in action using arrow keys, and execute it by pressing <kbd>Enter</kbd>
  or left mouse button.

Reveal displays action results in the results panel. Use <kbd>Ctrl ←</kbd> / <kbd>Ctrl →</kbd>
to switch between different tabs in the results panel. Use <kbd>Tab</kbd> to switch focus
between main output panel and results panel. Press <kbd>Tab</kbd> to switch focus between
output and results panels.

## Installation and requirements

Add a dev dependency on the latest version:

[![Reveal on Clojars](https://clojars.org/vlaaad/reveal/latest-version.svg)](https://clojars.org/vlaaad/reveal)

The minimum required Clojure version is `1.10`.

If you are using nrepl, add `vlaaad.reveal.nrepl/middleware` to the middleware list. 
The minimum nrepl version is `0.6.0`.

If you are using socket repl, run main in `vlaaad.reveal` namespace with `repl` argument.
Starting Reveal repl from the repl is done similarly: by invoking `(vlaaad.reveal/repl)`. 

If you don't want to use it as a repl, calling `(vlaaad.reveal/ui)` will open 
a window and return a function: call it with 1 argument to submit a value, call it 
with 0 arguments to close the window and dispose it.

## Try it out

#### As a repl in a shell

```sh
$ clj -Sdeps '{:deps {vlaaad/reveal {:mvn/version "0.1.0-ea26"}}}' -m vlaaad.reveal repl
```

#### As a client repl in a shell with prepl backend

Example of a remote prepl to connect to (doesn't need Reveal on the classpath):
```sh
$ clj -J-Dclojure.server.prepl="{:port 5555 :accept clojure.core.server/io-prepl}"
```

Connect to it with Reveal:
```sh
$ clj -Sdeps '{:deps {vlaaad/reveal {:mvn/version "0.1.0-ea29"}}}' \
-m vlaaad.reveal remote-prepl :port 5555
```

#### As a nrepl middleware in a shell

```sh
$ clj \
-Sdeps '{:deps {vlaaad/reveal {:mvn/version "0.1.0-ea29"} nrepl/nrepl {:mvn/version "0.7.0"}}}' \
-m nrepl.cmdline --middleware '[vlaaad.reveal.nrepl/middleware]'
```

#### As a `tap>` target

If you don't want to use Reveal as a repl, but only want to send to it values for 
inspection, you can add a dependency on Reveal and then evaluate this:
```clj
(add-tap ((requiring-resolve 'vlaaad.reveal/ui)))
```

It will open a window that will receive all `tap>`-ed values while the JVM process is 
alive.

## UI preferences

You can configure reveal looks with `vlaaad.reveal.prefs` java property that should be
an edn map. Supported keys (all optional):
- `:theme` - `:light` or `:dark`;
- `:font-family` - system font name (like `"Consolas"`) or url (like
  `"https://ff.static.1001fonts.net/u/b/ubuntu.mono.ttf"` or `"file:/path/to/font.ttf""`),
  reveal only supports monospaced fonts;
- `:font-size` - number.

Example:
```sh
$ clj -Sdeps '{:deps {vlaaad/reveal {:mvn/version "0.1.0-ea29"}}}' \
-J-Dvlaaad.reveal.prefs='{:font-family "Consolas" :font-size 15}' \
-m vlaaad.reveal repl
```

## To do
- structural navigation
- text search:
  - should be reversible: escape scrolls to the place of search start if there is
    a highlight
  - could show number of matches and index of current match
- contextual eval:
  - alt+up/down for history (persist history? per what?)
  - auto-insert closing brackets
  - pick some useful ns to eval in
- fork out/err in repl/nrepl as well:
  - for full experience we should fork `System/out` and `System/err`, and re-bind roots of `*out*` and `*err*` — is it 
    a good idea?
- option to dispose javafx on quitting the repl - useful for entry points
- multiple accordions
- more actions:
  - view files ending with `.html` as web pages
- remember window position and size
- popup might appear in weird locations
- *very* long lines have poor performance
