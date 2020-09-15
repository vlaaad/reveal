# ![logo](src/vlaaad/reveal/logo-32.png) Reveal: Read Eval Visualize Loop for Clojure

![demo](https://vlaaad.github.io/assets/reveal/demo.gif)

[![Clojars Project](https://img.shields.io/clojars/v/vlaaad/reveal.svg?logo=clojure&logoColor=white&style=for-the-badge)](https://clojars.org/vlaaad/reveal)
[![Slack Channel](https://img.shields.io/badge/slack-clojurians%20%23reveal-blue.svg?logo=slack&style=for-the-badge)](https://clojurians.slack.com/messages/reveal/)

## Rationale

Repl is a great window into a running program, but the textual nature of its 
output limits developer's ability to inspect the program: a text is not an 
object, and we are dealing with objects in the VM.

Reveal aims to solve this problem by creating an in-process repl output pane 
that makes inspecting values as easy as selecting an interesting datum. It 
recognizes the value of text as a universal interface, that's why its output 
looks like a text: you can select it, copy it, save it into a file. Unlike text,
reveal output holds references to printed values, making inspecting selected 
value a matter of opening a context menu.

Unlike datafy/nav based tools, Reveal does not enforce a particular data 
representation for any given object, making it an open set — that includes 
datafy/nav as one of the available options. It does not use datafy/nav by 
default because in the absence of inter-process communication to datafy is 
to lose.

Not being limited to text, Reveal uses judicious syntax highlighting to aid 
in differentiating various objects: text `java.lang.Integer` looks differently 
depending on whether it was produced from a symbol or a class.

## Documentation

You can find overview, setup instructions and more at 
[vlaaad.github.io/reveal](https://vlaaad.github.io/reveal/).

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
- bigger font size may break result header scrolling
