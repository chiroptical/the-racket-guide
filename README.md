'The Racket Guide' and other Racket learnings
===

I am working my way through [The Racket Guide][racket-guide]. The goal is to
learn more about contracts and metaprogramming. The `chapter*.rkt` files describe
which chapters the files cover. Additionally, as I discover more libraries
I'll likely add `library-*.rkt` to go over them.

Notes
---

- Using `LOOKUP:` to define things which would be worth looking up in the
  future.

Editor and snippets
---

To get the editor and formatting in `./.helix/languages.toml` working,

```shell
$ raco pkg install racket-langserver
$ raco pkg install fmt
```

Libraries
---

## Qi - An Embeddable Flow-Oriented Language

[The website][qi], installation, see [this file][./library-qi.rkt] for examples.

```shell
$ raco pkg install qi
```

[racket-guide]: https://docs.racket-lang.org/guide/index.html
[qi]: https://docs.racket-lang.org/qi/index.html
