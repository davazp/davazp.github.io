---
title: PostgreSQL from Lisp
tags: [lisp]
layout: post
---

Hello everybody,

It has been a long time before my last post. Many things happened: I
had a great time in [ECL2013](http://weitz.de/eclm2013/), I
[wrote a code generator for JSCL](https://github.com/davazp/jscl/blob/master/src/compiler/codegen.lisp),
exams in the university... and suffering the heat at home in Spain, of
course.

These days, at work, I am migrating our old SQL Server to PostgreSQL.
We used to use CLSQL to access to the server from Lisp-side, but in a
very straightforward way. We built everything on `query`. Although
CLSQL is a big monster, it misses parametric queries, and you have to
escape arguments yourself. Indeed, it was not as much stable as I
would like. But let apart exotic features.

Therefore I gave a try to other libraries. The obvious choice would be
[Postmodern](http://marijnhaverbeke.nl/postmodern/), but it failed
soon. DAO capabilities seem incomplete and, I think, they should be
other library, but I can live ignoring it. However, have a look to
[query](http://marijnhaverbeke.nl/postmodern/postmodern.html#query):
it has a strange arg/format mix and it is a macro! It means you will
fail to build simple abstractions on it.

Fortunately, Postmodern's author created a lower library
[cl-postgres](http://marijnhaverbeke.nl/postmodern/cl-postgres.html),
which I can suit easily:

```common-lisp
(defun query (query &rest args)
  (cl-postgres:prepare-query *database* "" query)
  (cl-postgres:exec-prepared *database* "" args 'cl-postgres:list-row-reader))
```

but of course I will have to write connection pooling and other
features myself, or perhaps mix postmodern and cl-postgres carefully.

In conclusion, I think we should be a little bit more *conventional*
in the abstractions we build, and not to forget intermediate
abstractions.
