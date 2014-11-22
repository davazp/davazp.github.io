---
title: Special arguments
tags: [lisp]
layout: post
---

I have learnt a new trick these days. Did you know that you can
declare an argument as a special variable? Quoting
[Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/03_daa.htm).

> Each required parameter is specified by a parameter variable var. var
> is bound as a lexical variable /unless it is declared special/.

It seems to work also for optional and keyword arguments, but
Hyperspec does not mention it explicitly. For example, yo can do:

```common-lisp
  (defvar *variable*)
  (defun foo () *variable*)
  (defun foobar (&optional *variable*)
    (foo))
```

I have learnt a new trick these days. Did you know that you can
declare an argument as a special variable? Quoting [Hyperspec](http://www.lispworks.com/documentation/HyperSpec/Body/03_daa.htm):

> Each required parameter is specified by a parameter variable var. var
> is bound as a lexical variable *unless it is declared special*.

It seems to work also for optional and keyword arguments, but
Hyperspec does not mention it explicitly. For example, yo can do:

```common-lisp
(defvar *variable*)
(defun foo () *variable*)
(defun foobar (&optional *variable*)
  (foo))
```
