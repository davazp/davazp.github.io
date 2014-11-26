---
title: How to write a modern Lisp library with ASDF3 and Package Inferred System
tags: [lisp]
layout: post
---

If you are a novice Lisp programmer and you are confused about the
difference between *systems* and *packages*, I recommend you to read
the [Packages, systems, modules, libraries - WTF?] [1] before you
continue.

### Introduction

The most common way that people use packages nowadays is by adding a
`packages.lisp` or `package.lisp` file. This file is usually the first
file to be loaded and define the packages that the other files
use. This approach has worked for many projects. However, when
packages become larger, it requires discipline to manage the
dependencies between the files.

An alternative approach to the definition of packages is named *one
package per file*. As the name suggests, it consists in starting every
file with a [defpackage](http://clhs.lisp.se/Body/m_defpkg.htm). Because
dependencies between packages are explicit and every file has a unique
package, the dependencies between the files can be inferred.

This style of programming was introduced a few years ago by *fastpath*
and *quick-build*. But recently, the *de facto* standard Common Lisp
build system [ASDF3](http://common-lisp.net/project/asdf/) added
support for it with the *asdf-package-system* extension. As a
consequence, now it is easier than ever to use it.

So, stay with me for the next minutes and you will learn how to use
this new approach in your projects. I hope you find it useful.


### How to use it


First of all, we have to enable the *asdf-package-system* extension on
our system. We will work on a system named `project` as definition
follows

{% highlight common-lisp hl_lines="4 5" %}
(asdf:defsystem :project
  :name "My Project"
  :version "0.0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:project/addons))
{% endhighlight %}

This defines a correspondence between systems, packages and files. A
system `project/foo/bar` refers the file `foo/bar.lisp`, which have to
provide a package `project/foo/bar`, and whose *used* and *imported*
packages refer to systems of the same name.

For example, as the system `project` depends on `project/addons`, the
file `addons.lisp` must be loaded first. The content of `addons.lisp`
starts with:

{% highlight common-lisp hl_lines="2" %}

(defpackage :project/addons
  (:use :common-lisp :project/core)
  (:import-from :cl-ppcre))

(in-package :project/addons)

(cl-ppcre:scan-to-strings "h.*o" (hello))

{% endhighlight %}

Note that it uses the package `project/core` and "import" the
`cl-ppcre` one. Therefore, ASDF will infer that it depends on both the
*systems* [cl-ppcre](http://weitz.de/cl-ppcre/) and `project/core`, so
they must be loaded first. But remember, the system `project/core`
refers to the file `core.lisp`:

{% highlight common-lisp hl_lines="3" %}
(defpackage :project/core
  (:use :common-lisp)
  (:export #:hello))

(in-package :project/core)

(defun hello ()
  "Hello!")
{% endhighlight %}


And that is all. This file has no external dependencies. Then, if we
try to load our system

```common-lisp
(asdf:load-system "project")
```

the systems and files `core.lisp`, `cl-ppcre` and `addons.lisp` will
be loaded in the proper order.


### Integration with other systems

What if we use a package but the system that provides such a package
has not the same (downcased) name? For example, the system
[closer-mop](http://common-lisp.net/project/closer/closer-mop.html)
provides a package named *c2cl*. In order to let ASDF know how to find
the system for a package, we can call the function
`register-system-packages` with the name of the systems and the
packages it provides as argument. In our case, we would include the
following in our `project.asd`:

```common-lisp
(register-system-packages :closer-mop '(:c2cl))
```

### A Last Trick

Most of the time we want to *export* a single package with all the
symbols from the "subpackages". This can be done very easily if you
use the *UIOP* library shipped with ASDF. For example, let us define a
file named `all.lisp` in our example like:

{% highlight common-lisp %}

(uiop/package:define-package :project/all
  (:nickname :project)
  (:use-reexport :project/core :project/addons))

{% endhighlight %}

Now, the system `project/all` depends on both `project/core` and
`project/addons`, and it will reexport the symbols into a `:project`
package.

### Futher information

If you want to know more about ASDF3 and package-system:

- [ASDF Manual. The package-inferred-system extension](http://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html#The-package_002dinferred_002dsystem-extension).

- ASDF 3, or Why Lisp is Now an Acceptable Scripting Language  
  (Extended version) by François-René Rideau.  
  <http://fare.tunes.org/files/asdf3/asdf3-2014.html>



[1]: http://weitz.de/packages.html "Packages, systems, modules, libraries - WTF?"
