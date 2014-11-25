---
title: How to write a modern Lisp library with ASDF3 and Package System
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
use. The package can *use* other packages, and *export* some symbols
for the packages than *use* it. This approach has worked for many
projects. However, when packages become larger, it requires discipline
to manage the dependencies between the files.

An alternative approach to the definition of packages is named *one
package per file*. As the name suggests, it consists in starting every
file with a [defpackage](http://clhs.lisp.se/Body/m_defpkg.htm). Because
dependencies between packages are explicit and every file has a unique
package, the dependencies between the files can be inferred.

This style of programming was introduced a few years ago by *fastpath*
and *quick-build*. But recently, the *de facto* standard Common Lisp
build system [ASDF3](http://common-lisp.net/project/asdf/) added
support for it the *asdf-package-system* extension. As a consequence,
now it is easier than ever to use it.

So, stay with me for the next minutes and you will learn how to use
this new approach in your projects. I hope you find it useful.

### How to use asdf-package-system

The first thing we need to do is to define our system. It is not much
different from defining an ordinary system with ASDF.

Assuming we are in a new empty directory, we create a `project.asd`
file. The content of this file is:

{% highlight common-lisp hl_lines="3 4" %}
(asdf:defsystem :project
  :name "My Project"
  :version "0.0.1"
  :class :package-inferred-system
  :defsystem-depends-on (:asdf-package-system)
  :depends-on (:project/core :project/addons))
{% endhighlight %}

As I mentioned in the introduction, this feature is provided by the
*asdf-package-system* ASDF's extension. The highlighed lines specify
that we want to use it for this package. Note that, unlike ordinary
ASDF system, it does not specify components of the system. It seems
there are not files this system is comprised of. But, it is not the
case. It is the first of the inferences that ASDF does:

> Systems prefixed with `project/` refer to files from the directory
> where the system is defined.

In the last line of our `project.asd`, note that our system depends on
both `project/core` and `project/addon`. These two systems refer to
two files named `core.lisp` and `addon.lisp` respectively.

Let us create a basic `core.lisp` file:

{% highlight common-lisp hl_lines="3" %}
(defpackage :project/core
  (:use :common-lisp)
  (:export #:hello))

(in-package :project/core)

(defun hello ()
  "Hello!")
{% endhighlight %}


Now, let us write the file `addons.lisp` to use the function exported
from `project/core`:

{% highlight common-lisp hl_lines="2" %}

(defpackage :project/addons
  (:use :common-lisp :project/core)
  (:import-from :cl-ppcre))

(in-package :project/addons)

(cl-ppcre:scan-to-strings "h.*o" (hello))

{% endhighlight %}

Note that `addons.lisp` use the `project/core` package. And now it
comes the other inference that ASDF does:

> The dependencies in the `defpackage` refer to systems with the same
> name (downcased).

It is to say, `project/addons` depends on `project/core`.

Let us follow the inferences: to load the system `project/addon`, the
file `addon.lisp` will be loaded. Because its package uses
`project/core`, we need to load that system before. But because
`project` system uses *asdf-package-system*, the system `project/core`
refers to the `core.lisp` file.

Indeed, note that a package does not need to use only `project/*`
packages. Indeed, if you look to our `addons.lisp`. It depends on the
`cl-ppcre` package. Therefore, it depends on the system named
"cl-ppcre". We did not import any symbols, so functions from the
package must be referenced by prefixing the symbol with the package.

We can now load the files with the correct order with just

{% highlight common-lisp %}
(asdf:load-system :project)
{% endhighlight %}

It will load [cl-ppcre](http://weitz.de/cl-ppcre/), then `core.lisp`
and finally `addons.lisp`. The dependencies are explicit, every file
can specify what system it uses. You do not need to specify
`:depends-on (:cl-ppcre)` on the `project.asd` file.


### A last trick

Most of the time, you do not want to "export" two or more packages,
but just one with all the symbols from the "subpackages". This can be
done very easily if you use the *UIOP* library shipped with ASDF.

Let us define a file named `all.lisp` in our example like:

{% highlight common-lisp %}

(uiop/package:define-package :project/all
  (:nickname :project)
  (:use-reexport :project/core :project/addons))

{% endhighlight %}

Now, the system `project/all` depends on both `project/core` and
`project/addons`, and it will reexport the symbols into a `:project`
package.

### Futher information

If you want to see an example, have a look to the library I am working
these days:

<https://github.com/davazp/cl-docker>

Also, both ASDF3 and
[lisp-interface-library](https://github.com/fare/lisp-interface-library/)
use this this style of programming.

If you want to know more about ASDF3 and package-system:

- [ASDF Manual. The package-inferred-system extension](http://common-lisp.net/project/asdf/asdf/The-package_002dinferred_002dsystem-extension.html#The-package_002dinferred_002dsystem-extension).

- ASDF 3, or Why Lisp is Now an Acceptable Scripting Language  
  (Extended version) by François-René Rideau.  
  <http://fare.tunes.org/files/asdf3/asdf3-2014.html>



[1]: http://weitz.de/packages.html "Packages, systems, modules, libraries - WTF?"
