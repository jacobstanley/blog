---
title: Installing Heist (and Hexpat) on Windows
author: Jacob Stanley
date: October 7, 2010
tags: cabal, expat, hakell, heist, hexpat, snap, windows
---

***Edit:** I've been in touch with Stephen Blackheath, the maintainer of
[Hexpat](http://hackage.haskell.org/package/hexpat), and we've worked
out a simple solution to this problem that looks promising.  Hopefully
by the time you read this Hexpat will install seamlessly via cabal,
without the need for these instructions. I'll leave the instructions
below because it could be a week or two before all of this bubbles up to
Heist.*

So you've heard about [Snap](http://snapframework.com), that awesome new
Haskell web framework, and you want to give it a try. You jump in to
your Windows command prompt and cabal install snap-core and snap-server
without any hassles, but when you try to install
[Heist](http://hackage.haskell.org/package/heist) you get this:

~~~{.sourceCode}
> cabal install heist
Resolving dependencies...
Configuring hexpat-0.18.3...
cabal: Missing dependency on a foreign library:
* Missing C library: expat
This problem can usually be solved by installing the system package that
provides this library (you may need the "-dev" version). If the library is
already installed but in a non-standard location then you can use the flags
--extra-include-dirs= and --extra-lib-dirs= to specify where it is.
cabal: Error: some packages failed to install:
heist-0.2.3 depends on hexpat-0.18.3 which failed to install.
hexpat-0.18.3 failed during the configure step. The exception was:
ExitFailure 1
~~~

Hmm, Heist depends on
[Hexpat](http://hackage.haskell.org/package/hexpat), which has a
dependency on a C library, [Expat](http://expat.sourceforge.net). Here
you have two options, you can follow the instructions from the Hexpat
documentation if you don't mind hauling around a libexpat.dll with your
application, or you can follow the instructions below for static
linking.


First download [this zip file](/files/install-heist-mingw.zip). It
contains the Expat dependencies you'll need to install Hexpat, and a
batch file which will do all the right stuff for you.

~~~{.sourceCode}
bin\libexpat.dll
include\expat.h
include\expat_external.h
lib\libexpat.a
install-heist.bat
~~~

Extract it somewhere that you don't mind keeping around, GHC will need
these files every time it builds an executable that uses Hexpat/Heist.
Now you can run `install-heist.bat` and everything should go smoothly this
time:

~~~{.sourceCode}
> install-hexpat.bat
cabal install heist --extra-include-dirs=C:\hexpat\include --extra-lib-dirs=C:\hexpat\lib
Resolving dependencies...
Configuring hexpat-0.18.3...
Preprocessing library hexpat-0.18.3...
Building hexpat-0.18.3...

etc...
~~~

If you only need to run your Hexpat/Heist program from GHCi (or via
`runghc`) then you'll need to put `bin\libexpat.dll` in your `PATH` or
in the current directory. If you don't need GHCi then you can just
delete the bin folder, it's not used when building via GHC. When
building using GHC Expat is static linked to your app, so there's no
need for extra dlls.

If you want to move the directory that your Expat dependencies are in,
the easiest thing to do is to unregister heist and hexpat and follow the
instructions again.

~~~{.sourceCode}
> ghc-pkg unregister heist
> ghc-pkg unregister hexpat
~~~
