---
title: Happstack win!
author: Jacob Stanley
date: October 21, 2009
tags: cabal, happstack, haskell
---

Hackage finally came back up, but I ran in to trouble with some guy
called `trhsx`. Every time I tried to `cabal install happstack` I got
the following error:

~~~{.sourceCode}
Configuring hsp-0.4.5...
Preprocessing library hsp-0.4.5...
Building hsp-0.4.5...
ghc: could not execute: trhsx
cabal: Error: some packages failed to install:
happstack-0.3.2 depends on hsp-0.4.5 which failed to install.
hsp-0.4.5 failed during the building phase. The exception was:
exit: ExitFailure 1
~~~

After some digging I found out that I was supposed to have
`~/.cabal/bin` in my path, who would have thought? Anyway, I added the
following to my `.bashrc`:

~~~{.bash}
PATH=$PATH:~/.cabal/bin
~~~

...and happstack installed without a hitch.

Win!
