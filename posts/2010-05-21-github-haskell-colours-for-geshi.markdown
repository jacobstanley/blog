---
title: GitHub Haskell colours for GeSHi
author: Jacob Stanley
date: May 21, 2010
tags: geshi, github, haskell, syntax highlighting
---

*This article is out of date, I don't use Wordpress or GeSHi anymore for
my blog*

[GeSHi](http://qbnz.com/highlighter/) is the php-based syntax
highlighter that I use for this blog. The Haskell highlighting included
with GeSHi is a bit below par compared to say
[GitHub](http://github.com/jystic/lambdasim/blob/master/Main.hs), which
uses [pygments](http://pygments.org/) for highlighting.

Not to be outdone, I though I'd delve in to GeSHi's regular expression
features to see if I could improve upon the default. I struggled a bit
with things getting highlighted that had already been processed and
matched to an alternate regular expression until I dug in to the GeSHi
source to and found out how it escapes potential highlights. This led to
some lovely regex like the one below as I attempted to work around
things like `<PIPE>`, `<SEMI>` and other magical GeSHi escapes
like `<|!REG3XP1!>`.

~~~{.perl}
/<[A-Z]+>|<\|!REG3XP\d*!>.*?\|>/
~~~

One I figured this out things got a lot easier and I was able to get to
a point that I'm pretty happy with. The symbol highlighting is still a
bit hacky and I'd like to get to a point where all non-alphanumeric
character groups are highlighted as symbols. For now though I've just
added a few more (`<$>`, `<|>`, etc) to the hardcoded list that
comes with GeSHi out of the box. So without any further ado, here are
the results.

Before:

![](/img/geshi-haskell-original.png "Original Colours")

After:

![](/img/geshi-haskell-github.png "GitHub Colours")

You can download my modifications to the Haskell syntax file for GeSHi
[here](/files/haskell.php). If I get some more time I'll probably submit
it back to the GeSHi project as a replacement or an alternative for
Haskell.

One thing to note is that I've stripped out the url linking of Prelude
functions. I felt that it wasn't really that useful as I rarely want to
know the details of those functions, it's all the other ones that I want
to know about! I toyed with the idea of linking functions and types to
[Hoogle](http://www.haskell.org/hoogle/) or
[Hayoo!](http://holumbus.fh-wedel.de/hayoo/hayoo.html) that might be
worthwhile in the future.

**Download [haskell.php](/files/haskell.php)**

*The syntax file above was built and tested on GeSHi 1.0.8.7, it will
probably work on other versions, but you never know :)*
