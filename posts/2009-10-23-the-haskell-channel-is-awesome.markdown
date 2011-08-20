---
title: #haskell is awesome!
author: Jacob Stanley
date: October 23, 2009
tags: cabal, happstack, haskell
---

I was trying to follow along with a
[few](http://softwaresimply.blogspot.com/2009/04/basic-happstack-blog-app.html)
[different](http://tutorial.happstack.com) happstack tutorials and I
found that I needed to cabal install a few things. Unfortunately when I
did `cabal install happs-tutorial` I got the following:

~~~{.sourceCode}
Happstack/Helpers/DirBrowse.hs:88:105:
    Couldn't match expected type `String' against inferred type `Bool'
    In the third argument of `hscolour', namely `False'
    In the second argument of `(.)', namely
        `(hscolour defaultColourPrefs False False f)'
    In the first argument of `($)', namely
        `BrowseHtmlString . (hscolour defaultColourPrefs False False f)'
cabal: Error: some packages failed to install:
happs-tutorial-0.9.0 depends on happstack-helpers-0.30 which failed to
install.
happstack-helpers-0.30 failed during the building phase. The exception was:
exit: ExitFailure 1
~~~

It turns out that happstack-helpers is listed as dependening on
`hscolour >= 1.0 && < 2.0` when in reality it fails to compile against
v1.15 of hscolour. I was thinking, damn, I'm going to have to manually
download the sources for happstack-helpers, edit the cabal file so it
has the right references and compile it manually. That's horribly
convoluted for something that must happen all the time, there must be a
better way!

I've been sitting on
[#haskell](http://www.haskell.org/haskellwiki/IRC_channel) recently so I
asked the question, and got some very speedy responses:

~~~{.sourceCode}
cabal install happs-tutorial --constraint="hscolor == 1.14"
~~~

That did the trick, and with a lot less effort!

I've asked quite a few questions on #haskell in the last few days and
the response has been almost immediate every time and I've had my
problem solved within minutes. Granted I'm still a Haskell newbie so my
questions are probably trivial, but even still, it puts to shame the
support I've received from a lot of commercial companies.
