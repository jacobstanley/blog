---
title: Hello World!
author: Jacob Stanley
date: May 20, 2009
tags: haskell
---

So, here we are again, attempt number three at maintaining a blog. Let's
see if I can keep it up this time :)

I'll be posting at least once a week, hopefully following Ayende's
method of posting about things I'm currently working on. I've moved my
web hosting from a server in the US to an Australian server, and that's
sped up my response times to the site considerably. It makes a big
difference to motivation when you're not sitting around waiting for most
of the time. Maybe I'm just impatient :P

Given this is a hello world post and I'm learning Haskell at the moment,
I thought I'd try and craft one line of Haskell that does something
interesting to come up with the classic `"Hello World!"`. Although,
perhaps I should have tried to write a blog engine in one line of
Haskell, it sure is terse enough :P

Here's what I came up with:

~~~{.haskell .numberLines}
foldr (\x y -> x ++ " " ++ y) "!"
  (map (\(x:xs) -> toUpper x : map toLower xs)
       ["HELLO", "WORLD"])
~~~

What does it do?

It creates a list containing the strings `"HELLO"` and `"WORLD"`

It maps a lambda on to the list so that the first character of each
string in the list is uppercase and the rest of the characters are
lowercase (giving us `"Hello"`, `"World"`)

It does a fold right on the resulting list, with a lambda that
concatenates each item in the list using a space as a separator and an
exclamation mark as the intial seed

The result?

~~~{.haskell}
"Hello World !"
~~~

Ideally I'd have liked to not have the space in between `"World"` and
`"!"`, but my haskell-fu isn't good enough yet :)
