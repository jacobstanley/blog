---
title: The Skyline Problem
author: Jacob Stanley
date: May 16, 2010
tags: code challenge, haskell
---

One of my friends recently had a job interview where he was asked to
complete [The Skyline
Problem](http://online-judge.uva.es/p/v1/105.html). It sounded
interesting so I thought I'd see if my Haskell skills were up to the
task.

<img src="/img/skyline-problem.gif" alt="The Skyline Problem" width="600" />

My first attempt took me a few hours to come up with. It's been a little
while since I've played with Haskell so a lot of time was spent fighting
with the type checker. Eventually though I produced a solution that
solved the problem. It works by exploding each building in to a set of
discrete points, where a building `(1,11,3)` becomes the coordinates
`[(1,11),(2,11),(3,11)]`. The points are then sorted and various filters
are run across the data until we get the answer.

~~~{.haskell}
import Data.List

main =
  putStrLn $
  unwords $ 
  concatMap (map show . list . head) $
  groupBy' snd $
  map last $
  groupBy' fst $
  sort $
  concatMap explode buildings

list (x,y) = [x,y]
groupBy' f = groupBy (\x y -> f x == f y)
explode (l,h,r) = [(x,h) | x <- [l..r-1]] ++ [(r,0)]

buildings = [(1,11,5),(2,6,7),(3,13,9),(12,7,16),
             (14,3,25),(19,18,22),(23,13,29),(24,4,28)]
~~~
 
This seemed like it wasn't the most efficient way to do things and I
wasn't reading my data from an input file yet. So, after a bit more
thinking (and some
[cheating](http://stackoverflow.com/questions/1066234/the-skyline-problem))
I got some ideas on how to implement the program coming from a different
angle. At some point I also wanted to try a golf solution (using as few
characters as possible) so I stuck to using only the `Prelude`.

The revised solution works by scanning each building for every
x-coordinate (1 - 9999) and calculating the height at that coordinate.
The output is built by showing the current coordinates every time the
maximum height changes. The thing I like about the revised solution is
that it is basically just restating the problem in Haskell. I've
included the type signatures below for completeness, but as always,
they're not required.

~~~{.haskell}
main :: IO ()
main = interact $ showSkyline . skyline . readBuildings

readBuildings :: String -> [[Int]]
readBuildings = map (map read . words) . lines

skyline :: [[Int]] -> [(Int,Int)]
skyline buildings = [(x, height x) | x <- [1..9999], isDiff x]
  where isDiff  x = height x /= height (x - 1)
        height    = maximum . heights
        heights x = 0 : [h | [s,h,e] <- buildings, s <= x, x < e]

showSkyline :: [(Int,Int)] -> String
showSkyline = unwords . (map showPoint)

showPoint :: (Int,Int) -> String
showPoint (x, y) = show x ++ " " ++ show y
~~~

Finally, I reduced the above solution to as few characters as possible
so I could write an answer for the [Stack
Overflow](http://stackoverflow.com/questions/1066234/the-skyline-problem)
code golf question.

It turned out to be 149 characters, which I was pretty happy with. I
could have shaved off four more by inlining 'b', but that made the
program take a lot longer to execute when using runghc for some reason.
I'm not sure if it would have made a difference if I compiled it.

~~~{.haskell}
main=interact f
f i=unwords[show x++" "++show(h x)|x<-[1..9999],h x/=h(x-1)] where
 h x=maximum$0:[y|[l,y,r]<-b,l<=x,x<r]
 b=map(map read.words)$lines i
~~~

If you discount reading the input or writing the output in a particular
format, then it can be trimmed down to 75 characters. The input, stored
in 'b', is the 'buildings' list from the first solution.

~~~{.haskell}
h x=maximum$0:[y|(l,y,r)<-b,l<=x,x<r]
print[(x,h x)|x<-[1..9999],h x/=h(x-1)]
~~~
