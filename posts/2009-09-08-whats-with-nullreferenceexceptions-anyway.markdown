---
title: What's with NullReferenceExceptions anyway?
author: Jacob Stanley
date: September 8, 2009
tags: c#, functional, linq, maybe monad, monads
---

Tony Hoare recently gave a talk at QCon about what he calls his [billion
dollar mistake](http://qconlondon.com/london-2009/presentation/Null+References:+The+Billion+Dollar+Mistake),
null references. It certainly rings true for me - not knowing whether a
value can be null or not has been quite a major source of frustration
for me. Conversely, checking for null when a method can never or should
never return null is horrible as well, it bloats the code and distracts
from the intent of the method.

ReSharper has some useful attributes that can be used to mitigate this
somewhat. `NotNullAttribute` and `CanBeNullAttribute` give the ReSharper
engine some hints that it can use to warn developers about potential
mistakes, but at the end of the day you still end up with a boilerplate
mess of if statements:

~~~{.cs}
Apple apple = PickApple();

if (apple == null)
{
    return null;
}

Orange orange = PickOrange();

if (orange == null)
{
    return null;
}

FruitSalad fruitSalad = MakeFruitSalad(apple, orange);

if (fruitSalad == null)
{
    return null;
}

return PackageForSale(fruitSalad);
~~~

Or perhaps even worse, the lovely arrow-head pattern. Reaching for the
far edge of the screen with every possible null:

~~~{.cs}
Apple apple = PickApple();

if (apple != null)
{
    Orange orange = PickOrange();

    if (orange != null)
    {
        FruitSalad fruitSalad = MakeFruitSalad(apple, orange);

        if (fruitSalad != null)
        {
            return PackageForSale(fruitSalad);
        }
    }
}
~~~

A solution to this problem that I've been using recently is straight out
of the Haskell playbook, the Maybe monad. This simple tool wraps up the
concept of uncertainty so that it is composable. This allows you to
rewrite the above code like this using LINQ:

~~~{.cs}
return from apple in PickApple()
       from orange in PickOrange()
       from fruitSalad in MakeFruitSalad(apple, orange)
       select PackageForSale(fruitSalad);
~~~

Sweet! So it's just like an `IEnumerable<T>` that is either empty, or
contains a single value. So how does it work? Lets look at the type
signature for `PickApple`:

~~~{.cs}
Maybe<Apple> PickApple()
~~~

Instead of returning an `Apple` directly, we return `Maybe<Apple>` (I
like to pronounce this "maybe an apple"). This gives us two advantages.
Firstly, it captures in the type system that we might not return a
value. Secondly, it allows us to define a raft of common functions for
working with `Maybe<T>` values.

So what does `Maybe<T>` look like? Here are the basics:

~~~{.cs}
public struct Maybe<T>
{
    private readonly bool m_HasValue;
    private readonly T m_Value;

    public Maybe(T value)
    {
        m_Value = value;
        m_HasValue = value != null;
    }

    public T Value
    {
        get
        {
            return m_Value;
        }
    }

    public bool HasValue
    {
        get
        {
            return m_HasValue;
        }
    }

    public bool HasNothing
    {
        get
        {
            return !m_HasValue;
        }
    }

    public static implicit operator Maybe<T>(T value)
    {
        return new Maybe<T>(value);
    }

    public static Maybe<T> Nothing
    {
        get
        {
            return default(Maybe<T>);
        }
    }
}
~~~

It's important to note that `Maybe<T>` is a struct. If it was a class then
`Maybe` could be null, nothing or contain a value. So you'd need to check
for three possible cases and we wouldn't have gained much at all!

So far this is looking pretty similar to
[Nullable<T>](http://msdn.microsoft.com/en-us/library/b3h38hb0.aspx)
from the BCL. Unfortunately we can't use `Nullable<T>` because it
constrains `T` to structs only. This does bother me too much, I think
`Maybe<T>` is a better name anyway.

So how do we do the funky LINQ stuff? We have to define what's called
the bind operator in  functional languages. It describes how we can
chain one maybe on to the next, and what to do if we encounter a maybe
which has nothing in it. Unfortunately, because LINQ was designed to
have a familiar SQL feel to it, this bind operation needs to be an
extension method called `SelectMany`. This enables C# and Visual Basic
to make use of it with the LINQ syntax.

So this is what "bind" looks like for our Maybe monad:

~~~{.cs}
public static Maybe<B> SelectMany<A, B>(this Maybe<A> maybeA, Func<A, Maybe<B>> aToMaybeB)
{
    if (maybeA.HasNothing)
    {
        return Maybe<B>.Nothing;
    }

    return aToMaybeB(maybeA.Value);
}
~~~

It's nothing special, it takes maybe an `A` and a function which goes
from an `A` to maybe a `B.` The important thing here is that if A
doesn't have a value then the function `aToMaybeB` isn't called. We're
still missing a small piece of the puzzle, but we're able to write our
original function as this:

~~~{.cs}
return PickApple().SelectMany(apple =>
       PickOrange().SelectMany(orange =>
       MakeFruitSalad(apple, orange).SelectMany(fruitSalad =>
       PackageForSale(fruitSalad))));
~~~

The query syntax does work yet because LINQ requires one extra overload
of `SelectMany.` This overload will describe how to chain two maybes
together to yield a third. It's purely an optimisation, so we're not
going to bother taking advantage of it in this case. We'll just
implement it in terms of our singular `SelectMany:`

~~~{.cs}
public static Maybe<C> SelectMany<A, B, C>(this Maybe<A> maybeA, Func<A, Maybe<B>> aToMaybeB, Func<A, B, Maybe<C>> abToMaybeC)
{
    return maybeA.SelectMany(a => aToMaybeB(a).SelectMany(b => abToMaybeC(a, b)));
}
~~~

Ouch! That's one scary looking method! You don't need to worry too much
about this guy. Unless you're planning to do optimisations it's exactly
the same for every single monad. Having said that, you should see
similarities between it and our usage of `SelectMany` in the previous
example.

Once we have our "double bind" operation defined, the linq syntax should
work as described earlier:

~~~{.cs}
return from apple in PickApple()
       from orange in PickOrange()
       from fruitSalad in MakeFruitSalad(apple, orange)
       select PackageForSale(fruitSalad);
~~~

Now that you have the basics you can add all sorts of extra functions
for dealing with `Maybe<T>.` If I'm feeling keen I might post a follow
up article on the additional functions that I've found useful. `OrNull`
is a handy one for "unboxing" a `Maybe<T>` back to a naked value and
`OrThrow` is another useful one which gets the inner value but throws if
the `Maybe<T>` contains nothing.

So now you can say goodbye to `if (a != null)` right? Well, not quite,
naked values always have the possibility of being null. It's just an
uncertainty that we're going to have to deal with for the time being.
However, if you always use `Maybe<T>` when a method might return
"nothing" then you can fairly safely avoid checking for null when you're
working with values returned from your own code.

Hopefully we'll be able to lock down naked values completely when we get
code contracts in .NET 4.0. I'd love to see something which
automatically annotates all arguments and return values with a contract
ensuring that they can never be null.

A world without null, luxury!
