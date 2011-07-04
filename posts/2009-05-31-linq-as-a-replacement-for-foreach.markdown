---
author: Jacob Stanley
title: Linq as a replacement for foreach
date: May 31, 2009
tags: c#, linq
---

The area of my code that has changed the most with the introduction of
[linq](http://en.wikipedia.org/wiki/Language_Integrated_Query) and
[extension methods](http://en.wikipedia.org/wiki/Extension_method) has
to be the way that I treat standard collections. The number of times I
used to write some code like this:

~~~{.cs}
IEnumerable<Fruit> fruits = GetAllTheFruit();
List<Fruit> freshOnes = new List<Fruit>();

foreach (Fruit fruit in fruits)
{
    if (fruit.IsFresh)
    {
        freshOnes.Add(fruit);
    }
}

MakeFruitSalad(freshOnes);
~~~

What a waste of time! The time it took to write all that boiler plate
code, as well as the time for someone else to read and understand all
that boiler plate code :( All we wanted was to get the fresh fruit, why
is that so hard? Well, actually, with linq it's pretty easy:

~~~{.cs}
IEnumerable<Fruit> fruits = GetAllTheFruit();
IEnumerable<Fruit> freshOnes = fruits.Where(x => x.IsFresh);

MakeFruitSalad(freshOnes);
~~~

How about sorting?  I used to find myself doing something like this:

~~~{.cs}
List<Fruit> sortedFruits = new List<Fruit>(GetAllTheFruit());
sortedFruits.Sort(new FreshnessComparer());

class FreshnessComparer : IComparer&lt;Fruit&gt;
{
    public int Compare(Fruit x, Fruit y)
    {
        return x.Freshness.CompareTo(y.Freshness);
    }
}
~~~

That's great, but do I really need to make a new class/method just to
sort by freshness? Not if I use linq:

~~~{.cs}
IEnumerable<Fruit> sortedFruits =
    GetAllTheFruit().OrderBy(x => x.Freshness);
~~~

What if I just want the single freshest fruit? I could do this:

~~~{.cs}
Fruit freshest = null;

foreach (Fruit fruit in sortedFruits)
{
    freshest = fruit;
    break;
}

// got the freshest, or null if there was no fruit in 'sortedFruits'
~~~

Or I can just use linq:

~~~{.cs}
Fruit freshest = sortedFruits.FirstOrDefault();
~~~

Great, so it's nicer to use from the consumer's point of view, but what
about the provider's? This is another area where I really like linq.
Linq works on `IEnumerable<T>`, a really simple interface to implement
when compared to the likes of `IList<T>` or `ICollection<T>`. It really
allows our implementations to choose the best data structure for the
job, instead of having to return an `IList<T>` or `ICollection<T>` just
because they'll be more convieniant for the consumer.

Linq is proof enough for me that object oriented and functional
programming paradigms can complement each other nicely. Not having
access to functional stuff like linq is one of my major pain points
anytime I have to use Java these days.
