---
author: Jacob Stanley
title: Design mode ViewModels with RhinoMocks
date: June 6, 2009
tags: c#, mvvm, wpf, xaml
---

I've been having quite a lot of success lately with WPF thanks to an
[awesome technique](http://weblogs.asp.net/thomaslebrun/archive/2009/05/04/wpf-mvvm-how-to-get-data-in-design-time.aspx)
that allows the injection of a dummy ViewModel at design time. This
allows you to tweak the view in Visual Studio or Blend and see changes
right away!

Unfortunately, having to add a property in so many places (the real
ViewModel, the dummy ViewModel and the interface) was becoming a bit of
a drag. So, I streamlined the process a bit by using RhinoMocks to
generate the dummy ViewModel as a stub of the ViewModel interface:

~~~{.cs}
public class SampleViewModel : ISampleViewModel
{
    // snip

    internal class DesignModeFactory : IDesignModeFactory<ISampleViewModel>
    {
        public ISampleViewModel CreateInstanceForDesignMode()
        {
            var viewModel = MockRepository.GenerateStub<ISampleViewModel>();

            viewModel.Stub(x => x.Title).Return("Design time title");
            viewModel.Text = "Design time text";

            return viewModel;
        }
    }
}

public interface ISampleViewModel
{
    string Title
    {
        get;
    }

    string Text
    {
        get;
        set;
    }
}
~~~

I really like this approach because it guarantees that everything I'm
binding to in design mode is available on the interface. This is
important to note, because WPF uses reflection for binding, so I would
have been allowed to bind to properties not on the interface and I
wouldn't realise until runtime.

So, all that's left now is to hook up the dummy ViewModel. It would
probably be best to do this with an attached property, but for me there
was a more pragmatic solution. All of my views already derive from a
specialised UserControl, which has a strongly typed ViewModel property
for use by IoC containers. Given this arrangement, it was more pratical
for me to add the dummy as part of the existing system.

~~~{.cs}
public class UserControlView<TViewModel> : UserControl
{
    public TViewModel ViewModel
    {
        get
        {
            return (TViewModel)DataContext;
        }
        set
        {
            DataContext = value;
        }
    }

    public UserControlView()
    {
        if (DesignerProperties.GetIsInDesignMode(this))
        {
            ViewModel = CreateDesignModeViewModel();
        }
    }

    private static TViewModel CreateDesignModeViewModel()
    {
        IDesignModeFactory<TViewModel> factory = ResolveFactory();

        if (factory == null)
        {
            return default(TViewModel);
        }

        return factory.CreateInstanceForDesignMode();
    }

    private static IDesignModeFactory<TViewModel> ResolveFactory()
    {
        return Resolve<IDesignModeFactory<TViewModel>>(typeof(TViewModel).Assembly);
    }

    private static T Resolve<T>(Assembly assembly)
    {
        return assembly
            .GetTypes()
            .Where(x => typeof(T).IsAssignableFrom(x))
            .Select(x => (T)Activator.CreateInstance(x))
            .FirstOrDefault();
    }
}

public interface IDesignModeFactory<T>
{
    T CreateInstanceForDesignMode();
}
~~~

As you can see in the code above, we're searching for the ViewModel's
DesignModeFactory. If we find one, we use it to create an instance of
our dummy ViewModel. In my case, because I was already deriving from
UserControlView, I don't have to do anything extra! But, in case you
haven't used generics with xaml before, here's the basic xaml for the
view, the important part is the x:TypeArguments property.

~~~{.xml}
<Sample:UserControlView
    x:TypeArguments="Sample:ISampleViewModel"
    x:Class="Sample.SampleView"
    xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
    xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
    xmlns:Sample="clr-namespace:Sample">

    <!-- snip -->

</Sample:UserControlView>
~~~

And there you have it, design mode support that's a touch more
[DRY](http://en.wikipedia.org/wiki/Don%27t_repeat_yourself) :)
