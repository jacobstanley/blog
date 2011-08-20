---
title: Installing Gtk2Hs 0.11 on Windows
author: Jacob Stanley
date: October 20, 2010
tags: cabal, gtk, gtk2hs, haskell, windows
---

This is a quick to note to myself about what I had to do to install
[Gtk2Hs](http://haskell.org/gtk2hs/) 0.11 on Windows, but others might
find it useful as well.

## Installing GTK+

Download the GTK+ all-in-one bundle from
[here](http://www.gtk.org/download-windows.html). I used 2.16 because I
had some problems with libpng [[1]](#libpng) and zlib [[2]](#zlib) when
I tried 2.22.

Extract it somewhere without spaces in the name, and add the `\bin`
directory to your PATH. I extracted the bundle to `c:\gtk-2.16` so I
had to add `c:\gtk-2.16\bin` to my PATH.

You can check you've done this right by running `pkg-config` from the
command line. You should get back something similar to this:

~~~{.sourceCode}
> pkg-config --cflags gtk+-2.0
-mms-bitfields -Ic:/gtk-2.16/include/gtk-2.0 -Ic:/gtk-2.16/lib...
~~~

## Installing Gtk2Hs

Add the Haskell Platform's `\mingw\bin` directory to your `PATH`. This
was `C:\Program Files\Haskell Platform\2010.2.0.0\mingw\bin` in my case.
I found this was necessary because the Gtk2Hs install requires
`cpp.exe`, the C preprocessor.

Once you have all the prerequisites set up, installation is simply a
`cabal install` away.

~~~{.sourceCode}
> cabal update
> cabal install gtk2hs-buildtools
> cabal install gtk
~~~

## Testing it out

You can try out your newly installed Gtk2Hs with the Hello World program
from the
[documentation](http://www.haskell.org/gtk2hs/documentation/#hello_world).

~~~{.haskell}
import Graphics.UI.Gtk

main :: IO ()
main = do
    initGUI
    window <- windowNew
    button <- buttonNew
    set window [ containerBorderWidth := 10,
                 containerChild := button ]
    set button [ buttonLabel := "Hello World" ]
    onClicked button (putStrLn "Hello World")
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI
~~~

I was able to run the code above using `runghc` without any issues.

~~~{.sourceCode}
> runghc Hello.hs
~~~

![](/img/gtk2hs-hello-world.png "Hello World with Gtk2Hs")

This gives us a window which is using the vanilla GTK+ theme. This looks
a bit out of place on Windows and it's easy to fix. Just add a `gtkrc`
to your `\etc\gtk-2.0` directory (for me
`c:\gtk-2.16\etc\gtk-2.0\gtkrc`) with the following contents:

~~~{.sourceCode}
gtk-theme-name = "MS-Windows"
~~~

After adding a `gtkrc`, running our demo yields a much nicer looking
window.

![](/img/gtk2hs-hello-world-native.png "Native Hello World with Gtk2Hs")

## Various problems I encountered

<span id="libpng">1.</span> If you're trying to install using the GTK+
2.22 bundle and you encounter this error:

~~~{.sourceCode}
setup.exe: gtk-0.11.2: library-dirs: c:/devel/dist/win32/libpng-1.4.3-1/lib
doesn't exist or isn't a directory
~~~

It can easily be fixed by creating an empty directory in the location
that setup.exe is searching for:
`c:\devel\dist\win32\libpng-1.4.3-1\lib`

<span id="zlib">2.</span> Unfortunately, solving the libpng problem
didn't help me. I found that I was able to build and install the gtk
package using cabal. The problem was that any programs which I built
failed with an error about deflateSetHeader not being exported from
zlib1.dll when I tried to run them.
