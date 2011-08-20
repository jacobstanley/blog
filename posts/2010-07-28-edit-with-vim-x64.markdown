---
title: 'Edit with Vim' on x64
author: Jacob Stanley
date: July 28, 2010
tags: vim, x64
---

I've been using the [64-bit version of
Vim](http://code.google.com/p/vim-win3264/) so that I could get the
'Edit with Vim' context menu in Windows Explorer. This was all fine and
good except that I wanted to use something that requires vim to be
compiled with +ruby. The vanilla x86 gVim72 installer you can get at
[vim.org](http://www.vim.org/download.php) is compiled with +ruby, but
it doesn't have the explorer context menu on a 64-bit computer :(

The solution was to install the x86 vim from vim.org, then take the
gvimext.dll from the 64-bit vim, and apply the following registry
tweaks.

~~~{.sourceCode}
Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\SOFTWARE\Vim\Gvim]
"path"="C:\\Program Files (x86)\\Vim\\vim72\\gvim.exe"

[HKEY_CLASSES_ROOT\CLSID\{51EEE242-AD87-11d3-9C1E-0090278BBD99}]
@="Vim Shell Extension"

[HKEY_CLASSES_ROOT\CLSID\{51EEE242-AD87-11d3-9C1E-0090278BBD99}\InProcServer32]
@="C:\\Program Files (x86)\\Vim\\vim72\\gvimext64.dll"
"ThreadingModel"="Apartment"
~~~

To save anyone else the hassle of getting the bits you need from the
64-bit vim, I've zipped up everything you need
([gvimext64.zip](/files/gvimext64.zip)). Just extract it to `C:\Program
Files (x86)\Vim\vim72` and run the `gvimext64.reg` file.
