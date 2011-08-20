---
title: IP addresses and MAC addresses in Haskell
author: Jacob Stanley
date: August 12, 2010
tags: haskell, networking
---

When opening a socket, if you don't specify the address of the network
interface you want to listen on, you'll actually be listening on all of
them. In a secure environment, this isn't always what you want. That
said, it's quite useful to be able to show an administrator of your
system a drop down with the addresses of the various network interfaces
available to the software.

When I set out to do this in Haskell I discovered that there wasn't
anything available that would let me get the address information for my
network interfaces. So, I set out build
[network-info](http://hackage.haskell.org/package/network-info) and
learned how to use the Foreign Function Interface (FFI) in the process.

Getting the IPv4, IPv6 and MAC addresses of your local network
interfaces is now just a `cabal install` away:

~~~{.sourceCode}
cabal install network-info
~~~

Once you've got the library installed it's simple to get your network details:

~~~{.haskell}
import Network.Info

main = do
    ns <- getNetworkInterfaces
    mapM_ (putStr . showInterface) ns

showInterface n = name n ++ "\n"
               ++ "  IPv4: " ++ show (ipv4 n) ++ "\n"
               ++ "  IPv6: " ++ show (ipv6 n) ++ "\n"
               ++ "  MAC:  " ++ show (mac n) ++ "\n"
~~~

Running this on my Ubuntu box yields something similar to this:

~~~{.sourceCode}
$ runghc getaddr.hs
lo
  IPv4: 127.0.0.1
  IPv6: 0:0:0:0:0:0:0:1
  MAC:  00:00:00:00:00:00
eth0
  IPv4: 10.0.2.5
  IPv6: fe80:0:0:0:f00:15ff:fcf9:c677
  MAC:  08:00:72:5f:6c:19
~~~

And on Windows 7:

~~~{.sourceCode}
> runghc getaddr.hs
Local Area Connection
  IPv4: 192.168.0.107
  IPv6: fe80:0:0:0:ef54:7861:7df:6b7c
  MAC:  00:26:c9:e1:87:8c
VirtualBox Host-Only Network
  IPv4: 192.168.56.1
  IPv6: fe80:0:0:0:14b4:b3af:557:ad4a
  MAC:  08:00:27:00:d8:2a
Loopback Pseudo-Interface 1
  IPv4: 127.0.0.1
  IPv6: 0:0:0:0:0:0:0:1
  MAC:  00:00:00:00:00:00
... snipped out some garbage network interfaces
~~~

You'll notice that the IPv6 addresses don't have fully collapsed groups of zeros just yet, but they are still valid addresses.

I've tested this on Ubuntu 10.04, Windows XP &amp; Windows 7. It won't
install on Mac OS X or BSD just yet because of restrictions in the cabal
file. I need to check out how
[getifaddrs](http://www.kernel.org/doc/man-pages/online/pages/man3/getifaddrs.3.html)
works on those systems so I know what to do in the FFI code.
