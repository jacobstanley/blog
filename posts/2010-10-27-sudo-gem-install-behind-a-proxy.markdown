---
title: 'sudo gem install ...' behind a proxy
author: Jacob Stanley
date: October 27, 2010
tags: proxy, ruby, sudo, ubuntu
---

*This post applies to Ubuntu Server 10.10 (Maverick Meerkat)*

I had some trouble installing gems (as root) behind our corporate proxy
server today. 

~~~{.sourceCode}
$ sudo gem install rake
ERROR:  http://rubygems.org/ does not appear to be a repository
ERROR:  Could not find a valid gem 'rake' (>= 0) in any repository
~~~

This is despite the fact that my `http_proxy` environment variable had
been set in my `~/.bashrc` file.

~~~{.bash}
# ~/.bashrc: executed by bash(1) for non-login shells.

# HTTP Proxy
export http_proxy=http://username:password@192.168.0.1:80/

# ...
~~~

It turns out that you need to do some tweaking if you want `sudo` to
carry your proxy environment across from the current user. To do this,
run `sudo visudo` and add the `env_keep` line below:

~~~{.bash}
# /etc/sudoers
#
# This file MUST be edited with the 'visudo' command as root.
#
# See the man page for details on how to write a sudoers file.
#

Defaults        env_reset
Defaults        env_keep += "http_proxy https_proxy ftp_proxy"
~~~

Once you're done editing, press `Ctrl+X` to exit, then `Y` to save your
changes and make sure you save them to `/etc/sudoers` and not
`/etc/sudoers.tmp` like it suggests. Press `Y` when it asks if you want
to overwrite.

Now everything should be sweet. You can check that the environment is
propagating using env.

~~~{.sourceCode}
$ sudo env | grep http_proxy
http_proxy=http://username:password@192.168.0.1:80/
~~~

And finally...

~~~{.sourceCode}
$ sudo gem install rake
Successfully installed rake-0.8.7
1 gem installed
Installing ri documentation for rake-0.8.7...
Installing RDoc documentation for rake-0.8.7...
~~~
