scmus - Scheme MPD Client
=========================

Copyright © 2014 Drew Thoreson

scmus is an MPD client written in the (Chicken) scheme programming language.
scmus is quite similar to the cmus music player, with a few major differences:

* scmus is an MPD client
* scmus can be controlled and scripted in scheme
* scmus is in the early stages of development


Building
--------

You'll need the Chicken scheme compiler/runtime to build and run scmus.
Consult http://wiki.call-cc.org/platforms for instructions on getting Chicken
on your platform.  You'll also need to install ncurses and libmpdclient.
Again, the procedure will depend on your platform.

scmus depends on a few "eggs".  To install them:

    $ make eggs

To build scmus:

    $ make


Installation
------------

    $ make install


Manual
------

    $ man scmus


Git Repository
--------------

https://github.com/drewt/scmus

    $ git clone https://github.com/drewt/scmus.git
