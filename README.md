scmus - Scheme MPD Client
=========================

Copyright © 2014-2018 Drew Thoreson

![screenshot of scmus](https://raw.github.com/drewt/scmus/master/screens/scmus.png "scmus - queue view")

scmus is an MPD client written in the (Chicken) scheme programming language.
scmus is similar to the cmus music player, with a few major differences:

* scmus is an MPD client
* scmus can be controlled and scripted using the Scheme programming language
* scmus is in the early stages of development


Building
--------

Requirements:

* CHICKEN 4
* ncurses

Consult http://wiki.call-cc.org/platforms for instructions on getting Chicken
on your platform. Note that scmus does not yet support Chicken 5.

First, run the configure script:

    $ ./configure

scmus depends on a few "eggs".  To install them:

    $ make eggs

To build scmus:

    $ make


Installation
------------

    # make install


Manual
------

[scmus(1)](http://drewt.github.io/scmus/man/scmus.1.html) (general program documentation)  
[scmus(3)](http://drewt.github.io/scmus/man/scmus.3.html) (scripting reference)


Git Repository
--------------

https://github.com/drewt/scmus

    $ git clone https://github.com/drewt/scmus.git
