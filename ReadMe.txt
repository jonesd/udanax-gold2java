UdanaxGold2Java
---------------

The UdanaxGold2Java project contains a number of sub-components
related to converting the released Udanax-Gold Smalltalk and C++
source code by XOC into Java. The aim is to provide a first pass
automatic translation of as much of the code as possible. The end
result is *NOT* compilable but may prove useful for non-Smalltalk users
who are interested in reading the Udanax-Gold source code.

The udanax-gold sub-directory holds a few versions of the Udanax-Gold
source code as supplied by XOC. This code was released under a MIT
open-source licence.

The translator sub-directory implements a simple Java translator that
is used to convert from the Udanax-Gold Smalltalk source to
(approximately) Java - though with lots of remaining compilation
problems.

The abora-gold sub-directory holds the translated Udanax-Gold code.
This includes the result of the translator application and some hand
coded translated classes from C++ code plus some place-holder classes
for source that has not been released by XOC yet but is referenced by
translated source.


Using UdanaxGold2Java
----------------------

You will first need to translate from the UdanaxGold source code. See
translator/ReadMe.txt, or if you have Ant, Java and friends setup then
run the following from the command line:

> ant

This will generate about 500 Java classes from the Udanax-Gold source,
and will put them in the abora-gold/src-gen directory.

At this point you can load abora-gold into you favourite Java IDE to
examine the code, mind all the errors though.


Copyright 2003 David G Jones
mailto:david_jones@night.dircon.co.uk
http://www.abora.org
