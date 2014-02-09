# UdanaxGold2Java

The UdanaxGold2Java project contains a number of sub-components
related to converting the released Udanax-Gold Smalltalk and C++
source code by XOC into Java. The aim is to provide a first pass
automatic translation of as much of the code as possible.

[![Build Status](https://travis-ci.org/jonesd/udanax-gold2java.png?branch=master)](https://travis-ci.org/jonesd/udanax-gold2java)

For more information on Abora see: http://abora.dgjones.info/

The `udanax-gold` sub-directory holds a few versions of the Udanax-Gold
source code as supplied by XOC. This code was released under a MIT
open-source licence.

The `translator` sub-directory implements a simple Java translator that
is used to convert from the Udanax-Gold Smalltalk source to
(approximately) Java - though with lots of remaining compilation
problems.

The `abora-gold` sub-directory holds the translated Udanax-Gold code.
This includes the result of the translator application and some hand
coded translated classes from C++ code plus some place-holder classes
for source that has not been released by XOC yet but is referenced by
translated source.


## Building and Testing

To build abora-gold you will need to have Java + Maven installed.

    mvn clean install

The result of translating the Udanax-Gold Smalltalk source to Java
can be found in the `abora-gold/src/generated-sources/translator`.

To regenerate the translated Smalltalk again, following a change
to the translator code or any of the source Smalltalk files:

    mvn clean install -Dtranslate

You may wish to add the `abora-gold/src/generated-sources/translator`
directory to your IDE's src path.
    

## Contact

david@dgjones.info

https://github.com/jonesd/udanax-gold2java


## Copyright and Licence

Abora Gold is Copyright 2003, 2014 David G Jones

Licensed under MIT X-11.

Substantial portions of the code are from the Udanax-Gold project and are Copyright 1979-1999 Udanax.com and licensed under MIT-X11. 
