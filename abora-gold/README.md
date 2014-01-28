# UdanaxGold2Java - Abora-Gold

Abora-Gold is just my name for the combination of auto and manual
translation of the Udanax-Gold open-source released by XOC. It is
*NOT* compilable but my prove useful for non-Smalltalk users who are
interested in reading the Udanax-Gold source code.

See ../translator/README.md for generating Java source.

There are two source directories present.

- src is the location for the code which I manually translated from
C++ for PrimArray and subclasses - this is broken. Additionally there
are a number of place-holder classes for classes which haven't seen to
have been released yet by XOC

- src-gen is the location of the classes auto-translated from
Smalltalk by my translator. It will only be present if you have run
the translator.


## Technical

## Keyed Collection API

### boolean store(Position key, Heaper value)
Store the value it the specified key. Return true if that key already existed in the
collection

### void introduce(Position key, Heaper value)
As store, but assumes that no matching key is currently being stored in the collection.
Throw an exception if there is already a matching key.

### boolean wipe(Position key)
Remove the association with a matching key. Return true if an entry with matching key
was found in the collection.

### void remove(Position key)
As wipe, but must be a matching key in the collection, will throw an exception if there
isn't.

Often there will be overloaded methods related to the key, to support the general Position
type, and also the more efficient int type.


## SetTable

Dictionary from key to values. Holds a SharedPtrArray, of which each element is a
TableEntry. A TableEntry is a single-linked list of key and value pairs.

A hash of the key is used to determine the index of the SharedPtrArray, holding the
head TableEntry.

The `SharedPtrArray` is an optimization to allow sharing of fragments of data structures
between different SetTable's. If a modification is to be made, a new SharedPtrArry is
created and populated with copies of all TableEntry lists it holds. This is triggered by
prefacing any modification with a call to the `aboutToWrite` method, and if the SharedPtrArray
is being shared with any other Tables, this copying behaviour will be triggered.

`BucketArrayStepper` is used to step over the elements of the table.

TODO Does wipeAssociation work? It seems to hold onto previous entries after an aboutToWrite,
so contaminating the shared data structure?

TODO `BuckArrayStepper` never gets finalized/destructed, so keeps a share onto the SharedPtrArray
of the SetTable.


## TableEntry

Internal element stored in a SetTable.

TableEntry is the abstract base class of entries, and there are other subclasses.

- HashIndexEntry
- HeaperAsEnty
- IndexEntry
- PositionEntry

Matching is used to determine if the current entry is the one that is being looked for
in the table. If no match is made, the next entry could be examined. Matching can occur
for both the key and the value.

TableEntry's are created through one of a pair of create factory methods.

- TableEntry.makeIntegerVar(int index, Heaper value)
- TableEntry.make(Position key, Heaper value)


## Copyright and licence

Abora Gold is Copyright 2003, 2014 David G Jones

Licensed under MIT X-11.

Substantial portions of the code are from the Udanax-Gold project and are Copyright 1979-1999 Udanax.com and licensed under MIT-X11. 
