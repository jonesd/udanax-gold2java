/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.collection.grand;

import info.dgjones.abora.gold.collection.grand.GrandEntry;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * GrandEntries probably want to not be remembered right when they are created,
 * and remembered when they are finally put into their place in the GrandDataPages
 * or GrandOverflows
 */
public class GrandEntry extends Abraham {

	protected Heaper objectInternal;
/*
udanax-top.st:6517:
Abraham subclass: #GrandEntry
	instanceVariableNames: 'objectInternal {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Collection-Grand'!
*/
/*
udanax-top.st:6521:
GrandEntry comment:
'GrandEntries probably want to not be remembered right when they are created,
and remembered when they are finally put into their place in the GrandDataPages
or GrandOverflows'!
*/
/*
udanax-top.st:6525:
(GrandEntry getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrandEntry.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public Heaper value() {
	if (objectInternal == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
	}
	return objectInternal;
/*
udanax-top.st:6530:GrandEntry methodsFor: 'accessing'!
{Heaper} value
	objectInternal == NULL ifTrue: [Heaper BLAST: #NotInTable].
	^ objectInternal!
*/
}
public GrandEntry(Heaper value, int hash) {
	super(hash);
	if (value == null) {
		throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
	}
	/* Removed smalltalkOnly */
	objectInternal = value;
/*
udanax-top.st:6536:GrandEntry methodsFor: 'protected: creation'!
create: value {Heaper} with: hash {UInt32}
	super create: hash.
	value == NULL ifTrue: [Heaper BLAST: #NullInsertion].
	[value == nil ifTrue: [Heaper BLAST: #NullInsertion]] smalltalkOnly.
	objectInternal _ value.!
*/
}
public boolean compare(Heaper anObj) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:6544:GrandEntry methodsFor: 'deferred: testing'!
{BooleanVar} compare: anObj {Heaper | Position}
	self subclassResponsibility!
*/
}
public boolean matches(GrandEntry anEntry) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:6547:GrandEntry methodsFor: 'deferred: testing'!
{BooleanVar} matches: anEntry {GrandEntry}
	self subclassResponsibility!
*/
}
public int contentsHash() {
	return (super.contentsHash() ^ (IntegerPos.integerHash(hashForEqual()))) ^ objectInternal.hashForEqual();
/*
udanax-top.st:6552:GrandEntry methodsFor: 'testing'!
{UInt32} contentsHash
	^(super contentsHash
		bitXor: (IntegerPos integerHash: self hashForEqual))
		bitXor: objectInternal hashForEqual!
*/
}
public GrandEntry(Rcvr receiver) {
	super(receiver);
	objectInternal = receiver.receiveHeaper();
/*
udanax-top.st:6560:GrandEntry methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	objectInternal _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(objectInternal);
/*
udanax-top.st:6564:GrandEntry methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: objectInternal.!
*/
}
public GrandEntry() {
/*

Generated during transformation
*/
}
}
