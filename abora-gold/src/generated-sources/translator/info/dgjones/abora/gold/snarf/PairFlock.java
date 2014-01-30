/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.PairFlock;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class PairFlock extends Abraham {

	protected Abraham myLeft;
	protected Abraham myRight;
/*
udanax-top.st:10429:
Abraham subclass: #PairFlock
	instanceVariableNames: '
		myLeft {Abraham}
		myRight {Abraham}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:10435:
(PairFlock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #EQ; add: #LOCKED; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:10479:
PairFlock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:10482:
(PairFlock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #EQ; add: #LOCKED; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PairFlock.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("EQ").add("LOCKED").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public Abraham left() {
	return myLeft;
/*
udanax-top.st:10440:PairFlock methodsFor: 'accessing'!
{Abraham} left
	^myLeft!
*/
}
public Abraham right() {
	return myRight;
/*
udanax-top.st:10443:PairFlock methodsFor: 'accessing'!
{Abraham} right
	^myRight!
*/
}
public PairFlock(Abraham left, Abraham right) {
	super();
	myLeft = left;
	myRight = right;
	newShepherd();
/*
udanax-top.st:10448:PairFlock methodsFor: 'creation'!
create: left {Abraham} with: right {Abraham}
	super create.
	myLeft _ left.
	myRight _ right.
	self newShepherd!
*/
}
public int contentsHash() {
	return (super.contentsHash() ^ myLeft.hashForEqual()) ^ myRight.hashForEqual();
/*
udanax-top.st:10456:PairFlock methodsFor: 'testing'!
{UInt32} contentsHash
	^(super contentsHash
		bitXor: myLeft hashForEqual)
		bitXor: myRight hashForEqual!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:10464:PairFlock methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public PairFlock(Rcvr receiver) {
	super(receiver);
	myLeft = (Abraham) receiver.receiveHeaper();
	myRight = (Abraham) receiver.receiveHeaper();
/*
udanax-top.st:10466:PairFlock methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myLeft _ receiver receiveHeaper.
	myRight _ receiver receiveHeaper.!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:10471:PairFlock methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myLeft);
	xmtr.sendHeaper(myRight);
/*
udanax-top.st:10473:PairFlock methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myLeft.
	xmtr sendHeaper: myRight.!
*/
}
public static PairFlock make(Abraham left, Abraham right) {
	return new PairFlock(left, right);
/*
udanax-top.st:10487:PairFlock class methodsFor: 'creation'!
make: left {Abraham} with: right {Abraham}
	^self create: left with: right!
*/
}
public PairFlock() {
/*

Generated during transformation
*/
}
}
