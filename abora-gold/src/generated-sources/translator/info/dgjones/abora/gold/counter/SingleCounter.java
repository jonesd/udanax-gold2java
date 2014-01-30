/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.counter;

import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.counter.SingleCounter;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.Sema4;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * This counter separates a very simple state change into another flock so that big objects
 * like GrandMaps and GrandHashTables don''t ned to flush their entirety to disk.  It
 * localizes the state change of a counter.
 */
public class SingleCounter extends Counter {

	protected int myCount;
	protected Sema4 myMutex;
/*
udanax-top.st:5783:
Counter subclass: #SingleCounter
	instanceVariableNames: '
		myCount {IntegerVar}
		myMutex {Sema4 NOCOPY}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-counter'!
*/
/*
udanax-top.st:5789:
SingleCounter comment:
'This counter separates a very simple state change into another flock so that big objects like GrandMaps and GrandHashTables don''t ned to flush their entirety to disk.  It localizes the state change of a counter.'!
*/
/*
udanax-top.st:5791:
(SingleCounter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:5883:
SingleCounter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:5886:
(SingleCounter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(SingleCounter.class).setAttributes( new Set().add("LOCKED").add("COPY").add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int count() {
	return myCount;
/*
udanax-top.st:5796:SingleCounter methodsFor: 'accessing'!
{IntegerVar} count
	^myCount!
*/
}
public int decrement() {
	synchronized (myMutex) {
		AboraBlockSupport.enterConsistent(1);
		try {
			myCount = myCount - 1;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return myCount;
/*
udanax-top.st:5799:SingleCounter methodsFor: 'accessing'!
{IntegerVar} decrement
	
	myMutex critical: 
		[DiskManager consistent: 1 with:
			[myCount _ myCount - 1.
			self diskUpdate]].
	^myCount!
*/
}
public int decrementBy(int count) {
	if ( ! (count >= 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	synchronized (myMutex) {
		AboraBlockSupport.enterConsistent(1);
		try {
			myCount = myCount - count;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return myCount;
/*
udanax-top.st:5807:SingleCounter methodsFor: 'accessing'!
{IntegerVar} decrementBy: count {IntegerVar}
	
	count >= IntegerVarZero ifFalse:
		[Heaper BLAST: #InvalidRequest].
	myMutex critical: 
		[DiskManager consistent: 1 with:
			[myCount _ myCount - count.
			self diskUpdate]].
	^myCount!
*/
}
public int increment() {
	synchronized (myMutex) {
		AboraBlockSupport.enterConsistent(1);
		try {
			myCount = myCount + 1;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return myCount;
/*
udanax-top.st:5817:SingleCounter methodsFor: 'accessing'!
{IntegerVar} increment
	
	myMutex critical: 
		[DiskManager consistent: 1 with:
			[myCount _ myCount + 1.
			self diskUpdate]].
	^myCount!
*/
}
public int incrementBy(int count) {
	if ( ! (count >= 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	synchronized (myMutex) {
		AboraBlockSupport.enterConsistent(1);
		try {
			myCount = myCount + count;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
	return myCount;
/*
udanax-top.st:5825:SingleCounter methodsFor: 'accessing'!
{IntegerVar} incrementBy: count {IntegerVar}
	
	count >= IntegerVarZero ifFalse:
		[Heaper BLAST: #InvalidRequest].
	myMutex critical: 
		[DiskManager consistent: 1 with:
			[myCount _ myCount + count.
			self diskUpdate]].
	^myCount!
*/
}
public void setCount(int count) {
	synchronized (myMutex) {
		AboraBlockSupport.enterConsistent(1);
		try {
			myCount = count;
			diskUpdate();
		}
		finally {
			AboraBlockSupport.exitConsistent();
		}
	}
/*
udanax-top.st:5835:SingleCounter methodsFor: 'accessing'!
{void} setCount: count {IntegerVar} 
	
	myMutex critical: 
		[DiskManager consistent: 1 with:
			[myCount _ count.
			self diskUpdate]]!
*/
}
/**
 * re-initialize the non-persistent part
 */
public void restartSingleCounter(Rcvr trans) {
	myMutex = Sema4.make(1);
/*
udanax-top.st:5844:SingleCounter methodsFor: 'receiver: restart'!
{void RECEIVE.HOOK} restartSingleCounter: trans {Rcvr unused default: NULL}
	"re-initialize the non-persistent part"
	myMutex _ Sema4 make: 1.!
*/
}
public SingleCounter() {
	super();
	myCount = 0;
	restartSingleCounter(null);
	newShepherd();
	remember();
/*
udanax-top.st:5850:SingleCounter methodsFor: 'protected: create'!
create
	super create.
	myCount _ IntegerVar0.
	self restartSingleCounter: NULL.
	self newShepherd.
	self remember!
*/
}
public SingleCounter(int count) {
	super();
	myCount = count;
	restartSingleCounter(null);
	newShepherd();
	remember();
/*
udanax-top.st:5857:SingleCounter methodsFor: 'protected: create'!
create: count {IntegerVar}
	super create.
	myCount _ count.
	self restartSingleCounter: NULL.
	self newShepherd.
	self remember!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ (IntegerPos.integerHash(myCount));
/*
udanax-top.st:5866:SingleCounter methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: (IntegerPos integerHash: myCount)!
*/
}
public SingleCounter(Rcvr receiver) {
	super(receiver);
	myCount = receiver.receiveIntegerVar();
	restartSingleCounter(receiver);
/*
udanax-top.st:5873:SingleCounter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCount _ receiver receiveIntegerVar.
	self restartSingleCounter: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myCount);
/*
udanax-top.st:5878:SingleCounter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myCount.!
*/
}
public static Counter make() {
	return new SingleCounter();
/*
udanax-top.st:5891:SingleCounter class methodsFor: 'pseudo-constructors'!
{Counter} make
	^self create.!
*/
}
public static Counter make(int count) {
	return new SingleCounter(count);
/*
udanax-top.st:5894:SingleCounter class methodsFor: 'pseudo-constructors'!
{Counter} make: count {IntegerVar}
	^self create: count!
*/
}
}
