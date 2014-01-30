/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.counter;

import info.dgjones.abora.gold.counter.BatchCounter;
import info.dgjones.abora.gold.counter.Counter;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.Sema4;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * Instances preallocate a bunch of numbers and record the preallocations to disk.  It then
 * increments purely in memory until the preallocated counts are used up.  It then
 * preallocates another bunch of numbers.  If the system crashes, all numbers between the
 * in-memory count and the on-disk count simply never get used.  This reduces the access to
 * disk for shepherd hashes and GrandMap IDs.
 */
public class BatchCounter extends Counter {

	protected int myCount;
	protected int myPersistentCount;
	protected Sema4 myMutex;
	protected int myBatchCount;
/*
udanax-top.st:5660:
Counter subclass: #BatchCounter
	instanceVariableNames: '
		myCount {IntegerVar NOCOPY}
		myPersistentCount {IntegerVar}
		myMutex {Sema4 NOCOPY}
		myBatchCount {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-counter'!
*/
/*
udanax-top.st:5668:
BatchCounter comment:
'Instances preallocate a bunch of numbers and record the preallocations to disk.  It then increments purely in memory until the preallocated counts are used up.  It then preallocates another bunch of numbers.  If the system crashes, all numbers between the in-memory count and the on-disk count simply never get used.  This reduces the access to disk for shepherd hashes and GrandMap IDs.'!
*/
/*
udanax-top.st:5670:
(BatchCounter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:5769:
BatchCounter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:5772:
(BatchCounter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(BatchCounter.class).setAttributes( new Set().add("LOCKED").add("COPY").add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public int count() {
	return myCount;
/*
udanax-top.st:5675:BatchCounter methodsFor: 'accessing'!
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
udanax-top.st:5678:BatchCounter methodsFor: 'accessing'!
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
udanax-top.st:5686:BatchCounter methodsFor: 'accessing'!
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
		myCount = myCount + 1;
		if (myCount > myPersistentCount) {
			AboraBlockSupport.enterConsistent(1);
			try {
				myPersistentCount = myCount + myBatchCount;
				diskUpdate();
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
	}
	return myCount;
/*
udanax-top.st:5696:BatchCounter methodsFor: 'accessing'!
{IntegerVar} increment
	myMutex critical:
	[myCount _ myCount + 1.
	myCount > myPersistentCount ifTrue:
		[DiskManager consistent: 1 with:
		[myPersistentCount _ myCount + myBatchCount.
		self diskUpdate]]].
	^myCount!
*/
}
public int incrementBy(int count) {
	if ( ! (count >= 0)) {
		throw new AboraRuntimeException(AboraRuntimeException.INVALID_REQUEST);
	}
	synchronized (myMutex) {
		myCount = myCount + count;
		if (myCount > myPersistentCount) {
			AboraBlockSupport.enterConsistent(1);
			try {
				myPersistentCount = myCount + myBatchCount;
				diskUpdate();
			}
			finally {
				AboraBlockSupport.exitConsistent();
			}
		}
	}
	return myCount;
/*
udanax-top.st:5705:BatchCounter methodsFor: 'accessing'!
{IntegerVar} incrementBy: count {IntegerVar}
	count >= IntegerVarZero ifFalse:
		[Heaper BLAST: #InvalidRequest].
	myMutex critical:
	[myCount _ myCount + count.
	myCount > myPersistentCount ifTrue:
		[DiskManager consistent: 1 with:
		[myPersistentCount _ myCount + myBatchCount.
		self diskUpdate]]].
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
udanax-top.st:5717:BatchCounter methodsFor: 'accessing'!
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
public void restartBatchCounter(Rcvr trans) {
	myCount = myPersistentCount;
	myMutex = Sema4.make(1);
/*
udanax-top.st:5726:BatchCounter methodsFor: 'receiver: stubble'!
{void RECEIVE.HOOK} restartBatchCounter: trans {Rcvr unused default: NULL}
	"re-initialize the non-persistent part"
	myCount _ myPersistentCount.
	myMutex _ Sema4 make: 1.!
*/
}
public BatchCounter(int count, int batchCount) {
	super();
	AboraBlockSupport.enterConsistent(1);
	try {
		myPersistentCount = myCount = count;
		myBatchCount = batchCount;
		restartBatchCounter(null);
		newShepherd();
		remember();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:5733:BatchCounter methodsFor: 'protected: create'!
create: count {IntegerVar} with: batchCount {IntegerVar}
	super create.
	DiskManager consistent: 1 with:
		[myPersistentCount _ myCount _ count.
		myBatchCount _ batchCount.
		self restartBatchCounter: NULL.
		self newShepherd.
		self remember]!
*/
}
public BatchCounter(int count, int batchCount, int hash) {
	super(hash);
	myPersistentCount = myCount = count;
	myBatchCount = batchCount;
	restartBatchCounter(null);
/*
udanax-top.st:5742:BatchCounter methodsFor: 'protected: create'!
create: count {IntegerVar} with: batchCount {IntegerVar} with: hash {UInt32}
	super create: hash.
	myPersistentCount _ myCount _ count.
	myBatchCount _ batchCount.
	self restartBatchCounter: NULL.!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ (IntegerPos.integerHash(myPersistentCount));
/*
udanax-top.st:5750:BatchCounter methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: (IntegerPos integerHash: myPersistentCount)!
*/
}
public BatchCounter(Rcvr receiver) {
	super(receiver);
	myPersistentCount = receiver.receiveIntegerVar();
	myBatchCount = receiver.receiveIntegerVar();
	restartBatchCounter(receiver);
/*
udanax-top.st:5757:BatchCounter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myPersistentCount _ receiver receiveIntegerVar.
	myBatchCount _ receiver receiveIntegerVar.
	self restartBatchCounter: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myPersistentCount);
	xmtr.sendIntegerVar(myBatchCount);
/*
udanax-top.st:5763:BatchCounter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myPersistentCount.
	xmtr sendIntegerVar: myBatchCount.!
*/
}
public static Counter make(int count, int batchCount) {
	return new BatchCounter(count, batchCount);
/*
udanax-top.st:5777:BatchCounter class methodsFor: 'pseudo-constructors'!
{Counter} make: count {IntegerVar} with: batchCount {IntegerVar}
	^self create: count with: batchCount!
*/
}
public static Counter makeFakeCounter(int count, int batchCount, int hash) {
	return new BatchCounter(count, batchCount, hash);
/*
udanax-top.st:5780:BatchCounter class methodsFor: 'pseudo-constructors'!
{Counter} makeFakeCounter: count {IntegerVar} with: batchCount {IntegerVar} with: hash {UInt32}
	^self create: count with: batchCount with: hash!
*/
}
public BatchCounter() {
/*

Generated during transformation
*/
}
}
