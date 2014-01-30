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
import info.dgjones.abora.gold.counter.SingleCounter;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class Counter extends Abraham {

/*
udanax-top.st:5585:
Abraham subclass: #Counter
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-counter'!
*/
/*
udanax-top.st:5589:
(Counter getOrMakeCxxClassDescription)
	friends:
'friend class SimpleTurtle;
';
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
/*
udanax-top.st:5637:
Counter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:5640:
(Counter getOrMakeCxxClassDescription)
	friends:
'friend class SimpleTurtle;
';
	attributes: ((Set new) add: #DEFERRED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #DEFERRED.LOCKED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Counter.class).setAttributes( new Set().add("DEFERRED").add("COPY").add("SHEPHERDPATRIARCH").add("DEFERREDLOCKED"));
/*

Generated during transformation: AddMethod
*/
}
public int count() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:5597:Counter methodsFor: 'accessing'!
{IntegerVar} count
	^self subclassResponsibility!
*/
}
public int decrement() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:5600:Counter methodsFor: 'accessing'!
{IntegerVar} decrement
	^self subclassResponsibility!
*/
}
public int decrementBy(int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:5603:Counter methodsFor: 'accessing'!
{IntegerVar} decrementBy: count {IntegerVar}
	^self subclassResponsibility!
*/
}
public int increment() {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:5606:Counter methodsFor: 'accessing'!
{IntegerVar} increment
	^self subclassResponsibility!
*/
}
public int incrementBy(int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:5609:Counter methodsFor: 'accessing'!
{IntegerVar} incrementBy: count {IntegerVar}
	^self subclassResponsibility!
*/
}
public void setCount(int count) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:5612:Counter methodsFor: 'accessing'!
{void} setCount: count {IntegerVar}
	self subclassResponsibility!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(count());
	oo.print(")");
/*
udanax-top.st:5617:Counter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << self count << ')'!
*/
}
public Counter() {
	super();
/*
udanax-top.st:5622:Counter methodsFor: 'protected: creation'!
create
	super create!
*/
}
public Counter(int hash) {
	super(hash);
/*
udanax-top.st:5625:Counter methodsFor: 'protected: creation'!
create: hash {UInt32}
	super create: hash!
*/
}
public Counter(Rcvr receiver) {
	super(receiver);
/*
udanax-top.st:5630:Counter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
/*
udanax-top.st:5633:Counter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.!
*/
}
public static Counter fakeCounter(int count, int batchCount, int hash) {
	return BatchCounter.makeFakeCounter(count, batchCount, hash);
/*
udanax-top.st:5648:Counter class methodsFor: 'pseudo-constructors'!
{Counter} fakeCounter: count {IntegerVar} with: batchCount {IntegerVar} with: hash {UInt32}
	^BatchCounter makeFakeCounter: count with: batchCount with: hash!
*/
}
public static Counter make() {
	return new SingleCounter();
/*
udanax-top.st:5651:Counter class methodsFor: 'pseudo-constructors'!
make
	^SingleCounter create.!
*/
}
public static Counter make(int count) {
	return new SingleCounter(count);
/*
udanax-top.st:5654:Counter class methodsFor: 'pseudo-constructors'!
make: count {IntegerVar}
	^SingleCounter create: count!
*/
}
public static Counter make(int count, int batchCount) {
	return BatchCounter.make(count, batchCount);
/*
udanax-top.st:5657:Counter class methodsFor: 'pseudo-constructors'!
make: count {IntegerVar} with: batchCount {IntegerVar}
	^BatchCounter make: count with: batchCount!
*/
}
}
