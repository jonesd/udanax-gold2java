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
import info.dgjones.abora.gold.counter.MultiCounter;
import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

public class MultiCounter extends Abraham {

	protected Counter myFirst;
	protected Counter mySecond;
/*
udanax-top.st:7154:
Abraham subclass: #MultiCounter
	instanceVariableNames: '
		myFirst {Counter}
		mySecond {Counter}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-counter'!
*/
/*
udanax-top.st:7160:
(MultiCounter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:7244:
MultiCounter class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:7247:
(MultiCounter getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #LOCKED; add: #COPY; add: #SHEPHERD.PATRIARCH; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(MultiCounter.class).setAttributes( new Set().add("LOCKED").add("COPY").add("SHEPHERDPATRIARCH").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public void decrementBoth() {
	AboraBlockSupport.enterConsistent(2);
	try {
		myFirst.decrement();
		mySecond.decrement();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7165:MultiCounter methodsFor: 'accessing'!
{void} decrementBoth
	
	DiskManager consistent: 2 with:
		[myFirst decrement.
		mySecond decrement]!
*/
}
public int decrementFirst() {
	return myFirst.decrement();
/*
udanax-top.st:7171:MultiCounter methodsFor: 'accessing'!
{IntegerVar} decrementFirst
	^myFirst decrement!
*/
}
public int decrementSecond() {
	return mySecond.decrement();
/*
udanax-top.st:7174:MultiCounter methodsFor: 'accessing'!
{IntegerVar} decrementSecond
	^mySecond decrement!
*/
}
public int firstCount() {
	return myFirst.count();
/*
udanax-top.st:7177:MultiCounter methodsFor: 'accessing'!
{IntegerVar} firstCount
	^myFirst count!
*/
}
public void incrementBoth() {
	AboraBlockSupport.enterConsistent(2);
	try {
		myFirst.increment();
		mySecond.increment();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:7180:MultiCounter methodsFor: 'accessing'!
{void} incrementBoth
	
	DiskManager consistent: 2 with:
		[myFirst increment.
		mySecond increment]!
*/
}
public int incrementFirst() {
	return myFirst.increment();
/*
udanax-top.st:7186:MultiCounter methodsFor: 'accessing'!
{IntegerVar} incrementFirst
	^myFirst increment!
*/
}
public int incrementSecond() {
	return mySecond.increment();
/*
udanax-top.st:7189:MultiCounter methodsFor: 'accessing'!
{IntegerVar} incrementSecond
	^mySecond increment!
*/
}
public int secondCount() {
	return mySecond.count();
/*
udanax-top.st:7192:MultiCounter methodsFor: 'accessing'!
{IntegerVar} secondCount
	^mySecond count!
*/
}
public MultiCounter() {
	super();
	myFirst = Counter.make(0);
	mySecond = Counter.make(0);
	newShepherd();
	remember();
/*
udanax-top.st:7197:MultiCounter methodsFor: 'creation'!
create
	super create.
	myFirst _ Counter make: IntegerVar0.
	mySecond _ Counter make: IntegerVar0.
	self newShepherd.
	self remember!
*/
}
public MultiCounter(int first) {
	super();
	myFirst = Counter.make(first);
	mySecond = Counter.make(0);
	newShepherd();
	remember();
/*
udanax-top.st:7204:MultiCounter methodsFor: 'creation'!
create: first {IntegerVar}
	super create.
	myFirst _ Counter make: first.
	mySecond _ Counter make: IntegerVar0.
	self newShepherd.
	self remember!
*/
}
public MultiCounter(int first, int second) {
	super();
	myFirst = Counter.make(first);
	mySecond = Counter.make(second);
	newShepherd();
	remember();
/*
udanax-top.st:7211:MultiCounter methodsFor: 'creation'!
create: first {IntegerVar} with: second {IntegerVar}
	super create.
	myFirst _ Counter make: first.
	mySecond _ Counter make: second.
	self newShepherd.
	self remember!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myFirst.count());
	oo.print(", ");
	oo.print(mySecond.count());
	oo.print(")");
/*
udanax-top.st:7220:MultiCounter methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myFirst count << ', ' << mySecond count << ')'!
*/
}
public int contentsHash() {
	return (super.contentsHash() ^ myFirst.hashForEqual()) ^ mySecond.hashForEqual();
/*
udanax-top.st:7225:MultiCounter methodsFor: 'testing'!
{UInt32} contentsHash
	^(super contentsHash
		bitXor: myFirst hashForEqual)
		bitXor: mySecond hashForEqual!
*/
}
public MultiCounter(Rcvr receiver) {
	super(receiver);
	myFirst = (Counter) receiver.receiveHeaper();
	mySecond = (Counter) receiver.receiveHeaper();
/*
udanax-top.st:7233:MultiCounter methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myFirst _ receiver receiveHeaper.
	mySecond _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendHeaper(myFirst);
	xmtr.sendHeaper(mySecond);
/*
udanax-top.st:7238:MultiCounter methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myFirst.
	xmtr sendHeaper: mySecond.!
*/
}
public static MultiCounter make() {
	return new MultiCounter();
/*
udanax-top.st:7252:MultiCounter class methodsFor: 'pseudo constructors '!
make
	^self create.!
*/
}
public static MultiCounter make(int count) {
	return new MultiCounter(count);
/*
udanax-top.st:7255:MultiCounter class methodsFor: 'pseudo constructors '!
make: count {IntegerVar}
	^self create: count!
*/
}
}
