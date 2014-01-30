/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.snarf;

import info.dgjones.abora.gold.java.AboraBlockSupport;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.AboraRuntimeException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.snarf.Abraham;
import info.dgjones.abora.gold.snarf.DoublingFlock;
import info.dgjones.abora.gold.spaces.integers.IntegerPos;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class DoublingFlock extends Abraham {

	protected int myCount;
/*
udanax-top.st:6009:
Abraham subclass: #DoublingFlock
	instanceVariableNames: 'myCount {Int32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Snarf'!
*/
/*
udanax-top.st:6013:
(DoublingFlock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #EQ; add: #LOCKED; add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:6077:
DoublingFlock class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:6080:
(DoublingFlock getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #SHEPHERD.PATRIARCH; add: #COPY; add: #EQ; add: #LOCKED; add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DoublingFlock.class).setAttributes( new Set().add("SHEPHERDPATRIARCH").add("COPY").add("EQ").add("LOCKED").add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public int count() {
	return myCount;
/*
udanax-top.st:6018:DoublingFlock methodsFor: 'accessing'!
{Int32} count
	^myCount!
*/
}
public void doDouble() {
	AboraBlockSupport.enterConsistent(1);
	try {
		myCount = myCount * 2;
		diskUpdate();
	}
	finally {
		AboraBlockSupport.exitConsistent();
	}
/*
udanax-top.st:6021:DoublingFlock methodsFor: 'accessing'!
{void} doDouble
	
	DiskManager consistent: 1 with:
		[myCount _ myCount * 2.
		self diskUpdate]!
*/
}
public void receiveTestFlock(Rcvr rcvr) {
	for (int i = 0; i < myCount; i ++ ) {
		if (rcvr.receiveInt32() != i) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_MATCH);
		}
	}
/*
udanax-top.st:6029:DoublingFlock methodsFor: 'hooks:'!
{void RECEIVE.HOOK} receiveTestFlock: rcvr {Rcvr} 
	Int32Zero almostTo: myCount do: 
		[:i {Int32} | rcvr receiveInt32 ~~ i ifTrue: [Heaper BLAST: #MustMatch]]!
*/
}
public void sendTestFlock(Xmtr xmtr) {
	for (int i = 0; i < myCount; i ++ ) {
		xmtr.sendInt32(i);
	}
/*
udanax-top.st:6033:DoublingFlock methodsFor: 'hooks:'!
{void SEND.HOOK} sendTestFlock: xmtr {Xmtr}
	Int32Zero almostTo: myCount do: [:i {Int32} | xmtr sendInt32: i]!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(hashForEqual());
	oo.print(", ");
	oo.print(myCount);
	oo.print(")");
/*
udanax-top.st:6038:DoublingFlock methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << self hashForEqual <<', ' << myCount << ')'!
*/
}
public DoublingFlock(int hash) {
	super(hash);
	myCount = 1;
	newShepherd();
/*
udanax-top.st:6043:DoublingFlock methodsFor: 'creation'!
create: hash {UInt32}
	super create: hash.
	myCount _ 1.
	self newShepherd!
*/
}
public DoublingFlock(int hash, int count) {
	super(hash);
	myCount = count;
	newShepherd();
/*
udanax-top.st:6048:DoublingFlock methodsFor: 'creation'!
create: hash {UInt32} with: count {Int32}
	super create: hash.
	myCount _ count.
	self newShepherd!
*/
}
public int contentsHash() {
	return super.contentsHash() ^ (IntegerPos.integerHash(myCount));
/*
udanax-top.st:6055:DoublingFlock methodsFor: 'testing'!
{UInt32} contentsHash
	^super contentsHash
		bitXor: (IntegerPos integerHash: myCount)!
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:6062:DoublingFlock methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public DoublingFlock(Rcvr receiver) {
	super(receiver);
	myCount = receiver.receiveInt32();
	receiveTestFlock(receiver);
/*
udanax-top.st:6064:DoublingFlock methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myCount _ receiver receiveInt32.
	self receiveTestFlock: receiver.!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:6069:DoublingFlock methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendInt32(myCount);
	sendTestFlock(xmtr);
/*
udanax-top.st:6071:DoublingFlock methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: myCount.
	self sendTestFlock: xmtr.!
*/
}
public static DoublingFlock make(int hash) {
	return new DoublingFlock(hash);
/*
udanax-top.st:6085:DoublingFlock class methodsFor: 'creation'!
make: hash {UInt32}
	^self create: hash!
*/
}
public static DoublingFlock make(int hash, int count) {
	return new DoublingFlock(hash, count);
/*
udanax-top.st:6088:DoublingFlock class methodsFor: 'creation'!
make: hash {UInt32} with: count {Int32}
	^self create: hash with: count!
*/
}
public DoublingFlock() {
/*

Generated during transformation
*/
}
}
