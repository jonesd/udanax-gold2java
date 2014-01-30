/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.tumbler;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.tumbler.IEEE8Pos;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

/**
 * For representing exactly those real numbers that can be represented in IEEE stupid
 * precision
 */
public class IEEE8Pos extends RealPos {

	protected float myValue;
/*
udanax-top.st:32266:
RealPos subclass: #IEEE8Pos
	instanceVariableNames: 'myValue {IEEE8}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:32270:
IEEE8Pos comment:
'For representing exactly those real numbers that can be represented in IEEE stupid precision'!
*/
/*
udanax-top.st:32272:
(IEEE8Pos getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IEEE8Pos.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public IEEE8Pos(float value) {
	super();
	myValue = value;
/*
udanax-top.st:32277:IEEE8Pos methodsFor: 'creation'!
create: value {IEEE8}
	super create.
	myValue := value!
*/
}
public double asIEEE() {
	MarkM.shouldImplement();
	return 0.0;
/*
udanax-top.st:32284:IEEE8Pos methodsFor: 'obsolete:'!
{IEEE64} asIEEE
	MarkM shouldImplement.
	^0.0 "fodder"!
*/
}
public double asIEEE64() {
	MarkM.shouldImplement();
	return 0.0;
/*
udanax-top.st:32289:IEEE8Pos methodsFor: 'obsolete:'!
{IEEE64} asIEEE64
	MarkM shouldImplement.
	^0.0 "fodder"!
*/
}
public int precision() {
	return 8;
/*
udanax-top.st:32294:IEEE8Pos methodsFor: 'obsolete:'!
{Int32} precision
	^8!
*/
}
public PrimFloatValue value() {
	MarkM.shouldImplement();
	return null;
/*
udanax-top.st:32300:IEEE8Pos methodsFor: 'accessing'!
{PrimFloatValue} value
	MarkM shouldImplement.
	^NULL "fodder"!
*/
}
public IEEE8Pos(Rcvr receiver) {
	super(receiver);
	myValue = receiver.receiveInt32();
/*
udanax-top.st:32306:IEEE8Pos methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myValue _ receiver receiveInt32.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIEEEDoubleVar(myValue);
/*
udanax-top.st:32310:IEEE8Pos methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendInt32: myValue.!
*/
}
public IEEE8Pos() {
/*

Generated during transformation
*/
}
}
