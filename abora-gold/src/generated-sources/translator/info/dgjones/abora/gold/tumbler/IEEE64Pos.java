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
import info.dgjones.abora.gold.tumbler.IEEE64Pos;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE64;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * For representing exactly those real numbers that can be represented in IEEE double
 * precision
 */
public class IEEE64Pos extends RealPos {

	protected double myValue;
/*
udanax-top.st:32215:
RealPos subclass: #IEEE64Pos
	instanceVariableNames: 'myValue {IEEE64}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:32219:
IEEE64Pos comment:
'For representing exactly those real numbers that can be represented in IEEE double precision'!
*/
/*
udanax-top.st:32221:
(IEEE64Pos getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IEEE64Pos.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public IEEE64Pos(double value) {
	super();
	myValue = value;
/*
udanax-top.st:32226:IEEE64Pos methodsFor: 'creation'!
create: value {IEEE64}
	super create.
	myValue := value!
*/
}
public double asIEEE() {
	return myValue;
/*
udanax-top.st:32233:IEEE64Pos methodsFor: 'obsolete:'!
{IEEE64} asIEEE
	^myValue!
*/
}
public double asIEEE64() {
	return myValue;
/*
udanax-top.st:32237:IEEE64Pos methodsFor: 'obsolete:'!
{IEEE64} asIEEE64
	^myValue!
*/
}
public int precision() {
	return 64;
/*
udanax-top.st:32241:IEEE64Pos methodsFor: 'obsolete:'!
{Int32} precision
	^64!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("<");
	oo.print(myValue);
	oo.print(">");
/*
udanax-top.st:32247:IEEE64Pos methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << '<' << myValue << '>'!
*/
}
public PrimFloatValue value() {
	return PrimIEEE64.make(myValue);
/*
udanax-top.st:32253:IEEE64Pos methodsFor: 'accessing'!
{PrimFloatValue} value
	^ PrimIEEE64 make: myValue!
*/
}
public IEEE64Pos(Rcvr receiver) {
	super(receiver);
	myValue = receiver.receiveIEEEDoubleVar()
	/* TODO was receiveHeaper */
	;
/*
udanax-top.st:32258:IEEE64Pos methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myValue _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIEEEDoubleVar(myValue)
	/* TODO was sendHeaper */
	;
/*
udanax-top.st:32262:IEEE64Pos methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myValue.!
*/
}
public IEEE64Pos() {
/*

Generated during transformation
*/
}
}
