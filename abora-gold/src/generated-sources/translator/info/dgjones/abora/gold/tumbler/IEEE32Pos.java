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
import info.dgjones.abora.gold.tumbler.IEEE32Pos;
import info.dgjones.abora.gold.tumbler.RealPos;
import info.dgjones.abora.gold.x.PrimFloatValue;
import info.dgjones.abora.gold.x.PrimIEEE32;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import java.io.PrintWriter;

/**
 * For representing exactly those real numbers that can be represented in IEEE single
 * precision
 */
public class IEEE32Pos extends RealPos {

	protected float myValue;
/*
udanax-top.st:32160:
RealPos subclass: #IEEE32Pos
	instanceVariableNames: 'myValue {IEEE32}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-tumbler'!
*/
/*
udanax-top.st:32164:
IEEE32Pos comment:
'For representing exactly those real numbers that can be represented in IEEE single precision'!
*/
/*
udanax-top.st:32166:
(IEEE32Pos getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IEEE32Pos.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public IEEE32Pos(float value) {
	super();
	myValue = value;
/*
udanax-top.st:32171:IEEE32Pos methodsFor: 'creation'!
create: value {IEEE32}
	super create.
	myValue := value!
*/
}
public double asIEEE() {
	return (double) myValue;
/*
udanax-top.st:32178:IEEE32Pos methodsFor: 'obsolete:'!
{IEEE64} asIEEE
	
	[^myValue basicCast: IEEE64] translateOnly.
	
	[^myValue asDouble] smalltalkOnly!
*/
}
public double asIEEE64() {
	return (double) myValue;
/*
udanax-top.st:32184:IEEE32Pos methodsFor: 'obsolete:'!
{IEEE64} asIEEE64
	
	[^myValue basicCast: IEEE64] translateOnly.
	
	[^myValue asDouble] smalltalkOnly!
*/
}
public int precision() {
	return 32;
/*
udanax-top.st:32190:IEEE32Pos methodsFor: 'obsolete:'!
{Int32} precision
	^32!
*/
}
public void printOn(PrintWriter oo) {
	oo.print("<");
	oo.print(myValue);
	oo.print(">");
/*
udanax-top.st:32196:IEEE32Pos methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << '<' << myValue << '>'!
*/
}
public PrimFloatValue value() {
	return PrimIEEE32.make(myValue);
/*
udanax-top.st:32202:IEEE32Pos methodsFor: 'accessing'!
{PrimFloatValue} value
	^ PrimIEEE32 make: myValue!
*/
}
public IEEE32Pos(Rcvr receiver) {
	super(receiver);
	myValue = (float) receiver.receiveIEEEDoubleVar()
	/* TODO was receiveHeaper */
	;
/*
udanax-top.st:32207:IEEE32Pos methodsFor: 'generated:'!
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
udanax-top.st:32211:IEEE32Pos methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendHeaper: myValue.!
*/
}
public IEEE32Pos() {
/*

Generated during transformation
*/
}
}
