/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.xcvr;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.CommIbid;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class CommIbid extends Heaper {

	protected int myNumber;
/*
udanax-top.st:13807:
Heaper subclass: #CommIbid
	instanceVariableNames: 'myNumber {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Xcvr'!
*/
/*
udanax-top.st:13811:
(CommIbid getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
/*
udanax-top.st:13846:
CommIbid class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13849:
(CommIbid getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommIbid.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public CommIbid(int number) {
	super();
	myNumber = number;
/*
udanax-top.st:13816:CommIbid methodsFor: 'creation'!
create: number {IntegerVar}
	super create.
	myNumber _ number.!
*/
}
public int number() {
	return myNumber;
/*
udanax-top.st:13822:CommIbid methodsFor: 'accessing'!
{IntegerVar} number
	^myNumber!
*/
}
public void printOn(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("(");
	oo.print(myNumber);
	oo.print(")");
/*
udanax-top.st:13827:CommIbid methodsFor: 'printing'!
{void} printOn: oo {ostream reference}
	oo << self getCategory name << '(' << myNumber << ')'.!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:13832:CommIbid methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public CommIbid(Rcvr receiver) {
	super(receiver);
	myNumber = receiver.receiveIntegerVar();
/*
udanax-top.st:13837:CommIbid methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myNumber _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myNumber);
/*
udanax-top.st:13841:CommIbid methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myNumber.!
*/
}
public static CommIbid make(int number) {
	return new CommIbid(number);
/*
udanax-top.st:13854:CommIbid class methodsFor: 'creation'!
make: number {IntegerVar} 
	^self create: number!
*/
}
public CommIbid() {
/*

Generated during transformation
*/
}
}
