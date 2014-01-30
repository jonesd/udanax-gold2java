/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.chameleon;

import info.dgjones.abora.gold.chameleon.Chameleon;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;
import java.io.PrintWriter;

public class Chameleon extends Heaper {

	protected int myA;
	protected Heaper myB;
	protected boolean myC;
/*
udanax-top.st:13438:
Heaper subclass: #Chameleon
	instanceVariableNames: '
		myA {IntegerVar}
		myB {Heaper}
		myC {BooleanVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13445:
(Chameleon getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Chameleon.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Chameleon() {
	super();
	myA = 0;
	myB = null;
	myC = false;
/*
udanax-top.st:13450:Chameleon methodsFor: 'instance creation'!
create
	super create.
	myA _ IntegerVar0.
	myB _ NULL.
	myC _ false.!
*/
}
public Chameleon(int a, Heaper b, boolean c) {
	super();
	myA = a;
	myB = b;
	myC = c;
/*
udanax-top.st:13456:Chameleon methodsFor: 'instance creation'!
create: a {IntegerVar} with: b {Heaper} with: c {BooleanVar}
	super create.
	myA _ a.
	myB _ b.
	myC _ c.!
*/
}
public void explain(PrintWriter oo) {
	oo.print(getAboraClass().name());
	oo.print("\n"+
"");
/*
udanax-top.st:13464:Chameleon methodsFor: 'accessing'!
{void} explain: oo {ostream reference}
	oo << self getCategory name << '
'.!
*/
}
public int actualHashForEqual() {
	return Heaper.takeOop();
/*
udanax-top.st:13470:Chameleon methodsFor: 'testing'!
{UInt32} actualHashForEqual
	^Heaper takeOop!
*/
}
public Chameleon(Rcvr receiver) {
	super(receiver);
	myA = receiver.receiveIntegerVar();
	myB = receiver.receiveHeaper();
	myC = receiver.receiveBooleanVar();
/*
udanax-top.st:13475:Chameleon methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myA _ receiver receiveIntegerVar.
	myB _ receiver receiveHeaper.
	myC _ receiver receiveBooleanVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myA);
	xmtr.sendHeaper(myB);
	xmtr.sendBooleanVar(myC);
/*
udanax-top.st:13481:Chameleon methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myA.
	xmtr sendHeaper: myB.
	xmtr sendBooleanVar: myC.!
*/
}
}
