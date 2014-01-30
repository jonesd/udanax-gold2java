/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.chameleon;

import info.dgjones.abora.gold.chameleon.Butterfly;
import info.dgjones.abora.gold.chameleon.Chameleon;
import info.dgjones.abora.gold.chameleon.Moth;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;

public class Moth extends Chameleon {

	protected int myD;
/*
udanax-top.st:13602:
Chameleon subclass: #Moth
	instanceVariableNames: 'myD {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13606:
(Moth getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(MAY.BECOME Butterfly ); add: #COPY; yourself)!
*/
/*
udanax-top.st:13631:
Moth class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:13634:
(Moth getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(MAY.BECOME Butterfly ); add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Moth.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"MAYBECOME", "Butterfly"}).add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public void molt() {
	/* TODO newBecome */
	new Butterfly();
/*
udanax-top.st:13611:Moth methodsFor: 'becoming'!
{void} molt
	(Butterfly new.Become: self) create!
*/
}
public Moth(int d) {
	super();
	myD = d;
/*
udanax-top.st:13616:Moth methodsFor: 'instance creation'!
create: d {IntegerVar}
	super create.
	myD _ d.!
*/
}
public Moth(Rcvr receiver) {
	super(receiver);
	myD = receiver.receiveIntegerVar();
/*
udanax-top.st:13622:Moth methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myD _ receiver receiveIntegerVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myD);
/*
udanax-top.st:13626:Moth methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myD.!
*/
}
public static Moth make() {
	return new Moth(4);
/*
udanax-top.st:13639:Moth class methodsFor: 'instance creation'!
make
	^self create: 4!
*/
}
public Moth() {
/*

Generated during transformation
*/
}
}
