/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.cxx.classx.stuff;

import info.dgjones.abora.gold.chameleon.Chameleon;
import info.dgjones.abora.gold.cxx.classx.stuff.DeadButterfly;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class DeadButterfly extends Chameleon {

	protected int myJ;
	protected Heaper myK;
	protected Heaper myL;
	protected Heaper myM;
/*
udanax-top.st:13536:
Chameleon subclass: #DeadButterfly
	instanceVariableNames: '
		myJ {IntegerVar}
		myK {Heaper}
		myL {Heaper}
		myM {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Cxx-class-stuff'!
*/
/*
udanax-top.st:13544:
(DeadButterfly getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DeadButterfly.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public DeadButterfly() {
	super();
	myJ = 0;
	myK = null;
/*
udanax-top.st:13549:DeadButterfly methodsFor: 'instance creation'!
create
	super create.
	myJ _ IntegerVar0.
	myK _ NULL.!
*/
}
public DeadButterfly(Rcvr receiver) {
	super(receiver);
	myJ = receiver.receiveIntegerVar();
	myK = receiver.receiveHeaper();
	myL = receiver.receiveHeaper();
	myM = receiver.receiveHeaper();
/*
udanax-top.st:13556:DeadButterfly methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myJ _ receiver receiveIntegerVar.
	myK _ receiver receiveHeaper.
	myL _ receiver receiveHeaper.
	myM _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myJ);
	xmtr.sendHeaper(myK);
	xmtr.sendHeaper(myL);
	xmtr.sendHeaper(myM);
/*
udanax-top.st:13563:DeadButterfly methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myJ.
	xmtr sendHeaper: myK.
	xmtr sendHeaper: myL.
	xmtr sendHeaper: myM.!
*/
}
}
