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
import info.dgjones.abora.gold.chameleon.DeadMoth;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class DeadMoth extends Chameleon {

	protected int myG;
	protected Heaper myH;
	protected boolean myI;
/*
udanax-top.st:13570:
Chameleon subclass: #DeadMoth
	instanceVariableNames: '
		myG {IntegerVar}
		myH {Heaper}
		myI {BooleanVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13577:
(DeadMoth getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(DeadMoth.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public DeadMoth() {
	super();
	myG = 0;
	myH = null;
	myI = false;
/*
udanax-top.st:13582:DeadMoth methodsFor: 'instance creation'!
create
	super create.
	myG _ IntegerVar0.
	myH _ NULL.
	myI _ false.!
*/
}
public DeadMoth(Rcvr receiver) {
	super(receiver);
	myG = receiver.receiveIntegerVar();
	myH = receiver.receiveHeaper();
	myI = receiver.receiveBooleanVar();
/*
udanax-top.st:13590:DeadMoth methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myG _ receiver receiveIntegerVar.
	myH _ receiver receiveHeaper.
	myI _ receiver receiveBooleanVar.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myG);
	xmtr.sendHeaper(myH);
	xmtr.sendBooleanVar(myI);
/*
udanax-top.st:13596:DeadMoth methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myG.
	xmtr sendHeaper: myH.
	xmtr sendBooleanVar: myI.!
*/
}
}
