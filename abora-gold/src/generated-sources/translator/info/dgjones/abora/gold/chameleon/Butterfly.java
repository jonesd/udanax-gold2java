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
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xcvr.Xmtr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class Butterfly extends Chameleon {

	protected int myE;
	protected Heaper myF;
/*
udanax-top.st:13487:
Chameleon subclass: #Butterfly
	instanceVariableNames: '
		myE {IntegerVar}
		myF {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13493:
(Butterfly getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(Butterfly.class).setAttributes( new Set().add("CONCRETE").add("COPY"));
/*

Generated during transformation: AddMethod
*/
}
public Butterfly() {
	super();
	myE = 0;
	myF = null;
/*
udanax-top.st:13498:Butterfly methodsFor: 'instance creation'!
create
	super create.
	myE _ IntegerVar0.
	myF _ NULL.!
*/
}
public Butterfly(Rcvr receiver) {
	super(receiver);
	myE = receiver.receiveIntegerVar();
	myF = receiver.receiveHeaper();
/*
udanax-top.st:13505:Butterfly methodsFor: 'generated:'!
create.Rcvr: receiver {Rcvr}
	super create.Rcvr: receiver.
	myE _ receiver receiveIntegerVar.
	myF _ receiver receiveHeaper.!
*/
}
public void sendSelfTo(Xmtr xmtr) {
	super.sendSelfTo(xmtr);
	xmtr.sendIntegerVar(myE);
	xmtr.sendHeaper(myF);
/*
udanax-top.st:13510:Butterfly methodsFor: 'generated:'!
{void} sendSelfTo: xmtr {Xmtr}
	super sendSelfTo: xmtr.
	xmtr sendIntegerVar: myE.
	xmtr sendHeaper: myF.!
*/
}
}
