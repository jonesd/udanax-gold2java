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
import info.dgjones.abora.gold.chameleon.LeadButterfly;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class LeadButterfly extends Butterfly {

/*
udanax-top.st:13529:
Butterfly subclass: #LeadButterfly
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13533:
(LeadButterfly getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #(MAY.BECOME DeadMoth ); add: #(MAY.BECOME Butterfly ); add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(LeadButterfly.class).setAttributes( new Set().add( new String[]
	{"MAYBECOME", "DeadMoth"}).add( new String[]
	{"MAYBECOME", "Butterfly"}).add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public LeadButterfly() {
/*

Generated during transformation
*/
}
public LeadButterfly(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
