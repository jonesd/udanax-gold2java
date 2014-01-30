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
import info.dgjones.abora.gold.chameleon.GoldButterfly;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class GoldButterfly extends Butterfly {

/*
udanax-top.st:13515:
Butterfly subclass: #GoldButterfly
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13519:
(GoldButterfly getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GoldButterfly.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public GoldButterfly() {
/*

Generated during transformation
*/
}
public GoldButterfly(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
