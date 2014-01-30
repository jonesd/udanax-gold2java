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
import info.dgjones.abora.gold.chameleon.IronButterfly;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;

public class IronButterfly extends Butterfly {

/*
udanax-top.st:13522:
Butterfly subclass: #IronButterfly
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-Chameleon'!
*/
/*
udanax-top.st:13526:
(IronButterfly getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #(MAY.BECOME.ANY.SUBCLASS.OF Chameleon ); yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(IronButterfly.class).setAttributes( new Set().add("CONCRETE").add( new String[]
	{"MAYBECOMEANYSUBCLASSOF", "Chameleon"}));
/*

Generated during transformation: AddMethod
*/
}
public IronButterfly() {
/*

Generated during transformation
*/
}
public IronButterfly(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
