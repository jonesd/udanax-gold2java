/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.x;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.x.PrimValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * A boxed representation of a primitive data type
 */
public class PrimValue extends Heaper {

/*
udanax-top.st:34696:
Heaper subclass: #PrimValue
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'X++ PrimArrays'!
*/
/*
udanax-top.st:34700:
PrimValue comment:
'A boxed representation of a primitive data type'!
*/
/*
udanax-top.st:34702:
(PrimValue getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:34706:
PrimValue class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:34709:
(PrimValue getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(PrimValue.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * {PrimSpec CLIENT} spec
 */
public static void infostProtocol() {
/*
udanax-top.st:34714:PrimValue class methodsFor: 'smalltalk: system'!
info.stProtocol
"{PrimSpec CLIENT} spec
"!
*/
}
public PrimValue() {
/*

Generated during transformation
*/
}
public PrimValue(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
