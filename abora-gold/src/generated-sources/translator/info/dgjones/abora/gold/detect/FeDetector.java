/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.detect;

import info.dgjones.abora.gold.detect.FeDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

/**
 * This generic superclass for detectors is so the comm system can tell what things are
 * detectors.
 */
public class FeDetector extends Heaper {

/*
udanax-top.st:19422:
Heaper subclass: #FeDetector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-detect'!
*/
/*
udanax-top.st:19426:
FeDetector comment:
'This generic superclass for detectors is so the comm system can tell what things are detectors.'!
*/
/*
udanax-top.st:19428:
(FeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeDetector.class).setAttributes( new Set().add("DEFERRED").add("EQ"));
/*

Generated during transformation: AddMethod
*/
}
public int actualHashForEqual() {
	return asOop();
/*
udanax-top.st:19433:FeDetector methodsFor: 'generated:'!
actualHashForEqual ^self asOop!
*/
}
public boolean isEqual(Heaper other) {
	return this == other;
/*
udanax-top.st:19435:FeDetector methodsFor: 'generated:'!
isEqual: other ^self == other!
*/
}
public FeDetector() {
/*

Generated during transformation
*/
}
public FeDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
