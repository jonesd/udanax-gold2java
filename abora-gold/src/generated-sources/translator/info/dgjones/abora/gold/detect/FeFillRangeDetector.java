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
import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.PasseException;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Client defines a subclass and passes it in to Edition::addFillRangeDetector, to be
 * notified whenever PlaceHolders become any other kind of RangeElement.
 */
public class FeFillRangeDetector extends FeDetector {

/*
udanax-top.st:19551:
FeDetector subclass: #FeFillRangeDetector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-detect'!
*/
/*
udanax-top.st:19555:
FeFillRangeDetector comment:
'Client defines a subclass and passes it in to Edition::addFillRangeDetector, to be notified whenever PlaceHolders become any other kind of RangeElement.'!
*/
/*
udanax-top.st:19557:
(FeFillRangeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:19574:
FeFillRangeDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19577:
(FeFillRangeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeFillRangeDetector.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Essential.  Some of the PlaceHolders in the Edition on which I was placed have become
 * something else. The Edition has their new identies as its RangeElements, though the keys
 * may bear no relationship to those in the original Edition.
 */
public void rangeFilled(FeEdition newIdentities) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:19562:FeFillRangeDetector methodsFor: 'triggering'!
{void CLIENT} rangeFilled: newIdentities {FeEdition}
	"Essential.  Some of the PlaceHolders in the Edition on which I was placed have become something else. The Edition has their new identies as its RangeElements, though the keys may bear no relationship to those in the original Edition."
	
	self subclassResponsibility!
*/
}
/**
 * @deprecated
 */
public void allFilled(FeEdition newIdentities) {
	throw new PasseException();
/*
udanax-top.st:19569:FeFillRangeDetector methodsFor: 'smalltalk: passe'!
{void} allFilled: newIdentities {FeEdition}
	self passe "rangeFilled"!
*/
}
/**
 * {NOWAIT CLIENT} rangeFilled: newIdentities {PrEdition}
 */
public static void infostProtocol() {
/*
udanax-top.st:19582:FeFillRangeDetector class methodsFor: 'smalltalk: system'!
info.stProtocol
"{NOWAIT CLIENT} rangeFilled: newIdentities {PrEdition}
"!
*/
}
public FeFillRangeDetector() {
/*

Generated during transformation
*/
}
public FeFillRangeDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
