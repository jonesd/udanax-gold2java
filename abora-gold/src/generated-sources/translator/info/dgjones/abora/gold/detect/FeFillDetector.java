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
import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.exception.SubclassResponsibilityException;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Client defines subclasses and passes in an instance in order to be notified of new results
 * from Edition::rangeTranscluders () or RangeElement::transcluders (). If passed to
 * Edition::addFillRangeDetector, this subclass merely passes in the Editions in the range
 * one by one, though they may appear in the result in batches.
 */
public class FeFillDetector extends FeDetector {

/*
udanax-top.st:19437:
FeDetector subclass: #FeFillDetector
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-detect'!
*/
/*
udanax-top.st:19441:
FeFillDetector comment:
'Client defines subclasses and passes in an instance in order to be notified of new results from Edition::rangeTranscluders () or RangeElement::transcluders (). If passed to Edition::addFillRangeDetector, this subclass merely passes in the Editions in the range one by one, though they may appear in the result in batches.'!
*/
/*
udanax-top.st:19443:
(FeFillDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
/*
udanax-top.st:19454:
FeFillDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19457:
(FeFillDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FeFillDetector.class).setAttributes( new Set().add("ONCLIENT").add("DEFERRED"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * A single PlaceHolder has been filled to become another kind of RangeElement
 */
public void filled(FeRangeElement newIdentity) {
	throw new SubclassResponsibilityException();
/*
udanax-top.st:19448:FeFillDetector methodsFor: 'triggering'!
{void CLIENT} filled: newIdentity {FeRangeElement}
	"A single PlaceHolder has been filled to become another kind of RangeElement"
	
	self subclassResponsibility!
*/
}
/**
 * {NOWAIT CLIENT} filled: newIdentity {PrRangeElement}
 */
public static void infostProtocol() {
/*
udanax-top.st:19462:FeFillDetector class methodsFor: 'smalltalk: system'!
info.stProtocol
"{NOWAIT CLIENT} filled: newIdentity {PrRangeElement}
"!
*/
}
public FeFillDetector() {
/*

Generated during transformation
*/
}
public FeFillDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
