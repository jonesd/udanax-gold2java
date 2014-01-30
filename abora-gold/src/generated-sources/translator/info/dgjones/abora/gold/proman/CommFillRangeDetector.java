/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.detect.FeFillRangeDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.proman.CommFillRangeDetector;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RangeFilledEvent;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Send the detector events over comm.
 */
public class CommFillRangeDetector extends FeFillRangeDetector {

	protected PromiseManager myManager;
	protected int myNumber;
	protected FeEdition myTarget;
/*
udanax-top.st:19586:
FeFillRangeDetector subclass: #CommFillRangeDetector
	instanceVariableNames: '
		myManager {PromiseManager}
		myNumber {IntegerVar}
		myTarget {FeEdition}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:19593:
CommFillRangeDetector comment:
'Send the detector events over comm.'!
*/
/*
udanax-top.st:19595:
(CommFillRangeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19614:
CommFillRangeDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19617:
(CommFillRangeDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommFillRangeDetector.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CommFillRangeDetector(PromiseManager pm, int number, FeEdition target) {
	super();
	myManager = pm;
	myNumber = number;
	myTarget = target;
/*
udanax-top.st:19600:CommFillRangeDetector methodsFor: 'creation'!
create: pm {PromiseManager} with: number {IntegerVar} with: target {FeEdition}
	super create.
	myManager _ pm.
	myNumber _ number.
	myTarget _ target!
*/
}
/**
 * Essential.  Some of the PlaceHolders in the Edition on which I was placed have become
 * something else. The Edition has their new identies as its RangeElements, though the keys
 * may bear no relationship to those in the original Edition.
 */
public void rangeFilled(FeEdition newIdentities) {
	myManager.queueDetectorEvent((RangeFilledEvent.make(myNumber, newIdentities)));
/*
udanax-top.st:19608:CommFillRangeDetector methodsFor: 'triggering'!
{void} rangeFilled: newIdentities {FeEdition}
	"Essential.  Some of the PlaceHolders in the Edition on which I was placed have become something else. The Edition has their new identies as its RangeElements, though the keys may bear no relationship to those in the original Edition."
	
	myManager queueDetectorEvent: (RangeFilledEvent make: myNumber with: newIdentities)!
*/
}
public static CommFillRangeDetector make(PromiseManager pm, int number, FeEdition target) {
	return new CommFillRangeDetector(pm, number, target);
/*
udanax-top.st:19622:CommFillRangeDetector class methodsFor: 'creation'!
make: pm {PromiseManager} with: number {IntegerVar} with: target {FeEdition}
	^self create: pm with: number with: target!
*/
}
public CommFillRangeDetector() {
/*

Generated during transformation
*/
}
public CommFillRangeDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
