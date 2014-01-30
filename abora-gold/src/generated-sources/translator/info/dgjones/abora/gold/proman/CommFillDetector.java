/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.detect.FeFillDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeRangeElement;
import info.dgjones.abora.gold.proman.CommFillDetector;
import info.dgjones.abora.gold.proman.FilledEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Send the detector events over comm.
 */
public class CommFillDetector extends FeFillDetector {

	protected PromiseManager myManager;
	protected int myNumber;
	protected FeRangeElement myTarget;
/*
udanax-top.st:19466:
FeFillDetector subclass: #CommFillDetector
	instanceVariableNames: '
		myManager {PromiseManager}
		myNumber {IntegerVar}
		myTarget {FeRangeElement}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:19473:
CommFillDetector comment:
'Send the detector events over comm.'!
*/
/*
udanax-top.st:19475:
(CommFillDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19494:
CommFillDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19497:
(CommFillDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommFillDetector.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CommFillDetector(PromiseManager pm, int number, FeRangeElement target) {
	super();
	myManager = pm;
	myNumber = number;
	myTarget = target;
/*
udanax-top.st:19480:CommFillDetector methodsFor: 'creation'!
create: pm {PromiseManager} with: number {IntegerVar} with: target {FeRangeElement}
	super create.
	myManager _ pm.
	myNumber _ number.
	myTarget _ target!
*/
}
/**
 * A single PlaceHolder has been filled to become another kind of RangeElement
 */
public void filled(FeRangeElement newIdentity) {
	myManager.queueDetectorEvent((FilledEvent.make(myNumber, newIdentity)));
/*
udanax-top.st:19488:CommFillDetector methodsFor: 'triggering'!
{void} filled: newIdentity {FeRangeElement}
	"A single PlaceHolder has been filled to become another kind of RangeElement"
	
	myManager queueDetectorEvent: (FilledEvent make: myNumber with: newIdentity)!
*/
}
public static CommFillDetector make(PromiseManager pm, int number, FeRangeElement target) {
	return new CommFillDetector(pm, number, target);
/*
udanax-top.st:19502:CommFillDetector class methodsFor: 'creation'!
make: pm {PromiseManager} with: number {IntegerVar} with: target {FeRangeElement}
	^self create: pm with: number with: target!
*/
}
public CommFillDetector() {
/*

Generated during transformation
*/
}
public CommFillDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
