/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.be.basic.ID;
import info.dgjones.abora.gold.detect.FeStatusDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.proman.CommStatusDetector;
import info.dgjones.abora.gold.proman.GrabbedEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.ReleasedEvent;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Send the detector events over comm.
 */
public class CommStatusDetector extends FeStatusDetector {

	protected PromiseManager myManager;
	protected int myNumber;
	protected FeWork myTarget;
/*
udanax-top.st:19814:
FeStatusDetector subclass: #CommStatusDetector
	instanceVariableNames: '
		myManager {PromiseManager}
		myNumber {IntegerVar}
		myTarget {FeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:19821:
CommStatusDetector comment:
'Send the detector events over comm.'!
*/
/*
udanax-top.st:19823:
(CommStatusDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19856:
CommStatusDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19859:
(CommStatusDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommStatusDetector.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CommStatusDetector(PromiseManager pm, int number, FeWork target) {
	super();
	myManager = pm;
	myNumber = number;
	myTarget = target;
/*
udanax-top.st:19828:CommStatusDetector methodsFor: 'creation'!
create: pm {PromiseManager} with: number {IntegerVar} with: target {FeWork}
	super create.
	myManager _ pm.
	myNumber _ number.
	myTarget _ target!
*/
}
/**
 * Essential. The Work has been grabbed, or regrabbed.
 */
public void grabbed(FeWork work, ID author, int reason) {
	myManager.queueDetectorEvent((GrabbedEvent.make(myNumber, work, author, reason)));
/*
udanax-top.st:19836:CommStatusDetector methodsFor: 'triggering'!
{void} grabbed: work {FeWork} with: author {ID} with: reason {IntegerVar}
	"Essential. The Work has been grabbed, or regrabbed."
	
	myManager queueDetectorEvent: 
		(GrabbedEvent
			make: myNumber
			with: work
			with: author
			with: reason)!
*/
}
/**
 * Essential. The revise capability of the Work has been lost.
 */
public void released(FeWork work, int reason) {
	myManager.queueDetectorEvent((ReleasedEvent.make(myNumber, work, reason)));
/*
udanax-top.st:19846:CommStatusDetector methodsFor: 'triggering'!
{void} released: work {FeWork} with: reason {IntegerVar}
	"Essential. The revise capability of the Work has been lost."
	
	myManager queueDetectorEvent: 
		(ReleasedEvent
			make: myNumber
			with: work
			with: reason)!
*/
}
public static CommStatusDetector make(PromiseManager pm, int number, FeWork target) {
	return new CommStatusDetector(pm, number, target);
/*
udanax-top.st:19864:CommStatusDetector class methodsFor: 'creation'!
make: pm {PromiseManager} with: number {IntegerVar} with: target {FeWork}
	^self create: pm with: number with: target!
*/
}
public CommStatusDetector() {
/*

Generated during transformation
*/
}
public CommStatusDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
