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
import info.dgjones.abora.gold.detect.FeRevisionDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeEdition;
import info.dgjones.abora.gold.nkernel.FeWork;
import info.dgjones.abora.gold.proman.CommRevisionDetector;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.RevisedEvent;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Send the detector events over comm.
 */
public class CommRevisionDetector extends FeRevisionDetector {

	protected PromiseManager myManager;
	protected int myNumber;
	protected FeWork myTarget;
/*
udanax-top.st:19704:
FeRevisionDetector subclass: #CommRevisionDetector
	instanceVariableNames: '
		myManager {PromiseManager}
		myNumber {IntegerVar}
		myTarget {FeWork}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:19711:
CommRevisionDetector comment:
'Send the detector events over comm.'!
*/
/*
udanax-top.st:19713:
(CommRevisionDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19747:
CommRevisionDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19750:
(CommRevisionDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommRevisionDetector.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CommRevisionDetector(PromiseManager pm, int number, FeWork target) {
	super();
	myManager = pm;
	myNumber = number;
	myTarget = target;
/*
udanax-top.st:19718:CommRevisionDetector methodsFor: 'creation'!
create: pm {PromiseManager} with: number {IntegerVar} with: target {FeWork}
	super create.
	myManager _ pm.
	myNumber _ number.
	myTarget _ target!
*/
}
/**
 * Essential. The Work has been revised. Gives the Work, the current Edition, the
 * author ID who had it grabbed, the sequence number of the revision to the
 * Work, and the clock time on the Server (note that the clock time is only as
 * reliable as the Server's operating system, which is usually not very).
 */
public void revised(FeWork work, FeEdition contents, ID author, int time, int sequence) {
	myManager.queueDetectorEvent((RevisedEvent.make(myNumber, work, contents, author, time, sequence)));
/*
udanax-top.st:19726:CommRevisionDetector methodsFor: 'triggering'!
{void} revised: work {FeWork}
	with: contents {FeEdition}
	with: author {ID}
	with: time {IntegerVar}
	with: sequence {IntegerVar}
	
	"Essential. The Work has been revised. Gives the Work, the current Edition, the 
	author ID who had it grabbed, the sequence number of the revision to the 
	Work, and the clock time on the Server (note that the clock time is only as 
	reliable as the Server's operating system, which is usually not very)."
	myManager queueDetectorEvent: 
		(RevisedEvent
			make: myNumber
			with: work
			with: contents
			with: author
			with: time
			with: sequence)!
*/
}
public static CommRevisionDetector make(PromiseManager pm, int number, FeWork target) {
	return new CommRevisionDetector(pm, number, target);
/*
udanax-top.st:19755:CommRevisionDetector class methodsFor: 'creation'!
make: pm {PromiseManager} with: number {IntegerVar} with: target {FeWork}
	^self create: pm with: number with: target!
*/
}
public CommRevisionDetector() {
/*

Generated during transformation
*/
}
public CommRevisionDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
