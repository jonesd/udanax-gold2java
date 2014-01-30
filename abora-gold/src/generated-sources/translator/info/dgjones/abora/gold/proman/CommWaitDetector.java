/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.detect.FeWaitDetector;
import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.nkernel.FeServer;
import info.dgjones.abora.gold.proman.CommWaitDetector;
import info.dgjones.abora.gold.proman.DoneEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.xcvr.Rcvr;

/**
 * Send the detector events over comm.
 */
public class CommWaitDetector extends FeWaitDetector {

	protected PromiseManager myManager;
	protected int myNumber;
/*
udanax-top.st:19948:
FeWaitDetector subclass: #CommWaitDetector
	instanceVariableNames: '
		myManager {PromiseManager}
		myNumber {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:19954:
CommWaitDetector comment:
'Send the detector events over comm.'!
*/
/*
udanax-top.st:19956:
(CommWaitDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
/*
udanax-top.st:19978:
CommWaitDetector class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:19981:
(CommWaitDetector getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(CommWaitDetector.class).setAttributes( new Set().add("CONCRETE"));
/*

Generated during transformation: AddMethod
*/
}
public CommWaitDetector(PromiseManager pm, int number) {
	super();
	myManager = pm;
	myNumber = number;
/*
udanax-top.st:19961:CommWaitDetector methodsFor: 'creation'!
create: pm {PromiseManager} with: number {IntegerVar}
	super create.
	myManager _ pm.
	myNumber _ number!
*/
}
public void destruct() {
	FeServer.removeWaitDetector(this);
	super.destruct();
/*
udanax-top.st:19966:CommWaitDetector methodsFor: 'creation'!
{void} destruct
	FeServer removeWaitDetector: self.
	super destruct!
*/
}
/**
 * Essential.  Whatever I was waiting for has happened
 */
public void done() {
	myManager.queueDetectorEvent((DoneEvent.make(myNumber)));
/*
udanax-top.st:19972:CommWaitDetector methodsFor: 'triggering'!
{void} done
	"Essential.  Whatever I was waiting for has happened"
	
	myManager queueDetectorEvent: (DoneEvent make: myNumber)!
*/
}
public static CommWaitDetector make(PromiseManager pm, int number) {
	return new CommWaitDetector(pm, number);
/*
udanax-top.st:19986:CommWaitDetector class methodsFor: 'creation'!
make: pm {PromiseManager} with: number {IntegerVar}
	^self create: pm with: number!
*/
}
public CommWaitDetector() {
/*

Generated during transformation
*/
}
public CommWaitDetector(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
