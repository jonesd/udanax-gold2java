/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */

package info.dgjones.abora.gold.proman;

import info.dgjones.abora.gold.java.AboraSupport;
import info.dgjones.abora.gold.java.missing.smalltalk.Set;
import info.dgjones.abora.gold.proman.DetectorEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.proman.ReleasedEvent;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class ReleasedEvent extends DetectorEvent {

	protected Heaper myWork;
	protected int myReason;
/*
udanax-top.st:16108:
DetectorEvent subclass: #ReleasedEvent
	instanceVariableNames: '
		myWork {Heaper}
		myReason {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:16114:
(ReleasedEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:16136:
ReleasedEvent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16139:
(ReleasedEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(ReleasedEvent.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public ReleasedEvent(int detector, Heaper work, int reason) {
	super(detector);
	myWork = work;
	myReason = reason;
/*
udanax-top.st:16119:ReleasedEvent methodsFor: 'creation'!
create: detector {IntegerVar} with: work {Heaper} with: reason {IntegerVar}
	super create: detector.
	myWork _ work.
	myReason _ reason!
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	pm.sendResponse(PromiseManager.releasedResponse());
	pm.sendIntegerVar(detector());
	pm.sendPromise(myWork);
	pm.sendIntegerVar(myReason);
	pm.sendPromise((PrimIntValue.make(myReason)));
/*
udanax-top.st:16126:ReleasedEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	pm sendResponse: PromiseManager releasedResponse.
	pm sendIntegerVar: self detector.
	pm sendPromise: myWork.
	pm sendIntegerVar: myReason.
	pm sendPromise: (PrimIntValue make: myReason)!
*/
}
public static DetectorEvent make(int detector, Heaper work, int reason) {
	return new ReleasedEvent(detector, work, reason);
/*
udanax-top.st:16144:ReleasedEvent class methodsFor: 'creation'!
{DetectorEvent} make: detector {IntegerVar} with: work {Heaper} with: reason {IntegerVar}
	^self create: detector with: work with: reason!
*/
}
public ReleasedEvent() {
/*

Generated during transformation
*/
}
public ReleasedEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
