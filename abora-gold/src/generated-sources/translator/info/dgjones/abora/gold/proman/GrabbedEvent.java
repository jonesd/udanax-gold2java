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
import info.dgjones.abora.gold.proman.GrabbedEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.x.PrimIntValue;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class GrabbedEvent extends DetectorEvent {

	protected Heaper myWork;
	protected Heaper myAuthor;
	protected int myReason;
/*
udanax-top.st:16032:
DetectorEvent subclass: #GrabbedEvent
	instanceVariableNames: '
		myWork {Heaper}
		myAuthor {Heaper}
		myReason {IntegerVar}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:16039:
(GrabbedEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:16063:
GrabbedEvent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16066:
(GrabbedEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(GrabbedEvent.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	pm.sendResponse(PromiseManager.grabbedResponse());
	pm.sendIntegerVar(detector());
	pm.sendPromise(myWork);
	pm.sendPromise(myAuthor);
	pm.sendIntegerVar(myReason);
	pm.sendPromise((PrimIntValue.make(myReason)));
/*
udanax-top.st:16044:GrabbedEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	pm sendResponse: PromiseManager grabbedResponse.
	pm sendIntegerVar: self detector.
	pm sendPromise: myWork.
	pm sendPromise: myAuthor.
	pm sendIntegerVar: myReason.
	pm sendPromise: (PrimIntValue make: myReason)!
*/
}
public GrabbedEvent(int detector, Heaper work, Heaper author, int reason) {
	super(detector);
	myWork = work;
	myAuthor = author;
	myReason = reason;
/*
udanax-top.st:16056:GrabbedEvent methodsFor: 'creation'!
create: detector {IntegerVar} with: work {Heaper} with: author {Heaper} with: reason {IntegerVar}
	super create: detector.
	myWork _ work.
	myAuthor _ author.
	myReason _ reason!
*/
}
public static DetectorEvent make(int detector, Heaper work, Heaper author, int reason) {
	return new GrabbedEvent(detector, work, author, reason);
/*
udanax-top.st:16071:GrabbedEvent class methodsFor: 'creation'!
{DetectorEvent} make: detector {IntegerVar} with: work {Heaper} with: author {Heaper} with: reason {IntegerVar}
	^self create: detector with: work with: author  with: reason!
*/
}
public GrabbedEvent() {
/*

Generated during transformation
*/
}
public GrabbedEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
