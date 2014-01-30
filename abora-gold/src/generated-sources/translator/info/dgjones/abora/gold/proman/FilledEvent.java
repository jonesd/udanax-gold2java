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
import info.dgjones.abora.gold.proman.FilledEvent;
import info.dgjones.abora.gold.proman.PromiseManager;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class FilledEvent extends DetectorEvent {

	protected Heaper myFilling;
/*
udanax-top.st:15998:
DetectorEvent subclass: #FilledEvent
	instanceVariableNames: 'myFilling {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:16002:
(FilledEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:16021:
FilledEvent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16024:
(FilledEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(FilledEvent.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	pm.sendResponse(PromiseManager.filledResponse());
	pm.sendIntegerVar(detector());
	pm.sendPromise(myFilling);
/*
udanax-top.st:16007:FilledEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	pm sendResponse: PromiseManager filledResponse.
	pm sendIntegerVar: self detector.
	pm sendPromise: myFilling!
*/
}
public FilledEvent(int detector, Heaper filling) {
	super(detector);
	myFilling = filling;
/*
udanax-top.st:16016:FilledEvent methodsFor: 'creation'!
create: detector {IntegerVar} with: filling {Heaper}
	super create: detector.
	myFilling _ filling!
*/
}
public static DetectorEvent make(int detector, Heaper filling) {
	return new FilledEvent(detector, filling);
/*
udanax-top.st:16029:FilledEvent class methodsFor: 'creation'!
{DetectorEvent} make: detector {IntegerVar} with: filling {Heaper}
	^ self create: detector with: filling!
*/
}
public FilledEvent() {
/*

Generated during transformation
*/
}
public FilledEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
