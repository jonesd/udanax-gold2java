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
import info.dgjones.abora.gold.proman.RangeFilledEvent;
import info.dgjones.abora.gold.xcvr.Rcvr;
import info.dgjones.abora.gold.xpp.basic.Heaper;

public class RangeFilledEvent extends DetectorEvent {

	protected Heaper myFilling;
/*
udanax-top.st:16074:
DetectorEvent subclass: #RangeFilledEvent
	instanceVariableNames: 'myFilling {Heaper}'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Xanadu-proman'!
*/
/*
udanax-top.st:16078:
(RangeFilledEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
/*
udanax-top.st:16097:
RangeFilledEvent class
	instanceVariableNames: ''!
*/
/*
udanax-top.st:16100:
(RangeFilledEvent getOrMakeCxxClassDescription)
	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
*/
public static void initializeClassAttributes() {
	AboraSupport.findAboraClass(RangeFilledEvent.class).setAttributes( new Set().add("CONCRETE").add("NOTATYPE"));
/*

Generated during transformation: AddMethod
*/
}
public RangeFilledEvent(int detector, Heaper filling) {
	super(detector);
	myFilling = filling;
/*
udanax-top.st:16083:RangeFilledEvent methodsFor: 'creation'!
create: detector {IntegerVar} with: filling {Heaper}
	super create: detector.
	myFilling _ filling!
*/
}
/**
 * Send the message across the wire.
 */
public void trigger(PromiseManager pm) {
	pm.sendResponse(PromiseManager.rangeFilledResponse());
	pm.sendIntegerVar(detector());
	pm.sendPromise(myFilling);
/*
udanax-top.st:16089:RangeFilledEvent methodsFor: 'triggering'!
{void} trigger: pm {PromiseManager}
	"Send the message across the wire."
	
	pm sendResponse: PromiseManager rangeFilledResponse.
	pm sendIntegerVar: self detector.
	pm sendPromise: myFilling!
*/
}
public static DetectorEvent make(int detector, Heaper filling) {
	return new RangeFilledEvent(detector, filling);
/*
udanax-top.st:16105:RangeFilledEvent class methodsFor: 'creation'!
{DetectorEvent} make: detector {IntegerVar} with: filling {Heaper}
	^ self create: detector with: filling!
*/
}
public RangeFilledEvent() {
/*

Generated during transformation
*/
}
public RangeFilledEvent(Rcvr receiver) {
	super(receiver);
/*

Generated during transformation
*/
}
}
